//! Take a WASM module and generate the IR for it.
//! This is done over two passes, the first generates mostly everything,
//! but leaves a few holes in place that have to be filled in afterwards,
//! since the values depend on the length of the output.
//!
//! What is unknownable is where to jump to in the case of
//! - calls
//! - br
//! - if-then-else
//! The rest can be determined from the instructions seen until then.
//!
//! We could however delay "emitting" certain blocks until we have figured out
//! the content exactly.
//!
//! We could also handle calls by predetermining the address of each function,
//! by e.g. guessing an upper bound on the function size, say 2^12,
//! then a call to function n is a jump to n * 2^12, roughly.
//!
//! This might cause more work in committing to the values,
//! since they're bigger, but also in the discrete log setting,
//! we could reduce the work by precomputing the value of g^(2^12) for
//! each generator g.
//!
//! Here, however, we choose to opt for patching the result post-parse for simplicity.

use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Add;
use std::ops::BitOrAssign;
use std::ops::Shl;

use combine::EasyParser;
use combine::Parser;
use combine::Stream;
use combine::any;
use combine::choice;
use combine::count;
use combine::error::Commit;
use combine::many;
use combine::optional;
use combine::parser;
use combine::parser::byte::byte;
use combine::value;

use crate::circuits;

fn leb128<N: Default + From<u8> + Shl<u32, Output = N> + BitOrAssign, Input: Stream<Token = u8>>()
-> impl Parser<Input, Output = N, PartialState = ()> {
    parser(move |input: &mut Input| {
        let mut val: N = N::default();
        let mut shift: u32 = 0;
        loop {
            let (b, _) = any().parse_stream(input).into_result()?;
            val |= N::from(b & 0x7f) << shift;
            shift += 7;
            /* FIXME: add this check back
            if shift > n {
                panic!("invalid leb128");
            }
            */
            if b & 0x80 == 0 {
                break;
            }
        }
        Ok((val, Commit::Commit(())))
    })
}

fn sleb128<Input: Stream<Token = u8>>(
    n: u32,
) -> impl Parser<Input, Output = i64, PartialState = ()> {
    parser(move |input: &mut Input| {
        let mut val: i64 = 0;
        let mut shift: u32 = 0;
        let b = loop {
            let (b, _) = any().parse_stream(input).into_result()?;
            val |= ((b & 0x7f) as i64) << shift;
            shift += 7;
            if shift > n {
                panic!("invalid sleb128");
            }
            if b & 0x80 == 0 {
                break b;
            }
        };
        if shift < n && (b & 0x40 != 0) {
            val |= !0i64 << shift;
        }
        Ok((val, Commit::Commit(())))
    })
}

fn u32_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = u32> {
    leb128()
}

fn list_p<Input: Stream<Token = u8>, P: Parser<Input>>(
    mut x_p: impl FnMut() -> P,
) -> impl Parser<Input, Output = Vec<P::Output>> {
    leb128().then(move |len: u32| count(len as usize, x_p()))
}

fn sized_p<Input: Stream<Token = u8>, P: Parser<Input>>(
    x_p: P,
) -> impl Parser<Input, Output = P::Output> {
    leb128::<u32, _>().with(x_p)
}

fn bytestring_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    list_p(any).map(|_| ())
}

fn name_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    bytestring_p()
}

fn numtype_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    byte(0x7F).map(|_| ())
}

fn valtype_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    numtype_p()
}

fn memarg_offset_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = u32> {
    choice((u32_p().with(u32_p()), u32_p().skip(u32_p()).with(u32_p())))
}

#[derive(Clone, Copy, Debug)]
#[allow(non_camel_case_types)]
enum Instr {
    Unreachable,
    Nop,
    Drop,
    Const(i32),
    Get { local: u32 },
    Set { local: u32 },
    Tee { local: u32 },
    GlobalGet { global: u32 },
    Load { offset: u32 },
    Load8_s { offset: u32 },
    Load8_u { offset: u32 },
    Load16_s { offset: u32 },
    Load16_u { offset: u32 },
    Store { offset: u32 },
    Store8 { offset: u32 },
    Store16 { offset: u32 },
    Return,
    Call { fn_idx: u32 },
    Select,
    Eqz,
    Eq,
    Ne,
    Lt_s,
    Lt_u,
    Gt_s,
    Gt_u,
    Le_s,
    Le_u,
    Ge_s,
    Ge_u,
    Add,
    Sub,
    Mul,
    Div_s,
    Div_u,
    Rem_s,
    Rem_u,
    And,
    Or,
    Xor,
    Shl,
    Shr_s,
    Shr_u,
    Rotl,
    Rotr,
}

fn control_non_rec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        byte(0x00).with(value(Instr::Unreachable)),
        byte(0x01).with(value(Instr::Nop)),
        // byte(0x0C).with(u32_p()).map(|l| Instr::Jump { l }),
        // byte(0x0D).with(u32_p()).map(|l| Instr::Jump { l }),
        byte(0x0F).with(value(Instr::Return)),
        byte(0x10)
            .with(u32_p())
            .map(|fn_idx| Instr::Call { fn_idx }),
    ))
}

fn parametric_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        byte(0x1A).with(value(Instr::Drop)),
        byte(0x1B).with(value(Instr::Select)),
        byte(0x1C)
            .skip(list_p(valtype_p))
            .with(value(Instr::Select)),
    ))
}

fn variable_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        byte(0x20).with(u32_p()).map(|local| Instr::Get { local }),
        byte(0x21).with(u32_p()).map(|local| Instr::Set { local }),
        byte(0x22).with(u32_p()).map(|local| Instr::Tee { local }),
        byte(0x23)
            .with(u32_p())
            .map(|global| Instr::GlobalGet { global }),
    ))
}

fn memory_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    #[derive(Clone, Copy, Debug)]
    #[allow(non_camel_case_types)]
    enum E {
        Load,
        Load8_s,
        Load8_u,
        Load16_s,
        Load16_u,
        Store,
        Store8,
        Store16,
    }
    (
        choice((
            byte(0x28).with(value(E::Load)),
            byte(0x2C).with(value(E::Load8_s)),
            byte(0x2D).with(value(E::Load8_u)),
            byte(0x2E).with(value(E::Load16_s)),
            byte(0x2F).with(value(E::Load16_u)),
            byte(0x36).with(value(E::Store)),
            byte(0x3A).with(value(E::Store8)),
            byte(0x3B).with(value(E::Store16)),
        )),
        memarg_offset_p(),
    )
        .map(|(i, offset)| match i {
            E::Load => Instr::Load { offset },
            E::Load8_s => Instr::Load8_s { offset },
            E::Load8_u => Instr::Load8_u { offset },
            E::Load16_s => Instr::Load16_s { offset },
            E::Load16_u => Instr::Load16_u { offset },
            E::Store => Instr::Store { offset },
            E::Store8 => Instr::Store8 { offset },
            E::Store16 => Instr::Store16 { offset },
        })
}

fn const_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    byte(0x41).with(sleb128(32).map(|n: i64| Instr::Const(n as i32)))
}

fn i32_unary_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    // choice((
    byte(0x45).with(value(Instr::Eqz))
    /*
    byte(0x67).with(value(Instr::Clz)),
    byte(0x68).with(value(Instr::Ctz)),
    byte(0x69).with(value(Instr::Popcnt)),
    */
    // ))
}

fn i32_binary_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        byte(0x46).with(value(Instr::Eq)),
        byte(0x47).with(value(Instr::Ne)),
        byte(0x48).with(value(Instr::Lt_s)),
        byte(0x49).with(value(Instr::Lt_u)),
        byte(0x4A).with(value(Instr::Gt_s)),
        byte(0x4B).with(value(Instr::Gt_u)),
        byte(0x4C).with(value(Instr::Le_s)),
        byte(0x4D).with(value(Instr::Le_u)),
        byte(0x4E).with(value(Instr::Ge_s)),
        byte(0x4F).with(value(Instr::Ge_u)),
        byte(0x6A).with(value(Instr::Add)),
        byte(0x6B).with(value(Instr::Sub)),
        byte(0x6C).with(value(Instr::Mul)),
        byte(0x6D).with(value(Instr::Div_s)),
        byte(0x6E).with(value(Instr::Div_u)),
        byte(0x6F).with(value(Instr::Rem_s)),
        byte(0x70).with(value(Instr::Rem_u)),
        byte(0x71).with(value(Instr::And)),
        byte(0x72).with(value(Instr::Or)),
        byte(0x73).with(value(Instr::Xor)),
        byte(0x74).with(value(Instr::Shl)),
        byte(0x75).with(value(Instr::Shr_s)),
        byte(0x76).with(value(Instr::Shr_u)),
        byte(0x77).with(value(Instr::Rotl)),
        byte(0x78).with(value(Instr::Rotr)),
    ))
}

fn non_rec_instr_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        control_non_rec_p(),
        parametric_p(),
        variable_p(),
        memory_p(),
        const_p(),
        i32_unary_p(),
        i32_binary_p(),
    ))
}

/*

fn block_p<Input: Stream<Token = u8>>(out: impl FnMut(Instr)) -> impl Parser<Input, Output = ()> {
    byte(0x02)
        .with(blocktype_p())
        .with(many::<(), _, _>(lazy(instr_p)))
        .skip(byte(0x0B))
        .map(|_| ())
}

fn loop_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    byte(0x03)
        .with(blocktype_p())
        .with(many::<(), _, _>(lazy(|| instr_p())))
        .skip(byte(0x0B))
        .map(|_| ())
}

fn if_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    byte(0x04)
        .with(blocktype_p())
        .with(many::<(), _, _>(lazy(|| instr_p())))
        .with(optional(
            byte(0x05).with(many::<(), _, _>(lazy(|| instr_p()))),
        ))
        .skip(byte(0x0B))
        .map(|_| ())
}

parser! {
    fn instr_p[Input]()(Input) -> () where [Input: Stream<Token = u8>]
    // { choice((non_rec_instr_p(), block_p(), loop_p(), if_p())) }
    { lazy(block_p) }
}

fn expr_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = impl DiffList> {
    many(instr_p()).skip(byte(0x0B))
}
*/

fn locals_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = u32> {
    u32_p().skip(valtype_p())
}

fn magic_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    (byte(0x00), byte(0x61), byte(0x73), byte(0x6D)).map(|_| ())
}

fn version_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    (byte(0x01), byte(0x00), byte(0x00), byte(0x00)).map(|_| ())
}

fn section_p<Input: Stream<Token = u8>, P: Parser<Input>>(
    id: u8,
    content_p: P,
) -> impl Parser<Input, Output = P::Output> {
    byte(id).with(sized_p(content_p))
}

fn custom_sections_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    many(byte(0).with(bytestring_p()))
}

fn unknown_section_p<Input: Stream<Token = u8>>(id: u8) -> impl Parser<Input, Output = ()> {
    optional(byte(id).with(bytestring_p())).map(|_| ())
}

fn type_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = (u32, u32)> {
    byte(0x60)
        .with((list_p(valtype_p), list_p(valtype_p)))
        // FIXME: these were originally u32,
        // somehow recover typing without `as`
        .map(|(i, o)| (i.len() as u32, o.len() as u32))
}

// The type section should be a simple list of function types and nothing more,
// otherwise this will fail.
// It will parse the data for `prelude`, which will then rearrange it according to the type
// indices of the function section.
fn typesec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Vec<(u32, u32)>> {
    section_p(1, list_p(type_p))
}

enum Import {
    Fn(u32),
    Global,
}

fn import_fun_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Import> {
    name_p()
        .skip(name_p())
        .skip(byte(0))
        .with(u32_p())
        .map(Import::Fn)
}

fn import_i32_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Import> {
    name_p()
        .skip(name_p())
        .skip(byte(3))
        .skip(byte(0x7E))
        .skip(byte(0))
        .map(|_| Import::Global)
}

fn import_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Import> {
    choice((import_fun_p(), import_i32_p()))
}

fn importsec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Vec<Import>> {
    section_p(2, list_p(import_p))
}

fn funcsec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Vec<u32>> {
    section_p(3, list_p(u32_p))
}

// returns the list of argument and result lengths of each function
fn prelude<Input: Stream<Token = u8>>()
-> impl Parser<Input, Output = (u32, Vec<(u32, u32)>, Vec<(u32, u32)>)> {
    (
        magic_p()
            .skip(version_p())
            .skip(custom_sections_p())
            .with(typesec_p())
            .skip(custom_sections_p()),
        importsec_p().skip(custom_sections_p()),
        funcsec_p(),
    )
        .skip(custom_sections_p())
        .skip(unknown_section_p(4))
        .skip(custom_sections_p())
        .skip(unknown_section_p(5))
        .skip(custom_sections_p())
        .skip(unknown_section_p(6))
        .skip(custom_sections_p())
        .skip(unknown_section_p(7))
        .skip(custom_sections_p())
        .skip(unknown_section_p(8))
        .skip(custom_sections_p())
        .skip(unknown_section_p(9))
        .skip(custom_sections_p())
        .map(|(types, import_idxs, func_idxs)| {
            let funcs = func_idxs
                .into_iter()
                .map(|idx| types[idx as usize])
                .collect();
            let mut global_counter = 0;
            let imports = import_idxs
                .into_iter()
                .filter_map(|import| match import {
                    Import::Fn(idx) => Some(types[idx as usize]),
                    Import::Global => {
                        global_counter += 1;
                        None
                    }
                })
                .collect();
            (global_counter, imports, funcs)
        })
}

fn postlude<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    custom_sections_p()
        .skip(unknown_section_p(11))
        .skip(custom_sections_p())
}

struct FunctionInfo {
    pc: usize,
}

enum Datum {
    Raw(i32),
    CurrentAddr,
    FnAddr(u32),
}

struct R {
    stack_diff: i32,
}

// pc
const RETURN_INFO_SIZE: i32 = 1;

fn calculate_local_idx(local: u32, stack_size: i32, n_args: i32) -> i32 {
    let local = local as i32;
    if local < n_args {
        stack_size + RETURN_INFO_SIZE + n_args - local
    } else {
        stack_size - local + n_args
    }
}

struct StackSize(i32);

fn instr_get_set(
    instr: circuits::Instr,
    local: u32,
    StackSize(stack_size): StackSize,
    n_args: i32,
    out: impl FnMut(Datum) -> (),
    min_idx: i32,
) {
    let idx = calculate_local_idx(local, stack_size, n_args);
    assert!(idx >= min_idx);
    [instr as i32, idx]
        .into_iter()
        .map(Datum::Raw)
        .for_each(out);
}

fn handle_instruction(
    instr: Instr,
    stack_size: i32,
    n_args: i32,
    n_results: i32,
    func_types: &[(u32, u32)],
    imports: &[(u32, u32)],
    globals: &[i32],
    out: impl FnMut(Datum) -> (),
) -> R {
    match instr {
        Instr::Unreachable => {
            [circuits::Instr::Unreachable as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R { stack_diff: 0 }
        }
        Instr::Nop => {
            [circuits::Instr::Nop as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R { stack_diff: 0 }
        }
        Instr::Drop => {
            [circuits::Instr::Drop as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R { stack_diff: -1 }
        }
        Instr::Const(c) => {
            [circuits::Instr::Const as i32, c]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R { stack_diff: 1 }
        }
        Instr::Get { local } => {
            instr_get_set(
                circuits::Instr::Get,
                local,
                StackSize(stack_size),
                n_args,
                out,
                1,
            );
            R { stack_diff: 1 }
        }
        Instr::Set { local } => {
            instr_get_set(
                circuits::Instr::Set,
                local,
                StackSize(stack_size),
                n_args,
                out,
                2,
            );
            R { stack_diff: -1 }
        }
        Instr::Tee { local } => {
            let idx = calculate_local_idx(local, stack_size + 1, n_args);
            [
                (circuits::Instr::Get as i32),
                (1),
                (circuits::Instr::Set as i32),
                idx,
            ]
            .into_iter()
            .map(Datum::Raw)
            .for_each(out);
            R { stack_diff: 0 }
        }
        Instr::GlobalGet { global } => {
            [circuits::Instr::Const as i32, globals[global as usize]]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R { stack_diff: 1 }
        }
        Instr::Load { offset: _ } => unimplemented!(),
        Instr::Load8_s { offset: _ } => unimplemented!(),
        Instr::Load8_u { offset: _ } => unimplemented!(),
        Instr::Load16_s { offset: _ } => unimplemented!(),
        Instr::Load16_u { offset: _ } => unimplemented!(),
        Instr::Store { offset: _ } => unimplemented!(),
        Instr::Store8 { offset: _ } => unimplemented!(),
        Instr::Store16 { offset: _ } => unimplemented!(),
        Instr::Call { fn_idx } if (fn_idx as usize) < imports.len() => {
            let (host_n_args, host_n_results) = imports[fn_idx as usize];
            [
                Datum::Raw(circuits::Instr::InitHostCall as i32),
                Datum::Raw(fn_idx as i32),
            ]
            .into_iter()
            .chain((0..host_n_args).map(|_| Datum::Raw(circuits::Instr::ToHost as i32)))
            .chain((0..host_n_results).map(|_| Datum::Raw(circuits::Instr::FromHost as i32)))
            .for_each(out);
            R {
                stack_diff: host_n_results as i32 - host_n_args as i32,
            }
        }
        Instr::Call { fn_idx } => {
            assert!(fn_idx >= imports.len() as u32);
            let fn_idx = fn_idx - imports.len() as u32;
            [
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::CurrentAddr,
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::FnAddr(fn_idx),
                Datum::Raw(circuits::Instr::Jump as i32),
            ]
            .into_iter()
            .for_each(out);
            R {
                stack_diff: match func_types[fn_idx as usize] {
                    (callee_n_args, callee_n_results) => {
                        callee_n_results as i32 - callee_n_args as i32
                    }
                },
            }
        }
        Instr::Return => {
            (0..n_results)
                .map(|_| circuits::Instr::ToHelper as i32)
                .chain((0..stack_size - n_results).map(|_| circuits::Instr::Drop as i32))
                .chain(Some(circuits::Instr::SetReg as i32))
                .chain((0..n_args).map(|_| circuits::Instr::Drop as i32))
                .chain((0..n_results).map(|_| circuits::Instr::FromHelper as i32))
                .chain(Some(circuits::Instr::GetReg as i32))
                .chain(Some(circuits::Instr::Jump as i32))
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: -stack_size,
            }
        }
        Instr::Select => {
            [circuits::Instr::Select as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R { stack_diff: -2 }
        }
        Instr::Eqz => unimplemented!(),
        Instr::Eq => unimplemented!(),
        Instr::Ne => unimplemented!(),
        Instr::Lt_s => unimplemented!(),
        Instr::Lt_u => unimplemented!(),
        Instr::Gt_s => unimplemented!(),
        Instr::Gt_u => unimplemented!(),
        Instr::Le_s => unimplemented!(),
        Instr::Le_u => unimplemented!(),
        Instr::Ge_s => unimplemented!(),
        Instr::Ge_u => unimplemented!(),
        Instr::Add => {
            [circuits::Instr::Add as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R { stack_diff: -1 }
        }
        Instr::Sub => unimplemented!(),
        Instr::Mul => unimplemented!(),
        Instr::Div_s => unimplemented!(),
        Instr::Div_u => unimplemented!(),
        Instr::Rem_s => unimplemented!(),
        Instr::Rem_u => unimplemented!(),
        Instr::And => unimplemented!(),
        Instr::Or => unimplemented!(),
        Instr::Xor => unimplemented!(),
        Instr::Shl => unimplemented!(),
        Instr::Shr_s => unimplemented!(),
        Instr::Shr_u => unimplemented!(),
        Instr::Rotl => unimplemented!(),
        Instr::Rotr => unimplemented!(),
    }
}

// FIXME: handle partial parsing correctly
pub fn parse<Input: Stream<Token = u8>>(input: Input, globals: &[i32]) -> Vec<i32>
where
    Input::Error: Debug,
    Input::Range: PartialEq + Debug + Display,
    Input::Position: Default + Debug + Display,
{
    let ((n_globals, imports, func_types), input) = match prelude().easy_parse(input) {
        Ok(r) => r,
        Err(e) => {
            panic!("{}", e);
        }
    };
    if n_globals as usize != globals.len() {
        panic!("need {} globals, got {}", globals.len(), n_globals);
    }
    let (_, input) = byte(10).parse(input).unwrap(); // codesec section id
    let (_codesec_size, input) = leb128::<u32, _>().parse(input).unwrap();
    let (n_functions, input) = leb128::<u32, _>().parse(input).unwrap();
    assert_eq!(n_functions as usize, func_types.len());
    // NB: we must have an initial 0 for cond_lookup to work
    let mut v: Vec<i32> = vec![0];
    let mut function_calls_to_patch: Vec<(usize, u32)> = Vec::new();
    let mut function_info: Vec<FunctionInfo> = Vec::new();
    let mut input = input;
    for (n_args, n_results) in &func_types {
        let n_args = *n_args as i32;
        let n_results = *n_results as i32;
        let _function_size: u32 = leb128().parse_with_state(&mut input, &mut ()).unwrap();
        let n_locals = list_p(locals_p)
            .parse_with_state(&mut input, &mut Default::default())
            .unwrap()
            .into_iter()
            .fold(0, Add::add) as i32;
        function_info.push(FunctionInfo { pc: v.len() });
        let mut stack_size: i32 = n_locals;
        let mut iter = non_rec_instr_p().iter(&mut input);
        for instr in &mut iter {
            let R { stack_diff } = handle_instruction(
                instr,
                stack_size,
                n_args,
                n_results,
                func_types.as_slice(),
                imports.as_slice(),
                globals,
                |d| match d {
                    Datum::Raw(n) => v.push(n),
                    Datum::CurrentAddr => v.push(v.len() as i32),
                    Datum::FnAddr(fn_idx) => {
                        function_calls_to_patch.push((v.len(), fn_idx));
                        v.push(0); // placeholder
                    }
                },
            );
            stack_size += stack_diff;
        }
        iter.into_result(()).unwrap();
        byte(0x0B).parse_with_state(&mut input, &mut ()).unwrap();
    }
    for (patch_location, fn_idx) in function_calls_to_patch {
        v[patch_location] = function_info[fn_idx as usize].pc as i32;
    }
    let ((), _) = postlude().parse(input).unwrap();
    v
}
