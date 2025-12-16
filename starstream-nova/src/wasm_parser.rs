//! Take a WASM module and generate the IR for it.
//! This is done over two passes, the first generates mostly everything,
//! but leaves a few holes in place that have to be filled in afterwards,
//! since the values depend on the length of the output.
//!
//! What is unknownable is where to jump to in the case of
//! - calls
//! - br
//! - if-then-else
//!
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
use combine::easy::Errors;
use combine::error::Commit;
use combine::error::Format;
use combine::many;
use combine::optional;
use combine::parser;
use combine::parser::byte::byte;
use combine::value;

use crate::circuits;

macro_rules! l {
    () => {{ Format(format!("at {}:{}", line!(), column!(),)) }};
}

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
    byte(0x7F).message(l!()).map(|_| ())
}

fn valtype_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    numtype_p().message(l!())
}

fn memarg_offset_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = u32> {
    choice((u32_p().with(u32_p()), u32_p().skip(u32_p()).with(u32_p())))
}

#[derive(Clone, Copy, Debug)]
#[allow(non_camel_case_types)]
enum Instr {
    Unreachable,
    Nop,
    Block,
    Loop,
    If,
    Else,
    EndBlock,
    Br { l: u32 },
    BrIf { l: u32 },
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
        byte(0x0C)
            .with(u32_p().message(l!()))
            .map(|l| Instr::Br { l }),
        byte(0x0D)
            .with(u32_p().message(l!()))
            .map(|l| Instr::BrIf { l }),
        byte(0x0F).with(value(Instr::Return)),
        byte(0x10)
            .with(u32_p().message(l!()))
            .map(|fn_idx| Instr::Call { fn_idx }),
    ))
    .message(l!())
}

fn parametric_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        byte(0x1A).with(value(Instr::Drop)),
        byte(0x1B).with(value(Instr::Select)),
        byte(0x1C)
            .skip(list_p(valtype_p))
            .with(value(Instr::Select)),
    ))
    .message(l!())
}

fn variable_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        byte(0x20)
            .with(u32_p().message(l!()))
            .map(|local| Instr::Get { local }),
        byte(0x21)
            .with(u32_p().message(l!()))
            .map(|local| Instr::Set { local }),
        byte(0x22)
            .with(u32_p().message(l!()))
            .map(|local| Instr::Tee { local }),
        byte(0x23)
            .with(u32_p().message(l!()))
            .map(|global| Instr::GlobalGet { global }),
    ))
    .message(l!())
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
        ))
        .message(l!()),
        memarg_offset_p().message(l!()),
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
    byte(0x41)
        .message(l!())
        .with(sleb128(32).map(|n: i64| Instr::Const(n as i32)))
        .message(l!())
}

fn i32_unary_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    // choice((
    byte(0x45).message(l!()).with(value(Instr::Eqz))
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
    .message(l!())
}

fn blocktype_p<Input: Stream<Token = u8>>() -> impl Parser<Input> {
    choice((
        byte(0x40).map(|_| ()),
        byte(0x7F).map(|_| ()),
        sleb128(33).map(|_| ()),
    ))
    .message(l!())
}

fn control_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        byte(0x02).skip(blocktype_p()).with(value(Instr::Block)),
        byte(0x03).skip(blocktype_p()).with(value(Instr::Loop)),
        byte(0x04).skip(blocktype_p()).with(value(Instr::If)),
        byte(0x05).with(value(Instr::Else)),
        byte(0x0B).with(value(Instr::EndBlock)),
    ))
    .message(l!())
}

fn instr_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Instr> {
    choice((
        control_non_rec_p(),
        parametric_p(),
        variable_p(),
        memory_p(),
        const_p(),
        i32_unary_p(),
        i32_binary_p(),
        control_p(),
    ))
}

fn locals_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = u32> {
    u32_p().message(l!()).skip(valtype_p().message(l!()))
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
        .with((
            list_p(valtype_p).message(l!()),
            list_p(valtype_p).message(l!()),
        ))
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
    byte(0).with(u32_p()).map(Import::Fn)
}

fn import_i32_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Import> {
    byte(3)
        .message(l!())
        .skip(byte(0x7F).message(l!()))
        .skip(byte(0).message(l!()))
        .map(|_| Import::Global)
}

fn import_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Import> {
    name_p()
        .message(l!())
        .skip(name_p().message(l!()))
        .with(choice((import_fun_p(), import_i32_p())).message(l!()))
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
            .message(l!())
            .skip(version_p().message(l!()))
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
    CurrentAddr(i32),
    FnAddr(u32),
    Label { l: u32 },
    Else,
}

enum Control {
    Same,
    Else,
    EnterBlock,
    EnterLoop,
    Leave,
}

struct R {
    stack_diff: i32,
    control: Control,
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

fn code_of_instr(
    instr: Instr,
    stack_size: i32,
    Ctx {
        n_args,
        n_results,
        func_types,
        imports,
        globals,
    }: Ctx,
    out: impl FnMut(Datum) -> (),
) -> R {
    match instr {
        Instr::Unreachable => {
            [circuits::Instr::Unreachable as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: 0,
                control: Control::Same,
            }
        }
        Instr::Nop => {
            [circuits::Instr::Nop as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: 0,
                control: Control::Same,
            }
        }
        Instr::Block => R {
            stack_diff: 0,
            control: Control::EnterBlock,
        },
        Instr::Loop => R {
            stack_diff: 0,
            control: Control::EnterLoop,
        },
        Instr::Br { l } => {
            [
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::Label { l },
                Datum::Raw(circuits::Instr::Jump as i32),
            ]
            .into_iter()
            .for_each(out);
            R {
                stack_diff: 0,
                control: Control::Same,
            }
        }
        Instr::BrIf { l } => {
            [
                Datum::Raw(circuits::Instr::Not as i32),
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::Label { l },
                Datum::Raw(circuits::Instr::CondJump as i32),
            ]
            .into_iter()
            .for_each(out);
            R {
                stack_diff: 0,
                control: Control::Same,
            }
        }
        Instr::If => {
            [
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::Else,
                Datum::Raw(circuits::Instr::CondJump as i32),
            ]
            .into_iter()
            .for_each(out);
            R {
                stack_diff: -1,
                control: Control::EnterBlock,
            }
        }
        Instr::Else => {
            [
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::Label { l: 0 },
                Datum::Raw(circuits::Instr::Jump as i32),
            ]
            .into_iter()
            .for_each(out);
            R {
                stack_diff: 0,
                control: Control::Else,
            }
        }
        Instr::EndBlock => R {
            stack_diff: 0,
            control: Control::Leave,
        },
        Instr::Drop => {
            [circuits::Instr::Drop as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: -1,
                control: Control::Same,
            }
        }
        Instr::Const(c) => {
            [circuits::Instr::Const as i32, c]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: 1,
                control: Control::Same,
            }
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
            R {
                stack_diff: 1,
                control: Control::Same,
            }
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
            R {
                stack_diff: -1,
                control: Control::Same,
            }
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
            R {
                stack_diff: 0,
                control: Control::Same,
            }
        }
        Instr::GlobalGet { global } => {
            [circuits::Instr::Const as i32, globals[global as usize]]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: 1,
                control: Control::Same,
            }
        }
        // FIXME: implement offset support
        Instr::Load { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
        Instr::Load8_s { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
        Instr::Load8_u { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
        Instr::Load16_s { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
        Instr::Load16_u { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
        Instr::Store { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
        Instr::Store8 { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
        Instr::Store16 { offset } => {
            assert_eq!(offset, 0);
            unimplemented!()
        }
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
                control: Control::Same,
            }
        }
        Instr::Call { fn_idx } => {
            assert!(fn_idx >= imports.len() as u32);
            let fn_idx = fn_idx - imports.len() as u32;
            [
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::CurrentAddr(4),
                Datum::Raw(circuits::Instr::Const as i32),
                Datum::FnAddr(fn_idx),
                Datum::Raw(circuits::Instr::Jump as i32),
            ]
            .into_iter()
            .for_each(out);
            R {
                stack_diff: {
                    let (callee_n_args, callee_n_results) = func_types[fn_idx as usize];
                    callee_n_results as i32 - callee_n_args as i32
                },
                control: Control::Same,
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
                control: Control::Same,
            }
        }
        Instr::Select => {
            [circuits::Instr::Select as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: -2,
                control: Control::Same,
            }
        }
        Instr::Eqz => {
            [circuits::Instr::Not as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: 0,
                control: Control::Same,
            }
        }
        Instr::Eq => {
            [
                circuits::Instr::Neg as i32,
                circuits::Instr::Add as i32,
                circuits::Instr::Not as i32,
            ]
            .into_iter()
            .map(Datum::Raw)
            .for_each(out);
            R {
                stack_diff: -1,
                control: Control::Same,
            }
        }
        Instr::Ne => {
            [
                circuits::Instr::Neg as i32,
                circuits::Instr::Add as i32,
                circuits::Instr::Not as i32,
                circuits::Instr::Not as i32,
            ]
            .into_iter()
            .map(Datum::Raw)
            .for_each(out);
            R {
                stack_diff: -1,
                control: Control::Same,
            }
        }
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
            R {
                stack_diff: -1,
                control: Control::Same,
            }
        }
        Instr::Sub => {
            [circuits::Instr::Neg as i32, circuits::Instr::Add as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: -1,
                control: Control::Same,
            }
        }
        Instr::Mul => {
            [circuits::Instr::Mul as i32]
                .into_iter()
                .map(Datum::Raw)
                .for_each(out);
            R {
                stack_diff: -1,
                control: Control::Same,
            }
        }
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

enum Label {
    Resolved(usize),
    Unresolved,
}

fn unwrap<A, T, R, P, Q>(
    x: Result<A, Errors<T, R, P>>,
    transform_position: impl FnOnce(P) -> Q,
) -> A
where
    Q: Display,
    T: Display,
    R: Display,
{
    match x {
        Ok(x) => x,
        Err(Errors { position, errors }) => {
            let position = transform_position(position);
            panic!("failed: {}", Errors { position, errors })
        }
    }
}

fn parse_pretty<Input: Stream<Token = u8>, O, P, Partial: Default>(
    input: &mut combine::easy::Stream<Input>,
    mut parser: impl Parser<combine::easy::Stream<Input>, Output = O, PartialState = Partial>,
    transform_position: impl FnOnce(Input::Position) -> P,
) -> O
where
    Input::Error: Debug,
    Input::Range: PartialEq + Debug + Display,
    Input::Position: Default + Debug + Display,
    P: Display,
{
    unwrap(
        parser.parse_with_state(input, &mut Default::default()),
        transform_position,
    )
}

#[derive(Clone, Copy)]
struct Ctx<'a> {
    n_args: i32,
    n_results: i32,
    func_types: &'a [(u32, u32)],
    imports: &'a [(u32, u32)],
    globals: &'a [i32],
}

struct C {
    label_idx: usize,
    else_pos: Option<usize>,
}
struct State {
    stack_size: i32,
    labels: Vec<Label>,
    control_stack: Vec<C>,
    label_uses: Vec<(usize, usize)>,
}

// FIXME: support br as return
fn iter_instrs<I: Iterator<Item = Instr>>(
    mut iter: I,
    ctx: Ctx<'_>,
    mut state: State,
    v: &mut Vec<i32>,
    mut call: impl FnMut((usize, u32)) -> (),
) -> (I, State) {
    let instr = iter.next().expect("expected instruction");
    eprintln!("got {instr:?}");
    let mut else_pos = None;
    let R {
        stack_diff,
        control,
    } = code_of_instr(instr, state.stack_size, ctx, |d| {
        let x = match d {
            Datum::Raw(n) => n,
            Datum::CurrentAddr(offset) => v.len() as i32 + offset,
            Datum::FnAddr(fn_idx) => {
                call((v.len(), fn_idx));
                0 // placeholder
            }
            Datum::Label { l } => {
                state.label_uses.push((
                    v.len(),
                    state.control_stack[state.control_stack.len() - l as usize].label_idx,
                ));
                0 // placeholder
            }
            Datum::Else => {
                else_pos = Some(v.len());
                0 // placeholder
            }
        };
        v.push(x);
    });
    state.stack_size += stack_diff;
    match control {
        Control::Same => iter_instrs(iter, ctx, state, v, call),
        Control::Else => {
            let C {
                label_idx: _,
                else_pos,
            } = state
                .control_stack
                .last_mut()
                .expect("else without if (or loop or block)");
            let p = else_pos.take().expect("else where no else expected");
            v[p] = v.len() as i32;
            iter_instrs(iter, ctx, state, v, call)
        }
        // end of function
        Control::Leave if state.control_stack.is_empty() => {
            let R {
                stack_diff: _,
                control,
            } = code_of_instr(Instr::Return, state.stack_size, ctx, |d| {
                let x = match d {
                    Datum::Raw(n) => n,
                    _ => unreachable!(),
                };
                v.push(x);
            });
            match control {
                Control::Same => {}
                _ => unreachable!(),
            }
            (iter, state)
        }
        Control::Leave => {
            let C {
                label_idx,
                else_pos,
            } = state
                .control_stack
                .pop()
                .expect("0x0B without corresponding block or loop or if");
            match state.labels[label_idx] {
                Label::Resolved(_) => {}
                Label::Unresolved => state.labels[label_idx] = Label::Resolved(v.len()),
            }
            match else_pos {
                None => {}
                // If there is an unfilled "else" branch,
                // then we fill it as we'd fill `br 0`.
                Some(pos) => v[pos] = v.len() as i32,
            }
            iter_instrs(iter, ctx, state, v, call)
        }
        Control::EnterBlock => {
            state.control_stack.push(C {
                label_idx: state.labels.len(),
                else_pos,
            });
            state.labels.push(Label::Unresolved);
            iter_instrs(iter, ctx, state, v, call)
        }
        Control::EnterLoop => {
            assert!(else_pos.is_none(), "impossible");
            state.control_stack.push(C {
                label_idx: state.labels.len(),
                else_pos,
            });
            state.labels.push(Label::Resolved(v.len()));
            iter_instrs(iter, ctx, state, v, call)
        }
    }
}

fn parse_function<Input: Stream<Token = u8>, P>(
    input: &mut combine::easy::Stream<Input>,
    ctx: Ctx<'_>,
    v: &mut Vec<i32>,
    call: impl FnMut((usize, u32)) -> (),
    transform_position: impl FnOnce(Input::Position) -> P,
) where
    Input::Error: Debug,
    Input::Range: PartialEq + Debug + Display,
    Input::Position: Default + Debug + Display,
    P: Display,
{
    eprintln!("parsing function");
    let mut transform_position = Some(transform_position);
    let (_function_size, locals): (u32, Vec<u32>) =
        parse_pretty(input, (leb128(), list_p(locals_p)), |p| {
            (transform_position.take().unwrap())(p)
        });
    let n_locals = locals.into_iter().fold(0, Add::add) as i32;
    if n_locals != 0 {
        v.push(circuits::Instr::Alloc as i32);
    }
    let iter = instr_p().iter(input);
    let stack_size: i32 = n_locals;
    let labels = vec![];
    let control_stack: Vec<C> = vec![];
    let label_uses = vec![];
    let state = State {
        stack_size,
        labels,
        control_stack,
        label_uses,
    };
    let (iter, state) = iter_instrs(iter, ctx, state, v, call);
    let ((), _commit) = unwrap(
        iter.into_result(()).map_err(|e| e.into_inner().error),
        |p| (transform_position.take().unwrap())(p),
    );
    for (pos, label_idx) in state.label_uses {
        v[pos] = match state.labels[label_idx] {
            Label::Unresolved => panic!("there should be no unresolved labels left"),
            Label::Resolved(target) => target as i32,
        }
    }
}

// FIXME: handle partial parsing correctly
pub fn parse<Input: Stream<Token = u8>, P>(
    input: Input,
    globals: &[i32],
    transform_position: impl FnOnce(Input::Position) -> P,
) -> Vec<i32>
where
    Input::Error: Debug,
    Input::Range: PartialEq + Debug + Display,
    Input::Position: Default + Debug + Display,
    P: Display,
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
    let mut input = combine::easy::Stream(input);
    let mut transform_position = Some(transform_position);
    let (_, _codesec_size, n_functions): (_, u32, u32) =
        parse_pretty(&mut input, (byte(10), leb128(), leb128()), |p| {
            (transform_position.take().unwrap())(p)
        });
    assert_eq!(n_functions as usize, func_types.len());
    // NB: we must have an initial 0 for cond_lookup to work
    let mut v: Vec<i32> = vec![0];
    let mut function_calls_to_patch: Vec<(usize, u32)> = Vec::new();
    let mut function_info: Vec<FunctionInfo> = Vec::new();
    for (n_args, n_results) in &func_types {
        function_info.push(FunctionInfo { pc: v.len() });
        parse_function(
            &mut input,
            Ctx {
                n_args: *n_args as i32,
                n_results: *n_results as i32,
                func_types: func_types.as_slice(),
                imports: imports.as_slice(),
                globals,
            },
            &mut v,
            |call| function_calls_to_patch.push(call),
            |p| (transform_position.take().unwrap())(p),
        );
    }
    for (patch_location, fn_idx) in function_calls_to_patch {
        v[patch_location] = function_info[fn_idx as usize].pc as i32;
    }
    let ((), _) = postlude().parse(input).unwrap();
    v
}
