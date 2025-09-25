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
//! Here, however, we choose to opt for a two-pass approach to avoid this problem.

use std::fmt::Debug;
use std::ops::Add;
use std::ops::BitOrAssign;
use std::ops::Shl;

use combine::Parser;
use combine::Stream;
use combine::any;
use combine::choice;
use combine::count;
use combine::eof;
use combine::error::Commit;
use combine::many;
use combine::optional;
use combine::parser;
use combine::parser::byte::byte;
use combine::parser::function;
use combine::value;
// FIXME: do we need this?
use combine::parser::combinator::lazy;

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
    byte(id).with(bytestring_p())
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

fn funcsec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Vec<u32>> {
    section_p(3, list_p(u32_p))
}

// returns the list of argument and result lengths of each function
fn prelude<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = Vec<(u32, u32)>> {
    (
        magic_p()
            .skip(version_p())
            .skip(custom_sections_p())
            .with(typesec_p())
            .skip(custom_sections_p())
            .skip(unknown_section_p(2))
            .skip(custom_sections_p()),
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
        .map(|(types, idxs)| {
            let mut out = Vec::new();
            for idx in idxs {
                out.push(types[idx as usize])
            }
            out
        })
}

fn postlude<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    custom_sections_p()
        .skip(unknown_section_p(11))
        .skip(custom_sections_p())
}

// FIXME: handle partial parsing correctly
pub fn parse<Input: Stream<Token = u8>>(input: Input) -> Vec<i32>
where
    Input::Error: Debug,
{
    let (func_types, input) = prelude().parse(input).unwrap();
    let (_, input) = byte(10).parse(input).unwrap(); // codesec section id
    let (_codesec_size, input) = leb128::<u32, _>().parse(input).unwrap();
    let (n_functions, input) = leb128::<u32, _>().parse(input).unwrap();
    assert_eq!(n_functions as usize, func_types.len());
    let mut v: Vec<i32> = Vec::new();
    // A call is 7 cells in the code,
    // alloc <n_locals> const <target pc_namespace> const <target pc> jump,
    // and we have to patch in the current 3 values, which we only know
    // after finishing parsing the whole module(s).
    let mut function_calls_to_patch: Vec<(usize, u32)> = Vec::new();
    let mut input = input;
    for (n_args, n_results) in func_types {
        let _function_size: u32 = leb128().parse_with_state(&mut input, &mut ()).unwrap();
        let n_locals = list_p(locals_p)
            .parse_with_state(&mut input, &mut Default::default())
            .unwrap()
            .into_iter()
            .fold(0, Add::add);
        // One for each argument, one for each local,
        // and two for the pc_namespace and pc to jump back to.
        // They are all allocated by the caller, albeit the locals are to be `0` by convention.
        let n_frame_vars = n_args + n_locals + 2;
        // the ith local is stored in the stack at sp - stack_size - n_frame_vars + i,
        // where sp equals the length of the stack at runtime, and stack_size
        // is the number of entries used up by the current function.
        // circuits::Instr::Get gets an arbitrary element from the stack relative to sp,
        // such that when we want to access the ith local, we put in `stack_size - n_frame_vars + i`.
        // NB: the 0th local is the first argument, the nth argument is the nth local, and so on.
        let mut stack_size: u32 = 0;
        let mut iter = non_rec_instr_p().iter(&mut input);
        for instr in iter {
            match instr {
                Instr::Unreachable => {
                    v.push(circuits::Instr::Unreachable as i32);
                }
                Instr::Nop => {
                    v.push(circuits::Instr::Nop as i32);
                }
                Instr::Drop => {
                    v.push(circuits::Instr::Drop as i32);
                    stack_size -= 1;
                }
                Instr::Const(c) => {
                    v.push(circuits::Instr::Const as i32);
                    v.push(c);
                    stack_size += 1;
                }
                Instr::Get { local } => {
                    let idx = stack_size + n_frame_vars - local;
                    v.push(circuits::Instr::Get as i32);
                    assert!(idx > 0);
                    v.push(idx as i32);
                    stack_size += 1;
                }
                Instr::Set { local } => {
                    let idx = stack_size + n_frame_vars - local;
                    v.push(circuits::Instr::Set as i32);
                    assert!(idx > 1);
                    v.push(idx as i32);
                    stack_size -= 1;
                }
                Instr::Tee { local } => {
                    let idx = stack_size + n_frame_vars - local;
                    v.push(circuits::Instr::Get as i32);
                    v.push(1);
                    v.push(circuits::Instr::Set as i32);
                    assert!(idx > 0);
                    v.push(idx as i32 + 1);
                }
                Instr::Load { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Read as i32);
                    stack_size += 1;
                }
                Instr::Load8_s { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Read as i32);
                    stack_size += 1;
                }
                Instr::Load8_u { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Read as i32);
                    stack_size += 1;
                }
                Instr::Load16_s { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Read as i32);
                    stack_size += 1;
                }
                Instr::Load16_u { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Read as i32);
                    stack_size += 1;
                }
                Instr::Store { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Write as i32);
                    stack_size -= 2;
                }
                Instr::Store8 { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Write as i32);
                    stack_size -= 2;
                }
                Instr::Store16 { offset } => {
                    // FIXME
                    v.push(circuits::Instr::Write as i32);
                    stack_size -= 2;
                }
                Instr::Return => {
                    assert!(stack_size > n_results);
                    if n_args + n_locals >= n_results {
                        for i in (0..n_results).rev() {
                            v.push(circuits::Instr::Set as i32);
                            let idx = stack_size + n_frame_vars - i;
                            v.push(idx as i32);
                            stack_size -= 1;
                        }
                        while stack_size > 0 {
                            v.push(circuits::Instr::Drop as i32);
                            stack_size -= 1;
                        }

                        let excess = n_args + n_locals - n_results;
                        // we have no unnecessary stack variables left,
                        // so we just jump back to caller
                        if excess == 0 {
                            v.push(circuits::Instr::Jump as i32);
                        // we have one unnecessary stack variable,
                        // so we first swap the top two elements
                        // (pc_namespace, pc -> pc, pc_namespace),
                        // then set the stack variable at index [sp-3] to pc_namespace,
                        // and then jump
                        } else if excess == 1 {
                            v.push(circuits::Instr::Swap as i32);
                            v.push(2);
                            v.push(circuits::Instr::Set as i32);
                            v.push(3);
                            v.push(circuits::Instr::Jump as i32);
                        // we first put the pc_namespace and pc where they ought to be,
                        // then drop all unnecessary stack variables
                        } else {
                            assert!(excess > 1);
                            let idx = n_frame_vars - n_results;
                            assert!(idx > 2);
                            v.push(circuits::Instr::Set as i32);
                            v.push(idx as i32 - 1);
                            v.push(circuits::Instr::Set as i32);
                            // NB: the stack is one smaller now,
                            // hence why it's still idx - 1
                            v.push(idx as i32 - 1);
                            for _ in 0..(excess - 4) {
                                v.push(circuits::Instr::Drop as i32);
                            }
                            v.push(circuits::Instr::Jump as i32);
                        }
                    }
                }
                Instr::Call { fn_idx } => {
                    let patch_location = v.len();
                    v.push(circuits::Instr::Alloc as i32);
                    v.push(0);
                    v.push(circuits::Instr::Const as i32);
                    v.push(0);
                    v.push(circuits::Instr::Const as i32);
                    v.push(0);
                    v.push(circuits::Instr::Jump as i32);
                    function_calls_to_patch.push((patch_location, fn_idx));
                }
                Instr::Select => {}
                Instr::Eqz => {}
                Instr::Eq => {}
                Instr::Ne => {}
                Instr::Lt_s => {}
                Instr::Lt_u => {}
                Instr::Gt_s => {}
                Instr::Gt_u => {}
                Instr::Le_s => {}
                Instr::Le_u => {}
                Instr::Ge_s => {}
                Instr::Ge_u => {}
                Instr::Add => {}
                Instr::Sub => {}
                Instr::Mul => {}
                Instr::Div_s => {}
                Instr::Div_u => {}
                Instr::Rem_s => {}
                Instr::Rem_u => {}
                Instr::And => {}
                Instr::Or => {}
                Instr::Xor => {}
                Instr::Shl => {}
                Instr::Shr_s => {}
                Instr::Shr_u => {}
                Instr::Rotl => {}
                Instr::Rotr => {}
            }
        }
        byte(0x0B).parse_with_state(&mut input, &mut ()).unwrap();
    }
    let ((), _) = postlude().parse(input).unwrap();
    v
}
