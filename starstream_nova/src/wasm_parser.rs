use combine::ParseResult;
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
// FIXME: do we need this?
use combine::parser::combinator::lazy;

fn leb128<Input: Stream<Token = u8>>(n: u32) -> impl Parser<Input, Output = u64> {
    parser(move |input: &mut Input| {
        let mut val: u64 = 0;
        let mut shift: u32 = 0;
        loop {
            let (b, _) = any().parse_stream(input).into_result()?;
            val |= ((b & 0x7f) as u64) << shift;
            shift += 7;
            if shift > n {
                panic!("invalid leb128");
            }
            if b & 0x80 == 0 {
                break;
            }
        }
        Ok((val, Commit::Commit(())))
    })
}

fn sleb128<Input: Stream<Token = u8>>(n: u32) -> impl Parser<Input, Output = ()> {
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
    .map(|_| ())
}

fn u32_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = u32> {
    leb128(32).map(|val: _| val as u32)
}

fn u64_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    leb128(64).map(|_| ())
}

fn s33_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    sleb128(33).map(|_| ())
}

fn list_p<Input: Stream<Token = u8>, P: Parser<Input>>(
    mut x_p: impl FnMut() -> P,
) -> impl Parser<Input, Output = ()> {
    leb128(32)
        .then(move |len| count(len as usize, x_p()))
        .map(|_: Vec<_>| ())
}

fn sized_p<Input: Stream<Token = u8>, P: Parser<Input, Output = ()>>(
    x_p: P,
) -> impl Parser<Input, Output = ()> {
    leb128(32).with(many(x_p))
}

fn bytestring_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    list_p(any)
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

fn limits_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        byte(0x00).with(u64_p()),
        byte(0x01).with(u64_p()).with(u64_p()),
    ))
    .map(|_| ())
}

fn memtype_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    limits_p()
}

fn blocktype_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((byte(0x40).map(|_| ()), valtype_p(), s33_p()))
}

fn memarg_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        u32_p().with(u32_p()).map(|_| ()),
        u32_p().with(u32_p()).with(u32_p()).map(|_| ()),
    ))
}

fn control_non_rec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        byte(0x00).map(|_| ()),
        byte(0x01).map(|_| ()),
        byte(0x0C).with(u32_p().map(|_| ())),
        byte(0x0D).with(u32_p().map(|_| ())),
        byte(0x0F).map(|_| ()),
        byte(0x10).with(u32_p().map(|_| ())),
    ))
}

fn parametric_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        byte(0x1A).map(|_| ()),
        byte(0x1B).map(|_| ()),
        byte(0x1C).with(list_p(valtype_p)),
    ))
}

fn variable_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        byte(0x20).with(u32_p().map(|_| ())),
        byte(0x21).with(u32_p().map(|_| ())),
        byte(0x22).with(u32_p().map(|_| ())),
    ))
}

fn memory_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        byte(0x28),
        byte(0x2C),
        byte(0x2D),
        byte(0x2E),
        byte(0x2F),
        byte(0x36),
        byte(0x3A),
        byte(0x3B),
    ))
    .with(memarg_p())
}

fn const_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    byte(0x41).with(sleb128(32).map(|_| ()))
}

fn i32_unary_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((byte(0x45), byte(0x67), byte(0x68), byte(0x69))).map(|_| ())
}

fn i32_binary_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        byte(0x46),
        byte(0x47),
        byte(0x48),
        byte(0x49),
        byte(0x4A),
        byte(0x4B),
        byte(0x4C),
        byte(0x4D),
        byte(0x4E),
        byte(0x4F),
        byte(0x6A),
        byte(0x6B),
        byte(0x6C),
        byte(0x6D),
        byte(0x6E),
        byte(0x6F),
        byte(0x70),
        byte(0x71),
        byte(0x72),
        byte(0x73),
        byte(0x74),
        byte(0x75),
        byte(0x76),
        byte(0x77),
        byte(0x78),
    ))
    .map(|_| ())
}

fn non_rec_instr_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
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

fn block_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    byte(0x02)
        .with(blocktype_p())
        .with(many::<(), _, _>(lazy(|| instr_p())))
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
    { choice((non_rec_instr_p(), block_p(), loop_p(), if_p())) }
}

fn expr_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    many(instr_p()).skip(byte(0x0B))
}

fn externidx_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    choice((
        byte(0x00).with(u32_p().map(|_| ())),
        byte(0x02).with(u32_p().map(|_| ())),
    ))
}

fn export_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    name_p().with(externidx_p())
}

fn locals_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    u32_p().with(valtype_p())
}

fn func_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    list_p(locals_p).with(expr_p())
}

fn code_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    sized_p(func_p())
}

fn data_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    u32_p()
        .map(|n| assert!(n == 0))
        .with(expr_p())
        .with(bytestring_p())
}

fn magic_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    (byte(0x00), byte(0x61), byte(0x73), byte(0x6D)).map(|_| ())
}

fn version_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    (byte(0x01), byte(0x00), byte(0x00), byte(0x00)).map(|_| ())
}

fn section_p<Input: Stream<Token = u8>, P: Parser<Input, Output = ()>>(
    id: u8,
    content_p: P,
) -> impl Parser<Input, Output = ()> {
    byte(id).with(sized_p(content_p))
}

fn memsec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    section_p(5, list_p(memtype_p))
}

fn exportsec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    section_p(7, list_p(export_p))
}

fn codesec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    section_p(10, list_p(code_p))
}

fn datasec_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    section_p(11, list_p(data_p))
}

fn unknown_section_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    any().with(bytestring_p())
}

pub fn module_p<Input: Stream<Token = u8>>() -> impl Parser<Input, Output = ()> {
    magic_p()
        .skip(version_p())
        .with(many::<(), _, _>(unknown_section_p()))
        .with(optional(memsec_p()))
        .with(many::<(), _, _>(unknown_section_p()))
        .with(optional(exportsec_p()))
        .with(many::<(), _, _>(unknown_section_p()))
        .with(optional(codesec_p()))
        .with(many::<(), _, _>(unknown_section_p()))
        .with(optional(datasec_p()))
        .with(many::<(), _, _>(unknown_section_p()))
        .skip(eof())
        .map(|_| ())
}
