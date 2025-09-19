use combine::ParseResult;
use combine::Parser;
use combine::any;
use combine::choice;
use combine::count;
use combine::easy::*;
use combine::eof;
use combine::error::Commit;
use combine::many;
use combine::optional;
use combine::parser;
use combine::parser::byte::byte;
use combine::parser::byte::bytes;
use combine::parser::combinator::lazy;
use combine::value;

fn leb128<'a>(n: u32) -> impl Parser<Stream<&'a [u8]>, Output = u64> {
    parser(move |input: &mut Stream<&[u8]>| {
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

fn sleb128<'a>(n: u32) -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    parser(move |input: &mut Stream<&[u8]>| {
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

fn u32_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = u32> {
    leb128(32).map(|val: _| val as u32)
}

fn u64_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    leb128(64).map(|_| ())
}

fn s33_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    sleb128(33).map(|_| ())
}

fn list_p<'a, P: Parser<Stream<&'a [u8]>>>(
    mut x_p: impl FnMut() -> P,
) -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    leb128(32)
        .then(move |len: _| count(len as usize, x_p()))
        .map(|_: Vec<_>| ())
}

fn sized_p<'a, P: Parser<Stream<&'a [u8]>, Output = ()>>(
    mut x_p: impl FnMut() -> P,
) -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    leb128(32).then(move |_| many(x_p()))
}

fn bytestring_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    list_p(any)
}

fn name_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    bytestring_p()
}

fn numtype_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    byte(0x7F).map(|_| ())
}

fn valtype_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    numtype_p()
}

fn limits_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((
        byte(0x00).then(|_| u64_p()),
        byte(0x01).then(|_| u64_p()).then(|_| u64_p()),
    ))
    .map(|_| ())
}

fn memtype_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    limits_p()
}

fn blocktype_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((byte(0x40).map(|_| ()), valtype_p(), s33_p()))
}

fn memarg_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((
        u32_p().then(|_| u32_p()).map(|_| ()),
        u32_p().then(|_| u32_p()).then(|_| u32_p()).map(|_| ()),
    ))
}

fn control_non_rec_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((
        byte(0x00).map(|_| ()),
        byte(0x01).map(|_| ()),
        byte(0x0C).then(|_| u32_p().map(|_| ())),
        byte(0x0D).then(|_| u32_p().map(|_| ())),
        byte(0x0F).map(|_| ()),
        byte(0x10).then(|_| u32_p().map(|_| ())),
    ))
}

fn parametric_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((
        byte(0x1A).map(|_| ()),
        byte(0x1B).map(|_| ()),
        byte(0x1C).then(|_| list_p(valtype_p)),
    ))
}

fn variable_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((
        byte(0x20).then(|_| u32_p().map(|_| ())),
        byte(0x21).then(|_| u32_p().map(|_| ())),
        byte(0x22).then(|_| u32_p().map(|_| ())),
    ))
}

fn memory_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
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
    .then(|_| memarg_p())
}

fn const_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    byte(0x41).then(|_| sleb128(32).map(|_| ()))
}

fn i32_unary_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((byte(0x45), byte(0x67), byte(0x68), byte(0x69))).map(|_| ())
}

fn i32_binary_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
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

fn non_rec_instr_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
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

fn block_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    byte(0x02)
        .then(|_| blocktype_p())
        .then(|_| many::<(), _, _>(lazy(|| instr_p())))
        .skip(byte(0x0B))
        .map(|_| ())
}

fn loop_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    byte(0x03)
        .then(|_| blocktype_p())
        .then(|_| many::<(), _, _>(lazy(|| instr_p())))
        .skip(byte(0x0B))
        .map(|_| ())
}

fn if_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    byte(0x04)
        .then(|_| blocktype_p())
        .then(|_| many::<(), _, _>(lazy(|| instr_p())))
        .then(|_| optional(byte(0x05).then(|_| many::<(), _, _>(lazy(|| instr_p())))))
        .skip(byte(0x0B))
        .map(|_| ())
}

parser! {
    fn instr_p['a]()(Stream<&'a [u8]>) -> () where []
    { choice((non_rec_instr_p(), block_p(), loop_p(), if_p())) }
}

fn expr_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    many(instr_p()).skip(byte(0x0B))
}

fn externidx_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    choice((
        byte(0x00).then(|_| u32_p().map(|_| ())),
        byte(0x02).then(|_| u32_p().map(|_| ())),
    ))
}

fn export_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    name_p().then(|_| externidx_p())
}

fn locals_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    u32_p().then(|_| valtype_p())
}

fn func_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    list_p(locals_p).then(|_| expr_p())
}

fn code_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    sized_p(func_p)
}

fn data_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    u32_p()
        .map(|n| assert!(n == 0))
        .then(|_| expr_p())
        .then(|_| bytestring_p())
}

fn magic_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    bytes(&[0x00, 0x61, 0x73, 0x6D]).map(|_| ())
}

fn version_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    bytes(&[0x01, 0x00, 0x00, 0x00]).map(|_| ())
}

fn section_p<'a, P: Parser<Stream<&'a [u8]>, Output = ()>>(
    id: u8,
    _content_p: impl FnMut() -> P + 'static,
) -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    byte(id).then(move |_| {
        unimplemented!();
        #[allow(unreachable_code)]
        value(())
        // let mut content_p = content_p;
        // sized_p(|| content_p())
    })
}

fn memsec_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    section_p(5, || list_p(memtype_p))
}

fn exportsec_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    section_p(7, || list_p(export_p))
}

fn codesec_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    section_p(10, || list_p(code_p))
}

fn datasec_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    section_p(11, || list_p(data_p))
}

fn unknown_section_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    any().then(|_| bytestring_p())
}

fn module_p<'a>() -> impl Parser<Stream<&'a [u8]>, Output = ()> {
    magic_p()
        .skip(version_p())
        .then(|_| many::<(), _, _>(unknown_section_p()))
        .then(|_| optional(memsec_p()))
        .then(|_| many::<(), _, _>(unknown_section_p()))
        .then(|_| optional(exportsec_p()))
        .then(|_| many::<(), _, _>(unknown_section_p()))
        .then(|_| optional(codesec_p()))
        .then(|_| many::<(), _, _>(unknown_section_p()))
        .then(|_| optional(datasec_p()))
        .then(|_| many::<(), _, _>(unknown_section_p()))
        .skip(eof())
        .map(|_| ())
}

pub fn validate_wasm_module<'a>(
    input: &mut Stream<&'a [u8]>,
) -> ParseResult<(), ParseError<Stream<&'a [u8]>>> {
    module_p().parse_stream(input).map(|_| ())
}
