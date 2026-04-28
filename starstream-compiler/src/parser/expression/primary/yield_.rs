use chumsky::prelude::*;
use starstream_types::{
    YieldPart,
    ast::{Block, Expr, Spanned},
};

use crate::parser::{ParserExt, context::Extra, definition::function, primitives::identifier};

pub fn yield_<'a>(
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    let yield_part_function = function(block.clone()).map(YieldPart::Function);

    let yield_part_abi_impl_default = just("impl")
        .padded()
        .ignore_then(identifier())
        .padded()
        .then_ignore(just(";"))
        .map(YieldPart::AbiImplDefault);

    let yield_part_abi_impl = just("impl")
        .padded()
        .ignore_then(identifier())
        .padded()
        .then(
            function(block)
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(abi, parts)| YieldPart::AbiImpl { abi, parts });

    let yield_part = choice((
        yield_part_function,
        yield_part_abi_impl,
        yield_part_abi_impl_default,
    ));

    just("yield")
        .padded()
        .ignore_then(
            yield_part
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(Expr::Yield)
        .spanned()
}
