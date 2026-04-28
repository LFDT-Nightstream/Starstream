use chumsky::prelude::*;
use starstream_types::{
    YieldPart,
    ast::{Block, Expr, Spanned},
};

use crate::parser::{ParserExt, context::Extra, definition::function};

pub fn yield_<'a>(
    block: impl Parser<'a, &'a str, Block, Extra<'a>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Spanned<Expr>, Extra<'a>> {
    // let impl_ = just("impl")
    //     .padded()
    //     .then(identifier())
    //     .then(choice((just(";"), just("{").then() )));
    let yield_part = choice((
        function(block).map(YieldPart::Function),
        // TODO: AbiImpl and AbiImplDefault
    ));

    just("yield")
        .padded()
        .ignore_then(
            yield_part
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|_| Expr::Literal(starstream_types::Literal::Unit))
        .spanned()
}
