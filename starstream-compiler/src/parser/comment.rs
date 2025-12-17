use chumsky::prelude::*;
use starstream_types::Comment;

use crate::parser::context::Extra;

pub fn shebang<'a>() -> impl Parser<'a, &'a str, Comment, Extra<'a>> {
    just("#!")
        .then(none_of("\n").repeated())
        .then(just("\n"))
        .to_span()
        .map(Comment)
}

pub fn comment<'a>() -> impl Parser<'a, &'a str, Comment, Extra<'a>> + Clone {
    let line_comment = just("//")
        .then(none_of("\n").repeated())
        .then(just("\n"))
        .to_span();
    let block_comment = just("/*")
        .then(any().and_is(just("*/").not()).repeated())
        .then(just("*/"))
        .to_span();
    choice((line_comment, block_comment)).map(Comment)
}

fn _comment<'a>(
    from: impl Parser<'a, &'a str, (), Extra<'a>>,
) -> impl Parser<'a, &'a str, (), Extra<'a>> {
    from.to_span().map_with(
        |span, extra: &mut chumsky::input::MapExtra<'_, '_, &str, Extra<'a>>| {
            // WARNING: Semantically meaningful side-effects in map_with contradicts chumsky intent.
            // https://docs.rs/chumsky/latest/chumsky/guide/_06_technical_notes/index.html#purity-and-optimisation
            // Don't use `ignore_then` or comments will be dropped.
            extra.state().comments.push(span);
        },
    )
}
