use chumsky::prelude::*;

use crate::parser::context::Extra;

pub fn shebang<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> {
    _comment(just("#!").ignore_then(none_of("\n").repeated()))
}

pub fn parser<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> {
    let line_comment = just("//").ignore_then(none_of("\n").repeated());
    let block_comment = just("/*")
        .ignore_then(any().and_is(just("*/").not()).repeated())
        .then_ignore(just("*/"));
    _comment(choice((line_comment, block_comment)))
}

fn _comment<'a>(
    from: impl Parser<'a, &'a str, (), Extra<'a>>,
) -> impl Parser<'a, &'a str, (), Extra<'a>> {
    from.to_span().map_with(
        |span, extra: &mut chumsky::input::MapExtra<'_, '_, &str, Extra<'a>>| {
            extra.state().comments.push(span);
        },
    )
}
