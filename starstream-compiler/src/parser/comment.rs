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

/// Parse a comment and return its span.
/// Used by tests; production code uses `comment_collecting` which pushes to State.
#[allow(dead_code)]
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

/// A comment parser that collects comments into State for the CommentMap.
///
/// WARNING: Semantically meaningful side-effects in map_with contradicts chumsky intent.
/// https://docs.rs/chumsky/latest/chumsky/guide/_06_technical_notes/index.html#purity-and-optimisation
/// Don't use `ignore_then` or comments will be dropped.
pub fn comment_collecting<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> + Clone {
    let line_comment = just("//")
        .then(none_of("\n").repeated())
        .then(just("\n").or_not())
        .to_span();
    let block_comment = just("/*")
        .then(any().and_is(just("*/").not()).repeated())
        .then(just("*/"))
        .to_span();
    choice((line_comment, block_comment)).map_with(
        |span, extra: &mut chumsky::input::MapExtra<'_, '_, &str, Extra<'a>>| {
            extra.state().comments.push(Comment(span));
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::context::State;

    #[test]
    fn comment_span_includes_full_text() {
        let source = "/// This is a doc comment\n";
        let mut state = State::new();

        let result = comment().parse_with_state(source, &mut state).into_result();

        assert!(result.is_ok(), "Comment should parse");
        let comment = result.unwrap();

        // Extract the text using the span
        let text = &source[comment.0.start..comment.0.end];

        // Prove what the span actually contains
        println!("Source: {:?}", source);
        println!("Span: {}..{}", comment.0.start, comment.0.end);
        println!("Text from span: {:?}", text);

        assert!(
            text.starts_with("///"),
            "Comment text should start with /// but got: {:?}",
            text
        );
    }

    #[test]
    fn regular_comment_does_not_start_with_triple_slash() {
        let source = "// Regular comment\n";
        let mut state = State::new();

        let result = comment().parse_with_state(source, &mut state).into_result();

        assert!(result.is_ok());
        let comment = result.unwrap();
        let text = &source[comment.0.start..comment.0.end];

        assert!(text.starts_with("//"), "Should start with //");
        assert!(!text.starts_with("///"), "Should NOT start with ///");
    }
}
