use chumsky::prelude::*;
use starstream_types::{
    Spanned,
    ast::{Definition, Program},
};

use crate::parser::ParserExt;

use super::{comment, context::Extra, definition};

/// Get a parser for a Starstream source file.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Program, Extra<'a>> {
    comment::shebang()
        .or_not()
        .then(
            definition()
                .spanned()
                .padded()
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(end())
        .map_with(|(shebang, definitions), extra| {
            let source = extra.slice();
            let definitions = reassign_comments(definitions, source);
            Program {
                shebang,
                definitions,
            }
        })
}

/// Reassign comments that are on their own line from `comments_after` of one
/// definition to `comments_before` of the next definition.
///
/// A comment is considered "on its own line" if there's a newline between
/// what precedes it and its start.
fn reassign_comments(
    mut definitions: Vec<Spanned<Definition>>,
    source: &str,
) -> Vec<Spanned<Definition>> {
    if definitions.len() < 2 {
        return definitions;
    }

    // Process pairs: move comments from def[i].comments_after to def[i+1].comments_before
    // if they're on their own line
    for i in 0..definitions.len() - 1 {
        let comments_after = std::mem::take(&mut definitions[i].comments_after);

        if comments_after.is_empty() {
            continue;
        }

        // Find where to split: first comment that's on its own line
        let mut split_index = comments_after.len(); // default: keep all

        for (idx, comment) in comments_after.iter().enumerate() {
            let preceding_end = if idx == 0 {
                // For first comment, check from start of comments_after region
                // (which is right after the node proper ends)
                comment.0.start
            } else {
                comments_after[idx - 1].0.end
            };

            // Check if there's a newline in the source before this comment
            let before_start = if comment.0.start > 0 {
                comment.0.start.saturating_sub(1)
            } else {
                0
            };

            // Look at what's between preceding content and this comment
            let check_start = if idx == 0 {
                // For first comment, look at chars before it to see if there's a newline
                // (indicating it's on its own line, not inline)
                before_start.saturating_sub(20).max(0) // look back up to 20 chars
            } else {
                preceding_end
            };

            if check_start < comment.0.start {
                let between = &source[check_start..comment.0.start];
                if between.contains('\n') {
                    split_index = idx;
                    break;
                }
            }
        }

        // Split comments: [0..split_index] stay, [split_index..] move
        let (inline, own_line): (Vec<_>, Vec<_>) = comments_after
            .into_iter()
            .enumerate()
            .partition(|(idx, _)| *idx < split_index);

        definitions[i].comments_after = inline.into_iter().map(|(_, c)| c).collect();

        // Prepend own-line comments to next definition's comments_before
        let own_line_comments: Vec<_> = own_line.into_iter().map(|(_, c)| c).collect();
        let next_comments_before = std::mem::take(&mut definitions[i + 1].comments_before);
        definitions[i + 1].comments_before = own_line_comments
            .into_iter()
            .chain(next_comments_before)
            .collect();
    }

    definitions
}

// NOTE: The test cases for the root program parser are included in the
// compiler integration tests, rather than separated out here.
