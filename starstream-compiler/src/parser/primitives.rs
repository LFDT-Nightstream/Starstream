use chumsky::{error::Rich, prelude::*};
use starstream_types::{
    ScopedName,
    ast::{Identifier, Literal},
};

use super::context::Extra;

/// The list of "strict keywords" / "reserved words" that cannot be used as
/// identifiers.
///
/// This list must contain all keywords that could conflict with identifiers,
/// namely those that could begin statements or expressions such as "let" since
/// arbitrary identifiers are otherwise allowed in that position.
///
/// Keywords appearing in the grammar but not in this list are "weak keywords"
/// or "contextual keywords" and can be valid identifiers because they appear
/// in the grammar only in places where arbitrary identifiers are not valid,
/// such as at the root level ("script", "contract") or in a Utxo or Token
/// block ("impl", "storage").
///
/// Also update `highlights.scm` when updating this list.
const KEYWORDS: &[&str] = &[
    "let", "pub", "mut", "if", "else", "while", "true", "false", "fn", "return", "struct", "enum",
    "match", "abi", "emit", "import", "from", "as", "raise", "runtime", "disclose", "is", "yield",
    "resume",
];
pub fn identifier<'a>() -> impl Parser<'a, &'a str, Identifier, Extra<'a>> + Clone {
    text::ident()
        .try_map(|ident: &'a str, span| {
            if KEYWORDS.contains(&ident) {
                Err(Rich::custom(
                    span,
                    format!("keyword `{ident}` cannot be used as an identifier"),
                ))
            } else {
                Ok(Identifier::new(ident.to_string(), span))
            }
        })
        .padded()
}

pub fn integer_literal<'a>() -> impl Parser<'a, &'a str, Literal, Extra<'a>> + Clone {
    text::int(10)
        .map(|digits: &str| {
            let value = digits.parse::<i128>().expect("integer literal");
            Literal::Integer(value)
        })
        .boxed()
}

pub fn boolean_literal<'a>() -> impl Parser<'a, &'a str, Literal, Extra<'a>> + Clone {
    choice((just("true").to(true), just("false").to(false)))
        .padded()
        .map(Literal::Boolean)
        .boxed()
}

pub fn unit_literal<'a>() -> impl Parser<'a, &'a str, Literal, Extra<'a>> + Clone {
    just("()").padded().to(Literal::Unit).boxed()
}

pub fn scoped_name<'a>() -> impl Parser<'a, &'a str, ScopedName, Extra<'a>> + Clone {
    identifier()
        .separated_by(just("::"))
        .at_least(1)
        .collect::<Vec<_>>()
}
