use chumsky::{error::Rich, prelude::*};
use starstream_types::ast::Identifier;

use super::context::Extra;

const KEYWORDS: &[&str] = &[
    "let", "if", "else", "while", "true", "false", "fn", "return",
];

pub fn identifier<'a>() -> impl Parser<'a, &'a str, Identifier, Extra<'a>> {
    text::ident()
        .try_map(|ident: &'a str, span| {
            if KEYWORDS.contains(&ident) {
                Err(Rich::custom(
                    span,
                    format!("keyword `{ident}` cannot be used as an identifier"),
                ))
            } else {
                Ok(Identifier::new(ident.to_string(), Some(span)))
            }
        })
        .padded()
}
