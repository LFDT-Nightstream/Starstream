use chumsky::{error::Rich, prelude::*};
use starstream_types::ast::{Identifier, Literal};

use super::context::Extra;

const KEYWORDS: &[&str] = &[
    "let", "mut", "if", "else", "while", "true", "false", "fn", "return", "struct", "enum",
    "match", "abi", "event", "emit", "import", "from", "as", "raise", "runtime",
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
