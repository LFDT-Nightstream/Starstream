use chumsky::{prelude::*, recursive::recursive};
use starstream_types::ast::{Identifier, TypeAnnotation};

use super::{context::Extra, primitives};

pub fn parser<'a>() -> impl Parser<'a, &'a str, TypeAnnotation, Extra<'a>> {
    recursive(|annotation| {
        type_name()
            .boxed()
            .then(
                just('<')
                    .padded()
                    .ignore_then(
                        annotation
                            .clone()
                            .separated_by(just(',').padded())
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just('>').padded())
                    .or_not(),
            )
            .map(|(name, generics)| TypeAnnotation {
                name,
                generics: generics.unwrap_or_default(),
            })
            .padded()
    })
    .boxed()
}

fn type_name<'a>() -> impl Parser<'a, &'a str, Identifier, Extra<'a>> {
    let unit = just("()").map_with(|_, extra| Identifier::new("()", Some(extra.span())));
    primitives::identifier().or(unit).padded()
}
