use chumsky::{prelude::*, recursive::recursive};
use starstream_types::{
    ScopedName,
    ast::{Identifier, TypeAnnotation},
};

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
}

fn type_name<'a>() -> impl Parser<'a, &'a str, ScopedName, Extra<'a>> {
    let unit = just("()").map_with(|_, extra| vec![Identifier::new("()", extra.span())]);

    primitives::scoped_name().or(unit).padded()
}
