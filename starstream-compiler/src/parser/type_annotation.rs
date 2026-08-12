use chumsky::{prelude::*, recursive::recursive};
use starstream_types::{
    ScopedName,
    ast::{Identifier, TypeAnnotation},
};

use super::{context::Extra, primitives};

pub fn parser<'a>() -> impl Parser<'a, &'a str, TypeAnnotation, Extra<'a>> {
    recursive(|annotation| {
        let named = type_name()
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
            .map(|(name, generics)| TypeAnnotation::Named {
                name,
                generics: generics.unwrap_or_default(),
            });

        // Anonymous tuple type: `(A, B)`. At least two elements, so `()`
        // stays the unit type below and `(A)` is not valid syntax.
        let tuple = annotation
            .separated_by(just(',').padded())
            .at_least(2)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('(').padded(), just(')').padded())
            .map_with(|items, extra| TypeAnnotation::Tuple {
                items,
                span: extra.span(),
            });

        choice((tuple, named)).padded()
    })
}

fn type_name<'a>() -> impl Parser<'a, &'a str, ScopedName, Extra<'a>> {
    let unit = just("()").map_with(|_, extra| vec![Identifier::new("()", extra.span())]);

    primitives::scoped_name().or(unit).padded()
}
