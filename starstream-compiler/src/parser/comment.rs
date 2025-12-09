use chumsky::prelude::*;

use crate::parser::context::Extra;

pub fn shebang<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> {
    just("#!").ignore_then(none_of("\n").repeated())
}
