use chumsky::prelude::*;
use starstream_types::ast::Definition;

use super::context::Extra;

mod abi;
mod enum_def;
mod function;
mod import;
mod struct_def;
mod utxo;

pub use abi::parser as abi;
pub use enum_def::parser as enum_def;
pub use function::parser as function;
pub use import::parser as import;
pub use struct_def::parser as struct_def;
pub use utxo::parser as utxo;

pub fn parser<'a>() -> impl Parser<'a, &'a str, Definition, Extra<'a>> {
    choice((
        import().map(Definition::Import),
        function().map(Definition::Function),
        struct_def().map(Definition::Struct),
        enum_def().map(Definition::Enum),
        utxo().map(Definition::Utxo),
        abi().map(Definition::Abi),
    ))
}
