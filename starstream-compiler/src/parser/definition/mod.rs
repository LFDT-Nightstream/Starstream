use chumsky::prelude::*;
use starstream_types::ast::Definition;

use crate::parser::recursives;

use super::context::Extra;

mod abi;
mod contract;
mod enum_def;
mod function;
mod import;
mod struct_def;
mod token;
mod utxo;

pub use abi::parser as abi;
pub use contract::parser as contract;
pub use enum_def::parser as enum_def;
pub use function::function_with_export;
pub use import::parser as import;
pub use struct_def::parser as struct_def;
pub use token::token;
pub use utxo::utxo;

pub fn parser<'a>() -> impl Parser<'a, &'a str, Definition, Extra<'a>> {
    let (_, block, _) = recursives();

    choice((
        contract().map(|_| Definition::Contract),
        import().map(Definition::Import),
        function_with_export(block.clone()).map(Definition::Function),
        struct_def().map(Definition::Struct),
        enum_def().map(Definition::Enum),
        utxo(block.clone()).map(Definition::Utxo),
        token(block).map(Definition::Token),
        abi().map(Definition::Abi),
    ))
}
