use chumsky::prelude::*;

use crate::parser::context::Extra;

/// `contract;` — marks the file as a contract entry point.
pub fn parser<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> {
    just("contract")
        .padded()
        .then_ignore(just(';').padded())
        .ignored()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bare_contract() {
        parser().parse("contract;").into_result().expect("parses");
    }

    #[test]
    fn contract_with_whitespace() {
        parser().parse("contract ;").into_result().expect("parses");
    }

    #[test]
    fn rejects_missing_semicolon() {
        assert!(parser().parse("contract").into_result().is_err());
    }
}
