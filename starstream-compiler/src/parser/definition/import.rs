use chumsky::prelude::*;
use starstream_types::ast::{ImportDef, ImportItems, ImportNamedItem, ImportSource};

use crate::parser::{context::Extra, primitives};

pub fn parser<'a>() -> impl Parser<'a, &'a str, ImportDef, Extra<'a>> {
    let named_item = primitives::identifier()
        .then(
            just("as")
                .padded()
                .ignore_then(primitives::identifier())
                .or_not(),
        )
        .map(|(imported, alias)| ImportNamedItem {
            local: alias.unwrap_or_else(|| imported.clone()),
            imported,
        });

    let named_items = named_item
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded())
        .map(ImportItems::Named);

    let namespace_import = primitives::identifier().map(ImportItems::Namespace);

    let import_source = primitives::identifier()
        .then_ignore(just(':'))
        .then(primitives::identifier())
        .then(just('/').ignore_then(primitives::identifier()).or_not())
        .map(|((namespace, package), interface)| ImportSource {
            namespace,
            package,
            interface,
        });

    just("import")
        .padded()
        .ignore_then(choice((named_items, namespace_import)))
        .then_ignore(just("from").padded())
        .then(import_source)
        .then_ignore(just(';').padded())
        .map(|(items, from)| ImportDef { items, from })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    macro_rules! assert_import_snapshot {
        ($code:expr) => {{
            let parsed = parser()
                .parse(indoc! { $code })
                .into_result()
                .expect("import should parse");

            insta::with_settings!({
                description => format!("Code:\n\n{}", indoc! { $code }),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_debug_snapshot!(parsed);
            });
        }};
    }

    #[test]
    fn import_named() {
        assert_import_snapshot!("import { blockHeight } from starstream:std/cardano;");
    }

    #[test]
    fn import_named_multiple() {
        assert_import_snapshot!("import { blockHeight, now } from starstream:std/cardano;");
    }

    #[test]
    fn import_named_with_alias() {
        assert_import_snapshot!("import { blockHeight as height } from starstream:std/cardano;");
    }

    #[test]
    fn import_namespace() {
        assert_import_snapshot!("import context from starstream:std;");
    }
}
