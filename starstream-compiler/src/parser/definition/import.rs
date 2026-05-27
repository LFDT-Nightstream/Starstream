use chumsky::prelude::*;
use starstream_types::ast::{ImportDef, ImportItems, ImportNamedItem, ImportPath, ImportSource};

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

    let wit_source = primitives::identifier()
        .then_ignore(just(':'))
        .then(primitives::identifier())
        .then(just('/').ignore_then(primitives::identifier()).or_not())
        .map(|((namespace, package), interface)| ImportSource::Wit {
            namespace,
            package,
            interface,
        });

    // Inline string literal: `"…"` with `\\`, `\"`, `\n`, `\r`, `\t` escapes.
    let escape = just('\\').ignore_then(choice((
        just('\\').to('\\'),
        just('"').to('"'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
    )));
    let string_char = escape.or(any().filter(|c: &char| *c != '"' && *c != '\\'));
    let path_source = string_char
        .repeated()
        .collect::<String>()
        .delimited_by(just('"'), just('"'))
        .map_with(
            |value, extra: &mut crate::parser::context::MapExtra<'_, '_>| {
                ImportSource::Path(ImportPath {
                    value,
                    span: extra.span(),
                })
            },
        );

    let import_source = choice((wit_source, path_source));

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

    #[test]
    fn import_path_named() {
        assert_import_snapshot!("import { add } from \"./helpers/math.star\";");
    }

    #[test]
    fn import_path_named_alias() {
        assert_import_snapshot!("import { add as plus } from \"./helpers/math.star\";");
    }

    #[test]
    fn import_path_namespace() {
        assert_import_snapshot!("import math from \"./helpers/math.star\";");
    }

    #[test]
    fn import_path_parent_relative() {
        assert_import_snapshot!("import { foo } from \"../shared/util.star\";");
    }
}
