use super::{TypeErrorKind, TypecheckOptions, typecheck_program};
use starstream_types::{TypedDefinition, TypedExprKind, TypedStatement, TypedUtxoPart};

fn check(source: &str) -> Result<super::TypecheckSuccess, super::TypecheckFailure> {
    let parsed = crate::parse_program(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    typecheck_program(&parsed.into_program().unwrap(), TypecheckOptions::default())
}

#[test]
fn public_methods_remain_direct_functions_in_the_typed_ast() {
    let checked = check(
        r#"
        abi Extra { fn extra(); }
        utxo Foo {
            main fn new() { yield(); yield(Extra); }
            main fn other() { if (true) { yield(); } else { yield(Extra); } }
            pub fn value(x: i64) -> i64 { x }
            pub fn advance() { resume; }
            impl Extra { fn extra() {} }
        }
        script fn test() -> i64 { Foo::new().value(42) }
    "#,
    )
    .unwrap();
    let TypedDefinition::Utxo(utxo) = &checked.program.definitions[1] else {
        panic!()
    };
    assert!(utxo.ty.always_abis.is_empty());
    assert_eq!(utxo.ty.possible_abis.len(), 1);
    assert_eq!(utxo.ty.possible_abis[0].name.as_str(), "Extra");
    assert_eq!(utxo.ty.public_methods.len(), 2);
    let TypedUtxoPart::Function(main) = &utxo.parts[0] else {
        panic!()
    };
    for (index, statement) in main.body.statements.iter().enumerate() {
        let TypedStatement::Expression(expression) = statement else {
            panic!()
        };
        let TypedExprKind::Yield { abis } = &expression.node.kind else {
            panic!()
        };
        assert_eq!(abis.len(), index);
    }
    for (part, method) in utxo.parts[2..4].iter().zip(&utxo.ty.public_methods) {
        let TypedUtxoPart::Function(function) = part else {
            panic!()
        };
        assert_eq!(
            function.export,
            Some(starstream_types::FunctionExport::UtxoPublic)
        );
        assert_eq!(function.id, method.id);
        assert_eq!(function.name, method.name);
        assert_eq!(function.ty, method.ty);
    }
    assert_eq!(utxo.parts.len(), 5);
}

#[test]
fn rejects_public_name_collisions() {
    for other in [
        "pub fn value() {}",
        "fn value() {}",
        "main fn value() { yield(); }",
        "impl Extra { fn value() {} }",
    ] {
        let source = format!(
            "abi Extra {{ fn value(); }} utxo Foo {{ main fn new() {{ yield(); }} pub fn value() {{}} {other} }}"
        );
        let failure = check(&source).unwrap_err();
        assert!(
            matches!(&*failure.errors[0].kind, TypeErrorKind::Redeclaration { name } if name == "value")
        );
    }
}

#[test]
fn private_and_erased_handles_do_not_expose_methods() {
    for source in [
        "utxo Foo { main fn new() { yield(); } fn hidden() {} } script fn test() { Foo::new().hidden(); }",
        "utxo Foo { main fn new() { yield(); } pub fn value() {} } script fn test(x: Utxo) { x.value(); }",
    ] {
        assert!(check(source).is_err());
    }
}

#[test]
fn public_methods_obey_abi_body_and_signature_rules() {
    for body in [
        "pub fn value() { yield(); }",
        "pub fn value() -> i64 { resume; }",
        "pub fn value() -> i64 { true }",
    ] {
        assert!(
            check(&format!(
                "utxo Foo {{ main fn new() {{ yield(); }} {body} }}"
            ))
            .is_err()
        );
    }
    assert!(check("utxo Foo { main fn new() { yield(); } pub fn value(x: i64) {} } script fn test() { Foo::new().value(true); }").is_err());
}

#[test]
fn pub_fn_is_only_valid_directly_inside_utxos() {
    for source in [
        "pub fn value() {}",
        "token Foo { pub fn value() {} }",
        "abi A { pub fn value(); }",
        "utxo Foo { impl A { pub fn value() {} } }",
        "utxo Foo { pub main fn value() {} }",
    ] {
        assert!(!crate::parse_program(source).errors.is_empty(), "{source}");
    }
}

#[test]
fn public_methods_round_trip_through_formatter() {
    let source = "utxo Foo {\n    main fn new() {\n        yield();\n    }\n\n    pub fn value(pub x: i64) -> i64 {\n        x\n    }\n}\n";
    let parsed = crate::parse_program(source);
    let comments = parsed.comment_map();
    let program = parsed.into_program().unwrap();
    assert_eq!(
        crate::formatter::program(&program, source, &comments).unwrap(),
        source
    );
}

#[test]
fn public_interfaces_are_specific_to_their_utxo() {
    check(
        r#"
        utxo Foo { main fn new() { yield(); } pub fn value() -> i64 { 1 } }
        utxo Bar { main fn new() { yield(); } pub fn value() -> bool { true } }
        script fn number() -> i64 { Foo::new().value() }
        script fn boolean() -> bool { Bar::new().value() }
    "#,
    )
    .unwrap();
}
