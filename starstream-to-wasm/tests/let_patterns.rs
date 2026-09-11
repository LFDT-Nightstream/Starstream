use starstream_compiler::TypecheckOptions;
use wasmtime::{Config, Engine, Linker, Module, Store, Val};

fn compile(source: &str) -> Vec<u8> {
    let parsed = starstream_compiler::parse_program(source);
    let (program, errors) = parsed.into_output_errors();
    assert!(errors.is_empty(), "parse errors: {errors:?}");
    let typed = starstream_compiler::typecheck_program(
        &program.expect("program"),
        TypecheckOptions::default(),
    )
    .expect("typecheck");
    let compiled = starstream_to_wasm::compile(&typed.program);
    assert!(
        compiled.errors.is_empty(),
        "compile errors: {:?}",
        compiled.errors
    );
    compiled.wasm.expect("wasm")
}

fn call_i64(wasm: &[u8], name: &str) -> i64 {
    let mut config = Config::new();
    config.wasm_component_model(false);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_binary(&engine, wasm).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define_unknown_imports_as_traps(&module).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let function = instance.get_func(&mut store, name).unwrap();
    let mut results = [Val::I64(0)];
    function.call(&mut store, &[], &mut results).unwrap();
    results[0].i64().unwrap()
}

#[test]
fn destructures_irrefutable_tuple() {
    let wasm = compile(
        r#"
script fn tuple_sum() -> i64 {
    let (left, right) = (20, 22);
    left + right
}
"#,
    );
    assert_eq!(call_i64(&wasm, "tuple-sum"), 42);
}

#[test]
fn let_else_runs_only_when_pattern_does_not_match() {
    let wasm = compile(
        r#"
script fn matched() -> i64 {
    let 1 = 1 else { return 0; };
    42
}

script fn unmatched() -> i64 {
    let 1 = 2 else { return 7; };
    42
}
"#,
    );
    assert_eq!(call_i64(&wasm, "matched"), 42);
    assert_eq!(call_i64(&wasm, "unmatched"), 7);
}
