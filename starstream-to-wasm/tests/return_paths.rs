//! Functions whose tail returns on every path, so the tail itself never
//! produces the function's value.

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

fn call_i64(wasm: &[u8], name: &str, arg: i64) -> i64 {
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
    function
        .call(&mut store, &[Val::I64(arg)], &mut results)
        .unwrap();
    results[0].i64().unwrap()
}

#[test]
fn tail_returns_on_every_path() {
    let wasm = compile(
        r#"
script fn branches(x: i64) -> i64 {
    if (x == 0) {
        return 10;
    } else {
        return 20;
    }
}

script fn chain(x: i64) -> i64 {
    if (x == 0) {
        return 10;
    } else if (x == 1) {
        return 20;
    } else {
        return 30;
    }
}

script fn arms(x: i64) -> i64 {
    match x {
        0 => {
            return 10;
        },
        _ => {
            return 20;
        },
    }
}

script fn block(x: i64) -> i64 {
    {
        return x + 1;
    }
}
"#,
    );
    assert_eq!(call_i64(&wasm, "branches", 0), 10);
    assert_eq!(call_i64(&wasm, "branches", 1), 20);
    assert_eq!(call_i64(&wasm, "chain", 0), 10);
    assert_eq!(call_i64(&wasm, "chain", 1), 20);
    assert_eq!(call_i64(&wasm, "chain", 2), 30);
    assert_eq!(call_i64(&wasm, "arms", 0), 10);
    assert_eq!(call_i64(&wasm, "arms", 5), 20);
    assert_eq!(call_i64(&wasm, "block", 41), 42);
}
