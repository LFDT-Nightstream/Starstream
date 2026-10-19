use std::fmt::Write;

use starstream_compiler::TypecheckOptions;
use wasmtime::{Config, Engine, Linker, Module, Store, Val};

const PAGE_SIZE: usize = 64 * 1024;
/// 128 `i64` fields make each return slot 1 KiB.
const FIELDS: usize = 128;
/// 65 return slots of 1 KiB push the static bump allocator past one page.
const FUNCTIONS: usize = 65;

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

fn field_value(function: usize, field: usize) -> i64 {
    (function * 1000 + field) as i64
}

/// Each exported function returns a struct too large to return flat, so it
/// gets its own static return slot in linear memory.
fn wide_returns_source() -> String {
    let mut source = String::from("struct Wide {\n");
    for field in 0..FIELDS {
        writeln!(source, "    f{field}: i64,").unwrap();
    }
    source.push_str("}\n");
    for function in 0..FUNCTIONS {
        writeln!(source, "\nscript fn wide{function}() -> Wide {{").unwrap();
        source.push_str("    Wide {\n");
        for field in 0..FIELDS {
            let value = field_value(function, field);
            writeln!(source, "        f{field}: {value},").unwrap();
        }
        source.push_str("    }\n}\n");
    }
    source
}

#[test]
fn memory_covers_return_slots_past_first_page() {
    let wasm = compile(&wide_returns_source());

    let mut config = Config::new();
    config.wasm_component_model(false);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_binary(&engine, &wasm).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define_unknown_imports_as_traps(&module).unwrap();
    let mut store = Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &module).unwrap();

    let memory = instance.get_memory(&mut store, "memory").expect("memory");
    assert!(
        memory.data_size(&store) > PAGE_SIZE,
        "memory has {} pages, but return slots need more than one",
        memory.size(&store)
    );

    // The last function's return slot starts past the first page.
    let last = FUNCTIONS - 1;
    let function = instance
        .get_func(&mut store, &format!("wide{last}"))
        .expect("function");
    let mut results = [Val::I32(0)];
    function
        .call(&mut store, &[], &mut results)
        .expect("writing the return slot should not trap");
    let slot = results[0].unwrap_i32() as usize;
    assert!(slot >= PAGE_SIZE, "return slot {slot} is in the first page");

    let data = memory.data(&store);
    for field in 0..FIELDS {
        let offset = slot + field * 8;
        let bytes = data[offset..offset + 8].try_into().unwrap();
        assert_eq!(i64::from_le_bytes(bytes), field_value(last, field));
    }
}
