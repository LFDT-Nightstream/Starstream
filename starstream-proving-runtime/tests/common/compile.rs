use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};
use starstream_to_wasm::compile;

pub fn compile_contract(source: &str) -> Vec<u8> {
    let (program, errors) = parse_program(source).into_output_errors();
    assert!(errors.is_empty(), "parsing failed: {errors:?}");
    let program = program.expect("parser produced no program");
    let typed = typecheck_program(&program, TypecheckOptions::default())
        .unwrap_or_else(|failure| panic!("typechecking failed: {:?}", failure.errors));
    let result = compile(&typed.program);
    assert!(
        result.errors.is_empty(),
        "compiling failed: {:?}",
        result.errors
    );
    result.wasm.expect("compiling produced no Wasm")
}

pub fn compile_component(source: &str) -> wasmtime::Result<Vec<u8>> {
    wit_component::ComponentEncoder::default()
        .validate(true)
        .module(&compile_contract(source))
        .map_err(|error| wasmtime::format_err!("failed to set core module: {error:#}"))?
        .encode()
        .map_err(|error| wasmtime::format_err!("failed to componentize contract: {error:#}"))
}
