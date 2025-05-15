//! WebAssembly bindings for the Starstream compiler.
#![no_main]

use starstream_compiler::write_errors;

// Imports to manipulate the UI contents, provided by the JS page.
unsafe extern "C" {
    unsafe fn read_input(ptr: *mut u8, len: usize);
    unsafe fn set_compiler_log(ptr: *const u8, len: usize, warnings: u32, errors: u32);
    unsafe fn set_ast(ptr: *const u8, len: usize);
    unsafe fn set_wat(ptr: *const u8, len: usize);
}

// Exports to do work, called by the JS page.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn run(input_len: usize) {
    // Fetch the input.
    let mut input = vec![0; input_len];
    unsafe { read_input(input.as_mut_ptr(), input.len()) };
    let input = std::str::from_utf8(&input).unwrap();

    // Parse to AST and format for the AST tab.
    let (ast, errors) = starstream_compiler::parse(input);
    let mut compiler_output = Vec::new();
    let mut error_count = errors.len() as u32;
    write_errors(&mut compiler_output, input, &errors);
    unsafe {
        set_compiler_log(
            compiler_output.as_ptr(),
            compiler_output.len(),
            // TODO: get real warning count.
            0,
            error_count,
        )
    };
    let Some(ast) = ast else { return };

    let str_ast = format!("{:#?}", ast);
    unsafe { set_ast(str_ast.as_ptr(), str_ast.len()) };

    // Compile to WASM.
    let (wasm, errors) = starstream_compiler::compile(&ast);
    error_count += errors.len() as u32;
    write_errors(&mut compiler_output, input, &errors);
    unsafe {
        set_compiler_log(
            compiler_output.as_ptr(),
            compiler_output.len(),
            // TODO: get real warning count.
            0,
            error_count,
        )
    }
    let Some(wasm) = wasm else { return };

    // Format to WAT from the WASM project.
    match wasmprinter::print_bytes(&wasm) {
        Ok(wat) => {
            unsafe { set_wat(wat.as_ptr(), wat.len()) };
        }
        Err(wat_err) => {
            let wat_err = wat_err.to_string();
            unsafe { set_wat(wat_err.as_ptr(), wat_err.len()) };
        }
    }
}
