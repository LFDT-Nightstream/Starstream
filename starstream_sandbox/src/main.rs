#![no_main]

// Imports to manipulate the UI contents, provided by the JS page.
unsafe extern "C" {
    unsafe fn read_input(ptr: *mut u8, len: usize);
    unsafe fn set_compiler_log(ptr: *const u8, len: usize);
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
    let (ast, mut errors) = starstream_compiler::parse(input);
    unsafe { set_compiler_log(errors.as_ptr(), errors.len()) };
    if !errors.is_empty() {
        //unsafe { set_error() };
        return;
    }

    let str_ast = format!("{:#?}", ast);
    unsafe { set_ast(str_ast.as_ptr(), str_ast.len()) };

    // Compile to WASM and format to WAT for the WASM tab.
    let wasm = match starstream_compiler::compile(&ast) {
        Ok(wasm) => wasm,
        Err(e) => {
            errors.push('\n');
            errors.push_str(&e);
            unsafe { set_compiler_log(errors.as_ptr(), errors.len()) };
            return;
        }
    };

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
