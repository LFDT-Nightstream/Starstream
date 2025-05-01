#![no_main]

// Imports to manipulate the UI contents, provided by the JS page.
unsafe extern "C" {
    unsafe fn read_input(ptr: *mut u8, len: usize);
    unsafe fn set_compiler_log(ptr: *const u8, len: usize);
    unsafe fn set_ast(ptr: *const u8, len: usize);
}

// Exports to do work, called by the JS page.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn run(input_len: usize) {
    let mut input = vec![0; input_len];
    unsafe { read_input(input.as_mut_ptr(), input.len()) };
    let input = std::str::from_utf8(&input).unwrap();

    let (ast, errors) = starstream_compiler::parse(input);
    unsafe { set_compiler_log(errors.as_ptr(), errors.len()) };
    if !errors.is_empty() {
        //unsafe { set_error() };
        return;
    }

    let ast = format!("{:#?}", ast);
    unsafe { set_ast(ast.as_ptr(), ast.len()) };
}
