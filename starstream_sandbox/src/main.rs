//! WebAssembly bindings for the Starstream compiler.
#![feature(internal_output_capture)]
#![no_main]

use log::info;
use starstream_compiler::write_errors;
use starstream_vm::Transaction;

// Imports to manipulate the UI contents, provided by the JS page.
unsafe extern "C" {
    unsafe fn getrandom(ptr: *mut u8, len: usize);

    unsafe fn read_input(ptr: *mut u8, len: usize);
    unsafe fn set_compiler_log(ptr: *const u8, len: usize, warnings: u32, errors: u32);
    unsafe fn set_ast(ptr: *const u8, len: usize);
    unsafe fn set_wat(ptr: *const u8, len: usize);
    unsafe fn set_run_log(ptr: *const u8, len: usize);
    unsafe fn append_run_log(
        level: u32,
        target: *const u8,
        target_len: usize,
        body: *const u8,
        body_len: usize,
    );
}

// Register a getrandom implementation for wasm32-unknown-unknown.
// Not using getrandom's "js" feature because it somehow causes Cargo to panic
// during feature resolution.
fn our_getrandom(dest: &mut [u8]) -> Result<(), getrandom::Error> {
    unsafe {
        getrandom(dest.as_mut_ptr(), dest.len());
    }
    Ok(())
}
getrandom::register_custom_getrandom!(our_getrandom);

// Exports to do work, called by the JS page.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn run(input_len: usize, run: bool) {
    _ = log::set_logger(&LOGGER);
    log::set_max_level(log::LevelFilter::Trace);

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

    // Compile to Wasm.
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

    // Format to WAT.
    let mut wat = Vec::new();
    match wasmprinter::Config::new().fold_instructions(true).print(
        &wasm,
        &mut wasmprinter::PrintTermcolor(termcolor::Ansi::new(&mut wat)),
    ) {
        Ok(()) => {
            unsafe { set_wat(wat.as_ptr(), wat.len()) };
        }
        Err(wat_err) => {
            let wat_err = wat_err.to_string();
            unsafe { set_wat(wat_err.as_ptr(), wat_err.len()) };
        }
    }

    if !run {
        return;
    }

    // Execute.
    unsafe { set_run_log("".as_ptr(), 0) };

    let mut transaction = Transaction::new();
    let coordination_code = transaction.code_cache().load(wasm);
    transaction.run_coordination_script(&coordination_code, "main", Vec::new());

    info!("Proof: {:?}", transaction.prove());
}

static LOGGER: Logger = Logger;

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        let body = record.args().to_string();
        unsafe {
            append_run_log(
                record.level() as u32,
                record.target().as_ptr(),
                record.target().len(),
                body.as_ptr(),
                body.len(),
            )
        };
    }

    fn flush(&self) {}
}
