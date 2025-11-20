//! WebAssembly bindings for the Starstream compiler.
use std::panic;

use log::error;
use wit_component::{ComponentEncoder, DecodedWasm};

// Imports to manipulate the UI contents, provided by the JS page.
unsafe extern "C" {
    unsafe fn read_input(ptr: *mut u8, len: usize);

    unsafe fn sandbox_log(
        level: u32,
        target: *const u8,
        target_len: usize,
        body: *const u8,
        body_len: usize,
    );
    unsafe fn set_wat(ptr: *const u8, len: usize);
    unsafe fn set_core_wasm(ptr: *const u8, len: usize);
    unsafe fn set_wit(ptr: *const u8, len: usize);
    unsafe fn set_component_wasm(ptr: *const u8, len: usize);
}

#[derive(serde::Deserialize)]
struct Input<'a> {
    code: &'a str,
}

/// # Safety
///
/// Exports to do work, called by the JS page.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn run(input_len: usize) {
    // Set up output and panic context.
    _ = log::set_logger(&LOGGER);
    log::set_max_level(log::LevelFilter::Trace);
    panic::set_hook(Box::new(|info| {
        if let Some(s) = info.payload().downcast_ref::<&str>() {
            error!("panic! {s}");
        } else if let Some(s) = info.payload().downcast_ref::<String>() {
            error!("panic! {s}");
        } else {
            error!("panic!");
        }
        if let Some(loc) = info.location() {
            error!("at {}:{}:{}", loc.file(), loc.line(), loc.column());
        }
    }));

    // Fetch the input.
    let mut input = vec![0; input_len];
    unsafe { read_input(input.as_mut_ptr(), input.len()) };
    let input: Input = serde_cbor::from_slice(&input).unwrap();
    let code = input.code;

    // Parse to AST.
    let (program, errors) = starstream_compiler::parse_program(code).into_output_errors();
    for error in errors {
        write_report(&error.into());
    }
    let Some(program) = program else {
        return;
    };

    // Typecheck.
    let typed = match starstream_compiler::typecheck_program(&program, Default::default()) {
        Ok(program) => program,
        Err(errors) => {
            for error in errors {
                write_report(&error.into());
            }
            return;
        }
    };

    // Compile to Wasm.
    let (wasm, errors) = starstream_to_wasm::compile(&typed.program);
    for error in errors {
        write_report(&error.into());
    }
    let Some(wasm) = wasm else {
        return;
    };

    unsafe { set_core_wasm(wasm.as_ptr(), wasm.len()) };

    // WITify core version if we can.
    match print_wit(&wasm, true) {
        Ok(wit) => unsafe { set_wit(wit.as_ptr(), wit.len()) },
        Err(error) => error!("print_wit(core): {}", error),
    }

    // Componentize.
    let wasm = match componentize(&wasm) {
        Ok(wasm) => {
            unsafe { set_component_wasm(wasm.as_ptr(), wasm.len()) };

            // WITify component version (it'll be more compact).
            match print_wit(&wasm, false) {
                Ok(wit) => unsafe { set_wit(wit.as_ptr(), wit.len()) },
                Err(error) => error!("print_wit(component): {}", error),
            }

            wasm
        }
        Err(error) => {
            error!("componentize: {}", error);
            wasm
        }
    };

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
}

fn print_wit(wasm: &[u8], is_core: bool) -> Result<String, Box<dyn std::error::Error>> {
    let decoded = if is_core {
        let (_, bindgen) = wit_component::metadata::decode(wasm)?;
        DecodedWasm::Component(bindgen.resolve, bindgen.world)
    } else {
        wit_component::decode(wasm)?
    };

    let mut printer = wit_component::WitPrinter::default();
    printer.print(decoded.resolve(), decoded.package(), &[])?;
    Ok(printer.output.to_string())
}

fn componentize(wasm: &[u8]) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut encoder = ComponentEncoder::default().validate(true);
    encoder = encoder.module(wasm)?;
    let wasm = encoder.encode()?;
    Ok(wasm)
}

// ----------------------------------------------------------------------------

fn write_report(report: &miette::Report) {
    error!("{}", report);
}

// ----------------------------------------------------------------------------

static LOGGER: Logger = Logger;

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        let body = record.args().to_string();
        unsafe {
            sandbox_log(
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
