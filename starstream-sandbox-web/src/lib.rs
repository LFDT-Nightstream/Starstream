//! WebAssembly bindings for the Starstream compiler.
mod platform;

use std::panic;
use std::sync::{LazyLock, Mutex};

use log::error;
use wasmtime::Store;
use wasmtime::component::{Component, InstancePre, Linker, Val};
use wit_component::{ComponentEncoder, DecodedWasm};

static ENGINE: LazyLock<wasmtime::Engine> = LazyLock::new(wasmtime::Engine::default);
static INSTANCE: Mutex<Option<InstancePre<()>>> = Mutex::new(None);

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

    // Describe the WIT contract of a deployed component as wit-parser's JSON
    // serialization of the resolved WIT (`wasm-tools component wit --json`).
    unsafe fn set_deployed_wit_json(ptr: *const u8, len: usize);
    // Report the WAVE-encoded result of a `call`.
    unsafe fn set_call_result(ptr: *const u8, len: usize);
}

#[derive(serde::Deserialize)]
struct Input<'a> {
    code: &'a str,
}

/// Cranelift profiler that does nothing: the default one calls
/// `Instant::now()`, which panics on wasm32-unknown-unknown.
struct NoopProfiler;

impl cranelift_codegen::timing::Profiler for NoopProfiler {
    fn start_pass(&self, _pass: cranelift_codegen::timing::Pass) -> Box<dyn std::any::Any> {
        Box::new(())
    }
}

/// Set up output and panic context.
fn init() {
    _ = cranelift_codegen::timing::set_thread_profiler(Box::new(NoopProfiler));
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
}

/// # Safety
///
/// Exports to do work, called by the JS page.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn run(input_len: usize) {
    init();

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
        Ok(mut program) => {
            for warning in program.warnings.drain(..) {
                write_report(&warning.into());
            }
            program
        }
        Err(failure) => {
            for warning in failure.warnings {
                write_report(&warning.into());
            }
            for error in failure.errors {
                write_report(&error.into());
            }
            return;
        }
    };

    // Compile to Wasm.
    let compile_result = starstream_to_wasm::compile(&typed.program);
    for error in compile_result.errors {
        write_report(&error.into());
    }
    let Some(wasm) = compile_result.wasm else {
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

fn handle_deploy(input_len: usize) -> Result<(), Box<dyn std::error::Error>> {
    let mut wasm = vec![0; input_len];
    unsafe { read_input(wasm.as_mut_ptr(), wasm.len()) };

    let component = Component::new(&ENGINE, &wasm)?;
    let mut linker = Linker::new(&ENGINE);
    // TODO: Implement imports
    linker.define_unknown_imports_as_traps(&component)?;
    let instance = linker.instantiate_pre(&component)?;

    let wasm = wit_component::decode(&wasm)?;
    let wit_json = serde_json::to_string(wasm.resolve())?;

    *INSTANCE.lock().unwrap() = Some(instance);
    unsafe { set_deployed_wit_json(wit_json.as_ptr(), wit_json.len()) };

    Ok(())
}

/// Deploys a contract, called by the JS page.
///
/// Describes the component's WIT contract back to the UI via
/// `set_deployed_wit_json` and returns 0, or -1 if the deploy failed.
///
/// # Safety
///
/// `input_len` must be the length of the component Wasm provided by `read_input`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn deploy(input_len: usize) -> i32 {
    init();
    match handle_deploy(input_len) {
        Ok(()) => 0,
        Err(error) => {
            error!("deploy: {error}");
            -1
        }
    }
}

#[derive(serde::Deserialize)]
struct CallInput {
    /// Name of the exported function to call.
    name: String,
    /// One WAVE-encoded value per function parameter.
    args: Vec<String>,
}

fn handle_call(input_len: usize) -> Result<(), Box<dyn std::error::Error>> {
    let mut input = vec![0; input_len];
    unsafe { read_input(input.as_mut_ptr(), input.len()) };
    let input: CallInput = serde_json::from_slice(&input)?;

    let instance = INSTANCE
        .lock()
        .map_err(|_| "instance lock poisoned, please redeploy")?;
    let instance = instance.as_ref().ok_or("no contract deployed")?;

    let mut store = Store::new(&ENGINE, ());
    let instance = instance.instantiate(&mut store)?;

    let func = instance
        .get_func(&mut store, input.name.as_str())
        .ok_or_else(|| format!("no exported function `{}`", input.name))?;

    let ty = func.ty(&store);
    if ty.params().len() != input.args.len() {
        return Err(format!(
            "`{}` takes {} argument(s), got {}",
            input.name,
            ty.params().len(),
            input.args.len()
        )
        .into());
    }
    let params = ty
        .params()
        .zip(&input.args)
        .map(|((_, ty), arg)| Val::from_wave(&ty, arg))
        .collect::<wasmtime::Result<Vec<_>>>()?;

    let mut results = vec![Val::Bool(false); ty.results().len()];
    func.call(&mut store, &params, &mut results)?;

    match &results[..] {
        [] => Ok(()),
        [result] => {
            let result = result.to_wave()?;
            unsafe { set_call_result(result.as_ptr(), result.len()) };
            Ok(())
        }
        [..] => Err("invalid result value count".into()),
    }
}

/// Calls an exported function of the deployed contract, called by the JS page.
///
/// Reports the function's result back to the UI via `set_call_result` (skipped
/// for functions with no result) and returns 0, or -1 if the call failed.
///
/// # Safety
///
/// `input_len` must be the length of the JSON-encoded `CallInput` provided by
/// `read_input`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn call(input_len: usize) -> i32 {
    init();
    match handle_call(input_len) {
        Ok(()) => 0,
        Err(error) => {
            error!("call: {error}");
            -1
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
