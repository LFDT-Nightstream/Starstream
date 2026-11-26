//! Starstream compiler as a Wasm module, compiling contracts from source files
//! in memory to Wasm components.
//!
//! `compiler_add` adds source files, and `compiler_compile` compiles the
//! contract at an entry path among them, passing the component, if any, and
//! the diagnostics to the `compiler_result` import, and then forgets the
//! files. Invalid arguments and panics pass a message to the `compiler_error`
//! import.

#![cfg(target_family = "wasm")]

use core::cell::RefCell;
use core::ptr;

use std::panic;
use std::path::Path;
use std::rc::Rc;
use std::sync::Once;

use anyhow::Context as _;
use miette::{Diagnostic, GraphicalReportHandler, GraphicalTheme, Report, SourceCode};
use starstream_compiler::{ModuleGraph, TypecheckOptions, typecheck_modules};
use starstream_types::{FileSystem, MemoryFs};

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    fn compiler_error(ptr: *const u8, len: usize);
    fn compiler_result(
        component: *const u8,
        component_len: usize,
        diagnostics: *const u8,
        diagnostics_len: usize,
    );
}

thread_local! {
    static FILES: RefCell<MemoryFs> = RefCell::default();
}

fn report_error(err: &str) {
    unsafe { compiler_error(err.as_ptr(), err.len()) }
}

fn init() {
    static INIT: Once = Once::new();
    INIT.call_once(|| panic::set_hook(Box::new(|info| report_error(&info.to_string()))));
}

fn report(res: anyhow::Result<()>) -> i32 {
    match res {
        Ok(()) => 0,
        Err(err) => {
            report_error(&format!("{err:#}"));
            -1
        }
    }
}

unsafe fn take(ptr: *mut u8, len: usize) -> Vec<u8> {
    let buf = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(ptr, len)) };
    buf.into_vec()
}

fn render(
    diagnostics: &mut String,
    source: impl SourceCode + 'static,
    error: impl Diagnostic + Send + Sync + 'static,
) {
    render_report(diagnostics, &Report::new(error).with_source_code(source));
}

fn render_report(diagnostics: &mut String, report: &Report) {
    let handler = GraphicalReportHandler::new_themed(GraphicalTheme::unicode_nocolor());
    if handler.render_report(diagnostics, report.as_ref()).is_err() {
        diagnostics.push_str(&report.to_string());
    }
    diagnostics.push('\n');
}

fn component(files: MemoryFs, entry: &Path, diagnostics: &mut String) -> Option<Rc<[u8]>> {
    let mut fs = FileSystem::with_vfs(files);
    let (graph, entry_id) = match ModuleGraph::from_entry(&mut fs, entry) {
        Ok(graph) => graph,
        Err(errors) => {
            for error in errors {
                render_report(diagnostics, &error.into());
            }
            return None;
        }
    };
    let typed = match typecheck_modules(&graph, TypecheckOptions::default()) {
        Ok(success) => {
            for (id, warning) in &success.warnings {
                render(diagnostics, graph.source(*id), warning.clone());
            }
            success
        }
        Err(failure) => {
            for (id, warning) in failure.warnings {
                render(diagnostics, graph.source(id), warning);
            }
            for (id, error) in failure.errors {
                render(diagnostics, graph.source(id), error);
            }
            return None;
        }
    };
    let result = starstream_to_wasm::compile_contract(&typed, entry_id);
    for error in &result.errors {
        let source = graph.source(error.module.unwrap_or(entry_id));
        render(diagnostics, source, error.clone());
    }
    result.wasm.as_ref()?;
    match result.to_component() {
        Ok(component) => Some(component),
        Err(err) => {
            diagnostics.push_str(&format!("failed to build component: {err:#}\n"));
            None
        }
    }
}

/// Allocate `len` uninitialized bytes to be written in full and passed to
/// another export, which takes ownership.
#[unsafe(no_mangle)]
pub extern "C" fn compiler_alloc(len: usize) -> *mut u8 {
    init();
    Box::into_raw(Box::<[u8]>::new_uninit_slice(len)).cast()
}

/// Add the source file at the UTF-8 `path` with `data`, replacing any file at
/// `path`.
///
/// # Safety
///
/// `path` and `data` must be results of `compiler_alloc`, written in full,
/// with their lengths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn compiler_add(
    path: *mut u8,
    path_len: usize,
    data: *mut u8,
    data_len: usize,
) -> i32 {
    let path = unsafe { take(path, path_len) };
    let data = unsafe { take(data, data_len) };
    report(
        String::from_utf8(path)
            .context("path is not UTF-8")
            .map(|path| {
                FILES.with_borrow_mut(|files| files.insert(path, data));
            }),
    )
}

/// Compile the contract at the UTF-8 path `entry` and the modules it imports
/// from the files added, passing the component, empty if the compilation
/// failed, and the warnings and errors rendered as text to `compiler_result`,
/// and forget the files.
///
/// # Safety
///
/// `entry` must be the result of `compiler_alloc(len)`, written in full.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn compiler_compile(entry: *mut u8, len: usize) -> i32 {
    let entry = unsafe { take(entry, len) };
    let files = FILES.take();
    report(
        String::from_utf8(entry)
            .context("entry is not UTF-8")
            .map(|entry| {
                let mut diagnostics = String::default();
                let component = component(files, Path::new(&entry), &mut diagnostics);
                let component = component.as_deref().unwrap_or_default();
                unsafe {
                    compiler_result(
                        component.as_ptr(),
                        component.len(),
                        diagnostics.as_ptr(),
                        diagnostics.len(),
                    );
                }
            }),
    )
}
