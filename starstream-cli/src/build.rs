use std::fs;
use std::path::{Path, PathBuf};

use clap::Args;
use miette::IntoDiagnostic;
use starstream_compiler::{TypecheckOptions, module_graph, typecheck_modules};
use starstream_types::FileSystem;
use wit_component::ComponentEncoder;

use crate::diagnostics::print_diagnostic;
use crate::project::default_scan_dir;
use crate::wasm::{build_named_sources, report_graph_error};

/// Compile every contract under the target directory to wasm.
///
/// With no path argument, walks up from cwd for `.git` and scans that
/// project root; otherwise scans the given dir. The scanner builds a
/// single workspace module graph, runs one typecheck pass, then emits
/// per-contract wasm under `<project-root>/artifacts/<filename-stem>/`.
#[derive(Args, Debug)]
pub struct Build {
    /// Optional directory to scan. If omitted, walks up for `.git` and scans
    /// the enclosing project root (falling back to cwd if no `.git` exists).
    target_dir: Option<PathBuf>,
}

impl Build {
    pub fn exec(self) -> miette::Result<()> {
        let scan_dir = match self.target_dir {
            Some(dir) => dir,
            None => default_scan_dir().into_diagnostic()?,
        };

        let mut fs_tracker = FileSystem::new();
        let graph = match module_graph::load_workspace(&scan_dir, &mut fs_tracker) {
            Ok(g) => g,
            Err(err) => {
                report_graph_error(&err);
                std::process::exit(1);
            }
        };

        if graph.contract_entries().is_empty() {
            eprintln!(
                "no contracts found under `{}` (looking for `.star` files containing `contract;`)",
                scan_dir.display()
            );
            return Ok(());
        }

        let sources = build_named_sources(&graph);

        let typed = match typecheck_modules(&graph, TypecheckOptions::default()) {
            Ok(success) => {
                for (module_id, warning) in &success.warnings {
                    if let Some(named) = sources.get(&module_id.0) {
                        print_diagnostic(named.clone(), warning.clone())?;
                    }
                }
                success
            }
            Err(failure) => {
                for (module_id, warning) in failure.warnings {
                    if let Some(named) = sources.get(&module_id.0) {
                        print_diagnostic(named.clone(), warning)?;
                    }
                }
                for (module_id, error) in failure.errors {
                    if let Some(named) = sources.get(&module_id.0) {
                        print_diagnostic(named.clone(), error)?;
                    }
                }
                std::process::exit(1);
            }
        };

        let target_abs = fs::canonicalize(&scan_dir).unwrap_or(scan_dir.clone());
        let artifacts_dir = target_abs.join("artifacts");
        if let Err(err) = fs::create_dir_all(&artifacts_dir) {
            eprintln!(
                "error: failed to create `{}`: {}",
                artifacts_dir.display(),
                err
            );
            std::process::exit(1);
        }

        let mut had_errors = false;
        for &entry_id in &typed.contract_entries {
            let entry_module = typed.module(entry_id);
            let stem = entry_module
                .abs_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("contract")
                .to_string();
            let out_dir = artifacts_dir.join(&stem);
            if let Err(err) = fs::create_dir_all(&out_dir) {
                eprintln!("error: failed to create `{}`: {}", out_dir.display(), err);
                had_errors = true;
                continue;
            }

            let entry_named = sources
                .get(&entry_id.0)
                .cloned()
                .expect("entry module has a NamedSource");

            let mut compile_result = starstream_to_wasm::compile_contract(&typed, entry_id);
            for error in compile_result.errors.drain(..) {
                print_diagnostic(entry_named.clone(), error)?;
            }
            let Some(wasm) = compile_result.wasm.clone() else {
                had_errors = true;
                continue;
            };

            if let Err(err) =
                write_outputs(&out_dir, &wasm, &compile_result, &stem, &mut fs_tracker)
            {
                eprintln!("error: {err}");
                had_errors = true;
            }
        }

        if had_errors {
            std::process::exit(1);
        }
        Ok(())
    }
}

fn write_outputs(
    out_dir: &Path,
    wasm: &[u8],
    compile_result: &starstream_to_wasm::CompileResult,
    stem: &str,
    fs_tracker: &mut FileSystem,
) -> Result<(), String> {
    let core_path = out_dir.join("core.wasm");
    fs_tracker
        .write(&core_path, wasm)
        .map_err(|e| format!("failed to write `{}`: {e}", core_path.display()))?;

    if let Some(binary_wit) = &compile_result.binary_wit {
        let binary_wit_path = out_dir.join("contract.binary.wit");
        fs_tracker
            .write(&binary_wit_path, binary_wit)
            .map_err(|e| format!("failed to write `{}`: {e}", binary_wit_path.display()))?;
    }

    let mut enc = ComponentEncoder::default()
        .validate(true)
        .module(wasm)
        .map_err(|e| format!("failed to wrap module as component for `{stem}`: {e}"))?;
    let component_wasm = enc
        .encode()
        .map_err(|e| format!("failed to encode component for `{stem}`: {e}"))?;

    let component_path = out_dir.join("component.wasm");
    fs_tracker
        .write(&component_path, &component_wasm)
        .map_err(|e| format!("failed to write `{}`: {e}", component_path.display()))?;

    let decoded = wit_component::decode(&component_wasm)
        .map_err(|e| format!("failed to decode component WIT for `{stem}`: {e}"))?;
    let mut printer = wit_component::WitPrinter::default();
    printer.emit_docs(true);
    let ids: Vec<_> = decoded
        .resolve()
        .packages
        .iter()
        .map(|(id, _)| id)
        .filter(|id| *id != decoded.package())
        .collect();
    printer
        .print(decoded.resolve(), decoded.package(), &ids)
        .ok();
    let wit_text = printer.output.to_string();

    let wit_path = out_dir.join("contract.wit");
    fs_tracker
        .write(wit_path.as_path(), wit_text.as_bytes())
        .map_err(|e| format!("failed to write `{}`: {e}", wit_path.display()))?;

    let mut depfile_contents = Vec::new();
    if write_rule(
        &mut depfile_contents,
        fs_tracker.outputs.iter().map(PathBuf::as_path),
        fs_tracker.dependencies.iter().map(PathBuf::as_path),
    )
    .is_ok()
    {
        let depfile_path = out_dir.join("deps.d");
        fs_tracker
            .write(&depfile_path, &depfile_contents)
            .map_err(|e| format!("failed to write `{}`: {e}", depfile_path.display()))?;
    }

    Ok(())
}

/*
    Depfile grammar from https://cmake.org/cmake/help/latest/command/add_custom_command.html#grammar-token-depfile-depfile

    depfile       ::= rule*
    rule          ::= targets (':' (separator dependencies?)?)? eol
    targets       ::= target (separator target)* separator*
    target        ::= pathname
    dependencies  ::= dependency (separator dependency)* separator*
    dependency    ::= pathname
    separator     ::= (space | line_continue)+
    line_continue ::= '\' eol
    space         ::= ' ' | '\t'
    pathname      ::= character+
    character     ::= std_character | dollar | hash | whitespace
    std_character ::= <any character except '$', '#' or ' '>
    dollar        ::= '$$'
    hash          ::= '\#'
    whitespace    ::= '\ '
    eol           ::= '\r'? '\n'
*/

fn write_rule<'a>(
    dest: &mut impl std::io::Write,
    targets: impl Iterator<Item = &'a Path>,
    dependencies: impl Iterator<Item = &'a Path>,
) -> Result<(), std::io::Error> {
    let mut any = false;
    for target in targets {
        write_pathname(dest, target)?;
        dest.write_all(b" ")?;
        any = true;
    }

    if any {
        dest.write_all(b": ")?;
        for dep in dependencies {
            write_pathname(dest, dep)?;
            dest.write_all(b" ")?;
        }
        dest.write_all(b"\n")?;
    }
    Ok(())
}

fn write_pathname(dest: &mut impl std::io::Write, path: &Path) -> Result<(), std::io::Error> {
    for &byte in path.as_os_str().as_encoded_bytes() {
        match byte {
            b'$' => dest.write_all(b"$$")?,
            b'#' => dest.write_all(b"\\#")?,
            b' ' => dest.write_all(b"\\ ")?,
            b'\\' => dest.write_all(b"/")?,
            _ => dest.write_all(&[byte])?,
        }
    }
    Ok(())
}
