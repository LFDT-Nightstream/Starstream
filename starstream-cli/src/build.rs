use std::fs;
use std::path::{Path, PathBuf};

use clap::Args;
use starstream_compiler::{TypecheckOptions, typecheck_modules};
use starstream_types::FileSystem;

use crate::InputArg;
use crate::diagnostics::print_diagnostic;

/// Compile every contract under the target directory to Wasm.
///
/// The scanner builds a single workspace module graph, runs one typecheck
/// pass, then emits per-contract wasm under
/// `<project-root>/artifacts/<filename-stem>/`.
#[derive(Args, Debug)]
pub struct Build {
    #[command(flatten)]
    pub input: InputArg,
}

pub struct BuildSuccess {
    pub component_wasm: Vec<PathBuf>,
}

impl Build {
    pub fn exec(self) -> miette::Result<BuildSuccess> {
        let mut result = BuildSuccess {
            component_wasm: Vec::new(),
        };

        let mut fs = FileSystem::new();
        let graph = self.input.to_module_graph(&mut fs)?;

        let typed = match typecheck_modules(&graph, TypecheckOptions::default()) {
            Ok(success) => {
                for (module_id, warning) in &success.warnings {
                    print_diagnostic(graph.source(*module_id), warning.clone())?;
                }
                success
            }
            Err(failure) => {
                for (module_id, warning) in failure.warnings {
                    print_diagnostic(graph.source(module_id), warning)?;
                }
                for (module_id, error) in failure.errors {
                    print_diagnostic(graph.source(module_id), error)?;
                }
                std::process::exit(1);
            }
        };

        let artifacts_dir = self.input.get_artifacts_dir()?;

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

            let mut compile_result = starstream_to_wasm::compile_contract(&typed, entry_id);
            for error in compile_result.errors.drain(..) {
                print_diagnostic(graph.source(error.module.unwrap_or(entry_id)), error)?;
            }
            let Some(wasm) = compile_result.wasm.clone() else {
                had_errors = true;
                continue;
            };

            if let Err(err) = write_outputs(
                &mut result,
                &out_dir,
                &wasm,
                &compile_result,
                &stem,
                &mut fs,
            ) {
                eprintln!("error: {err}");
                had_errors = true;
            }
        }

        if had_errors {
            std::process::exit(1);
        }
        Ok(result)
    }
}

fn write_outputs(
    result: &mut BuildSuccess,
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

    let component_wasm = compile_result
        .to_component()
        .map_err(|e| format!("failed to link: {e}"))?;

    let component_path = out_dir.join("component.wasm");
    fs_tracker
        .write(&component_path, &component_wasm)
        .map_err(|e| format!("failed to write `{}`: {e}", component_path.display()))?;
    result.component_wasm.push(component_path);

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
