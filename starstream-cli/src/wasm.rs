use std::collections::HashMap;
use std::path::{Path, PathBuf};

use clap::Args;
use miette::NamedSource;
use starstream_compiler::{
    ModuleGraph, ModuleGraphError, TypecheckOptions, module_graph, typecheck_modules,
};
use starstream_types::FileSystem;
use wit_component::ComponentEncoder;

use crate::diagnostics::print_diagnostic;

/// Compile a single `.star` file to wasm.
///
/// The file is treated as a contract regardless of whether it declares
/// `contract;`. Path imports inside it are resolved relative to the file's
/// directory; helpers reached through that chain must NOT themselves declare
/// `contract;` (cross-contract calls aren't supported yet).
///
/// Output flags follow the legacy shape (`--output-core` etc.) so this command
/// stays scriptable for one-off compilation. Use `starstream build` to compile
/// every contract in a workspace at once.
#[derive(Args, Debug)]
pub struct Wasm {
    /// The Starstream source file to compile.
    #[arg(short = 'c')]
    compile_file: PathBuf,

    /// Output core Wasm to this file.
    #[arg(long, short = 'o')]
    output_core: Option<PathBuf>,

    /// Output component Wasm to this file.
    #[arg(long)]
    output_component: Option<PathBuf>,

    /// Output a binary WIT representation to this file.
    #[arg(long)]
    output_binary_wit: Option<PathBuf>,

    /// Output component WIT text to this file.
    #[arg(long)]
    output_wit: Option<PathBuf>,

    /// Output dependency information in Make/Ninja compatible format.
    #[arg(long, short = 'M')]
    depfile: Option<PathBuf>,
}

impl Wasm {
    pub fn exec(self) -> miette::Result<()> {
        let mut fs = FileSystem::new();

        let graph = match module_graph::load_from_entry(&self.compile_file, &mut fs) {
            Ok(graph) => graph,
            Err(err) => {
                report_graph_error(&err);
                std::process::exit(1);
            }
        };

        let sources = build_named_sources(&graph);
        let entry_id = graph
            .contract_entries()
            .first()
            .copied()
            .expect("load_from_entry always sets a single contract entry");
        let entry_named = sources
            .get(&entry_id.0)
            .cloned()
            .expect("entry module always has a NamedSource");

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

        let compile_result = starstream_to_wasm::compile_contract(&typed, entry_id);
        for error in compile_result.errors {
            print_diagnostic(entry_named.clone(), error)?;
        }
        let Some(wasm) = compile_result.wasm else {
            std::process::exit(1);
        };

        if let Some(output_core) = &self.output_core {
            fs.write(output_core, &wasm)
                .expect("Error writing Wasm output");
        }

        if let Some(output_binary_wit) = self.output_binary_wit {
            let binary_wit = compile_result
                .binary_wit
                .expect("Strange: compilation succeeded, but there was no binary WIT");
            fs.write(&output_binary_wit, &binary_wit)
                .expect("Error writing binary WIT output");
        }

        if self.output_component.is_some() || self.output_wit.is_some() {
            let component_wasm = ComponentEncoder::default()
                .validate(true)
                .module(&wasm)
                .expect("ComponentEncoder::module failed")
                .encode()
                .expect("ComponentEncoder::encode failed");

            if let Some(output_component) = &self.output_component {
                fs.write(output_component, &component_wasm)
                    .expect("Error writing Wasm output");
            }

            if let Some(output_wit) = &self.output_wit {
                let decoded = wit_component::decode(&component_wasm).unwrap();
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
                    .unwrap();
                let output = printer.output.to_string();
                fs.write(output_wit, output.as_bytes())
                    .expect("Error writing WIT output");
            }
        }

        if let Some(depfile) = self.depfile {
            let mut depfile_contents = Vec::new();
            write_rule(
                &mut depfile_contents,
                fs.outputs.iter().map(|s| s.as_path()),
                fs.dependencies.iter().map(|s| s.as_path()),
            )
            .expect("Error writing depfile");
            fs.write(&depfile, &depfile_contents)
                .expect("Error writing depfile");
        }

        Ok(())
    }
}

pub(crate) fn build_named_sources(graph: &ModuleGraph) -> HashMap<u32, NamedSource<String>> {
    graph
        .modules()
        .iter()
        .map(|module| {
            let path = module.abs_path.display().to_string();
            let source = module.source.as_ref().to_string();
            (module.id.0, NamedSource::new(path, source))
        })
        .collect()
}

pub(crate) fn report_graph_error(err: &ModuleGraphError) {
    match err {
        ModuleGraphError::EntryIo { path, error } => {
            eprintln!("error: failed to read `{}`: {}", path.display(), error);
        }
        ModuleGraphError::ImportIo { path, error, .. } => {
            eprintln!(
                "error: failed to resolve path import `{}`: {}",
                path.display(),
                error
            );
        }
        ModuleGraphError::NonRelativePath { path, .. } => {
            eprintln!("error: path import `{path}` must start with `./` or `../`");
        }
        ModuleGraphError::NotStarExtension { path, .. } => {
            eprintln!("error: path import `{path}` must end with `.star`");
        }
        ModuleGraphError::CrossContractImport {
            importer_path,
            target_path,
            ..
        } => {
            eprintln!(
                "error: cross-contract calls not supported yet — `{}` imports `{}`, which also declares `contract;`",
                importer_path.display(),
                target_path.display()
            );
        }
        ModuleGraphError::Cycle { chain } => {
            eprintln!("error: cyclic path import detected:");
            for (_id, p, _span) in chain {
                eprintln!("  - {}", p.display());
            }
        }
        ModuleGraphError::ParseFailed { failures } => {
            for (module_id, errors) in failures {
                eprintln!("parse errors in module #{}:", module_id.0);
                for e in errors {
                    eprintln!("  {e}");
                }
            }
        }
    }
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
