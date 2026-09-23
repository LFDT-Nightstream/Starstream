use std::fs;
use std::path::PathBuf;

use clap::Args;
use miette::IntoDiagnostic;
use starstream_compiler::module_graph::ModuleContents;
use starstream_compiler::typecheck::TypedModuleContents;
use starstream_compiler::{ModuleGraph, TypecheckOptions, generate_docs, typecheck_modules};
use starstream_types::FileSystem;

use crate::diagnostics::{print_diagnostic, print_report};
use crate::project::default_scan_dir;

/// Generate JSON documentation for every contract under the target directory.
///
/// With no path argument, walks up from cwd for `.git` and scans that
/// project root; otherwise scans the given dir. Builds one workspace
/// graph, runs typecheck once, then emits per-contract docs at
/// `<project-root>/artifacts/<filename-stem>/docs.json`.
#[derive(Args, Debug)]
pub struct Docs {
    /// Optional directory to scan. If omitted, walks up for `.git` and scans
    /// the enclosing project root (falling back to cwd if no `.git` exists).
    target_dir: Option<PathBuf>,

    /// Pretty-print the JSON output.
    #[clap(long)]
    pretty: bool,
}

impl Docs {
    pub fn exec(self) -> miette::Result<()> {
        let scan_dir = match self.target_dir {
            Some(dir) => dir,
            None => default_scan_dir().into_diagnostic()?,
        };

        let mut fs = FileSystem::new();
        let graph = match ModuleGraph::from_workspace(&mut fs, &scan_dir) {
            Ok(g) => g,
            Err(errors) => {
                for error in errors {
                    print_report(miette::Report::new(error))?;
                }
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

        let target_abs = fs::canonicalize(&scan_dir).into_diagnostic()?;
        let artifacts_dir = target_abs.join("artifacts");
        fs::create_dir_all(&artifacts_dir).into_diagnostic()?;

        for &entry_id in &typed.contract_entries {
            let entry_module = graph.module(entry_id);
            let entry_typed_module = typed.module(entry_id);

            let ModuleContents::Starstream(entry_program) = &entry_module.contents else {
                continue;
            };
            let TypedModuleContents::Starstream(entry_typed_program) = &entry_typed_module.contents
            else {
                continue;
            };

            let docs = generate_docs(
                entry_program,
                entry_typed_program,
                &starstream_types::CommentMap::new(),
                entry_module.source.as_ref(),
            );

            let json = if self.pretty {
                serde_json::to_string_pretty(&docs).into_diagnostic()?
            } else {
                serde_json::to_string(&docs).into_diagnostic()?
            };

            let stem = entry_typed_module
                .abs_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("contract")
                .to_string();
            let out_dir = artifacts_dir.join(stem);
            fs::create_dir_all(&out_dir).into_diagnostic()?;
            fs::write(out_dir.join("docs.json"), json).into_diagnostic()?;
        }
        Ok(())
    }
}
