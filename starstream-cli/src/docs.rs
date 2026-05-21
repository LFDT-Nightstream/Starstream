use std::fs;
use std::path::PathBuf;

use clap::Args;
use miette::IntoDiagnostic;
use starstream_compiler::{TypecheckOptions, generate_docs, module_graph, typecheck_modules};
use starstream_types::FileSystem;

use crate::diagnostics::print_diagnostic;
use crate::project::default_scan_dir;
use crate::wasm::{build_named_sources, report_graph_error};

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

        let mut tracker = FileSystem::new();
        let graph = match module_graph::load_workspace(&scan_dir, &mut tracker) {
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

        let target_abs = fs::canonicalize(&scan_dir).into_diagnostic()?;
        let artifacts_dir = target_abs.join("artifacts");
        fs::create_dir_all(&artifacts_dir).into_diagnostic()?;

        for &entry_id in &typed.contract_entries {
            let entry_typed = typed.module(entry_id);
            let entry_source_module = graph.module(entry_id);

            let docs = generate_docs(
                &entry_source_module.program,
                &entry_typed.program,
                &starstream_types::CommentMap::new(),
                entry_source_module.source.as_ref(),
            );

            let json = if self.pretty {
                serde_json::to_string_pretty(&docs).into_diagnostic()?
            } else {
                serde_json::to_string(&docs).into_diagnostic()?
            };

            let stem = entry_typed
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
