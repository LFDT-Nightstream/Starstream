use std::path::PathBuf;

use clap::Args;
use miette::IntoDiagnostic;
use starstream_compiler::{ModuleGraph, TypecheckOptions, typecheck_modules};
use starstream_types::FileSystem;

use crate::diagnostics::{print_diagnostic, print_report};
use crate::project::default_scan_dir;
use crate::style;

/// Build one workspace module graph for the target directory and type-check
/// every module in it.
///
/// With no path argument, walks up from cwd for `.git` and scans that
/// project root (falling back to cwd if no `.git`). With an explicit dir,
/// scans exactly that dir.
#[derive(Args, Debug)]
pub struct Check {
    /// Optional directory to scan. If omitted, walks up for `.git` and scans
    /// the enclosing project root (falling back to cwd if no `.git` exists).
    target_dir: Option<PathBuf>,

    /// Exit with a non-zero code if warnings are emitted.
    #[arg(short = 'D', long, visible_alias = "warnings-as-errors")]
    deny_warnings: bool,
}

impl Check {
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

        if graph.modules().is_empty() {
            eprintln!("no `.star` files found under `{}`", scan_dir.display());
            return Ok(());
        }

        let mut errors = 0usize;
        let mut warnings = 0usize;

        match typecheck_modules(&graph, TypecheckOptions::default()) {
            Ok(success) => {
                for (module_id, warning) in success.warnings {
                    print_diagnostic(graph.source(module_id), warning)?;
                    warnings += 1;
                }
            }
            Err(failure) => {
                for (module_id, warning) in failure.warnings {
                    print_diagnostic(graph.source(module_id), warning)?;
                    warnings += 1;
                }
                for (module_id, error) in failure.errors {
                    print_diagnostic(graph.source(module_id), error)?;
                    errors += 1;
                }
            }
        }

        let contract_count = graph.contract_entries().len();
        if errors > 0 || warnings > 0 {
            eprintln!(
                "Summary: {} contract{}, {} module{}, {} error{}, {} warning{}",
                contract_count,
                style::r_if_then(contract_count != 1, "s"),
                graph.modules().len(),
                style::r_if_then(graph.modules().len() != 1, "s"),
                errors,
                style::r_if_then(errors != 1, "s"),
                warnings,
                style::r_if_then(warnings != 1, "s")
            );
        }

        if errors > 0 || (self.deny_warnings && warnings > 0) {
            std::process::exit(1);
        }
        Ok(())
    }
}
