use clap::Args;
use starstream_compiler::{TypecheckOptions, typecheck_modules};
use starstream_types::FileSystem;

use crate::diagnostics::print_diagnostic;
use crate::{InputArg, style};

/// Build one workspace module graph for the target directory and type-check
/// every module in it.
///
/// With no path argument, walks up from cwd for `.git` and scans that
/// project root (falling back to cwd if no `.git`). With an explicit dir,
/// scans exactly that dir.
#[derive(Args, Debug)]
pub struct Check {
    #[command(flatten)]
    pub input: InputArg,

    /// Exit with a non-zero code if warnings are emitted.
    #[arg(short = 'D', long, visible_alias = "warnings-as-errors")]
    pub deny_warnings: bool,
}

impl Check {
    pub fn exec(self) -> miette::Result<()> {
        let mut fs = FileSystem::new();
        let graph = self.input.to_module_graph(&mut fs)?;

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
