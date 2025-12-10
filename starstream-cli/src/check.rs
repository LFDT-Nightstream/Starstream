use std::{
    fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use clap::Args;
use miette::{IntoDiagnostic, NamedSource};
use starstream_compiler::{TypecheckOptions, parse_program, typecheck_program};

use crate::{diagnostics::print_diagnostic, starstream_files_excluding_gitignore, style};

/// Type-check Starstream source files.
#[derive(Args, Debug)]
pub struct Check {
    /// Files or directories to check. Defaults to the current directory.
    #[clap(default_value = ".")]
    targets: Vec<String>,
}

impl Check {
    /// Walk the requested paths, printing diagnostics and returning a non-zero
    /// exit code if any errors were encountered.
    pub fn exec(self) -> miette::Result<()> {
        let mut had_errors = false;
        let mut total_errors = 0usize;

        for target in self.targets {
            let path = PathBuf::from_str(&target).into_diagnostic()?;

            if path.is_dir() {
                for file in starstream_files_excluding_gitignore(&path) {
                    let result = check_file(&file)?;

                    had_errors |= result.errors > 0;

                    total_errors += result.errors;
                }
            } else {
                let result = check_file(&path)?;

                had_errors |= result.errors > 0;

                total_errors += result.errors;
            }
        }

        if had_errors {
            eprintln!(
                "Summary: {} error{}",
                total_errors,
                style::r_if_then(total_errors != 1, "s")
            );

            std::process::exit(1);
        } else {
            Ok(())
        }
    }
}

struct CheckResult {
    errors: usize,
}

/// Parse and type-check a single file, returning the number of diagnostics
/// emitted so the caller can aggregate totals and decide the exit status.
fn check_file(path: &Path) -> miette::Result<CheckResult> {
    let source = fs::read_to_string(path).into_diagnostic()?;

    let parse_output = parse_program(&source);

    let named = NamedSource::new(path.display().to_string(), source.clone());

    let mut errors = 0usize;

    if !parse_output.errors().is_empty() {
        for error in parse_output.errors {
            print_diagnostic(named.clone(), error)?;

            errors += 1;
        }
    }

    let Some(program) = parse_output.program else {
        return Ok(CheckResult { errors });
    };

    match typecheck_program(&program, TypecheckOptions::default()) {
        Ok(_typed) => Ok(CheckResult { errors }),
        Err(type_errors) => {
            for error in type_errors {
                print_diagnostic(named.clone(), error)?;

                errors += 1;
            }
            Ok(CheckResult { errors })
        }
    }
}
