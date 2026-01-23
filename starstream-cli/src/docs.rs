use std::{fs, path::PathBuf, str::FromStr};

use clap::Args;
use miette::{IntoDiagnostic, NamedSource};
use starstream_compiler::{TypecheckOptions, generate_docs, parse_program, typecheck_program};

use crate::diagnostics::print_diagnostic;

/// Generate JSON documentation for Starstream source files.
#[derive(Args, Debug)]
pub struct Docs {
    /// Source file to document.
    file: String,

    /// Output file (default: stdout).
    #[clap(short, long)]
    output: Option<String>,

    /// Pretty-print the JSON output.
    #[clap(long)]
    pretty: bool,
}

impl Docs {
    pub fn exec(self) -> miette::Result<()> {
        let path = PathBuf::from_str(&self.file).into_diagnostic()?;
        let source = fs::read_to_string(&path).into_diagnostic()?;

        let parse_output = parse_program(&source);
        let named = NamedSource::new(path.display().to_string(), source.clone());

        if !parse_output.errors().is_empty() {
            for error in parse_output.errors {
                print_diagnostic(named.clone(), error)?;
            }
            std::process::exit(1);
        }

        let Some(ref program) = parse_output.program else {
            std::process::exit(1);
        };

        let typed = match typecheck_program(&program, TypecheckOptions::default()) {
            Ok(success) => success.program,
            Err(errors) => {
                for error in errors {
                    print_diagnostic(named.clone(), error)?;
                }
                std::process::exit(1);
            }
        };

        let comment_map = parse_output.comment_map();
        let docs = generate_docs(&program, &typed, &comment_map, &source);

        let json = if self.pretty {
            serde_json::to_string_pretty(&docs).into_diagnostic()?
        } else {
            serde_json::to_string(&docs).into_diagnostic()?
        };

        match self.output {
            Some(path) => fs::write(path, json).into_diagnostic()?,
            None => println!("{}", json),
        }

        Ok(())
    }
}
