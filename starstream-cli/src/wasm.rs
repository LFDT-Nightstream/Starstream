use std::{fs, path::PathBuf};

use crate::diagnostics::print_diagnostic;
use clap::Args;
use miette::{IntoDiagnostic, NamedSource};

/// Compile Starstream source to Wasm.
#[derive(Args, Debug)]
pub struct Wasm {
    /// The Starstream source file to compile.
    #[arg(short = 'c')]
    compile_file: PathBuf,
    /// The Wasm output file.
    #[arg(short = 'o')]
    output_file: Option<PathBuf>,
}

impl Wasm {
    /// Parse, type-check, and compile the requested source file into a Wasm module.
    pub fn exec(self) -> miette::Result<()> {
        let source_text = fs::read_to_string(&self.compile_file).into_diagnostic()?;
        let named = NamedSource::new(self.compile_file.display().to_string(), source_text.clone());

        let parse_output = starstream_compiler::parse_program(&source_text);
        for error in parse_output.errors().iter().cloned() {
            print_diagnostic(named.clone(), error)?;
        }

        let Some(program) = parse_output.into_program() else {
            std::process::exit(1);
        };

        let typed = match starstream_compiler::typecheck_program(&program, Default::default()) {
            Ok(program) => program,
            Err(errors) => {
                for error in errors {
                    print_diagnostic(named.clone(), error)?;
                }
                std::process::exit(1);
            }
        };

        // Wasm
        let (wasm, errors) = starstream_to_wasm::compile(&typed.program);
        for error in errors {
            print_diagnostic(named.clone(), error)?;
        }
        let Some(wasm) = wasm else {
            std::process::exit(1);
        };

        if let Some(output_file) = &self.output_file {
            std::fs::write(output_file, &wasm).expect("Error writing Wasm output");
        }

        Ok(())
    }
}
