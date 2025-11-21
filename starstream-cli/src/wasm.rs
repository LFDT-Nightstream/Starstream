use std::{fs, path::PathBuf};

use crate::diagnostics::print_diagnostic;
use clap::Args;
use miette::{IntoDiagnostic, NamedSource};
use wit_component::ComponentEncoder;

/// Compile Starstream source to Wasm.
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

    /// Output component WIT text to this file.
    #[arg(long)]
    output_wit: Option<PathBuf>,
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
        if let Some(output_core) = &self.output_core {
            std::fs::write(output_core, &wasm).expect("Error writing Wasm output");
        }

        // Componentize
        if self.output_component.is_some() || self.output_wit.is_some() {
            let mut encoder = ComponentEncoder::default().validate(true);
            // TODO: less .unwrap()
            encoder = encoder.module(&wasm).unwrap();
            let wasm = encoder.encode().unwrap();

            if let Some(output_component) = &self.output_component {
                std::fs::write(output_component, &wasm).expect("Error writing Wasm output");
            }

            if let Some(output_wit) = &self.output_wit {
                let decoded = wit_component::decode(&wasm).unwrap();
                let mut printer = wit_component::WitPrinter::default();
                printer
                    .print(decoded.resolve(), decoded.package(), &[])
                    .unwrap();
                let output = printer.output.to_string();
                std::fs::write(output_wit, output.as_bytes()).expect("Error writing WIT output");
            }
        }

        Ok(())
    }
}
