//! The unified command-line interface to the Starstream language compiler and
//! test environment.

use std::path::PathBuf;

use ariadne::{Cache, FileCache};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(arg_required_else_help(true))]
enum Args {
    /// Compile Starstream source to Wasm.
    Wasm {
        /// The Starstream source file to compile.
        #[arg(short = 'c')]
        compile_file: PathBuf,
        /// The Wasm output file.
        #[arg(short = 'o')]
        output_file: Option<PathBuf>,
    },
}

fn main() {
    match Args::parse() {
        Args::Wasm {
            compile_file,
            output_file,
        } => {
            // Load
            let mut cache = FileCache::default();
            let source = cache
                .fetch(&compile_file)
                .expect("Error reading Starstream input");

            // Parse
            let parse_result = starstream_compiler::parse_program(source.text());
            for error in parse_result.errors() {
                // TODO: have error reports include the filename, perhaps by  passing `cache` instead of just `source`
                starstream_compiler::error_to_report(error.clone())
                    .eprint(source)
                    .unwrap();
            }
            let Some(program) = parse_result.into_output() else {
                std::process::exit(1);
            };

            // Wasm
            let wasm = starstream_to_wasm::compile(&program);
            // TODO: error handling

            if let Some(output_file) = output_file {
                std::fs::write(&output_file, &wasm).expect("Error writing Wasm output");
            }
        }
    }
}
