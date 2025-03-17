//! The command-line interface to the Starstream language compiler.

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(arg_required_else_help(true))]
enum Args {
    /// Compile Starstream source to WASM.
    Compile {
        /// The Starstream source file to compile.
        #[arg(short = 'c')]
        compile_file: PathBuf,
        /// The WASM output file.
        #[arg(short = 'o')]
        output_file: PathBuf,
    },
}

fn main() {
    match Args::parse() {
        Args::Compile {
            compile_file,
            output_file,
        } => {
            let source =
                std::fs::read_to_string(&compile_file).expect("Error reading Starstream input");
            let wasm = starstream_compiler::starstream_to_wasm(&source)
                .expect("Error compiling Starstream code");
            std::fs::write(&output_file, wasm).expect("Error writing WASM output");
        }
    }
}
