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
            let source_code =
                std::fs::read_to_string(&compile_file).expect("Error reading Starstream input");

            let (ast, errors) = starstream_compiler::parse(&source_code);
            for error in errors {
                error.eprint(ariadne::Source::from(&source_code)).unwrap();
            }
            let Some(ast) = ast else {
                std::process::exit(1);
            };

            let (module, errors) = starstream_compiler::compile(&ast);
            for error in errors {
                error.eprint(ariadne::Source::from(&source_code)).unwrap();
            }
            let Some(module) = module else {
                std::process::exit(1);
            };

            std::fs::write(&output_file, module).expect("Error writing WASM output");
        }
    }
}
