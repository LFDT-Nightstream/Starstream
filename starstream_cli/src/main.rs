//! The command-line interface to the Starstream language compiler.

use std::path::PathBuf;

use clap::Parser;
use starstream_vm::Transaction;

#[derive(Parser, Debug)]
#[command(arg_required_else_help(true))]
enum Args {
    /// Compile Starstream source to Wasm.
    Compile {
        /// The Starstream source file to compile.
        #[arg(short = 'c')]
        compile_file: PathBuf,
        /// The Wasm output file.
        #[arg(short = 'o')]
        output_file: PathBuf,
    },
    /// Run a coordination script from a Starstream Wasm module.
    Run {
        /// The coordination script Wasm file.
        #[arg(short = 'm')]
        module: PathBuf,
        /// The entry point name.
        #[arg(short = 'e', default_value = "main")]
        entry: String,
    },
}

fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or(
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "info"
        },
    ))
    .init();

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

            std::fs::write(&output_file, module).expect("Error writing Wasm output");
        }
        Args::Run { module, entry } => {
            let mut transaction = Transaction::new();
            let coordination_code = transaction.code_cache().load_file(&module);
            transaction.run_coordination_script(&coordination_code, &entry, Vec::new());
        }
    }
}
