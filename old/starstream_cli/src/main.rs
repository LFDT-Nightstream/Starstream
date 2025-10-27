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
        output_file: Option<PathBuf>,
    },
    /// Run a coordination script from a Starstream Wasm module.
    Run {
        /// The coordination script Wasm file.
        #[arg(short = 'm')]
        module: PathBuf,
        /// The entry point name.
        #[arg(short = 'e', default_value = "main")]
        entry: String,

        /// Path to which to output a Mermaid diagram.
        #[arg(long = "output-mermaid")]
        output_mermaid: Option<PathBuf>,
        /// Path to which to output a proof.
        #[arg(long = "output-proof-cbor")]
        output_proof_cbor: Option<PathBuf>,
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

            let source = ariadne::Source::from(&source_code);

            let (ast, errors) = starstream_compiler::parse(&source_code);
            for error in errors {
                error.eprint(&source).unwrap();
            }
            let Some(ast) = ast else {
                std::process::exit(1);
            };

            let (ast, mut symbols) = match starstream_compiler::do_scope_analysis(ast) {
                Ok(success) => success,
                Err(errors) => {
                    for error in errors {
                        ariadne::Report::from(&error).eprint(&source).unwrap();
                    }

                    std::process::exit(1);
                }
            };

            let ast = match starstream_compiler::do_type_inference(ast, &mut symbols) {
                Ok((ast, warnings)) => {
                    for warning in warnings {
                        ariadne::Report::from(&warning).eprint(&source).unwrap();
                    }
                    ast
                }
                Err(errors) => {
                    for error in errors {
                        ariadne::Report::from(&error).eprint(&source).unwrap();
                    }
                    std::process::exit(1);
                }
            };

            let (module, errors) = starstream_compiler::compile(&ast, symbols);
            for error in errors {
                error.eprint(&source).unwrap();
            }
            let Some(module) = module else {
                std::process::exit(1);
            };

            if let Some(output_file) = output_file {
                std::fs::write(&output_file, module).expect("Error writing Wasm output");
            }
        }
        Args::Run {
            module,
            entry,
            output_proof_cbor,
            output_mermaid,
        } => {
            let mut transaction = Transaction::new();
            let coordination_code = transaction.code_cache().load_file(&module);
            transaction.run_coordination_script(&coordination_code, &entry, Vec::new());
            if let Some(output_mermaid) = output_mermaid {
                std::fs::write(output_mermaid, transaction.to_mermaid_diagram()).unwrap();
            }
            if let Some(output_proof_cbor) = output_proof_cbor {
                // NOTE on sizes for a dummy proof:
                // .json    1.2G
                // .json.gz 30M
                // .cbor    635M
                // .cbor.gz 31M
                std::fs::write(
                    output_proof_cbor,
                    serde_cbor::to_vec(&transaction.prove()).unwrap(),
                )
                .unwrap();
            }
        }
    }
}
