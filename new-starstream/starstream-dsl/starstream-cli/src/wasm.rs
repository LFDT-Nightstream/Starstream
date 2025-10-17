use std::path::PathBuf;

use ariadne::{Cache, FileCache};
use clap::Args;

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
    pub fn exec(&self) {
        // Load
        let mut cache = FileCache::default();
        let source = cache
            .fetch(&self.compile_file)
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

        if let Some(output_file) = &self.output_file {
            std::fs::write(&output_file, &wasm).expect("Error writing Wasm output");
        }
    }
}
