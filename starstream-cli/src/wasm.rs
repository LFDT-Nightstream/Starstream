use std::path::{Path, PathBuf};

use crate::diagnostics::print_diagnostic;
use clap::Args;
use miette::{IntoDiagnostic, NamedSource};
use starstream_types::FileSystem;
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

    /// Output dependency information in Make/Ninja compatible format.
    #[arg(long, short = 'M')]
    depfile: Option<PathBuf>,
}

impl Wasm {
    /// Parse, type-check, and compile the requested source file into a Wasm module.
    pub fn exec(self) -> miette::Result<()> {
        let mut fs = FileSystem::std();

        let source_text = fs.read_to_string(&self.compile_file).into_diagnostic()?;
        let named = NamedSource::new(self.compile_file.display().to_string(), source_text.clone());

        let parse_output = starstream_compiler::parse_program(&source_text);
        for error in parse_output.errors {
            print_diagnostic(named.clone(), error)?;
        }

        let Some(program) = parse_output.program else {
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
            fs.write(output_core, &wasm)
                .expect("Error writing Wasm output");
        }

        // Componentize
        if self.output_component.is_some() || self.output_wit.is_some() {
            let wasm = ComponentEncoder::default()
                .validate(true)
                .module(&wasm)
                .expect("ComponentEncoder::module failed")
                .encode()
                .expect("ComponentEncoder::encode failed");

            if let Some(output_component) = &self.output_component {
                fs.write(output_component, &wasm)
                    .expect("Error writing Wasm output");
            }

            if let Some(output_wit) = &self.output_wit {
                let decoded = wit_component::decode(&wasm).unwrap();
                let mut printer = wit_component::WitPrinter::default();
                printer.emit_docs(true);
                let ids = decoded
                    .resolve()
                    .packages
                    .iter()
                    .map(|(id, _)| id)
                    .filter(|id| *id != decoded.package())
                    .collect::<Vec<_>>();
                printer
                    .print(decoded.resolve(), decoded.package(), &ids)
                    .unwrap();
                let output = printer.output.to_string();
                fs.write(output_wit, output.as_bytes())
                    .expect("Error writing WIT output");
            }
        }

        if let Some(depfile) = self.depfile {
            let mut depfile_contents = Vec::new();
            write_rule(
                &mut depfile_contents,
                fs.outputs.iter().map(|s| s.as_path()),
                fs.dependencies.iter().map(|s| s.as_path()),
            )
            .expect("Error writing depfile");
            fs.write(&depfile, &depfile_contents)
                .expect("Error writing depfile");
        }

        Ok(())
    }
}

/*
    Depfile grammar from https://cmake.org/cmake/help/latest/command/add_custom_command.html#grammar-token-depfile-depfile

    depfile       ::= rule*
    rule          ::= targets (':' (separator dependencies?)?)? eol
    targets       ::= target (separator target)* separator*
    target        ::= pathname
    dependencies  ::= dependency (separator dependency)* separator*
    dependency    ::= pathname
    separator     ::= (space | line_continue)+
    line_continue ::= '\' eol
    space         ::= ' ' | '\t'
    pathname      ::= character+
    character     ::= std_character | dollar | hash | whitespace
    std_character ::= <any character except '$', '#' or ' '>
    dollar        ::= '$$'
    hash          ::= '\#'
    whitespace    ::= '\ '
    eol           ::= '\r'? '\n'
*/

fn write_rule<'a>(
    dest: &mut impl std::io::Write,
    targets: impl Iterator<Item = &'a Path>,
    dependencies: impl Iterator<Item = &'a Path>,
) -> Result<(), std::io::Error> {
    let mut any = false;
    for target in targets {
        write_pathname(dest, target)?;
        dest.write_all(b" ")?;
        any = true;
    }

    if any {
        dest.write_all(b": ")?;

        for dep in dependencies {
            write_pathname(dest, dep)?;
            dest.write_all(b" ")?;
        }

        dest.write_all(b"\n")?;
    }

    Ok(())
}

fn write_pathname(dest: &mut impl std::io::Write, path: &Path) -> Result<(), std::io::Error> {
    for &byte in path.as_os_str().as_encoded_bytes() {
        match byte {
            b'$' => dest.write_all(b"$$")?,
            b'#' => dest.write_all(b"\\#")?,
            b' ' => dest.write_all(b"\\ ")?,
            b'\\' => dest.write_all(&[b'/'])?,
            _ => dest.write_all(&[byte])?,
        }
    }
    Ok(())
}
