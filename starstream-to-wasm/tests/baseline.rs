use std::fs;

use starstream_to_wasm::compile;
use starstream_types::{
    BinaryOp, FunctionExport, Identifier, Literal, Spanned, Type, TypedBlock, TypedDefinition,
    TypedExpr, TypedExprKind, TypedFunctionDef, TypedProgram, TypedStatement,
};
use wasmprinter::Print;

/// [Print] impl that expands contents of `component-type` custom sections.
struct CustomPrinter<T>(T);

impl<T: Print> Print for CustomPrinter<T> {
    fn write_str(&mut self, s: &str) -> std::io::Result<()> {
        self.0.write_str(s)
    }

    fn print_custom_section(
        &mut self,
        name: &str,
        _binary_offset: usize,
        data: &[u8],
    ) -> std::io::Result<bool> {
        if name == "component-type" {
            let mut wat = String::new();
            wasmprinter::Config::new()
                .print(data, &mut wasmprinter::PrintFmtWrite(&mut wat))
                .unwrap();
            self.write_str("\n  (@custom \"component-type\"")?;
            for line in wat.split("\n") {
                if !line.is_empty() {
                    self.write_str("\n    ")?;
                    self.write_str(line)?;
                }
            }
            self.write_str("\n  )")?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

fn from_source(source: &str) -> TypedProgram {
    let program = starstream_compiler::parse_program(source)
        .into_program()
        .unwrap();

    starstream_compiler::typecheck_program(&program, Default::default())
        .unwrap()
        .program
}

macro_rules! assert_wat_snapshot {
    ($program:expr) => {{
        let program: &TypedProgram = $program;
        let (wasm, errors) = compile(program);
        assert!(errors.is_empty(), "{:?}", errors);
        let wasm = wasm.unwrap();
        let mut wat = String::new();
        wasmprinter::Config::new().fold_instructions(true).print(
            &wasm,
            &mut CustomPrinter(wasmprinter::PrintFmtWrite(&mut wat)),
        ).unwrap();
        insta::with_settings!({
            description => format!("{:?}", program),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(wat);
        });
    }}
}

#[test]
fn inputs() {
    insta::glob!("inputs/*.star", |path| {
        let input = fs::read_to_string(path).unwrap();
        assert_wat_snapshot!(&from_source(&input));
    });
}
