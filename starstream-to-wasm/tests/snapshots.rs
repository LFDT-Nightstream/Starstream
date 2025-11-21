use std::{fs, path::Path};

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

#[test]
fn inputs() {
    let test_file = |path: &Path| {
        let mut output = String::new();

        let source = fs::read_to_string(path).unwrap();
        let program = starstream_compiler::parse_program(&source)
            .into_program()
            .expect("from_source failed to parse");

        let program = starstream_compiler::typecheck_program(&program, Default::default())
            .unwrap()
            .program;

        let (wasm, errors) = starstream_to_wasm::compile(&program);
        assert!(errors.is_empty(), "{:?}", errors);
        let wasm = wasm.unwrap();
        wasmprinter::Config::new()
            .fold_instructions(true)
            .print(
                &wasm,
                &mut CustomPrinter(wasmprinter::PrintFmtWrite(&mut output)),
            )
            .unwrap();

        insta::with_settings!({
            omit_expression => true,
            prepend_module_to_snapshot => false,
        }, {
            insta::assert_snapshot!(output);
        });
    };
    insta::glob!("inputs/*.star", test_file);
}
