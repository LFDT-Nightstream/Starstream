use std::fmt::Write;
use std::{fs, path::Path};

use miette::{GraphicalReportHandler, GraphicalTheme, Report};
use starstream_compiler::TypecheckOptions;
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
        let (program, errors) = starstream_compiler::parse_program(&source).into_output_errors();
        writeln!(output, "==== AST ====").unwrap();
        for error in errors {
            let report = Report::new(error).with_source_code(source.clone());
            GraphicalReportHandler::new_themed(GraphicalTheme::none())
                .render_report(&mut output, report.as_ref())
                .expect("failed to render diagnostic");
        }
        if let Some(program) = program {
            writeln!(output, "{:#?}\n", program).unwrap();

            let formatted_source =
                starstream_compiler::formatter::program(&program).expect("formatter error");
            assert_eq!(
                source, formatted_source,
                "formatted source differs from original"
            );

            match starstream_compiler::typecheck_program(
                &program,
                TypecheckOptions {
                    capture_traces: true,
                },
            ) {
                Err(errors) => {
                    writeln!(output, "==== Type error ====").unwrap();
                    for error in errors {
                        let report = Report::new(error).with_source_code(source.clone());
                        GraphicalReportHandler::new_themed(GraphicalTheme::none())
                            .render_report(&mut output, report.as_ref())
                            .expect("failed to render diagnostic");
                    }
                }
                Ok(success) => {
                    writeln!(
                        output,
                        "==== Inference trace ====\n{}",
                        success.display_traces()
                    )
                    .unwrap();
                    writeln!(output, "==== Typed AST ====\n{:#?}\n", success.program).unwrap();
                    let (wasm, errors) = starstream_to_wasm::compile(&success.program);
                    writeln!(output, "==== Core WebAssembly ====").unwrap();
                    for error in errors {
                        let report = Report::new(error).with_source_code(source.clone());
                        GraphicalReportHandler::new_themed(GraphicalTheme::none())
                            .render_report(&mut output, report.as_ref())
                            .expect("failed to render diagnostic");
                    }
                    if let Some(wasm) = wasm {
                        wasmprinter::Config::new()
                            .fold_instructions(true)
                            .print(
                                &wasm,
                                &mut CustomPrinter(wasmprinter::PrintFmtWrite(&mut output)),
                            )
                            .unwrap();
                        writeln!(output).unwrap();

                        // Componentize and then extract WIT from the final component.
                        // Not printing component Wasm because it's mostly core Wasm but inside-out.
                        writeln!(output, "==== WIT ====").unwrap();
                        let component_wasm = wit_component::ComponentEncoder::default()
                            .validate(true)
                            .module(&wasm)
                            .expect("ComponentEncoder::module failed")
                            .encode()
                            .expect("ComponentEncoder::encode failed");
                        let decoded = wit_component::decode(&component_wasm).unwrap();
                        let mut printer = wit_component::WitPrinter::default();
                        printer
                            .print(decoded.resolve(), decoded.package(), &[])
                            .unwrap();
                        writeln!(output, "{}\n", printer.output).unwrap();
                    }
                }
            }
        }

        insta::with_settings!({
            omit_expression => true,
            prepend_module_to_snapshot => false,
        }, {
            insta::assert_snapshot!(output);
        });
    };
    insta::glob!("inputs/*.star", test_file);
}
