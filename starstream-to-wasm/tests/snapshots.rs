use std::fmt::Write;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::{fs, path::Path};

use miette::{GraphicalReportHandler, GraphicalTheme, Report};
use starstream_compiler::{TypecheckOptions, module_graph, typecheck};
use starstream_types::FileSystem;
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
            for line in wat.split('\n') {
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

fn try_paths<F: Fn(&Path, &mut String)>(pattern: &str, func: F) {
    let mut panicked = Vec::new();
    let closure = |path: &Path| {
        let mut output = String::new();

        match catch_unwind(AssertUnwindSafe(|| {
            func(path, &mut output);
        })) {
            Ok(()) => {}
            Err(e) => {
                eprintln!(
                    "==== Partial output for {path:?} ====\n{output}==== End partial output for {path:?} ===="
                );
                panicked.push((path.to_owned(), e));
                return;
            }
        }
    };

    insta::glob!(pattern, closure);

    if !panicked.is_empty() {
        let mut message = String::new();
        writeln!(message, "==== {} snapshots panicked ====", panicked.len()).unwrap();
        for (path, error) in panicked {
            writeln!(message, "---- {path:?}").unwrap();
            if let Some(str) = error.downcast_ref::<String>() {
                if let Some((before, _)) = str.split_once("Stack backtrace:") {
                    // Truncate anyhow stack traces for the summary.
                    writeln!(message, "{}", before.trim()).unwrap();
                } else {
                    writeln!(message, "{}", str.trim()).unwrap();
                }
            } else if let Some(str) = error.downcast_ref::<&'static str>() {
                writeln!(message, "{}", str.trim()).unwrap();
            }
        }
        writeln!(
            message,
            "==== See above for backtraces and partial output ===="
        )
        .unwrap();
        panic!("{}", message);
    }
}

#[test]
fn inputs() {
    try_paths("inputs/*.star", |path, output| {
        let source = fs::read_to_string(path).unwrap();
        let parse_output = starstream_compiler::parse_program(&source);
        let comments = parse_output.comment_map();
        let (program, errors) = parse_output.into_output_errors();
        writeln!(output, "==== AST ====").unwrap();
        for error in errors {
            let report = Report::new(error).with_source_code(source.clone());
            GraphicalReportHandler::new_themed(GraphicalTheme::none())
                .render_report(output, report.as_ref())
                .expect("failed to render diagnostic");
        }
        if let Some(program) = program {
            writeln!(output, "{program:#?}\n").unwrap();

            let formatted_source =
                starstream_compiler::formatter::program(&program, &source, &comments)
                    .expect("formatter error");
            assert!(
                source == formatted_source,
                "Formatted source differs from original, replace with:\n----\n{formatted_source}\n----"
            );

            match starstream_compiler::typecheck_program(
                &program,
                TypecheckOptions {
                    capture_traces: true,
                },
            ) {
                Err(failure) => {
                    if !failure.warnings.is_empty() {
                        writeln!(output, "==== Type warnings ====").unwrap();
                        for warning in failure.warnings {
                            let report = Report::new(warning).with_source_code(source.clone());
                            GraphicalReportHandler::new_themed(GraphicalTheme::none())
                                .render_report(output, report.as_ref())
                                .expect("failed to render diagnostic");
                        }
                    }
                    writeln!(output, "==== Type error ====").unwrap();
                    for error in failure.errors {
                        let report = Report::new(error).with_source_code(source.clone());
                        GraphicalReportHandler::new_themed(GraphicalTheme::none())
                            .render_report(output, report.as_ref())
                            .expect("failed to render diagnostic");
                    }
                }
                Ok(mut success) => {
                    if !success.warnings.is_empty() {
                        writeln!(output, "==== Type warnings ====").unwrap();
                        for warning in success.warnings.drain(..) {
                            let report = Report::new(warning).with_source_code(source.clone());
                            GraphicalReportHandler::new_themed(GraphicalTheme::none())
                                .render_report(output, report.as_ref())
                                .expect("failed to render diagnostic");
                        }
                    }
                    writeln!(
                        output,
                        "==== Inference trace ====\n{}",
                        success.display_traces()
                    )
                    .unwrap();
                    writeln!(output, "==== Typed AST ====\n{:#?}\n", success.program).unwrap();
                    let compile_result = starstream_to_wasm::compile(&success.program);
                    writeln!(output, "==== Core WebAssembly ====").unwrap();
                    for error in compile_result.errors {
                        let report = Report::new(error).with_source_code(source.clone());
                        GraphicalReportHandler::new_themed(GraphicalTheme::none())
                            .render_report(output, report.as_ref())
                            .expect("failed to render diagnostic");
                    }
                    if let Some(wasm) = compile_result.wasm {
                        wasmprinter::Config::new()
                            .fold_instructions(true)
                            .print(
                                &wasm,
                                &mut CustomPrinter(wasmprinter::PrintFmtWrite(&mut *output)),
                            )
                            .unwrap();
                        writeln!(output).unwrap();

                        // Componentize and then extract WIT from the final component.
                        // Not printing component Wasm because it's mostly core Wasm but inside-out.
                        writeln!(output, "==== WIT ====").unwrap();
                        let component_wasm = wit_component::ComponentEncoder::default()
                            .validate(true)
                            .module(&wasm)
                            .unwrap_or_else(|err| {
                                panic!("ComponentEncoder::module failed: {err:?}")
                            })
                            .encode()
                            .expect("ComponentEncoder::encode failed");
                        let decoded = wit_component::decode(&component_wasm).unwrap();
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
    });
}

#[test]
fn multifile() {
    try_paths("multifile/*", |path, output| {
        let mut fs = FileSystem::new();
        writeln!(output, "==== Load workspace ====").unwrap();
        match module_graph::load_workspace(path, &mut fs) {
            Err(err) => {
                writeln!(output, "{:#?}", err).unwrap();
            }
            Ok(graph) => {
                writeln!(output, "{:#?}", graph).unwrap();
                assert!(!graph.contract_entries().is_empty());

                // ...
            }
        }

        insta::with_settings!({
            omit_expression => true,
            prepend_module_to_snapshot => false,
        }, {
            insta::assert_snapshot!(output);
        });
    });
}
