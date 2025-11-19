use super::{TypecheckOptions, typecheck_program};
use indoc::indoc;
use miette::{GraphicalReportHandler, GraphicalTheme, NamedSource, Report};

/// Parse the provided snippet, run type-checking, and snapshot the rendered
/// diagnostics or inference trees for regression testing.
macro_rules! assert_typecheck_snapshot {
    ($code:expr) => {{
        let source = indoc! { $code };
        let parse_output = crate::parser::parse_program(source);
        let named = NamedSource::new("test.star", source.to_string());
        let mut rendered = String::new();

        let result: Result<(), miette::Error> = (|| {
            if !parse_output.errors().is_empty() {
                for error in parse_output.errors().iter().cloned() {
                    let report = Report::new(error).with_source_code(named.clone());
                    rendered.push_str(&render_report(&report)?);
                    rendered.push('\n');
                }
            } else {
                let program = parse_output
                    .into_program()
                    .expect("parser succeeded but returned no program");

                match typecheck_program(&program, TypecheckOptions { capture_traces: true }) {
                    Ok(success) => {
                        for (index, tree) in success.traces.into_iter().enumerate() {
                            if index > 0 {
                                rendered.push('\n');
                            }
                            let tree_str = tree.to_string();
                            rendered.push_str(&tree_str);
                            if !tree_str.ends_with('\n') {
                                rendered.push('\n');
                            }
                        }
                    }
                    Err(errors) => {
                        for error in errors {
                            let report =
                                Report::new(error).with_source_code(named.clone());
                            rendered.push_str(&render_report(&report)?);
                            rendered.push('\n');
                        }
                    }
                }
            }
            Ok(())
        })();

        if let Err(err) = result {
            panic!("failed to render diagnostic: {err}");
        }

        insta::with_settings!(
            {
                description => format!("Source:\n\n{}", source),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            },
            {
                insta::assert_snapshot!(rendered);
            }
        );
    }};
}

/// Render a `miette` report into a trimmed string for inclusion in snapshots.
fn render_report(report: &Report) -> miette::Result<String> {
    let mut rendered = String::new();
    GraphicalReportHandler::new_themed(GraphicalTheme::none())
        .render_report(&mut rendered, report.as_ref())
        .map_err(|err| miette::miette!("failed to render diagnostic: {err}"))?;
    Ok(rendered.trim_end_matches('\n').to_string())
}

#[test]
fn let_binding_traces() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let answer = 42;
            let foo: i64 = 10;
            let bar: bool = false;
            let baz: _ = 2;
        }
        "#
    );
}

#[test]
fn binary_add_traces() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let left = 1;
            let right = 2;
            let total = left + right;
        }
        "#
    );
}

#[test]
fn if_branch_traces() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let mut score = 10;
            if (score > 5) {
                score = score + 1;
            }
        }
        "#
    );
}

#[test]
fn while_loop_traces() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let mut counter = 0;
            while (counter < 3) {
                counter = counter + 1;
            }
        }
        "#
    );
}

#[test]
fn struct_literals_and_field_access() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
            y: i64,
        }

        fn total(point: Point) -> i64 {
            let translated = Point {
                x: point.x + 1,
                y: point.y + 1,
            };

            translated.x + translated.y
        }
        "#
    );
}

#[test]
fn enum_match_inference() {
    assert_typecheck_snapshot!(
        r#"
        enum Message {
            Ping,
            Pong(i64),
        }

        fn respond(msg: Message) -> i64 {
            match msg {
                Message::Ping => {
                    0
                },
                Message::Pong(value) => {
                    value
                },
            }
        }
        "#
    );
}

#[test]
fn struct_literal_missing_field_error() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
            y: i64,
        }

        fn bad() {
            let value = Point { x: 1 };
        }
        "#
    );
}

#[test]
fn reports_type_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let flag = true;
            let x = flag + 1;
        }
        "#
    );
}

#[test]
fn reports_immutable_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let flag = true;
            flag = false;
        }
        "#
    );
}

#[test]
fn reports_let_annotation_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let wrong: bool = 1;
        }
        "#
    );
}
