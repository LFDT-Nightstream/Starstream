use super::{TypecheckOptions, typecheck_program};
use indoc::indoc;
use miette::{GraphicalReportHandler, GraphicalTheme, NamedSource, Report};
use std::fmt::Write;

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
                for error in parse_output.errors {
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
                        write!(rendered, "{}", success.display_traces()).unwrap();
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
fn enum_struct_pattern_punning() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
        }

        enum Message {
            Ping,
            Pong {
                x: i64,
            },
        }

        fn add(a: Point, b: Message) -> i64 {
            match b {
                Message::Ping => {
                    a.x
                },
                Message::Pong { x } => {
                    x + a.x
                },
            }
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

#[test]
fn unknown_variable_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            missing;
        }
        "#
    );
}

#[test]
fn redeclaration_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let value = 1;
            let value = 2;
        }
        "#
    );
}

#[test]
fn assignment_mismatch_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let mut value: i64 = 0;
            value = true;
        }
        "#
    );
}

#[test]
fn unary_mismatch_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let bad = -true;
        }
        "#
    );
}

#[test]
fn condition_not_bool_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            if (1) {
                ()
            }
        }
        "#
    );
}

#[test]
fn return_mismatch_error() {
    assert_typecheck_snapshot!(
        r#"
        fn compute() -> i64 {
            return true;
        }
        "#
    );
}

#[test]
fn missing_return_error() {
    assert_typecheck_snapshot!(
        r#"
        fn compute() -> i64 {
            let value = 1;
        }
        "#
    );
}

#[test]
fn unknown_type_annotation_error() {
    assert_typecheck_snapshot!(
        r#"
        fn takes(value: Missing) {
            value;
        }
        "#
    );
}

#[test]
fn unsupported_type_feature_error() {
    assert_typecheck_snapshot!(
        r#"
        fn takes(value: Box<i64>) {
            value;
        }
        "#
    );
}

#[test]
fn type_already_defined_error() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
        }

        struct Point {
            x: i64,
        }

        fn noop() { }
        "#
    );
}

#[test]
fn duplicate_struct_field_error() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
            x: i64,
        }
        "#
    );
}

#[test]
fn duplicate_enum_variant_error() {
    assert_typecheck_snapshot!(
        r#"
        enum Message {
            Ping,
            Ping,
        }
        "#
    );
}

#[test]
fn unknown_struct_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            Missing {
                value: 1,
            };
        }
        "#
    );
}

#[test]
fn unknown_enum_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            Missing::Value;
        }
        "#
    );
}

#[test]
fn unknown_struct_field_error() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
        }

        fn test() {
            Point {
                x: 1,
                y: 2,
            };
        }
        "#
    );
}

#[test]
fn duplicate_struct_literal_field_error() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
        }

        fn test() {
            Point {
                x: 1,
                x: 2,
            };
        }
        "#
    );
}

#[test]
fn struct_literal_field_type_mismatch_error() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
        }

        fn test() {
            Point {
                x: true,
            };
        }
        "#
    );
}

#[test]
fn field_access_not_struct_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            let value = 5;
            value.field;
        }
        "#
    );
}

#[test]
fn field_access_unknown_field_error() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
        }

        fn test() {
            let point = Point {
                x: 0,
            };

            point.y;
        }
        "#
    );
}

#[test]
fn unknown_enum_variant_error() {
    assert_typecheck_snapshot!(
        r#"
        enum Message {
            Ping,
        }

        fn test() {
            Message::Pong;
        }
        "#
    );
}

#[test]
fn enum_payload_mismatch_error() {
    assert_typecheck_snapshot!(
        r#"
        enum Message {
            Ping(i64),
        }

        fn test() {
            Message::Ping();
        }
        "#
    );
}

#[test]
fn unknown_struct_pattern_error() {
    assert_typecheck_snapshot!(
        r#"
        fn test() {
            match 0 {
                Missing::Value => {
                    ()
                },
            }
        }
        "#
    );
}

#[test]
fn pattern_enum_mismatch_error() {
    assert_typecheck_snapshot!(
        r#"
        enum First {
            Alpha,
        }

        enum Second {
            Beta,
        }

        fn test(value: First) {
            match value {
                Second::Beta => {
                    ()
                },
            }
        }
        "#
    );
}

#[test]
fn struct_field_order_preserved() {
    assert_typecheck_snapshot!(
        r#"
        struct User {
            name: i64,
            age: i64,
            email: i64,
        }

        struct ZOrder {
            z: i64,
            a: i64,
        }

        fn test() {
            let u = User {
                name: 1,
                age: 2,
                email: 3,
            };
            let z = ZOrder {
                z: 1,
                a: 2,
            };
        }
        "#
    );
}

#[test]
fn non_exhaustive_match_error() {
    assert_typecheck_snapshot!(
        r#"
        enum Color {
            Red,
            Green,
            Blue,
            Yellow(i64, i64),
        }

        fn test(c: Color) {
            match c {
                Color::Red => {
                    0
                },
                Color::Green => {
                    1
                },
            }
        }
        "#
    );
}

#[test]
fn exhaustive_match_with_wildcard() {
    assert_typecheck_snapshot!(
        r#"
        enum Color {
            Red,
            Green,
            Blue,
        }

        fn test(c: Color) -> i64 {
            match c {
                Color::Red => {
                    0
                },
                _ => {
                    1
                },
            }
        }
        "#
    );
}

#[test]
fn unreachable_pattern_error() {
    assert_typecheck_snapshot!(
        r#"
        enum Option {
            None,
            Some(i64),
        }

        fn test(o: Option) -> i64 {
            match o {
                _ => {
                    0
                },
                Option::None => {
                    1
                },
            }
        }
        "#
    );
}

#[test]
fn exhaustive_match_all_variants() {
    assert_typecheck_snapshot!(
        r#"
        enum Result {
            Ok(i64),
            Err(i64),
        }

        fn test(r: Result) -> i64 {
            match r {
                Result::Ok(value) => {
                    value
                },
                Result::Err(code) => {
                    0 - code
                },
            }
        }
        "#
    );
}

#[test]
fn exhaustive_match_with_struct_payload() {
    assert_typecheck_snapshot!(
        r#"
        struct Point {
            x: i64,
            y: i64,
        }

        enum Shape {
            Circle(i64),
            Rectangle { width: i64, height: i64 },
        }

        fn test(s: Shape) -> i64 {
            match s {
                Shape::Circle(radius) => {
                    radius
                },
                Shape::Rectangle { width, height } => {
                    width + height
                },
            }
        }
        "#
    );
}

#[test]
fn match_bool_literal_exhaustive() {
    assert_typecheck_snapshot!(
        r#"
        fn test(b: bool) -> i64 {
            match b {
                true => {
                    1
                },
                false => {
                    0
                },
            }
        }
        "#
    );
}

#[test]
fn match_bool_literal_non_exhaustive() {
    assert_typecheck_snapshot!(
        r#"
        fn test(b: bool) -> i64 {
            match b {
                true => {
                    1
                },
            }
        }
        "#
    );
}

#[test]
fn match_int_literal_needs_wildcard() {
    assert_typecheck_snapshot!(
        r#"
        fn test(n: i64) -> i64 {
            match n {
                0 => {
                    100
                },
                1 => {
                    200
                },
            }
        }
        "#
    );
}

#[test]
fn match_int_literal_with_wildcard() {
    assert_typecheck_snapshot!(
        r#"
        fn test(n: i64) -> i64 {
            match n {
                0 => {
                    100
                },
                1 => {
                    200
                },
                _ => {
                    0
                },
            }
        }
        "#
    );
}
