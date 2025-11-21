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
        .expect("from_source failed to parse");

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
fn empty() {
    let program = TypedProgram {
        definitions: vec![TypedDefinition::Function(TypedFunctionDef {
            export: Some(FunctionExport::Script),
            name: Identifier::new("main", None),
            params: Vec::new(),
            return_type: Type::Unit,
            body: TypedBlock {
                statements: vec![],
                tail_expression: None,
            },
        })],
    };

    assert_wat_snapshot!(&program);
}

#[test]
fn simple_while_loop() {
    let program = TypedProgram {
        definitions: vec![TypedDefinition::Function(TypedFunctionDef {
            export: Some(FunctionExport::Script),
            name: Identifier::new("main", None),
            params: Vec::new(),
            return_type: Type::Unit,
            body: TypedBlock {
                statements: vec![
                    TypedStatement::VariableDeclaration {
                        mutable: false,
                        name: Identifier::new("foo", None),
                        value: Spanned::none(TypedExpr::new(
                            Type::Int,
                            TypedExprKind::Literal(Literal::Integer(0)),
                        )),
                    },
                    TypedStatement::While {
                        condition: Spanned::none(TypedExpr::new(
                            Type::Bool,
                            TypedExprKind::Binary {
                                op: BinaryOp::Less,
                                left: Box::new(Spanned::none(TypedExpr::new(
                                    Type::Int,
                                    TypedExprKind::Identifier(Identifier::new("foo", None)),
                                ))),
                                right: Box::new(Spanned::none(TypedExpr::new(
                                    Type::Int,
                                    TypedExprKind::Literal(Literal::Integer(10)),
                                ))),
                            },
                        )),
                        body: TypedBlock {
                            statements: vec![TypedStatement::Assignment {
                                target: Identifier::new("foo", None),
                                value: Spanned::none(TypedExpr::new(
                                    Type::Int,
                                    TypedExprKind::Binary {
                                        op: BinaryOp::Add,
                                        left: Box::new(Spanned::none(TypedExpr::new(
                                            Type::Int,
                                            TypedExprKind::Identifier(Identifier::new("foo", None)),
                                        ))),
                                        right: Box::new(Spanned::none(TypedExpr::new(
                                            Type::Int,
                                            TypedExprKind::Literal(Literal::Integer(1)),
                                        ))),
                                    },
                                )),
                            }],
                            tail_expression: None,
                        },
                    },
                ],
                tail_expression: None,
            },
        })],
    };

    assert_wat_snapshot!(&program);
}

#[test]
fn if_elseif_else() {
    assert_wat_snapshot!(&from_source(
        "
        script fn main() {
            if (false) {
                1;
            } else if (true) {
                2;
            } else {
                3;
            }
        }
        "
    ));
}

#[test]
fn if_expression() {
    assert_wat_snapshot!(&from_source(
        "
        script fn main() -> i64 {
            if (false) {
                1
            } else if (true) {
                2
            } else {
                3
            }
        }
        "
    ));
}

#[test]
fn add() {
    assert_wat_snapshot!(&from_source(
        "
        script fn add(x: i64, y: i64) -> i64 {
            x + y
        }
        "
    ));
}

#[test]
fn struct_param() {
    assert_wat_snapshot!(&from_source(
        "
        struct Token {
            amount: i64,
            price: i64,
        }

        script fn total_value(token: Token) -> i64 {
            token.amount * token.price
        }
        "
    ))
}
