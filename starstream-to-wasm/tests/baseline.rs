use starstream_to_wasm::compile;
use starstream_types::{
    BinaryOp, Identifier, Literal, Spanned, Type, TypedBlock, TypedDefinition, TypedExpr,
    TypedExprKind, TypedFunctionDef, TypedProgram, TypedStatement,
};

macro_rules! assert_wat_snapshot {
    ($program:expr) => {{
        let program: &TypedProgram = $program;
        let (wasm, errors) = compile(program);
        assert!(errors.is_empty(), "{:?}", errors);
        let wasm = wasm.unwrap();
        let mut wat = String::new();
        wasmprinter::Config::new().fold_instructions(true).print(
            &wasm,
            &mut wasmprinter::PrintFmtWrite(&mut wat),
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

fn program_with_body(statements: Vec<TypedStatement>) -> TypedProgram {
    TypedProgram {
        definitions: vec![TypedDefinition::Function(TypedFunctionDef {
            name: Identifier::new("main", None),
            params: Vec::new(),
            return_type: Type::Unit,
            body: TypedBlock {
                statements,
                tail_expression: None,
            },
        })],
    }
}

#[test]
fn empty() {
    assert_wat_snapshot!(&program_with_body(vec![]));
}

#[test]
fn simple_while_loop() {
    assert_wat_snapshot!(&program_with_body(vec![
        TypedStatement::VariableDeclaration {
            name: Identifier::new("foo", None),
            value: Spanned::none(TypedExpr::new(
                Type::Int,
                TypedExprKind::Literal(Literal::Integer(0))
            )),
        },
        TypedStatement::While {
            condition: Spanned::none(TypedExpr::new(
                Type::Bool,
                TypedExprKind::Binary {
                    op: BinaryOp::Less,
                    left: Box::new(Spanned::none(TypedExpr::new(
                        Type::Int,
                        TypedExprKind::Identifier(Identifier::new("foo", None))
                    ))),
                    right: Box::new(Spanned::none(TypedExpr::new(
                        Type::Int,
                        TypedExprKind::Literal(Literal::Integer(10))
                    ))),
                }
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
                                TypedExprKind::Identifier(Identifier::new("foo", None))
                            ))),
                            right: Box::new(Spanned::none(TypedExpr::new(
                                Type::Int,
                                TypedExprKind::Literal(Literal::Integer(1))
                            ))),
                        }
                    )),
                }],
                tail_expression: None,
            },
        },
    ]));
}

#[test]
fn if_elseif_else() {
    assert_wat_snapshot!(&program_with_body(vec![TypedStatement::If {
        branches: vec![
            (
                Spanned::none(TypedExpr::new(
                    Type::Bool,
                    TypedExprKind::Literal(Literal::Boolean(false))
                )),
                TypedBlock {
                    statements: vec![TypedStatement::Expression(Spanned::none(TypedExpr::new(
                        Type::Int,
                        TypedExprKind::Literal(Literal::Integer(1))
                    )))],
                    tail_expression: None,
                },
            ),
            (
                Spanned::none(TypedExpr::new(
                    Type::Bool,
                    TypedExprKind::Literal(Literal::Boolean(true))
                )),
                TypedBlock {
                    statements: vec![TypedStatement::Expression(Spanned::none(TypedExpr::new(
                        Type::Bool,
                        TypedExprKind::Literal(Literal::Integer(2))
                    )))],
                    tail_expression: None,
                },
            ),
        ],
        else_branch: Some(TypedBlock {
            statements: vec![TypedStatement::Expression(Spanned::none(TypedExpr::new(
                Type::Int,
                TypedExprKind::Literal(Literal::Integer(3))
            )))],
            tail_expression: None,
        })
    }]))
}
