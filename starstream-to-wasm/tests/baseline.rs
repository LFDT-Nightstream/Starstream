use starstream_to_wasm::compile;
use starstream_types::{BinaryOp, Block, Expr, Identifier, Literal, Program, Spanned, Statement};

macro_rules! assert_wat_snapshot {
    ($program:expr) => {{
        let program = $program;
        let wasm = compile(&program);
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

#[test]
fn empty() {
    assert_wat_snapshot!(&Program { statements: vec![] });
}

#[test]
fn simple_while_loop() {
    assert_wat_snapshot!(&Program {
        statements: vec![
            Statement::VariableDeclaration {
                name: Identifier::new("foo", None),
                value: Spanned::none(Expr::Literal(Literal::Integer(0))),
            },
            Statement::While {
                condition: Spanned::none(Expr::Binary {
                    op: BinaryOp::Less,
                    left: Box::new(Spanned::none(Expr::Identifier(Identifier::new(
                        "foo", None
                    )))),
                    right: Box::new(Spanned::none(Expr::Literal(Literal::Integer(10)))),
                }),
                body: Block {
                    statements: vec![Statement::Assignment {
                        target: Identifier::new("foo", None),
                        value: Spanned::none(Expr::Binary {
                            op: BinaryOp::Add,
                            left: Box::new(Spanned::none(Expr::Identifier(Identifier::new(
                                "foo", None
                            )))),
                            right: Box::new(Spanned::none(Expr::Literal(Literal::Integer(1)))),
                        }),
                    }],
                },
            },
        ],
    });
}
