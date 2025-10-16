use starstream_to_wasm::compile;
use starstream_types::{BinaryOp, Block, Expr, Identifier, Literal, Program, Spanned, Statement};

#[test]
fn derp() {
    let program = Program {
        statements: vec![
            Statement::VariableDeclaration {
                name: Identifier::new("foo", None),
                value: Spanned::none(Expr::Literal(Literal::Integer(0))),
            },
            Statement::While {
                condition: Spanned::none(Expr::Binary {
                    op: BinaryOp::Less,
                    left: Box::new(Spanned::none(Expr::Identifier(Identifier::new("foo", None)))),
                    right: Box::new(Spanned::none(Expr::Literal(Literal::Integer(10)))),
                }),
                body: Block {
                    statements: vec![Statement::Assignment {
                        target: Identifier::new("foo", None),
                        value: Spanned::none(Expr::Binary {
                            op: BinaryOp::Add,
                            left: Box::new(Spanned::none(Expr::Identifier(Identifier::new("foo", None)))),
                            right: Box::new(Spanned::none(Expr::Literal(Literal::Integer(1)))),
                        }),
                    }],
                },
            },
        ],
    };
    let wasm = compile(&program);
    let mut wat = String::new();
    wasmprinter::Config::new().fold_instructions(true).print(
        &wasm,
        &mut wasmprinter::PrintFmtWrite(&mut wat),
    ).unwrap();
    eprintln!("{}", wat);
}
