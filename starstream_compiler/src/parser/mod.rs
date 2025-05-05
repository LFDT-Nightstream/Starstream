use crate::ast::*;
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::{
    pratt::{infix, left, prefix, right},
    prelude::*,
};

/// Convert a Chumsky parse error to a fancy diagnostic report.
pub fn error_to_report(e: Rich<char>) -> Report {
    Report::build(ReportKind::Error, e.span().into_range())
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(e.to_string())
        .with_label(
            Label::new(e.span().into_range())
                .with_message(e.reason().to_string())
                .with_color(Color::Red),
        )
        .finish()
}

/// Get a Chumsky parser for a Starstream source file.
pub fn starstream_program<'a>()
-> impl Parser<'a, &'a str, StarstreamProgram, extra::Err<Rich<'a, char>>> {
    utxo()
        .map(ProgramItem::Utxo)
        .or(script().map(ProgramItem::Script))
        .or(token().map(ProgramItem::Token))
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(|items| StarstreamProgram { items })
}

fn utxo<'a>() -> impl Parser<'a, &'a str, Utxo, extra::Err<Rich<'a, char>>> {
    just("utxo")
        .ignore_then(identifier().padded())
        .then(
            abi()
                .map(UtxoItem::Abi)
                .or(main().map(UtxoItem::Main))
                .or(r#impl().map(UtxoItem::Impl))
                .or(storage().map(UtxoItem::Storage))
                .padded()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, items)| Utxo { name, items })
}

fn fn_sig<'a>() -> impl Parser<'a, &'a str, FnSig, extra::Err<Rich<'a, char>>> {
    just("fn").ignore_then(sig()).map(FnSig)
}

fn effect_sig<'a>() -> impl Parser<'a, &'a str, EffectSig, extra::Err<Rich<'a, char>>> {
    choice((
        just("effect").ignore_then(sig()).map(EffectSig::EffectSig),
        just("event").ignore_then(sig()).map(EffectSig::EventSig),
        just("error").ignore_then(sig()).map(EffectSig::ErrorSig),
    ))
}

fn sig<'a>() -> impl Parser<'a, &'a str, Sig, extra::Err<Rich<'a, char>>> {
    identifier()
        .padded()
        .then(
            r#type()
                .padded()
                .separated_by(just(','))
                .collect::<Vec<_>>()
                .delimited_by(just('('), just(')')),
        )
        .then(just(':').ignore_then(r#type().padded()).or_not())
        .map(|((name, input_types), output_type)| Sig {
            name,
            input_types,
            output_type,
        })
}

fn fn_def<'a>() -> impl Parser<'a, &'a str, FnDef, extra::Err<Rich<'a, char>>> {
    just("fn")
        .padded()
        .ignore_then(identifier())
        .padded()
        .then(
            optionally_typed_bindings(r#type())
                .padded()
                .delimited_by(just('('), just(')')),
        )
        .then(just(':').ignore_then(r#type().padded()).or_not())
        .then(block())
        .map(|(((name, inputs), output), body)| FnDef {
            name,
            inputs,
            output,
            body,
        })
}

fn token<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, char>>> {
    just("token")
        .padded()
        .ignore_then(identifier())
        .then(
            abi()
                .map(TokenItem::Abi)
                .or(just("bind")
                    .padded()
                    .ignore_then(block())
                    .map(Bind)
                    .map(TokenItem::Bind))
                .or(just("unbind")
                    .padded()
                    .ignore_then(block())
                    .map(Unbind)
                    .map(TokenItem::Unbind))
                .or(just("mint")
                    .padded()
                    .ignore_then(block())
                    .map(Mint)
                    .map(TokenItem::Mint))
                .padded()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, items)| Token { name, items })
}

fn r#impl<'a>() -> impl Parser<'a, &'a str, Impl, extra::Err<Rich<'a, char>>> {
    just("impl")
        .padded()
        .ignore_then(identifier())
        .then(
            fn_def()
                .padded()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(name, definitions)| Impl { name, definitions })
}

fn script<'a>() -> impl Parser<'a, &'a str, Script, extra::Err<Rich<'a, char>>> {
    just("script")
        .padded()
        .ignore_then(
            fn_def()
                .padded()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|definitions| Script { definitions })
}

fn abi<'a>() -> impl Parser<'a, &'a str, Abi, extra::Err<Rich<'a, char>>> {
    just("abi")
        .ignore_then(
            choice((
                fn_sig().map(AbiElem::FnSig),
                effect_sig().map(AbiElem::EffectSig),
            ))
            .then_ignore(just(';').padded())
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|values| Abi { values })
}

fn storage<'a>() -> impl Parser<'a, &'a str, Storage, extra::Err<Rich<'a, char>>> {
    just("storage")
        .ignore_then(
            typed_binding(r#type())
                .then_ignore(just(';').padded())
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|values| Storage {
            bindings: TypedBindings { values },
        })
}

fn main<'a>() -> impl Parser<'a, &'a str, Main, extra::Err<Rich<'a, char>>> {
    just("main")
        .ignore_then(
            optionally_typed_bindings(r#type())
                .delimited_by(just('(').padded(), just(')').padded())
                .or_not(),
        )
        .then(block())
        .map(|(typed_bindings, block)| Main {
            type_sig: typed_bindings,
            block,
        })
}

fn statement<'a>(
    expr_parser: impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone + 'a,
    block_parser: impl Parser<'a, &'a str, Block, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Statement, extra::Err<Rich<'a, char>>> {
    recursive(|rec| {
        let bind_var = just("let")
            .padded()
            .ignore_then(just("mut").padded().or_not().map(|x| x.is_some()))
            .then(identifier())
            .then_ignore(just('=').padded())
            .then(expr_parser.clone())
            .then_ignore(just(';').padded())
            .map(|((mutable, binding), expr)| Statement::BindVar {
                var: binding,
                value: expr,
                mutable,
            })
            .boxed();

        let assign = identifier()
            .then_ignore(just('=').padded())
            .then(expr_parser.clone())
            .then_ignore(just(';'))
            .map(|(var, expr)| Statement::Assign(var, expr))
            .boxed();

        let loop_body = rec
            .clone()
            .map(Box::new)
            .map(LoopBody::Statement)
            .or(block_parser.clone().map(LoopBody::Block))
            .or(expr_parser
                .clone()
                .then_ignore(just(';'))
                .map(LoopBody::Expr));

        let while_statement = just("while")
            .padded()
            .ignore_then(
                expr_parser
                    .clone()
                    .delimited_by(just('(').padded(), just(')').padded()),
            )
            .then(loop_body.clone())
            .map(|(cond, body)| Statement::While(cond, body))
            .boxed();

        let loop_statement = just("loop")
            .padded()
            .ignore_then(loop_body)
            .map(Statement::Loop)
            .boxed();

        let try_with = just("try")
            .ignore_then(block_parser.clone())
            .then(
                just("with")
                    .ignore_then(effect().padded())
                    .then(block_parser.clone().padded())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(body, handlers)| Statement::With(body, handlers))
            .boxed();

        let resume = just("resume")
            .ignore_then(expr_parser.clone().padded().or_not())
            .then_ignore(just(';').padded())
            .map(Statement::Resume);

        let ret = just("return")
            .ignore_then(expr_parser.clone().padded().or_not())
            .then_ignore(just(';').padded())
            .map(Statement::Return);

        choice((
            bind_var,
            assign,
            while_statement,
            loop_statement,
            try_with,
            resume,
            ret,
        ))
    })
}

fn effect<'a>() -> impl Parser<'a, &'a str, Effect, extra::Err<Rich<'a, char>>> {
    let typed_bindings = optionally_typed_bindings(r#type());

    identifier()
        .then(typed_bindings.delimited_by(just('('), just(')')))
        .map(|(ident, type_sig)| Effect { ident, type_sig })
}

fn typed_binding<'a>(
    type_parser: impl Parser<'a, &'a str, Type, extra::Err<Rich<'a, char>>>,
) -> impl Parser<'a, &'a str, (Identifier, Type), extra::Err<Rich<'a, char>>> {
    identifier().then(just(':').padded().ignore_then(type_parser.padded()))
}

fn optionally_typed_binding<'a>(
    type_parser: impl Parser<'a, &'a str, Type, extra::Err<Rich<'a, char>>>,
) -> impl Parser<'a, &'a str, (Identifier, Option<Type>), extra::Err<Rich<'a, char>>> {
    identifier().then(
        just(':')
            .padded()
            .ignore_then(type_parser.padded())
            .or_not(),
    )
}

fn optionally_typed_bindings<'a>(
    type_parser: impl Parser<'a, &'a str, Type, extra::Err<Rich<'a, char>>>,
) -> impl Parser<'a, &'a str, OptionallyTypedBindings, extra::Err<Rich<'a, char>>> {
    optionally_typed_binding(type_parser)
        .separated_by(just(',').padded())
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|values| OptionallyTypedBindings { values })
}

fn expr<'a>(
    block_parser: impl Parser<'a, &'a str, Block, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> {
    let op = |c| just(c).padded();

    recursive(|expr_parser| {
        let function_call = expr_parser
            .clone()
            .separated_by(just(',').padded())
            .allow_trailing()
            .collect::<Vec<_>>()
            .map(|xs| Arguments { xs })
            .delimited_by(just('('), just(')'))
            .or_not();

        let atom = primary_expr(expr_parser.clone())
            .then(function_call.clone())
            .map(|(expr, call)| Expr::PrimaryExpr(expr, call, vec![]))
            .foldl(
                just('.')
                    .padded()
                    .ignore_then(identifier().then(function_call))
                    .repeated(),
                |mut accum, new| {
                    match &mut accum {
                        Expr::PrimaryExpr(_expr, _call, trail) => {
                            trail.push(new);
                        }
                        _ => unreachable!(),
                    }

                    accum
                },
            )
            .or(block_expr(expr_parser, block_parser).map(Expr::BlockExpr));

        atom.pratt((
            prefix(4, op('!'), |_, atom, _| Expr::Neg(Box::new(atom))),
            infix(right(3), op('^'), |l, _, r, _| {
                Expr::Pow(Box::new(l), Box::new(r))
            }),
            infix(left(2), op('*'), |l, _, r, _| {
                Expr::Mul(Box::new(l), Box::new(r))
            }),
            infix(left(2), just("&&").padded(), |l, _, r, _| {
                Expr::And(Box::new(l), Box::new(r))
            }),
            infix(left(2), op('/'), |l, _, r, _| {
                Expr::Div(Box::new(l), Box::new(r))
            }),
            infix(left(1), op('+'), |l, _, r, _| {
                Expr::Add(Box::new(l), Box::new(r))
            }),
            infix(left(1), just("||").padded(), |l, _, r, _| {
                Expr::Or(Box::new(l), Box::new(r))
            }),
            infix(left(1), op('-'), |l, _, r, _| {
                Expr::Sub(Box::new(l), Box::new(r))
            }),
            infix(left(1), op('<'), |l, _, r, _| {
                Expr::LessThan(Box::new(l), Box::new(r))
            }),
            infix(left(1), just("==").padded(), |l, _, r, _| {
                Expr::Equals(Box::new(l), Box::new(r))
            }),
            infix(left(1), just("!=").padded(), |l, _, r, _| {
                Expr::NotEquals(Box::new(l), Box::new(r))
            }),
        ))
        .boxed()
    })
}

fn block<'a>() -> impl Parser<'a, &'a str, Block, extra::Err<Rich<'a, char>>> {
    let mut block_expr = Recursive::declare();
    let mut block_body = Recursive::declare();

    let expr_parser = expr(block_expr.clone()).boxed();

    block_body.define({
        let end_block = just(';')
            .padded()
            .or_not()
            .then_ignore(just('}').padded())
            .map(|semicolon| Block::Close {
                semicolon: semicolon.is_some(),
            });

        let if_branch = if_expr(expr_parser.clone(), block_expr.clone())
            .padded()
            .map(Expr::BlockExpr)
            .map(ExprOrStatement::Expr)
            .then(end_block.or(block_body.clone()))
            .padded();

        let expr_with_semicolon = expr_parser
            .clone()
            .padded()
            .map(ExprOrStatement::Expr)
            .then(
                end_block.or(just(";")
                    .ignored()
                    .padded()
                    .ignore_then(block_body.clone())
                    .padded()),
            );

        let statement = statement(expr_parser.clone(), block_expr.clone())
            .padded()
            .map(ExprOrStatement::Statement)
            .then(block_body.clone().padded().or(end_block))
            .boxed();

        let block_body_item = just('}')
            .to(Block::Close { semicolon: false })
            .padded()
            .or(
                choice((if_branch, expr_with_semicolon, statement)).map(|(x, xs)| Block::Chain {
                    head: Box::new(x),
                    tail: Box::new(xs),
                }),
            );

        comment().boxed().ignore_then(block_body_item)
    });

    block_expr.define(just('{').padded().ignore_then(block_body));

    block_expr
}

fn block_expr<'a>(
    expr_parser: impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone + 'a,
    block_parser: impl Parser<'a, &'a str, Block, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, BlockExpr, extra::Err<Rich<'a, char>>> {
    let parse_block = block_parser.clone().map(BlockExpr::Block);
    let if_expr = if_expr(expr_parser.clone(), block_parser.clone());

    choice((parse_block, if_expr))
}

fn if_expr<'a>(
    expr_parser: impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone + 'a,
    block_parser: impl Parser<'a, &'a str, Block, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, BlockExpr, extra::Err<Rich<'a, char>>> + Clone {
    just("if")
        .ignore_then(
            expr_parser
                .clone()
                .padded()
                .delimited_by(just("(").padded(), just(")").padded()),
        )
        .then(block_parser.clone().padded())
        .then(
            just("else")
                .padded()
                .ignore_then(block_parser.padded())
                .or_not(),
        )
        .map(|((expr1, expr2), expr3)| {
            BlockExpr::IfThenElse(Box::new(expr1), Box::new(expr2), expr3.map(Box::new))
        })
        .labelled("if-expr")
        .boxed()
}

fn primary_expr<'a>(
    expr_parser: impl Parser<'a, &'a str, Expr, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, PrimaryExpr, extra::Err<Rich<'a, char>>> {
    let number = just('-')
        .or_not()
        .then(text::int(10))
        .to_slice()
        .map(|s: &str| s.parse().unwrap())
        .map(PrimaryExpr::Number);

    let bool = choice((
        just("true").to(PrimaryExpr::Bool(true)),
        just("false").to(PrimaryExpr::Bool(false)),
    ));

    let par_expr = expr_parser
        .clone()
        .padded()
        .delimited_by(just('('), just(')'))
        .map(|expr| PrimaryExpr::ParExpr(Box::new(expr)));

    let yield_expr = just("yield")
        .ignore_then(expr_parser.clone().padded())
        .map(|expr| PrimaryExpr::Yield(Box::new(expr)));

    let raise_expr = just("raise")
        .ignore_then(expr_parser.clone().padded())
        .map(|expr| PrimaryExpr::Raise(Box::new(expr)));

    let object = r#type()
        .then(
            identifier()
                .then_ignore(just(":"))
                .then(expr_parser.clone().padded())
                .separated_by(just(',').padded())
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just('{').padded(), just('}').padded()),
        )
        .map(|(ty, values)| PrimaryExpr::Object(ty, values));

    let ident = identifier().map(PrimaryExpr::Ident);

    choice((
        number, bool, par_expr, yield_expr, raise_expr, object, ident,
    ))
    .boxed()
}

fn identifier<'a>() -> impl Parser<'a, &'a str, Identifier, extra::Err<Rich<'a, char>>> {
    text::ident().map(|s: &'a str| Identifier(s.to_string()))
}

fn r#type<'a>() -> impl Parser<'a, &'a str, Type, extra::Err<Rich<'a, char>>> {
    let mut type_parser = Recursive::declare();

    type_parser.define({
        let type_application = identifier()
            .padded()
            .then(
                type_parser
                    .clone()
                    .separated_by(just(',').padded())
                    .collect::<Vec<_>>()
                    .delimited_by(just('<').padded(), just('>').padded())
                    .or_not(),
            )
            .map(|(base, params)| Type::BaseType(base, params))
            .boxed();

        let typed_bindings = typed_binding(type_parser.clone())
            .separated_by(just(',').padded())
            .collect::<Vec<_>>()
            .boxed();

        let object = typed_bindings
            .clone()
            .delimited_by(just('{').padded(), just('}').padded())
            .map(|values| Type::Object(TypedBindings { values }))
            .boxed();

        let fn_type = typed_bindings
            .delimited_by(just('(').padded(), just(')').padded())
            .then(
                just("->")
                    .padded()
                    .ignore_then(type_parser.clone())
                    .or_not(),
            )
            .map(|(inputs, output)| {
                Type::FnType(TypedBindings { values: inputs }, output.map(Box::new))
            })
            .boxed();

        choice((type_application, object, fn_type)).clone()
    });

    type_parser
}

fn comment<'a>() -> impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char>>> {
    just("//")
        .padded()
        .then_ignore(none_of('\n').repeated())
        .or_not()
        .ignored()
}

#[cfg(test)]
mod tests {
    use super::*;

    use ariadne::Source;

    fn test_with_diagnostics<'a, T: std::fmt::Debug>(
        input: &'a str,
        parser: impl Parser<'a, &'a str, T, extra::Full<Rich<'a, char>, (), ()>>,
    ) -> T {
        let (output, errors) = parser.parse(input).into_output_errors();

        for e in errors {
            error_to_report(e).eprint(Source::from(input)).unwrap();
        }

        dbg!(output.unwrap())
    }

    #[test]
    fn parse_expr() {
        let input = "foo.x()";
        test_with_diagnostics(input, expr(block().boxed()));

        let input = "foo()";
        test_with_diagnostics(input, expr(block().boxed()));

        let input = "foo.x.y(3, 4)";
        test_with_diagnostics(input, expr(block().boxed()));

        let input = "Type { x: 4, y: 5}";
        test_with_diagnostics(input, expr(block().boxed()));
    }

    #[test]
    fn parse_main() {
        let input = "main {
            let y = 5;
            while(true) yield 4 + 4;
            loop { let z = 4; }
            y = 3;
            try { let z = 4; }
            with Effect1(x: T) { yield 4; }
            with Effect2(x) { yield x; }
        }";
        test_with_diagnostics(input, main());
    }

    #[test]
    fn parse_block() {
        let input = "{ 4 }";
        let output = test_with_diagnostics(input, block());
        match output {
            Block::Chain { head: _, tail } => match *tail {
                Block::Close { semicolon } => assert!(!semicolon),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }

        let input = "{ 4; }";
        let output = test_with_diagnostics(input, block());

        match output {
            Block::Chain { head: _, tail } => match *tail {
                Block::Close { semicolon } => assert!(semicolon),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[test]
    fn parse_storage() {
        let input = "storage { x: BigInt; y: F32; }";
        test_with_diagnostics(input, storage());
    }

    #[test]
    fn parse_abi() {
        let input = "abi { fn foo(): number; fn bar(Value); effect Effect1(Value): number; }";
        test_with_diagnostics(input, abi());
    }

    #[test]
    fn parse_impl() {
        let input = "impl Contract { fn foo(x: Int, y:Int): number { let x = 3; yield 3 } }";
        test_with_diagnostics(input, r#impl());
    }

    #[test]
    fn parse_token() {
        let input = "token Token1 { bind { let mut caller = 3; } unbind { let x = 4 + 5; } }";
        test_with_diagnostics(input, token());
    }

    #[test]
    fn parse_utxo() {
        let input = "utxo Contract { abi {} main {} }";
        test_with_diagnostics(input, utxo());
    }

    #[test]
    fn parse_program() {
        let input = "utxo Contract { abi {} main {} } token Token {}";
        test_with_diagnostics(input, starstream_program());
    }

    #[test]
    fn parse_type() {
        let input = "Intermediate<T, any>";
        test_with_diagnostics(input, r#type());

        let input = "{x: Int, y: Intermediate<T, any>}";
        test_with_diagnostics(input, r#type());

        let input = "(x: Int) -> Bool";
        test_with_diagnostics(input, r#type());

        let input = "(x: Int)";
        test_with_diagnostics(input, r#type());
    }
}
