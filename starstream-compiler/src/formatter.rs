use pretty::RcDoc;
use starstream_types::{
    BinaryOp, Block, Expr, Identifier, Literal, Program, Spanned, Statement, UnaryOp,
};
use std::fmt;

pub fn program(program: &Program) -> Result<String, fmt::Error> {
    let mut out = String::new();

    program_to_doc(program).render_fmt(80, &mut out)?;

    Ok(out)
}

pub fn statement(statement: &Statement) -> Result<String, fmt::Error> {
    let mut out = String::new();

    statement_to_doc(statement).render_fmt(80, &mut out)?;

    Ok(out)
}

pub fn expression(expr: &Expr) -> Result<String, fmt::Error> {
    let mut out = String::new();

    expr_to_doc(expr).render_fmt(80, &mut out)?;

    Ok(out)
}

fn program_to_doc(program: &Program) -> RcDoc<'_, ()> {
    let statements = program.statements.iter().map(statement_to_doc);

    RcDoc::intersperse(statements, RcDoc::hardline()).append(RcDoc::hardline())
}

fn statement_to_doc(statement: &Statement) -> RcDoc<'_, ()> {
    match statement {
        Statement::VariableDeclaration { name, value } => RcDoc::text("let")
            .append(RcDoc::space())
            .append(identifier_to_doc(name))
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(expr_to_doc(&value.node))
            .append(RcDoc::text(";")),
        Statement::Assignment { target, value } => identifier_to_doc(target)
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(expr_to_doc(&value.node))
            .append(RcDoc::text(";")),
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let base = RcDoc::text("if")
                .append(RcDoc::space())
                .append(parened_expr(condition))
                .append(RcDoc::space())
                .append(block_to_doc(then_branch));

            if let Some(else_branch) = else_branch {
                base.append(RcDoc::space())
                    .append(RcDoc::text("else"))
                    .append(RcDoc::space())
                    .append(block_to_doc(else_branch))
            } else {
                base
            }
        }
        Statement::While { condition, body } => RcDoc::text("while")
            .append(RcDoc::space())
            .append(parened_expr(condition))
            .append(RcDoc::space())
            .append(block_to_doc(body)),
        Statement::Block(block) => block_to_doc(block),
        Statement::Expression(expr) => expr_to_doc(&expr.node).append(RcDoc::text(";")),
    }
}

fn block_to_doc(block: &Block) -> RcDoc<'_, ()> {
    if block.statements.is_empty() {
        RcDoc::text("{ }")
    } else {
        let statements = block.statements.iter().map(statement_to_doc);

        let body = RcDoc::intersperse(statements, RcDoc::line());

        RcDoc::text("{")
            .append(RcDoc::line().append(body).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn parened_expr(expr: &Spanned<Expr>) -> RcDoc<'_, ()> {
    RcDoc::text("(")
        .append(expr_to_doc(&expr.node))
        .append(RcDoc::text(")"))
}

fn identifier_to_doc(identifier: &Identifier) -> RcDoc<'_, ()> {
    RcDoc::text(identifier.name.clone())
}

fn expr_to_doc(expr: &Expr) -> RcDoc<'_, ()> {
    expr_with_prec(expr, PREC_LOWEST, ChildPosition::Top)
}

fn expr_with_prec(expr: &Expr, parent_prec: u8, position: ChildPosition) -> RcDoc<'_, ()> {
    match expr {
        Expr::Grouping(inner) => RcDoc::text("(")
            .append(expr_with_prec(&inner.node, PREC_LOWEST, ChildPosition::Top))
            .append(RcDoc::text(")")),
        _ => {
            let prec = precedence(expr);
            let doc = match expr {
                Expr::Literal(literal) => literal_to_doc(literal),
                Expr::Identifier(identifier) => identifier_to_doc(identifier),
                Expr::Unary { op, expr } => {
                    let operand = expr_with_prec(&expr.node, prec, ChildPosition::Unary);

                    RcDoc::text(unary_op_str(op)).append(operand)
                }
                Expr::Binary { op, left, right } => {
                    let left_doc = expr_with_prec(&left.node, prec, ChildPosition::Left).group();
                    let right_doc = expr_with_prec(&right.node, prec, ChildPosition::Right).group();

                    left_doc
                        .append(RcDoc::space())
                        .append(RcDoc::text(binary_op_str(op)))
                        .append(RcDoc::space())
                        .append(right_doc)
                }
                Expr::Grouping(_) => unreachable!(),
            };

            if needs_parentheses(prec, parent_prec, position) {
                RcDoc::text("(").append(doc).append(RcDoc::text(")"))
            } else {
                doc
            }
        }
    }
}

fn literal_to_doc(literal: &Literal) -> RcDoc<'_, ()> {
    match literal {
        Literal::Integer(value) => RcDoc::as_string(*value),
        Literal::Boolean(value) => RcDoc::text(if *value { "true" } else { "false" }),
    }
}

fn binary_op_str(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Remainder => "%",
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
    }
}

fn unary_op_str(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Negate => "-",
        UnaryOp::Not => "!",
    }
}

fn precedence(expr: &Expr) -> u8 {
    match expr {
        Expr::Literal(_) | Expr::Identifier(_) => PREC_PRIMARY,
        Expr::Grouping(inner) => precedence(&inner.node),
        Expr::Unary { .. } => PREC_UNARY,
        Expr::Binary { op, .. } => precedence_binary(op),
    }
}

fn precedence_binary(op: &BinaryOp) -> u8 {
    match op {
        BinaryOp::Or => PREC_OR,
        BinaryOp::And => PREC_AND,
        BinaryOp::Equal | BinaryOp::NotEqual => PREC_EQUALITY,
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
            PREC_COMPARISON
        }
        BinaryOp::Add | BinaryOp::Subtract => PREC_ADDITIVE,
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => PREC_MULTIPLICATIVE,
    }
}

fn needs_parentheses(prec: u8, parent_prec: u8, position: ChildPosition) -> bool {
    if matches!(position, ChildPosition::Top) {
        return false;
    }

    if prec < parent_prec {
        return true;
    }

    if prec == parent_prec {
        matches!(position, ChildPosition::Right)
    } else {
        false
    }
}

#[derive(Clone, Copy, Debug)]
enum ChildPosition {
    Top,
    Left,
    Right,
    Unary,
}

const PREC_LOWEST: u8 = 0;
const PREC_OR: u8 = 1;
const PREC_AND: u8 = 2;
const PREC_EQUALITY: u8 = 3;
const PREC_COMPARISON: u8 = 4;
const PREC_ADDITIVE: u8 = 5;
const PREC_MULTIPLICATIVE: u8 = 6;
const PREC_UNARY: u8 = 7;
const PREC_PRIMARY: u8 = 8;
const INDENT: isize = 4;

#[cfg(test)]
mod tests {
    use crate::parser;
    use indoc::indoc;

    fn formatted(code: &str) -> String {
        let parse_output = parser::parse_program(code);
        assert!(
            parse_output.errors().is_empty(),
            "program should parse without errors"
        );

        let ast = parse_output.into_program().expect("program should parse");

        super::program(&ast).expect("formatting succeeds")
    }

    macro_rules! assert_format_snapshot {
        ($code:expr $(,)?) => {{
            let code = indoc! { $code };
            let formatted = formatted(code);

            insta::with_settings!({
                description => format!("Code:\n\n{}", code),
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_snapshot!(formatted);
            });
        }};
    }

    #[test]
    fn control_flow() {
        assert_format_snapshot!(
            r#"
                let flag = true;
            if (    flag) {
                let answer = 42;
                while (answer < 100) {
                    answer = answer + 1;
                }
            } else {
                answer = 0;
            }
            "#,
        );
    }

    #[test]
    fn expressions() {
        assert_format_snapshot!(
            r#"
            let value      = -(-5);
            value = (1 + 2) * (3 - 4) / 5;
            value = value + (10 / (3 + 2));
            result = (1 + 2 == 3) && !(false || true);
            "#,
        );
    }

    #[test]
    fn nested_blocks() {
        assert_format_snapshot!(
            r#"
            {
                let x = 1;
                {
                    let y = x + 2;

                    y = y * (x + y);
                }
            }
            "#,
        );
    }
}
