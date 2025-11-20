use pretty::RcDoc;
use starstream_types::{
    BinaryOp, Block, Definition, Expr, FunctionDef, FunctionExport, FunctionParam, Literal,
    Spanned, Statement, TypeAnnotation, UnaryOp,
    ast::{
        EnumConstructorPayload, EnumDef, EnumPatternPayload, EnumVariant, EnumVariantPayload,
        Identifier, MatchArm, Pattern, Program, StructDef, StructField, StructLiteralField,
        StructPatternField,
    },
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
    if program.definitions.is_empty() {
        RcDoc::hardline()
    } else {
        let defs = program.definitions.iter().map(definition_to_doc);
        RcDoc::intersperse(defs, RcDoc::hardline().append(RcDoc::hardline()))
            .append(RcDoc::hardline())
    }
}

fn definition_to_doc(definition: &Definition) -> RcDoc<'_, ()> {
    match definition {
        Definition::Function(function) => function_to_doc(function),
        Definition::Struct(definition) => struct_definition_to_doc(definition),
        Definition::Enum(definition) => enum_definition_to_doc(definition),
    }
}

fn function_to_doc(function: &FunctionDef) -> RcDoc<'_, ()> {
    let params = params_to_doc(&function.params);
    let mut doc = if let Some(export) = &function.export {
        function_export_to_doc(export).append(RcDoc::space())
    } else {
        RcDoc::nil()
    }
    .append("fn")
    .append(RcDoc::space())
    .append(identifier_to_doc(&function.name))
    .append(RcDoc::text("("))
    .append(params)
    .append(RcDoc::text(")"));

    if let Some(return_type) = &function.return_type {
        doc = doc
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(type_annotation_to_doc(return_type));
    }

    doc.append(RcDoc::space())
        .append(block_to_doc(&function.body))
}

fn function_export_to_doc(export: &FunctionExport) -> RcDoc<'_, ()> {
    match export {
        FunctionExport::Script => RcDoc::text("script"),
    }
}

fn struct_definition_to_doc(definition: &StructDef) -> RcDoc<'_, ()> {
    RcDoc::text("struct")
        .append(RcDoc::space())
        .append(identifier_to_doc(&definition.name))
        .append(RcDoc::space())
        .append(struct_fields_to_doc(&definition.fields))
}

fn struct_fields_to_doc(fields: &[StructField]) -> RcDoc<'_, ()> {
    if fields.is_empty() {
        RcDoc::text("{ }")
    } else {
        let entries = RcDoc::intersperse(
            fields.iter().map(|field| {
                identifier_to_doc(&field.name)
                    .append(RcDoc::text(": "))
                    .append(type_annotation_to_doc(&field.ty))
                    .append(RcDoc::text(","))
            }),
            RcDoc::line(),
        );

        RcDoc::text("{")
            .append(RcDoc::line().append(entries).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn enum_definition_to_doc(definition: &EnumDef) -> RcDoc<'_, ()> {
    RcDoc::text("enum")
        .append(RcDoc::space())
        .append(identifier_to_doc(&definition.name))
        .append(RcDoc::space())
        .append(enum_variants_to_doc(&definition.variants))
}

fn enum_variants_to_doc(variants: &[EnumVariant]) -> RcDoc<'_, ()> {
    if variants.is_empty() {
        RcDoc::text("{ }")
    } else {
        let entries = RcDoc::intersperse(
            variants
                .iter()
                .map(|variant| enum_variant_to_doc(variant).append(RcDoc::text(","))),
            RcDoc::line(),
        );

        RcDoc::text("{")
            .append(RcDoc::line().append(entries).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn enum_variant_to_doc(variant: &EnumVariant) -> RcDoc<'_, ()> {
    match &variant.payload {
        EnumVariantPayload::Unit => identifier_to_doc(&variant.name),
        EnumVariantPayload::Tuple(types) => {
            if types.is_empty() {
                identifier_to_doc(&variant.name).append(RcDoc::text("()"))
            } else {
                let payload =
                    RcDoc::intersperse(types.iter().map(type_annotation_to_doc), RcDoc::text(", "));
                identifier_to_doc(&variant.name)
                    .append(RcDoc::text("("))
                    .append(payload)
                    .append(RcDoc::text(")"))
            }
        }
        EnumVariantPayload::Struct(fields) => identifier_to_doc(&variant.name)
            .append(RcDoc::space())
            .append(struct_fields_to_doc(fields)),
    }
}

fn params_to_doc(params: &[FunctionParam]) -> RcDoc<'_, ()> {
    if params.is_empty() {
        RcDoc::nil()
    } else {
        RcDoc::intersperse(
            params.iter().map(|param| {
                identifier_to_doc(&param.name)
                    .append(RcDoc::text(": "))
                    .append(type_annotation_to_doc(&param.ty))
            }),
            RcDoc::text(", "),
        )
    }
}

fn statement_to_doc(statement: &Statement) -> RcDoc<'_, ()> {
    match statement {
        Statement::VariableDeclaration {
            mutable,
            name,
            ty,
            value,
        } => RcDoc::text("let")
            .append(RcDoc::space())
            .append(if *mutable {
                RcDoc::text("mut").append(RcDoc::space())
            } else {
                RcDoc::nil()
            })
            .append(identifier_to_doc(name))
            .append(if let Some(ty) = ty {
                RcDoc::text(":")
                    .append(RcDoc::space())
                    .append(type_annotation_to_doc(ty))
            } else {
                RcDoc::nil()
            })
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
            branches,
            else_branch,
        } => {
            let mut out = RcDoc::nil();
            for (i, (condition, block)) in branches.iter().enumerate() {
                if i > 0 {
                    out = out
                        .append(RcDoc::space())
                        .append("else")
                        .append(RcDoc::space());
                }
                out = out
                    .append("if")
                    .append(RcDoc::space())
                    .append(parened_expr(condition))
                    .append(RcDoc::space())
                    .append(block_to_doc(block));
            }

            if let Some(else_branch) = else_branch {
                out.append(RcDoc::space())
                    .append(RcDoc::text("else"))
                    .append(RcDoc::space())
                    .append(block_to_doc(else_branch))
            } else {
                out
            }
        }
        Statement::While { condition, body } => RcDoc::text("while")
            .append(RcDoc::space())
            .append(parened_expr(condition))
            .append(RcDoc::space())
            .append(block_to_doc(body)),
        Statement::Block(block) => block_to_doc(block),
        Statement::Expression(expr) => expr_to_doc(&expr.node).append(RcDoc::text(";")),
        Statement::Return(Some(expr)) => RcDoc::text("return")
            .append(RcDoc::space())
            .append(expr_to_doc(&expr.node))
            .append(RcDoc::text(";")),
        Statement::Return(None) => RcDoc::text("return;"),
    }
}

fn block_to_doc(block: &Block) -> RcDoc<'_, ()> {
    if block.statements.is_empty() && block.tail_expression.is_none() {
        RcDoc::text("{ }")
    } else {
        let mut items: Vec<RcDoc<'_, ()>> = block.statements.iter().map(statement_to_doc).collect();
        if let Some(expr) = &block.tail_expression {
            items.push(expr_to_doc(&expr.node));
        }

        let body = RcDoc::intersperse(items, RcDoc::line());

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

fn type_annotation_to_doc(annotation: &TypeAnnotation) -> RcDoc<'_, ()> {
    let mut doc = identifier_to_doc(&annotation.name);
    if !annotation.generics.is_empty() {
        let generics = RcDoc::intersperse(
            annotation.generics.iter().map(type_annotation_to_doc),
            RcDoc::text(", "),
        );
        doc = doc
            .append(RcDoc::text("<"))
            .append(generics)
            .append(RcDoc::text(">"));
    }
    doc
}

fn struct_literal_expr_to_doc<'a>(
    name: &'a Identifier,
    fields: &'a [StructLiteralField],
) -> RcDoc<'a, ()> {
    identifier_to_doc(name)
        .append(RcDoc::space())
        .append(struct_literal_fields_to_doc(fields))
}

fn struct_literal_fields_to_doc<'a>(fields: &'a [StructLiteralField]) -> RcDoc<'a, ()> {
    if fields.is_empty() {
        RcDoc::text("{ }")
    } else {
        let body = RcDoc::intersperse(
            fields.iter().map(|field| {
                identifier_to_doc(&field.name)
                    .append(RcDoc::text(": "))
                    .append(expr_to_doc(&field.value.node))
                    .append(RcDoc::text(","))
            }),
            RcDoc::line(),
        );

        RcDoc::text("{")
            .append(RcDoc::line().append(body).nest(INDENT))
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
    }
}

fn enum_constructor_to_doc<'a>(
    enum_name: &'a Identifier,
    variant: &'a Identifier,
    payload: &'a EnumConstructorPayload,
) -> RcDoc<'a, ()> {
    let mut doc = identifier_to_doc(enum_name)
        .append(RcDoc::text("::"))
        .append(identifier_to_doc(variant));

    doc = match payload {
        EnumConstructorPayload::Unit => doc,
        EnumConstructorPayload::Tuple(values) => {
            if values.is_empty() {
                doc.append(RcDoc::text("()"))
            } else {
                let args = RcDoc::intersperse(
                    values.iter().map(|expr| expr_to_doc(&expr.node)),
                    RcDoc::text(", "),
                );
                doc.append(RcDoc::text("("))
                    .append(args)
                    .append(RcDoc::text(")"))
            }
        }
        EnumConstructorPayload::Struct(fields) => doc
            .append(RcDoc::space())
            .append(struct_literal_fields_to_doc(fields)),
    };

    doc
}

fn match_expr_to_doc<'a>(scrutinee: &'a Spanned<Expr>, arms: &'a [MatchArm]) -> RcDoc<'a, ()> {
    let doc = RcDoc::text("match")
        .append(RcDoc::space())
        .append(expr_to_doc(&scrutinee.node))
        .append(RcDoc::space());

    if arms.is_empty() {
        doc.append(RcDoc::text("{ }"))
    } else {
        let items = RcDoc::intersperse(
            arms.iter()
                .map(|arm| match_arm_to_doc(arm).append(RcDoc::text(","))),
            RcDoc::line(),
        );
        doc.append(
            RcDoc::text("{")
                .append(RcDoc::line().append(items).nest(INDENT))
                .append(RcDoc::line())
                .append(RcDoc::text("}")),
        )
    }
}

fn match_arm_to_doc<'a>(arm: &'a MatchArm) -> RcDoc<'a, ()> {
    pattern_to_doc(&arm.pattern)
        .append(RcDoc::space())
        .append(RcDoc::text("=>"))
        .append(RcDoc::space())
        .append(block_to_doc(&arm.body))
}

fn pattern_to_doc<'a>(pattern: &'a Pattern) -> RcDoc<'a, ()> {
    match pattern {
        Pattern::Binding(name) => identifier_to_doc(name),
        Pattern::Struct { name, fields } => identifier_to_doc(name)
            .append(RcDoc::space())
            .append(struct_pattern_fields_to_doc(fields)),
        Pattern::EnumVariant {
            enum_name,
            variant,
            payload,
        } => {
            let doc = identifier_to_doc(enum_name)
                .append(RcDoc::text("::"))
                .append(identifier_to_doc(variant));
            match payload {
                EnumPatternPayload::Unit => doc,
                EnumPatternPayload::Tuple(items) => {
                    if items.is_empty() {
                        doc.append(RcDoc::text("()"))
                    } else {
                        let inner =
                            RcDoc::intersperse(items.iter().map(pattern_to_doc), RcDoc::text(", "));
                        doc.append(RcDoc::text("("))
                            .append(inner)
                            .append(RcDoc::text(")"))
                    }
                }
                EnumPatternPayload::Struct(fields) => doc
                    .append(RcDoc::space())
                    .append(struct_pattern_fields_to_doc(fields)),
            }
        }
    }
}

fn struct_pattern_fields_to_doc<'a>(fields: &'a [StructPatternField]) -> RcDoc<'a, ()> {
    if fields.is_empty() {
        RcDoc::text("{ }")
    } else {
        let items = RcDoc::intersperse(
            fields.iter().map(|field| {
                if let Pattern::Binding(binding) = field.pattern.as_ref()
                    && binding.name == field.name.name
                {
                    return identifier_to_doc(&field.name);
                }

                identifier_to_doc(&field.name)
                    .append(RcDoc::text(": "))
                    .append(pattern_to_doc(&field.pattern))
            }),
            RcDoc::text(", "),
        );
        RcDoc::text("{")
            .append(RcDoc::space())
            .append(items)
            .append(RcDoc::space())
            .append(RcDoc::text("}"))
    }
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
                Expr::StructLiteral { name, fields } => struct_literal_expr_to_doc(name, fields),
                Expr::FieldAccess { target, field } => {
                    let receiver = expr_with_prec(&target.node, PREC_PRIMARY, ChildPosition::Top);
                    receiver
                        .append(RcDoc::text("."))
                        .append(identifier_to_doc(field))
                }
                Expr::EnumConstructor {
                    enum_name,
                    variant,
                    payload,
                } => enum_constructor_to_doc(enum_name, variant, payload),
                Expr::Match { scrutinee, arms } => match_expr_to_doc(scrutinee, arms),
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
        Literal::Unit => RcDoc::text("()"),
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
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::StructLiteral { .. }
        | Expr::EnumConstructor { .. } => PREC_PRIMARY,
        Expr::Grouping(inner) => precedence(&inner.node),
        Expr::Unary { .. } => PREC_UNARY,
        Expr::Binary { op, .. } => precedence_binary(op),
        Expr::FieldAccess { .. } => PREC_FIELD_ACCESS,
        Expr::Match { .. } => PREC_LOWEST,
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
const PREC_FIELD_ACCESS: u8 = 9;
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
            fn main() {
                    let flag = true;
                if (    flag) {
                    let mut answer = 42;
                    while (answer < 100) {
                        answer = answer + 1;
                    }
                }else if( ! flag && true ){
                    answer = 17;
                } else {
                    answer = 0;
                }
            }
            "#,
        );
    }

    #[test]
    fn expressions() {
        assert_format_snapshot!(
            r#"
            fn main() {
                let mut value      = -(-5);
                value = (1 + 2) * (3 - 4) / 5;
                value = value + (10 / (3 + 2));
                result = (1 + 2 == 3) && !(false || true);
            }
            "#,
        );
    }

    #[test]
    fn nested_blocks() {
        assert_format_snapshot!(
            r#"
            fn main() {
                {
                    let x = 1;
                    {
                        let mut y = x + 2;

                        y = y * (x + y);
                    }
                }
            }
            "#,
        );
    }

    #[test]
    fn struct_field_access() {
        assert_format_snapshot!(
            r#"
            struct Point { x: i64 }

            fn add(a: Point, b: i64) -> i64 {
                a.x + b
            }
            "#,
        );
    }

    #[test]
    fn enum_variants_comma_separated() {
        assert_format_snapshot!(
            r#"
            enum Message {
                Ping,
                Pong { x: i64     },
            }
            "#,
        );
    }

    #[test]
    fn struct_definition_commas() {
        assert_format_snapshot!(
            r#"
            struct Point {
                x:   i64,
                y: i64
            }
            "#,
        );
    }

    #[test]
    fn enum_struct_variant_commas() {
        assert_format_snapshot!(
            r#"
            enum Message {
                Ping,
                Pong {
                    x: i64,
                    y: i64
                }
            }
            "#,
        );
    }

    #[test]
    fn match_arms_with_commas_and_punning() {
        assert_format_snapshot!(
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
                    }
                }
            }
            "#,
        );
    }
}
