#![allow(clippy::result_large_err)]

use std::collections::{HashMap, HashSet};

use starstream_types::{
    Scheme, Span, Spanned, Type, TypeVarId,
    ast::{
        BinaryOp, Block, Definition, Expr, FunctionDef, Identifier, Literal, Program, Statement,
        TypeAnnotation, UnaryOp,
    },
    typed_ast::{
        TypedBlock, TypedDefinition, TypedExpr, TypedExprKind, TypedFunctionDef,
        TypedFunctionParam, TypedProgram, TypedStatement,
    },
};

use super::{
    env::{Binding, TypeEnv},
    errors::{ConditionContext, TypeError, TypeErrorKind},
    tree::InferenceTree,
};
use crate::formatter;

/// Optional settings that control type-checker behavior.
#[derive(Clone, Debug, Default)]
pub struct TypecheckOptions {
    pub capture_traces: bool,
}

/// Successful type-checking result holding the typed AST and any inference traces.
#[derive(Clone, Debug)]
pub struct TypecheckSuccess {
    pub program: TypedProgram,
    pub traces: Vec<InferenceTree>,
}

/// Run Hindley–Milner style inference over the parsed program and return the
/// typed AST along with optional tracing information.
pub fn typecheck_program(
    program: &Program,
    options: TypecheckOptions,
) -> Result<TypecheckSuccess, Vec<TypeError>> {
    let mut inferencer = Inferencer::new(options.capture_traces);
    let mut typed_definitions = Vec::with_capacity(program.definitions.len());
    let mut definition_traces = Vec::with_capacity(program.definitions.len());
    let mut errors = Vec::new();

    for definition in &program.definitions {
        match inferencer.infer_definition(definition) {
            Ok((typed_definition, trace)) => {
                typed_definitions.push(typed_definition);
                definition_traces.push(trace);
            }
            Err(error) => {
                errors.push(error);
                break;
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    let mut typed_program = TypedProgram::new(typed_definitions);
    inferencer.apply_substitutions_program(&mut typed_program);

    let traces = if options.capture_traces {
        definition_traces
    } else {
        Vec::new()
    };

    Ok(TypecheckSuccess {
        program: typed_program,
        traces,
    })
}

/// Internal stateful helper that owns the substitution map and generates fresh
/// type variables while walking the AST.
struct Inferencer {
    capture_traces: bool,
    next_type_var: u32,
    subst: HashMap<TypeVarId, Type>,
}

struct FunctionCtx {
    expected_return: Type,
    return_span: Span,
    saw_return: bool,
}

impl Inferencer {
    /// Construct a fresh inferencer with an empty substitution environment.
    fn new(capture_traces: bool) -> Self {
        Self {
            capture_traces,
            next_type_var: 0,
            subst: HashMap::new(),
        }
    }

    /// Type-check a top-level definition.
    fn infer_definition(
        &mut self,
        definition: &Definition,
    ) -> Result<(TypedDefinition, InferenceTree), TypeError> {
        match definition {
            Definition::Function(function) => {
                let (typed_function, trace) = self.infer_function(function)?;
                Ok((TypedDefinition::Function(typed_function), trace))
            }
        }
    }

    fn infer_function(
        &mut self,
        function: &FunctionDef,
    ) -> Result<(TypedFunctionDef, InferenceTree), TypeError> {
        let mut env = TypeEnv::new();
        let mut typed_params = Vec::with_capacity(function.params.len());
        for param in &function.params {
            let ty = self.type_from_annotation(&param.ty)?;
            env.insert(
                param.name.name.clone(),
                Binding {
                    decl_span: param
                        .name
                        .span
                        .or(function.name.span)
                        .unwrap_or_else(dummy_span),
                    mutable: false,
                    scheme: Scheme::monomorphic(ty.clone()),
                },
            );
            typed_params.push(TypedFunctionParam {
                name: param.name.clone(),
                ty,
            });
        }

        let (expected_return, return_span) = match &function.return_type {
            Some(annotation) => (
                self.type_from_annotation(annotation)?,
                annotation
                    .name
                    .span
                    .or(function.name.span)
                    .unwrap_or_else(dummy_span),
            ),
            None => (Type::unit(), function.name.span.unwrap_or_else(dummy_span)),
        };

        let mut ctx = FunctionCtx {
            expected_return: expected_return.clone(),
            return_span,
            saw_return: false,
        };

        let (typed_body, body_traces) =
            self.infer_block(&mut env, &function.body, &mut ctx, true)?;

        if expected_return != Type::unit()
            && !ctx.saw_return
            && typed_body.tail_expression.is_none()
        {
            return Err(TypeError::new(
                TypeErrorKind::MissingReturn {
                    expected: expected_return,
                },
                function.name.span.unwrap_or(return_span),
            )
            .with_help("add a `return` or tail expression to satisfy the signature"));
        }

        let subject = self.maybe_string(|| function.name.name.clone());
        let result = self.maybe_string(|| self.format_type(&ctx.expected_return));
        let trace = self.make_trace("T-Fn", None, subject, result, || body_traces);

        Ok((
            TypedFunctionDef {
                name: function.name.clone(),
                params: typed_params,
                return_type: ctx.expected_return,
                body: typed_body,
            },
            trace,
        ))
    }

    /// Type-check a single statement, yielding its typed form and an inference trace.
    fn infer_statement(
        &mut self,
        env: &mut TypeEnv,
        statement: &Statement,
        ctx: &mut FunctionCtx,
    ) -> Result<(TypedStatement, InferenceTree), TypeError> {
        let env_context = self.maybe_string(|| self.format_env(env));
        let stmt_repr = self.maybe_string(|| self.format_statement_src(statement));
        match statement {
            Statement::VariableDeclaration {
                mutable,
                name,
                value,
            } => {
                if env.contains_in_current_scope(&name.name) {
                    return Err(TypeError::new(
                        TypeErrorKind::Redeclaration {
                            name: name.name.clone(),
                        },
                        value.span,
                    ));
                }

                let (typed_value, value_trace) = self.infer_expr(env, value)?;
                let value_type = self.apply(&typed_value.node.ty);
                let scheme = self.generalize(env, &value_type);
                env.insert(
                    name.name.clone(),
                    Binding {
                        decl_span: name.span.unwrap_or(value.span),
                        mutable: *mutable,
                        scheme,
                    },
                );

                let value_type_repr = self.maybe_string(|| self.format_type(&value_type));
                let tree = self.make_trace(
                    "T-Let",
                    env_context.clone(),
                    stmt_repr.clone(),
                    value_type_repr,
                    || vec![value_trace],
                );

                Ok((
                    TypedStatement::VariableDeclaration {
                        mutable: *mutable,
                        name: name.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::Assignment { target, value } => {
                let binding = env.get(&target.name).cloned().ok_or_else(|| {
                    TypeError::new(
                        TypeErrorKind::UnknownVariable {
                            name: target.name.clone(),
                        },
                        target.span.unwrap_or(value.span),
                    )
                })?;

                if !binding.mutable {
                    return Err(TypeError::new(
                        TypeErrorKind::AssignmentToImmutable {
                            name: target.name.clone(),
                        },
                        target.span.unwrap_or(value.span),
                    )
                    .with_secondary(binding.decl_span, "binding declared immutable here"));
                }

                let expected_type = self.instantiate(&binding.scheme);
                let (typed_value, value_trace) = self.infer_expr(env, value)?;
                let actual_type = typed_value.node.ty.clone();

                let (_, unify_trace) = self.unify(
                    actual_type.clone(),
                    expected_type.clone(),
                    value.span,
                    target.span.unwrap_or(value.span),
                    TypeErrorKind::AssignmentMismatch {
                        name: target.name.clone(),
                        expected: self.apply(&expected_type),
                        found: self.apply(&actual_type),
                    },
                )?;

                let expected_repr = self.maybe_string(|| self.format_type(&expected_type));
                let tree = self.make_trace(
                    "T-Assign",
                    env_context.clone(),
                    stmt_repr.clone(),
                    expected_repr,
                    || vec![value_trace, unify_trace],
                );

                Ok((
                    TypedStatement::Assignment {
                        target: target.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::Return(value) => {
                let result_repr = self.maybe_string(|| self.format_type(&ctx.expected_return));
                match value {
                    Some(expr) => {
                        let (typed_expr, expr_trace) = self.infer_expr(env, expr)?;
                        let actual_type = typed_expr.node.ty.clone();
                        let (_, unify_trace) = self.unify(
                            actual_type.clone(),
                            ctx.expected_return.clone(),
                            expr.span,
                            ctx.return_span,
                            TypeErrorKind::ReturnMismatch {
                                expected: self.apply(&ctx.expected_return),
                                found: self.apply(&actual_type),
                            },
                        )?;
                        ctx.saw_return = true;
                        let tree = self.make_trace(
                            "T-Return",
                            env_context,
                            stmt_repr,
                            result_repr,
                            || vec![expr_trace, unify_trace],
                        );
                        Ok((TypedStatement::Return(Some(typed_expr)), tree))
                    }
                    None => {
                        let unit = Type::unit();
                        let (_, unify_trace) = self.unify(
                            unit.clone(),
                            ctx.expected_return.clone(),
                            ctx.return_span,
                            ctx.return_span,
                            TypeErrorKind::ReturnMismatch {
                                expected: self.apply(&ctx.expected_return),
                                found: self.apply(&unit),
                            },
                        )?;
                        ctx.saw_return = true;
                        let tree = self.make_trace(
                            "T-ReturnUnit",
                            env_context,
                            stmt_repr,
                            result_repr,
                            || vec![unify_trace],
                        );
                        Ok((TypedStatement::Return(None), tree))
                    }
                }
            }
            Statement::If {
                branches,
                else_branch,
            } => {
                let mut children = Vec::new();

                let mut typed_branches = Vec::with_capacity(branches.len());
                for (condition, then_branch) in branches {
                    let (typed_condition, cond_trace) = self.infer_expr(env, condition)?;
                    children.push(cond_trace);
                    let bool_check = self.require_bool(
                        &typed_condition.node.ty,
                        condition.span,
                        ConditionContext::If,
                    )?;
                    children.push(bool_check);

                    let (typed_then, then_traces) =
                        self.infer_block(env, then_branch, ctx, false)?;
                    children.extend(then_traces);

                    typed_branches.push((typed_condition, typed_then));
                }

                let typed_else_block = if let Some(block) = else_branch {
                    let (typed_block, else_traces) = self.infer_block(env, block, ctx, false)?;
                    children.extend(else_traces);
                    Some(typed_block)
                } else {
                    None
                };

                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-If",
                    env_context.clone(),
                    stmt_repr.clone(),
                    unit_result,
                    || children,
                );

                Ok((
                    TypedStatement::If {
                        branches: typed_branches,
                        else_branch: typed_else_block,
                    },
                    tree,
                ))
            }
            Statement::While { condition, body } => {
                let (typed_condition, cond_trace) = self.infer_expr(env, condition)?;
                let bool_check = self.require_bool(
                    &typed_condition.node.ty,
                    condition.span,
                    ConditionContext::While,
                )?;

                let (typed_body, body_traces) = self.infer_block(env, body, ctx, false)?;

                let mut children = vec![cond_trace, bool_check];
                children.extend(body_traces);

                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-While",
                    env_context.clone(),
                    stmt_repr.clone(),
                    unit_result,
                    || children,
                );

                Ok((
                    TypedStatement::While {
                        condition: typed_condition,
                        body: typed_body,
                    },
                    tree,
                ))
            }
            Statement::Block(block) => {
                let (typed_block, block_traces) = self.infer_block(env, block, ctx, false)?;
                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace(
                    "T-Block",
                    env_context.clone(),
                    stmt_repr.clone(),
                    unit_result,
                    || block_traces,
                );
                Ok((TypedStatement::Block(typed_block), tree))
            }
            Statement::Expression(expr) => {
                let (typed_expr, expr_trace) = self.infer_expr(env, expr)?;
                let unit_result = self.maybe_string(|| "()".to_string());
                let tree = self.make_trace("T-Expr", env_context, stmt_repr, unit_result, || {
                    vec![expr_trace]
                });
                Ok((TypedStatement::Expression(typed_expr), tree))
            }
        }
    }

    /// Type-check a block, returning typed statements plus per-statement traces.
    fn infer_block(
        &mut self,
        env: &mut TypeEnv,
        block: &Block,
        ctx: &mut FunctionCtx,
        treat_tail_as_return: bool,
    ) -> Result<(TypedBlock, Vec<InferenceTree>), TypeError> {
        env.push_scope();
        let mut typed_statements = Vec::with_capacity(block.statements.len());
        let mut traces = Vec::with_capacity(block.statements.len() + 1);
        for statement in &block.statements {
            let (typed, trace) = self.infer_statement(env, statement, ctx)?;
            typed_statements.push(typed);
            traces.push(trace);
        }

        let mut tail_expression = None;
        if let Some(expr) = &block.tail_expression {
            let (typed_expr, expr_trace) = self.infer_expr(env, expr)?;
            let mut children = vec![expr_trace];
            if treat_tail_as_return {
                ctx.saw_return = true;
                let actual = typed_expr.node.ty.clone();
                let (_, unify_trace) = self.unify(
                    actual.clone(),
                    ctx.expected_return.clone(),
                    expr.span,
                    ctx.return_span,
                    TypeErrorKind::ReturnMismatch {
                        expected: self.apply(&ctx.expected_return),
                        found: self.apply(&actual),
                    },
                )?;
                children.push(unify_trace);
            }

            let label = if treat_tail_as_return {
                "T-ReturnTail"
            } else {
                "T-Tail"
            };
            let subject = self.maybe_string(|| self.format_expr_src(expr));
            let result = self.maybe_string(|| self.format_type(&typed_expr.node.ty));
            let tail_trace = self.make_trace(label, None, subject, result, || children);
            traces.push(tail_trace);
            tail_expression = Some(typed_expr);
        }

        env.pop_scope();
        Ok((TypedBlock::new(typed_statements, tail_expression), traces))
    }

    /// Type-check an expression, returning the typed node and corresponding trace tree.
    fn infer_expr(
        &mut self,
        env: &mut TypeEnv,
        expr: &Spanned<Expr>,
    ) -> Result<(Spanned<TypedExpr>, InferenceTree), TypeError> {
        let env_context = self.maybe_string(|| self.format_env(env));
        let subject_repr = self.maybe_string(|| self.format_expr_src(expr));
        match &expr.node {
            Expr::Literal(lit) => {
                let (ty, kind, rule) = match lit {
                    Literal::Integer(value) => (
                        Type::int(),
                        TypedExprKind::Literal(Literal::Integer(*value)),
                        "T-Int",
                    ),
                    Literal::Boolean(value) => (
                        Type::bool(),
                        TypedExprKind::Literal(Literal::Boolean(*value)),
                        "T-Bool",
                    ),
                };
                let typed = Spanned::new(TypedExpr::new(ty.clone(), kind), expr.span);
                let result_repr = self.maybe_string(|| self.format_type(&ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    Vec::new,
                );
                Ok((typed, tree))
            }
            Expr::Identifier(Identifier { name, span }) => {
                let binding = env.get(name).cloned().ok_or_else(|| {
                    let span = span.unwrap_or(expr.span);
                    TypeError::new(TypeErrorKind::UnknownVariable { name: name.clone() }, span)
                })?;
                let ty = self.instantiate(&binding.scheme);
                let typed = Spanned::new(
                    TypedExpr::new(
                        ty.clone(),
                        TypedExprKind::Identifier(Identifier {
                            name: name.clone(),
                            span: *span,
                        }),
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&ty));
                let tree = self.make_trace(
                    "T-Var",
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    Vec::new,
                );
                Ok((typed, tree))
            }
            Expr::Unary { op, expr: inner } => {
                let (typed_inner, inner_trace) = self.infer_expr(env, inner)?;
                let check = match op {
                    UnaryOp::Negate => self.require_is(
                        &typed_inner.node.ty,
                        Type::int(),
                        inner.span,
                        inner.span,
                        TypeErrorKind::UnaryMismatch {
                            op: *op,
                            expected: Type::int(),
                            found: self.apply(&typed_inner.node.ty),
                        },
                    )?,
                    UnaryOp::Not => self.require_is(
                        &typed_inner.node.ty,
                        Type::bool(),
                        inner.span,
                        inner.span,
                        TypeErrorKind::UnaryMismatch {
                            op: *op,
                            expected: Type::bool(),
                            found: self.apply(&typed_inner.node.ty),
                        },
                    )?,
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        typed_inner.node.ty.clone(),
                        TypedExprKind::Unary {
                            op: *op,
                            expr: Box::new(typed_inner.clone()),
                        },
                    ),
                    expr.span,
                );

                let rule = match op {
                    UnaryOp::Negate => "T-Unary-Neg",
                    UnaryOp::Not => "T-Unary-Not",
                };

                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    || vec![inner_trace, check],
                );

                Ok((typed, tree))
            }
            Expr::Binary { op, left, right } => {
                let (typed_left, left_trace) = self.infer_expr(env, left)?;
                let (typed_right, right_trace) = self.infer_expr(env, right)?;
                let left_ty = self.apply(&typed_left.node.ty);
                let right_ty = self.apply(&typed_right.node.ty);
                let left_label_span = self.label_span_for_expr(left);
                let right_label_span = self.label_span_for_expr(right);

                let mut children = vec![left_trace, right_trace];
                let expr_type = match op {
                    BinaryOp::Add
                    | BinaryOp::Subtract
                    | BinaryOp::Multiply
                    | BinaryOp::Divide
                    | BinaryOp::Remainder => {
                        let both_int =
                            matches!(&left_ty, Type::Int) && matches!(&right_ty, Type::Int);
                        if !both_int {
                            let left_repr = self.format_type(&left_ty);
                            let right_repr = self.format_type(&right_ty);
                            return Err(TypeError::new(
                                TypeErrorKind::BinaryOperandMismatch {
                                    op: *op,
                                    left: left_ty.clone(),
                                    right: right_ty.clone(),
                                },
                                left_label_span,
                            )
                            .with_primary_message(format!("has type `{left_repr}`"))
                            .with_secondary(right_label_span, format!("has type `{right_repr}`")));
                        }

                        children.push(self.require_is(
                            &typed_left.node.ty,
                            Type::int(),
                            left_label_span,
                            left_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        children.push(self.require_is(
                            &typed_right.node.ty,
                            Type::int(),
                            right_label_span,
                            right_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        Type::int()
                    }
                    BinaryOp::Less
                    | BinaryOp::LessEqual
                    | BinaryOp::Greater
                    | BinaryOp::GreaterEqual => {
                        let comparison = self.require_numeric_or_bool_pair(
                            op,
                            &typed_left,
                            left_label_span,
                            &typed_right,
                            right_label_span,
                        )?;
                        children.push(comparison);
                        Type::bool()
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        let check = self.require_same_primitive(
                            op,
                            &typed_left,
                            left_label_span,
                            &typed_right,
                            right_label_span,
                        )?;
                        children.push(check);
                        Type::bool()
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        let both_bool =
                            matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool);
                        if !both_bool {
                            let left_repr = self.format_type(&left_ty);
                            let right_repr = self.format_type(&right_ty);
                            return Err(TypeError::new(
                                TypeErrorKind::BinaryOperandMismatch {
                                    op: *op,
                                    left: left_ty.clone(),
                                    right: right_ty.clone(),
                                },
                                left_label_span,
                            )
                            .with_primary_message(format!("has type `{left_repr}`"))
                            .with_secondary(right_label_span, format!("has type `{right_repr}`")));
                        }

                        children.push(self.require_is(
                            &typed_left.node.ty,
                            Type::bool(),
                            left_label_span,
                            left_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        children.push(self.require_is(
                            &typed_right.node.ty,
                            Type::bool(),
                            right_label_span,
                            right_label_span,
                            TypeErrorKind::BinaryOperandMismatch {
                                op: *op,
                                left: self.apply(&typed_left.node.ty),
                                right: self.apply(&typed_right.node.ty),
                            },
                        )?);
                        Type::bool()
                    }
                };

                let typed = Spanned::new(
                    TypedExpr::new(
                        expr_type.clone(),
                        TypedExprKind::Binary {
                            op: *op,
                            left: Box::new(typed_left),
                            right: Box::new(typed_right),
                        },
                    ),
                    expr.span,
                );

                let rule = match op {
                    BinaryOp::Add => "T-Bin-Add",
                    BinaryOp::Subtract => "T-Bin-Sub",
                    BinaryOp::Multiply => "T-Bin-Mul",
                    BinaryOp::Divide => "T-Bin-Div",
                    BinaryOp::Remainder => "T-Bin-Rem",
                    BinaryOp::Less => "T-Bin-Lt",
                    BinaryOp::LessEqual => "T-Bin-Le",
                    BinaryOp::Greater => "T-Bin-Gt",
                    BinaryOp::GreaterEqual => "T-Bin-Ge",
                    BinaryOp::Equal => "T-Bin-Eq",
                    BinaryOp::NotEqual => "T-Bin-Neq",
                    BinaryOp::And => "T-Bin-And",
                    BinaryOp::Or => "T-Bin-Or",
                };

                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree = self.make_trace(
                    rule,
                    env_context.clone(),
                    subject_repr.clone(),
                    result_repr,
                    || children,
                );

                Ok((typed, tree))
            }
            Expr::Grouping(inner) => {
                let (typed_inner, inner_trace) = self.infer_expr(env, inner)?;
                let typed = Spanned::new(
                    TypedExpr::new(
                        typed_inner.node.ty.clone(),
                        TypedExprKind::Grouping(Box::new(typed_inner.clone())),
                    ),
                    expr.span,
                );
                let result_repr = self.maybe_string(|| self.format_type(&typed.node.ty));
                let tree =
                    self.make_trace("T-Group", env_context, subject_repr, result_repr, || {
                        vec![inner_trace]
                    });
                Ok((typed, tree))
            }
        }
    }

    fn type_from_annotation(&self, annotation: &TypeAnnotation) -> Result<Type, TypeError> {
        if !annotation.generics.is_empty() {
            return Err(TypeError::new(
                TypeErrorKind::UnsupportedTypeFeature {
                    description: "generic type parameters are not supported yet".to_string(),
                },
                annotation.name.span.unwrap_or_else(dummy_span),
            )
            .with_help("remove `<...>` until generics are implemented"));
        }

        match annotation.name.name.as_str() {
            "i64" => Ok(Type::int()),
            "bool" => Ok(Type::bool()),
            "()" => Ok(Type::unit()),
            other => Err(TypeError::new(
                TypeErrorKind::UnknownTypeAnnotation {
                    name: other.to_string(),
                },
                annotation.name.span.unwrap_or_else(dummy_span),
            )),
        }
    }

    /// Ensure a type is boolean, emitting a trace entry describing the check.
    fn require_bool(
        &mut self,
        ty: &Type,
        span: Span,
        context: ConditionContext,
    ) -> Result<InferenceTree, TypeError> {
        self.require_is(
            ty,
            Type::bool(),
            span,
            span,
            TypeErrorKind::ConditionNotBool {
                context,
                found: self.apply(ty),
            },
        )
    }

    /// Unify two types, capturing the resulting unification step in the trace.
    fn require_is(
        &mut self,
        actual: &Type,
        expected: Type,
        left_span: Span,
        right_span: Span,
        kind: TypeErrorKind,
    ) -> Result<InferenceTree, TypeError> {
        let (_, tree) = self.unify(actual.clone(), expected, left_span, right_span, kind)?;
        Ok(tree)
    }

    /// Ensure comparison operands are either both integers or both booleans.
    fn require_numeric_or_bool_pair(
        &mut self,
        op: &BinaryOp,
        left: &Spanned<TypedExpr>,
        left_span: Span,
        right: &Spanned<TypedExpr>,
        right_span: Span,
    ) -> Result<InferenceTree, TypeError> {
        let left_ty = self.apply(&left.node.ty);
        let right_ty = self.apply(&right.node.ty);

        if matches!(&left_ty, Type::Int) && matches!(&right_ty, Type::Int) {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok (int)".to_string());
            Ok(self.make_trace("Check-Compare", None, subject, result, Vec::new))
        } else if matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool) {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok (bool)".to_string());
            Ok(self.make_trace("Check-Compare", None, subject, result, Vec::new))
        } else {
            Err(TypeError::new(
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: left_ty.clone(),
                    right: right_ty.clone(),
                },
                left_span,
            )
            .with_primary_message(format!("has type `{left_ty}`"))
            .with_secondary(right_span, format!("has type `{right_ty}`")))
        }
    }

    /// Ensure equality operands share the same primitive type.
    fn require_same_primitive(
        &mut self,
        op: &BinaryOp,
        left: &Spanned<TypedExpr>,
        left_span: Span,
        right: &Spanned<TypedExpr>,
        right_span: Span,
    ) -> Result<InferenceTree, TypeError> {
        let left_ty = self.apply(&left.node.ty);
        let right_ty = self.apply(&right.node.ty);

        if (matches!(&left_ty, Type::Int) && matches!(&right_ty, Type::Int))
            || (matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool))
        {
            let subject = self.maybe_string(|| {
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                )
            });
            let result = self.maybe_string(|| "ok".to_string());
            Ok(self.make_trace("Check-Eq", None, subject, result, Vec::new))
        } else {
            Err(TypeError::new(
                TypeErrorKind::BinaryOperandMismatch {
                    op: *op,
                    left: left_ty.clone(),
                    right: right_ty.clone(),
                },
                left_span,
            )
            .with_primary_message(format!("has type `{left_ty}`"))
            .with_secondary(right_span, format!("has type `{right_ty}`")))
        }
    }

    /// Fully normalize a type by applying the current substitution set.
    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => match self.subst.get(id) {
                Some(ty) => self.apply(ty),
                None => Type::Var(*id),
            },
            Type::Function(params, result) => Type::Function(
                params.iter().map(|t| self.apply(t)).collect(),
                Box::new(self.apply(result)),
            ),
            Type::Tuple(items) => Type::Tuple(items.iter().map(|t| self.apply(t)).collect()),
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::Unit => Type::Unit,
        }
    }

    /// Rewrite every definition in the program with normalized types.
    fn apply_substitutions_program(&self, program: &mut TypedProgram) {
        for definition in &mut program.definitions {
            self.apply_definition(definition);
        }
    }

    fn apply_definition(&self, definition: &mut TypedDefinition) {
        match definition {
            TypedDefinition::Function(function) => self.apply_function(function),
        }
    }

    fn apply_function(&self, function: &mut TypedFunctionDef) {
        function.return_type = self.apply(&function.return_type);
        for param in &mut function.params {
            param.ty = self.apply(&param.ty);
        }
        self.apply_block(&mut function.body);
    }

    /// Visit a single statement and normalize any embedded type annotations.
    fn apply_statement(&self, statement: &mut TypedStatement) {
        match statement {
            TypedStatement::VariableDeclaration { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::Assignment { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::If {
                branches,
                else_branch,
            } => {
                for (condition, then_branch) in branches {
                    self.apply_expr(condition);
                    self.apply_block(then_branch);
                }
                if let Some(block) = else_branch {
                    self.apply_block(block);
                }
            }
            TypedStatement::While { condition, body } => {
                self.apply_expr(condition);
                self.apply_block(body);
            }
            TypedStatement::Block(block) => self.apply_block(block),
            TypedStatement::Expression(expr) => self.apply_expr(expr),
            TypedStatement::Return(Some(expr)) => self.apply_expr(expr),
            TypedStatement::Return(None) => {}
        }
    }

    /// Visit each statement inside a block and normalize its annotations.
    fn apply_block(&self, block: &mut TypedBlock) {
        for statement in &mut block.statements {
            self.apply_statement(statement);
        }
        if let Some(expr) = &mut block.tail_expression {
            self.apply_expr(expr);
        }
    }

    /// Normalize the type attached to an expression and recursively visit its children.
    fn apply_expr(&self, expr: &mut Spanned<TypedExpr>) {
        expr.node.ty = self.apply(&expr.node.ty);
        match &mut expr.node.kind {
            TypedExprKind::Literal(_) | TypedExprKind::Identifier(_) => {}
            TypedExprKind::Unary { expr: inner, .. } => self.apply_expr(inner),
            TypedExprKind::Binary { left, right, .. } => {
                self.apply_expr(left);
                self.apply_expr(right);
            }
            TypedExprKind::Grouping(inner) => self.apply_expr(inner),
        }
    }

    /// Quantify over all type variables that are free in `ty` but not in the environment.
    fn generalize(&self, env: &TypeEnv, ty: &Type) -> Scheme {
        let applied = self.apply(ty);
        let mut ty_free = free_type_vars_type(&applied);
        let env_free = env.free_type_vars();
        ty_free.retain(|var| !env_free.contains(var));
        let mut vars: Vec<_> = ty_free.into_iter().collect();
        vars.sort();
        Scheme { vars, ty: applied }
    }

    /// Replace every quantified variable in `scheme` with a fresh type variable.
    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut mapping = HashMap::new();
        for var in &scheme.vars {
            mapping.insert(*var, self.fresh_var());
        }
        substitute_type(&scheme.ty, &mapping)
    }

    /// Allocate a new inference variable unique to this inferencer.
    fn fresh_var(&mut self) -> Type {
        let id = TypeVarId(self.next_type_var);
        self.next_type_var += 1;
        Type::Var(id)
    }

    /// Render the current environment snapshot into a deterministic string.
    fn format_env(&self, env: &TypeEnv) -> String {
        let snapshot = env.snapshot();
        if snapshot.is_empty() {
            "{}".to_string()
        } else {
            let entries = snapshot
                .iter()
                .map(|(name, scheme)| format!("{name}: {scheme}"))
                .collect::<Vec<_>>();
            format!("{{{}}}", entries.join(", "))
        }
    }

    /// Pretty-print a statement for inclusion in the inference trace.
    fn format_statement_src(&self, statement: &Statement) -> String {
        formatter::statement(statement)
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|_| format!("{statement:?}"))
    }

    /// Pretty-print an expression for inclusion in the inference trace.
    fn format_expr_src(&self, expr: &Spanned<Expr>) -> String {
        formatter::expression(&expr.node)
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|_| format!("{:?}", expr.node))
    }

    /// Compute a string lazily, only when capture_traces is enabled.
    fn maybe_string<F>(&self, f: F) -> Option<String>
    where
        F: FnOnce() -> String,
    {
        if self.capture_traces { Some(f()) } else { None }
    }

    /// Assemble an inference tree node when tracing is active; otherwise return the default node.
    fn make_trace<C>(
        &self,
        rule: &str,
        context: Option<String>,
        subject: Option<String>,
        result: Option<String>,
        children: C,
    ) -> InferenceTree
    where
        C: FnOnce() -> Vec<InferenceTree>,
    {
        if !self.capture_traces {
            return InferenceTree::default();
        }

        let context = context.unwrap_or_default();
        let subject = subject.unwrap_or_default();
        let result = result.unwrap_or_default();
        let mut tree = InferenceTree::new(rule, context, subject, result);
        let kids = children();
        if !kids.is_empty() {
            tree = tree.with_children(kids);
        }
        tree
    }

    /// Produce a span sized to the formatted expression, avoiding trailing whitespace.
    fn label_span_for_expr(&self, expr: &Spanned<Expr>) -> Span {
        let formatted = self.format_expr_src(expr);
        let max_len = expr.span.end.saturating_sub(expr.span.start);
        let new_end = expr.span.start + formatted.len().min(max_len);
        Span {
            start: expr.span.start,
            end: new_end,
            context: expr.span.context,
        }
    }

    /// Short helper to format a type using existing `Display` impls.
    fn format_type(&self, ty: &Type) -> String {
        format!("{ty}")
    }

    /// Format the difference between the stored substitution map and a prior snapshot.
    fn format_subst_diff(&self, before: &HashMap<TypeVarId, Type>) -> String {
        let mut entries = Vec::new();
        for (key, value) in &self.subst {
            if before.get(key) != Some(value) {
                entries.push(format!("{}/{}", self.format_type(value), key.as_str()));
            }
        }
        entries.sort();
        format!("{{{}}}", entries.join(", "))
    }

    /// Unify two types, updating the substitution set and returning a trace node.
    fn unify(
        &mut self,
        left: Type,
        right: Type,
        left_span: Span,
        right_span: Span,
        error_kind: TypeErrorKind,
    ) -> Result<(Type, InferenceTree), TypeError> {
        let left = self.apply(&left);
        let right = self.apply(&right);
        let subject = self
            .maybe_string(|| format!("{} ~ {}", self.format_type(&left), self.format_type(&right)));
        let before = if self.capture_traces {
            Some(self.subst.clone())
        } else {
            None
        };

        let (result_ty, children, rule) = match (left.clone(), right.clone()) {
            (Type::Int, Type::Int) => (Type::Int, Vec::new(), "Unify-Const"),
            (Type::Bool, Type::Bool) => (Type::Bool, Vec::new(), "Unify-Const"),
            (Type::Unit, Type::Unit) => (Type::Unit, Vec::new(), "Unify-Const"),
            (Type::Tuple(ls), Type::Tuple(rs)) => {
                if ls.len() != rs.len() {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "type mismatch"));
                }

                let mut tuple_children = Vec::new();
                for (l, r) in ls.iter().zip(rs.iter()) {
                    let (_, child) = self.unify(
                        l.clone(),
                        r.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: l.clone(),
                            found: r.clone(),
                        },
                    )?;
                    tuple_children.push(child);
                }
                (Type::Tuple(ls), tuple_children, "Unify-Tuple")
            }
            (Type::Function(lp, lr), Type::Function(rp, rr)) => {
                if lp.len() != rp.len() {
                    return Err(TypeError::new(error_kind, left_span)
                        .with_secondary(right_span, "function arity mismatch"));
                }

                let mut arrow_children = Vec::new();
                for (l, r) in lp.iter().zip(rp.iter()) {
                    let (_, child) = self.unify(
                        l.clone(),
                        r.clone(),
                        left_span,
                        right_span,
                        TypeErrorKind::GeneralMismatch {
                            expected: l.clone(),
                            found: r.clone(),
                        },
                    )?;
                    arrow_children.push(child);
                }

                let (_, ret_child) = self.unify(
                    (*lr).clone(),
                    (*rr).clone(),
                    left_span,
                    right_span,
                    TypeErrorKind::GeneralMismatch {
                        expected: (*lr).clone(),
                        found: (*rr).clone(),
                    },
                )?;
                arrow_children.push(ret_child);

                (Type::Function(lp, lr), arrow_children, "Unify-Arrow")
            }
            (Type::Var(id), ty) => {
                self.bind(id, ty.clone(), left_span, right_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            (ty, Type::Var(id)) => {
                self.bind(id, ty.clone(), right_span, left_span, error_kind.clone())?;
                (ty, Vec::new(), "Unify-Var")
            }
            _ => {
                let left_repr = self.format_type(&left);
                let right_repr = self.format_type(&right);
                return Err(TypeError::new(error_kind, left_span)
                    .with_primary_message(format!("has type `{left_repr}`"))
                    .with_secondary(right_span, format!("has type `{right_repr}`")));
            }
        };

        let result_repr = if let Some(before) = before.as_ref() {
            self.maybe_string(|| self.format_subst_diff(before))
        } else {
            None
        };
        let tree = self.make_trace(rule, None, subject, result_repr, || children);
        Ok((result_ty, tree))
    }

    fn bind(
        &mut self,
        var: TypeVarId,
        ty: Type,
        var_span: Span,
        other_span: Span,
        kind: TypeErrorKind,
    ) -> Result<(), TypeError> {
        if ty == Type::Var(var) {
            return Ok(());
        }

        if occurs_in(var, &ty, &self.subst) {
            return Err(TypeError::new(kind, var_span)
                .with_secondary(other_span, "would create an infinite type"));
        }

        self.subst.insert(var, ty);
        Ok(())
    }
}

fn dummy_span() -> Span {
    Span {
        start: 0,
        end: 0,
        context: (),
    }
}

/// Recursively replace any variables mentioned in `mapping` within `ty`.
fn substitute_type(ty: &Type, mapping: &HashMap<TypeVarId, Type>) -> Type {
    match ty {
        Type::Var(id) => mapping.get(id).cloned().unwrap_or(Type::Var(*id)),
        Type::Function(params, result) => Type::Function(
            params
                .iter()
                .map(|ty| substitute_type(ty, mapping))
                .collect(),
            Box::new(substitute_type(result, mapping)),
        ),
        Type::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|ty| substitute_type(ty, mapping))
                .collect(),
        ),
        Type::Int => Type::Int,
        Type::Bool => Type::Bool,
        Type::Unit => Type::Unit,
    }
}

/// Return `true` if `var` appears anywhere inside `ty`, expanding substitutions as needed.
fn occurs_in(var: TypeVarId, ty: &Type, subst: &HashMap<TypeVarId, Type>) -> bool {
    match ty {
        Type::Var(id) => {
            if id == &var {
                true
            } else {
                subst
                    .get(id)
                    .map(|ty| occurs_in(var, ty, subst))
                    .unwrap_or(false)
            }
        }
        Type::Function(params, result) => {
            params.iter().any(|t| occurs_in(var, t, subst)) || occurs_in(var, result, subst)
        }
        Type::Tuple(items) => items.iter().any(|t| occurs_in(var, t, subst)),
        Type::Int | Type::Bool | Type::Unit => false,
    }
}

/// Collect all free type variables present in `ty`.
pub(crate) fn free_type_vars_type(ty: &Type) -> HashSet<TypeVarId> {
    let mut set = HashSet::new();
    collect_free_type_vars(ty, &mut set);
    set
}

/// Helper for `free_type_vars_type` that walks the type tree.
fn collect_free_type_vars(ty: &Type, set: &mut HashSet<TypeVarId>) {
    match ty {
        Type::Var(id) => {
            set.insert(*id);
        }
        Type::Function(params, result) => {
            for ty in params {
                collect_free_type_vars(ty, set);
            }
            collect_free_type_vars(result, set);
        }
        Type::Tuple(items) => {
            for ty in items {
                collect_free_type_vars(ty, set);
            }
        }
        Type::Int | Type::Bool | Type::Unit => {}
    }
}
