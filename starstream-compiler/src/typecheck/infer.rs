#![allow(clippy::result_large_err)]

use std::collections::{HashMap, HashSet};

use starstream_types::{
    Scheme, Span, Spanned, Type, TypeVarId,
    ast::{BinaryOp, Block, Expr, Identifier, Literal, Program, Statement, UnaryOp},
    typed_ast::{TypedBlock, TypedExpr, TypedExprKind, TypedProgram, TypedStatement},
};

use super::{
    env::TypeEnv,
    errors::{ConditionContext, TypeError, TypeErrorKind},
    tree::InferenceTree,
};
use crate::formatter;

/// Optional settings that control type-checker behaviour.
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
    let mut inferencer = Inferencer::new();
    let mut env = TypeEnv::new();
    let mut typed_statements = Vec::with_capacity(program.statements.len());
    let mut statement_traces = Vec::with_capacity(program.statements.len());
    let mut errors = Vec::new();

    for statement in &program.statements {
        match inferencer.infer_statement(&mut env, statement) {
            Ok((typed_statement, trace)) => {
                typed_statements.push(typed_statement);
                statement_traces.push(trace);
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

    let mut typed_program = TypedProgram::new(typed_statements);
    inferencer.apply_substitutions_program(&mut typed_program);

    let traces = if options.capture_traces {
        statement_traces
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
    next_type_var: u32,
    subst: HashMap<TypeVarId, Type>,
}

impl Inferencer {
    /// Construct a fresh inferencer with an empty substitution environment.
    fn new() -> Self {
        Self {
            next_type_var: 0,
            subst: HashMap::new(),
        }
    }

    /// Type-check a single statement, yielding its typed form and an inference trace.
    fn infer_statement(
        &mut self,
        env: &mut TypeEnv,
        statement: &Statement,
    ) -> Result<(TypedStatement, InferenceTree), TypeError> {
        let env_str = self.format_env(env);
        let stmt_repr = self.format_statement_src(statement);
        match statement {
            Statement::VariableDeclaration { name, value } => {
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
                env.insert(name.name.clone(), scheme);

                let children = vec![value_trace];
                let tree =
                    InferenceTree::new("T-Let", env_str, stmt_repr, self.format_type(&value_type))
                        .with_children(children);

                Ok((
                    TypedStatement::VariableDeclaration {
                        name: name.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::Assignment { target, value } => {
                let scheme = env.get(&target.name).cloned().ok_or_else(|| {
                    TypeError::new(
                        TypeErrorKind::UnknownVariable {
                            name: target.name.clone(),
                        },
                        value.span,
                    )
                })?;

                let expected_type = self.instantiate(&scheme);
                let (typed_value, value_trace) = self.infer_expr(env, value)?;
                let actual_type = typed_value.node.ty.clone();

                let (_, unify_trace) = self.unify(
                    actual_type.clone(),
                    expected_type.clone(),
                    value.span,
                    target
                        .span
                        .unwrap_or_else(|| Span::from(value.span.start..value.span.end)),
                    TypeErrorKind::AssignmentMismatch {
                        name: target.name.clone(),
                        expected: self.apply(&expected_type),
                        found: self.apply(&actual_type),
                    },
                )?;

                let children = vec![value_trace, unify_trace];
                let tree = InferenceTree::new(
                    "T-Assign",
                    env_str,
                    stmt_repr,
                    self.format_type(&expected_type),
                )
                .with_children(children);

                Ok((
                    TypedStatement::Assignment {
                        target: target.clone(),
                        value: typed_value,
                    },
                    tree,
                ))
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (typed_condition, cond_trace) = self.infer_expr(env, condition)?;
                let bool_check = self.require_bool(
                    &typed_condition.node.ty,
                    condition.span,
                    ConditionContext::If,
                )?;

                let (typed_then, then_traces) = self.infer_block(env, then_branch)?;
                let typed_else = match else_branch {
                    Some(block) => {
                        let (typed_block, else_traces) = self.infer_block(env, block)?;
                        Some((typed_block, else_traces))
                    }
                    None => None,
                };

                let mut children = vec![cond_trace, bool_check];
                children.extend(then_traces);

                let typed_else_block = if let Some((block, traces)) = typed_else {
                    children.extend(traces);
                    Some(block)
                } else {
                    None
                };

                let typed_then_block = typed_then;

                Ok((
                    TypedStatement::If {
                        condition: typed_condition,
                        then_branch: typed_then_block,
                        else_branch: typed_else_block,
                    },
                    InferenceTree::new("T-If", env_str, stmt_repr, "()").with_children(children),
                ))
            }
            Statement::While { condition, body } => {
                let (typed_condition, cond_trace) = self.infer_expr(env, condition)?;
                let bool_check = self.require_bool(
                    &typed_condition.node.ty,
                    condition.span,
                    ConditionContext::While,
                )?;

                let (typed_body, body_traces) = self.infer_block(env, body)?;

                let mut children = vec![cond_trace, bool_check];
                children.extend(body_traces);

                Ok((
                    TypedStatement::While {
                        condition: typed_condition,
                        body: typed_body,
                    },
                    InferenceTree::new("T-While", env_str, stmt_repr, "()").with_children(children),
                ))
            }
            Statement::Block(block) => {
                let (typed_block, block_traces) = self.infer_block(env, block)?;
                Ok((
                    TypedStatement::Block(typed_block),
                    InferenceTree::new("T-Block", env_str, stmt_repr, "()")
                        .with_children(block_traces),
                ))
            }
            Statement::Expression(expr) => {
                let (typed_expr, expr_trace) = self.infer_expr(env, expr)?;
                Ok((
                    TypedStatement::Expression(typed_expr),
                    InferenceTree::new("T-Expr", env_str, stmt_repr, "()")
                        .with_children(vec![expr_trace]),
                ))
            }
        }
    }

    /// Type-check a block, returning typed statements plus per-statement traces.
    fn infer_block(
        &mut self,
        env: &mut TypeEnv,
        block: &Block,
    ) -> Result<(TypedBlock, Vec<InferenceTree>), TypeError> {
        env.push_scope();
        let mut typed_statements = Vec::with_capacity(block.statements.len());
        let mut traces = Vec::with_capacity(block.statements.len());
        for statement in &block.statements {
            let (typed, trace) = self.infer_statement(env, statement)?;
            typed_statements.push(typed);
            traces.push(trace);
        }
        env.pop_scope();
        Ok((TypedBlock::new(typed_statements), traces))
    }

    /// Type-check an expression, returning the typed node and corresponding trace tree.
    fn infer_expr(
        &mut self,
        env: &mut TypeEnv,
        expr: &Spanned<Expr>,
    ) -> Result<(Spanned<TypedExpr>, InferenceTree), TypeError> {
        let env_str = self.format_env(env);
        let subject = self.format_expr_src(expr);
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
                let tree = InferenceTree::new(rule, env_str, subject, self.format_type(&ty));
                Ok((typed, tree))
            }
            Expr::Identifier(Identifier { name, span }) => {
                let scheme = env.get(name).cloned().ok_or_else(|| {
                    let span = span.unwrap_or(expr.span);
                    TypeError::new(TypeErrorKind::UnknownVariable { name: name.clone() }, span)
                })?;
                let ty = self.instantiate(&scheme);
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
                let tree = InferenceTree::new("T-Var", env_str, subject, self.format_type(&ty));
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

                let tree =
                    InferenceTree::new(rule, env_str, subject, self.format_type(&typed.node.ty))
                        .with_children(vec![inner_trace, check]);

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

                let tree =
                    InferenceTree::new(rule, env_str, subject, self.format_type(&typed.node.ty))
                        .with_children(children);

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
                let tree = InferenceTree::new(
                    "T-Group",
                    env_str,
                    subject,
                    self.format_type(&typed.node.ty),
                )
                .with_children(vec![inner_trace]);
                Ok((typed, tree))
            }
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
            Ok(InferenceTree::new(
                "Check-Compare",
                String::new(),
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                ),
                "ok (int)".to_string(),
            ))
        } else if matches!(&left_ty, Type::Bool) && matches!(&right_ty, Type::Bool) {
            Ok(InferenceTree::new(
                "Check-Compare",
                String::new(),
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                ),
                "ok (bool)".to_string(),
            ))
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
            Ok(InferenceTree::new(
                "Check-Eq",
                String::new(),
                format!(
                    "{} vs {}",
                    self.format_type(&left_ty),
                    self.format_type(&right_ty)
                ),
                "ok".to_string(),
            ))
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

    fn apply_substitutions_program(&self, program: &mut TypedProgram) {
        for statement in &mut program.statements {
            self.apply_statement(statement);
        }
    }

    fn apply_statement(&self, statement: &mut TypedStatement) {
        match statement {
            TypedStatement::VariableDeclaration { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::Assignment { value, .. } => {
                self.apply_expr(value);
            }
            TypedStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.apply_expr(condition);
                self.apply_block(then_branch);
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
        }
    }

    fn apply_block(&self, block: &mut TypedBlock) {
        for statement in &mut block.statements {
            self.apply_statement(statement);
        }
    }

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

    fn generalize(&self, env: &TypeEnv, ty: &Type) -> Scheme {
        let applied = self.apply(ty);
        let mut ty_free = free_type_vars_type(&applied);
        let env_free = env.free_type_vars();
        ty_free.retain(|var| !env_free.contains(var));
        let mut vars: Vec<_> = ty_free.into_iter().collect();
        vars.sort();
        Scheme { vars, ty: applied }
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        let mut mapping = HashMap::new();
        for var in &scheme.vars {
            mapping.insert(*var, self.fresh_var());
        }
        substitute_type(&scheme.ty, &mapping)
    }

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
        let subject = format!("{} ~ {}", self.format_type(&left), self.format_type(&right));
        let before = self.subst.clone();

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

        let subst_str = self.format_subst_diff(&before);
        let tree =
            InferenceTree::new(rule, String::new(), subject, subst_str).with_children(children);
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

pub(crate) fn free_type_vars_type(ty: &Type) -> HashSet<TypeVarId> {
    let mut set = HashSet::new();
    collect_free_type_vars(ty, &mut set);
    set
}

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
