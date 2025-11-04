//! Tree-walking interpreter for the Starstream DSL.

use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::{cell::Cell, collections::BTreeMap, rc::Rc};

use starstream_types::{
    BinaryOp, Identifier, Literal, TypedExpr, TypedExprKind as Expr, TypedProgram as Program,
    TypedStatement as Statement, UnaryOp,
};

#[cfg(test)]
use starstream_types::{Spanned, Type};

// ----------------------------------------------------------------------------
// Tree walker

/// Execute a program.
pub fn exec_program(program: &Program) {
    eval_block(&program.statements, &Default::default());
}

fn eval_block(block: &[Statement], locals: &Locals) -> Locals {
    let mut locals = locals.clone();
    for statement in block {
        match statement {
            Statement::VariableDeclaration { name, value } => {
                let value = eval(&value.node, &locals);
                locals
                    .vars
                    .insert(name.name.clone(), Rc::new(Cell::new(value)));
            }
            Statement::Assignment { target, value } => {
                let value = eval(&value.node, &locals);
                locals.set(&target.name, value);
            }
            Statement::Expression(expr) => {
                eval(&expr.node, &locals);
            }
            Statement::Block(block) => {
                eval_block(&block.statements, &locals);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if eval(&condition.node, &locals).to_bool() {
                    eval_block(&then_branch.statements, &locals);
                } else if let Some(else_branch) = else_branch {
                    eval_block(&else_branch.statements, &locals);
                }
            }
            Statement::While { condition, body } => {
                while eval(&condition.node, &locals).to_bool() {
                    eval_block(&body.statements, &locals);
                }
            }
        }
    }
    locals
}

/// Evaluate an expression.
pub fn eval(expr: &TypedExpr, locals: &Locals) -> Value {
    match &expr.kind {
        // Identifiers
        Expr::Identifier(Identifier { name, .. }) => locals.get(name),
        // Literals
        Expr::Literal(Literal::Integer(i)) => Value::Number(*i),
        Expr::Literal(Literal::Boolean(b)) => Value::Boolean(*b),
        // Arithmetic operators
        Expr::Binary {
            op: BinaryOp::Add,
            left,
            right,
        } => eval(&left.node, locals) + eval(&right.node, locals),
        Expr::Binary {
            op: BinaryOp::Subtract,
            left,
            right,
        } => eval(&left.node, locals) - eval(&right.node, locals),
        Expr::Binary {
            op: BinaryOp::Multiply,
            left,
            right,
        } => eval(&left.node, locals) * eval(&right.node, locals),
        Expr::Binary {
            op: BinaryOp::Divide,
            left,
            right,
        } => eval(&left.node, locals) / eval(&right.node, locals),
        Expr::Binary {
            op: BinaryOp::Remainder,
            left,
            right,
        } => eval(&left.node, locals) % eval(&right.node, locals),
        Expr::Unary {
            op: UnaryOp::Negate,
            expr,
        } => -eval(&expr.node, locals),
        Expr::Unary {
            op: UnaryOp::Not,
            expr,
        } => !eval(&expr.node, locals),
        // Expr::BitNot(lhs) => eval(&lhs.node, locals).bitnot(),
        // Expr::BitAnd(lhs, rhs) => eval(&lhs.node, locals) & eval(&rhs.node, locals),
        // Expr::BitOr(lhs, rhs) => eval(&lhs.node, locals) | eval(&rhs.node, locals),
        // Expr::BitXor(lhs, rhs) => eval(&lhs.node, locals) ^ eval(&rhs.node, locals),
        // Expr::LShift(lhs, rhs) => eval(&lhs.node, locals) << eval(&rhs.node, locals),
        // Expr::RShift(lhs, rhs) => eval(&lhs.node, locals) >> eval(&rhs.node, locals),
        // Comparison operators
        Expr::Binary {
            op: BinaryOp::Equal,
            left,
            right,
        } => Value::from(eval(&left.node, locals) == eval(&right.node, locals)),
        Expr::Binary {
            op: BinaryOp::NotEqual,
            left,
            right,
        } => Value::from(eval(&left.node, locals) != eval(&right.node, locals)),
        Expr::Binary {
            op: BinaryOp::Less,
            left,
            right,
        } => Value::from(eval(&left.node, locals) < eval(&right.node, locals)),
        Expr::Binary {
            op: BinaryOp::Greater,
            left,
            right,
        } => Value::from(eval(&left.node, locals) > eval(&right.node, locals)),
        Expr::Binary {
            op: BinaryOp::LessEqual,
            left,
            right,
        } => Value::from(eval(&left.node, locals) <= eval(&right.node, locals)),
        Expr::Binary {
            op: BinaryOp::GreaterEqual,
            left,
            right,
        } => Value::from(eval(&left.node, locals) >= eval(&right.node, locals)),
        // Short-circuiting operators
        Expr::Binary {
            op: BinaryOp::And,
            left,
            right,
        } => {
            let left = eval(&left.node, locals);
            if left.to_bool() {
                eval(&right.node, locals)
            } else {
                left
            }
        }
        Expr::Binary {
            op: BinaryOp::Or,
            left,
            right,
        } => {
            let left = eval(&left.node, locals);
            if left.to_bool() {
                left
            } else {
                eval(&right.node, locals)
            }
        }
        // Nesting
        Expr::Grouping(expr) => eval(&expr.node, locals),
    }
}

#[test]
fn eval_math() {
    assert_eq!(
        eval(
            &TypedExpr::new(
                Type::Int,
                Expr::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Spanned::none(TypedExpr::new(
                        Type::Int,
                        Expr::Literal(Literal::Integer(17))
                    ))),
                    right: Box::new(Spanned::none(TypedExpr::new(
                        Type::Int,
                        Expr::Literal(Literal::Integer(33))
                    ))),
                },
            ),
            &Default::default()
        ),
        Value::Number(50)
    );
}

#[test]
fn eval_locals() {
    let locals = eval_block(
        &[
            Statement::VariableDeclaration {
                name: Identifier::new("foo", None),
                value: Spanned::none(TypedExpr::new(
                    Type::Int,
                    Expr::Literal(Literal::Integer(6)),
                )),
            },
            Statement::Assignment {
                target: Identifier::new("foo", None),
                value: Spanned::none(TypedExpr::new(
                    Type::Int,
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Spanned::none(TypedExpr::new(
                            Type::Int,
                            Expr::Identifier(Identifier::new("foo", None)),
                        ))),
                        right: Box::new(Spanned::none(TypedExpr::new(
                            Type::Int,
                            Expr::Literal(Literal::Integer(3)),
                        ))),
                    },
                )),
            },
        ],
        &Default::default(),
    );
    assert_eq!(locals.get("foo"), Value::Number(18));
}

// ----------------------------------------------------------------------------
// Variable semantics

#[derive(Debug, Default, Clone)]
pub struct Locals {
    vars: BTreeMap<String, Rc<Cell<Value>>>,
}

impl Locals {
    fn get(&self, name: &str) -> Value {
        if let Some(value) = self.vars.get(name) {
            value.get()
        } else {
            panic!("unknown variable {name:?}")
        }
    }

    fn set(&self, name: &str, value: Value) {
        if let Some(cell) = self.vars.get(name) {
            cell.set(value);
        } else {
            panic!("unknown variable {name:?}")
        }
    }
}

// ----------------------------------------------------------------------------
// Value semantics

#[derive(Debug, Clone, Copy)]
pub enum Value {
    None,
    // Primitive values
    Number(i64),
    Boolean(bool),
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Number(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match *self {
            Value::Boolean(v) => v,
            _ => panic!("Not convertible to boolean: {self:?}"),
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} + {rhs:?}"),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs - rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} - {rhs:?}"),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs * rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} * {rhs:?}"),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs / rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} / {rhs:?}"),
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs.rem_euclid(rhs)),
            (lhs, rhs) => panic!("bad: {lhs:?} % {rhs:?}"),
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(lhs) => Value::Number(-lhs),
            lhs => panic!("bad: -{lhs:?}"),
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(lhs) => Value::Boolean(!lhs),
            lhs => panic!("bad: !{lhs:?}"),
        }
    }
}

/*
impl Value {
    pub fn bitnot(self) -> Value {
        match (self) {
            Value::Number(lhs) => Value::Number(!lhs),
            lhs => panic!("bad: ~{lhs:?}"),
        }
    }
}

impl BitAnd for Value {
    type Output = Value;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Value::Boolean(lhs & rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs & rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} & {rhs:?}"),
        }
    }
}

impl BitOr for Value {
    type Output = Value;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Value::Boolean(lhs | rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs | rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} | {rhs:?}"),
        }
    }
}

impl BitXor for Value {
    type Output = Value;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs ^ rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} ^ {rhs:?}"),
        }
    }
}

impl Shl for Value {
    type Output = Value;

    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs << rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} << {rhs:?}"),
        }
    }
}

impl Shr for Value {
    type Output = Value;

    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs >> rhs),
            (lhs, rhs) => panic!("bad: {lhs:?} >> {rhs:?}"),
        }
    }
}
*/

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::None, Value::None) => true,
            (Value::Number(lhs), Self::Number(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Self::Boolean(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Some(lhs.cmp(rhs)),
            (Value::Boolean(lhs), Value::Boolean(rhs)) => Some(lhs.cmp(rhs)),
            _ => None,
        }
    }
}
