//! Tree-walking interpreter for the Starstream DSL.

use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::{cell::Cell, collections::BTreeMap, rc::Rc};

use starstream_types::{
    BinaryOp, Identifier, Literal, TypedExpr, TypedExprKind, TypedProgram, TypedStatement, UnaryOp,
};

#[cfg(test)]
use starstream_types::{Spanned, Type};

// ----------------------------------------------------------------------------
// Tree walker

/// Execute a program.
pub fn exec_program(program: &TypedProgram) {
    eval_block(&program.statements, &Default::default());
}

fn eval_block(block: &[TypedStatement], locals: &Locals) -> Locals {
    let mut locals = locals.clone();
    for statement in block {
        match statement {
            TypedStatement::VariableDeclaration {
                mutable: _,
                name,
                value,
            } => {
                let value = eval(&value.node, &locals);
                locals
                    .vars
                    .insert(name.name.clone(), Rc::new(Cell::new(value)));
            }
            TypedStatement::Assignment { target, value } => {
                let value = eval(&value.node, &locals);
                locals.set(&target.name, value);
            }
            TypedStatement::Expression(expr) => {
                eval(&expr.node, &locals);
            }
            TypedStatement::Block(block) => {
                eval_block(&block.statements, &locals);
            }
            TypedStatement::If {
                branches,
                else_branch,
            } => {
                let mut else_ = true;
                for (condition, block) in branches {
                    if eval(&condition.node, &locals).to_bool() {
                        eval_block(&block.statements, &locals);
                        else_ = false;
                        break;
                    }
                }
                if else_ && let Some(else_branch) = else_branch {
                    eval_block(&else_branch.statements, &locals);
                }
            }
            TypedStatement::While { condition, body } => {
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
        TypedExprKind::Identifier(Identifier { name, .. }) => locals.get(name),
        // Literals
        TypedExprKind::Literal(Literal::Integer(i)) => Value::Number(*i),
        TypedExprKind::Literal(Literal::Boolean(b)) => Value::Boolean(*b),
        // Arithmetic operators
        TypedExprKind::Binary {
            op: BinaryOp::Add,
            left,
            right,
        } => eval(&left.node, locals) + eval(&right.node, locals),
        TypedExprKind::Binary {
            op: BinaryOp::Subtract,
            left,
            right,
        } => eval(&left.node, locals) - eval(&right.node, locals),
        TypedExprKind::Binary {
            op: BinaryOp::Multiply,
            left,
            right,
        } => eval(&left.node, locals) * eval(&right.node, locals),
        TypedExprKind::Binary {
            op: BinaryOp::Divide,
            left,
            right,
        } => eval(&left.node, locals) / eval(&right.node, locals),
        TypedExprKind::Binary {
            op: BinaryOp::Remainder,
            left,
            right,
        } => eval(&left.node, locals) % eval(&right.node, locals),
        TypedExprKind::Unary {
            op: UnaryOp::Negate,
            expr,
        } => -eval(&expr.node, locals),
        TypedExprKind::Unary {
            op: UnaryOp::Not,
            expr,
        } => !eval(&expr.node, locals),
        // TypedExprKind::BitNot(lhs) => eval(&lhs.node, locals).bitnot(),
        // TypedExprKind::BitAnd(lhs, rhs) => eval(&lhs.node, locals) & eval(&rhs.node, locals),
        // TypedExprKind::BitOr(lhs, rhs) => eval(&lhs.node, locals) | eval(&rhs.node, locals),
        // TypedExprKind::BitXor(lhs, rhs) => eval(&lhs.node, locals) ^ eval(&rhs.node, locals),
        // TypedExprKind::LShift(lhs, rhs) => eval(&lhs.node, locals) << eval(&rhs.node, locals),
        // TypedExprKind::RShift(lhs, rhs) => eval(&lhs.node, locals) >> eval(&rhs.node, locals),
        // Comparison operators
        TypedExprKind::Binary {
            op: BinaryOp::Equal,
            left,
            right,
        } => Value::from(eval(&left.node, locals) == eval(&right.node, locals)),
        TypedExprKind::Binary {
            op: BinaryOp::NotEqual,
            left,
            right,
        } => Value::from(eval(&left.node, locals) != eval(&right.node, locals)),
        TypedExprKind::Binary {
            op: BinaryOp::Less,
            left,
            right,
        } => Value::from(eval(&left.node, locals) < eval(&right.node, locals)),
        TypedExprKind::Binary {
            op: BinaryOp::Greater,
            left,
            right,
        } => Value::from(eval(&left.node, locals) > eval(&right.node, locals)),
        TypedExprKind::Binary {
            op: BinaryOp::LessEqual,
            left,
            right,
        } => Value::from(eval(&left.node, locals) <= eval(&right.node, locals)),
        TypedExprKind::Binary {
            op: BinaryOp::GreaterEqual,
            left,
            right,
        } => Value::from(eval(&left.node, locals) >= eval(&right.node, locals)),
        // Short-circuiting operators
        TypedExprKind::Binary {
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
        TypedExprKind::Binary {
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
        TypedExprKind::Grouping(expr) => eval(&expr.node, locals),
    }
}

#[test]
fn eval_math() {
    assert_eq!(
        eval(
            &TypedExpr::new(
                Type::Int,
                TypedExprKind::Binary {
                    op: BinaryOp::Add,
                    left: Box::new(Spanned::none(TypedExpr::new(
                        Type::Int,
                        TypedExprKind::Literal(Literal::Integer(17))
                    ))),
                    right: Box::new(Spanned::none(TypedExpr::new(
                        Type::Int,
                        TypedExprKind::Literal(Literal::Integer(33))
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
            TypedStatement::VariableDeclaration {
                mutable: true,
                name: Identifier::new("foo", None),
                value: Spanned::none(TypedExpr::new(
                    Type::Int,
                    TypedExprKind::Literal(Literal::Integer(6)),
                )),
            },
            TypedStatement::Assignment {
                target: Identifier::new("foo", None),
                value: Spanned::none(TypedExpr::new(
                    Type::Int,
                    TypedExprKind::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Spanned::none(TypedExpr::new(
                            Type::Int,
                            TypedExprKind::Identifier(Identifier::new("foo", None)),
                        ))),
                        right: Box::new(Spanned::none(TypedExpr::new(
                            Type::Int,
                            TypedExprKind::Literal(Literal::Integer(3)),
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

fn div_floor(lhs: i64, rhs: i64) -> i64 {
    // Based on core::num::int_macros::div_floor
    let d = lhs / rhs;
    let r = lhs % rhs;
    let correction = (lhs ^ rhs) >> (i64::BITS - 1);
    if r != 0 { d + correction } else { d }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(div_floor(lhs, rhs)),
            (lhs, rhs) => panic!("bad: {lhs:?} / {rhs:?}"),
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => {
                Value::Number(lhs - div_floor(lhs, rhs) * rhs)
            }
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

#[test]
fn divmod() {
    let lhs = Value::Number(3);
    let rhs = Value::Number(16);

    assert_eq!(lhs / rhs, Value::Number(0));
    assert_eq!(lhs % rhs, Value::Number(3));

    assert_eq!(-lhs / rhs, Value::Number(-1));
    assert_eq!(-lhs % rhs, Value::Number(13));

    assert_eq!(lhs / -rhs, Value::Number(-1));
    assert_eq!(lhs % -rhs, Value::Number(-13));

    assert_eq!(-lhs / -rhs, Value::Number(0));
    assert_eq!(-lhs % -rhs, Value::Number(-3));
}
