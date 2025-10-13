//! Tree-walking interpreter for the Starstream DSL.
#![allow(dead_code, unused)]

use std::ops::*;

use starstream_types::ast::*;

// ----------------------------------------------------------------------------
// Tree walker

fn eval_block(block: &Block) {
    for statement in &block.statements {
        match statement {
            Statement::Expression(expr) => {
                eval(&expr.node);
            }
            _ => todo!(),
        }
    }
}

/// Evaluate an expression.
pub fn eval(expr: &Expr /* , locals: &BTreeMap<String, Value>*/) -> Value {
    match expr {
        // Literals
        Expr::Literal(Literal::Integer(i)) => Value::Number(*i),
        Expr::Literal(Literal::Boolean(b)) => Value::Boolean(*b),
        // Arithmetic operators
        // Non-control-flow operators
        Expr::Binary {
            op: BinaryOp::Add,
            left,
            right,
        } => eval(&left.node) + eval(&right.node),
        Expr::Binary {
            op: BinaryOp::Subtract,
            left,
            right,
        } => eval(&left.node) - eval(&right.node),
        Expr::Binary {
            op: BinaryOp::Multiply,
            left,
            right,
        } => eval(&left.node) * eval(&right.node),
        Expr::Binary {
            op: BinaryOp::Divide,
            left,
            right,
        } => eval(&left.node) / eval(&right.node),
        Expr::Binary {
            op: BinaryOp::Remainder,
            left,
            right,
        } => eval(&left.node) % eval(&right.node),
        Expr::Unary {
            op: UnaryOp::Negate,
            expr,
        } => -eval(&expr.node),
        Expr::Unary {
            op: UnaryOp::Not,
            expr,
        } => !eval(&expr.node),
        // Expr::BitNot(lhs) => eval(&lhs.node).bitnot(),
        // Expr::BitAnd(lhs, rhs) => eval(&lhs.node) & eval(&rhs.node),
        // Expr::BitOr(lhs, rhs) => eval(&lhs.node) | eval(&rhs.node),
        // Expr::BitXor(lhs, rhs) => eval(&lhs.node) ^ eval(&rhs.node),
        // Expr::LShift(lhs, rhs) => eval(&lhs.node) << eval(&rhs.node),
        // Expr::RShift(lhs, rhs) => eval(&lhs.node) >> eval(&rhs.node),
        // Comparison operators
        Expr::Binary {
            op: BinaryOp::Equal,
            left,
            right,
        } => Value::from(eval(&left.node) == eval(&right.node)),
        Expr::Binary {
            op: BinaryOp::NotEqual,
            left,
            right,
        } => Value::from(eval(&left.node) != eval(&right.node)),
        Expr::Binary {
            op: BinaryOp::Less,
            left,
            right,
        } => Value::from(eval(&left.node) < eval(&right.node)),
        Expr::Binary {
            op: BinaryOp::Greater,
            left,
            right,
        } => Value::from(eval(&left.node) > eval(&right.node)),
        Expr::Binary {
            op: BinaryOp::LessEqual,
            left,
            right,
        } => Value::from(eval(&left.node) <= eval(&right.node)),
        Expr::Binary {
            op: BinaryOp::GreaterEqual,
            left,
            right,
        } => Value::from(eval(&left.node) >= eval(&right.node)),
        // Short-circuiting operators
        Expr::Binary {
            op: BinaryOp::And,
            left,
            right,
        } => {
            let left = eval(&left.node);
            if left.to_bool() {
                eval(&right.node)
            } else {
                left
            }
        }
        Expr::Binary {
            op: BinaryOp::Or,
            left,
            right,
        } => {
            let left = eval(&left.node);
            if left.to_bool() {
                left
            } else {
                eval(&right.node)
            }
        }
        // Nesting
        Expr::Grouping(expr) => eval(&expr.node),
        _ => todo!(),
    }
}

#[test]
fn eval_math() {
    assert_eq!(
        eval(&Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Spanned::none(Expr::Literal(Literal::Integer(17)))),
            right: Box::new(Spanned::none(Expr::Literal(Literal::Integer(33)))),
        }),
        Value::Number(50)
    );
}

// ----------------------------------------------------------------------------
// Value semantics

#[derive(Debug, Clone)]
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
        match (self) {
            Value::Number(lhs) => Value::Number(-lhs),
            lhs => panic!("bad: -{lhs:?}"),
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match (self) {
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
