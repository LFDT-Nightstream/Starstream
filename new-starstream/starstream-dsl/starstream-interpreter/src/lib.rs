//! Tree-walking interpreter for the Starstream DSL.
#![allow(dead_code, unused)]

use std::ops::*;

use starstream_types::ast::*;

// ----------------------------------------------------------------------------
// Tree walker

/// Run a coordination script.
pub fn run_script(program: &StarstreamProgram, entry_point: &str, args: Vec<Value>) -> Value {
    todo!()
}

fn eval_block(block: &Block) -> Value {
    match block {
        Block::Close { semicolon: _ } => Value::None,
        Block::Chain { head, tail } => todo!(),
    }
}

/// Evaluate an expression.
pub fn eval(expr: &Expr /* , locals: &BTreeMap<String, Value>*/) -> Value {
    match expr {
        // Literals
        Expr::PrimaryExpr(fae) => eval_field_access(fae),
        // Arithmetic operators
        // Non-control-flow operators
        Expr::Add(lhs, rhs) => eval(&lhs.node) + eval(&rhs.node),
        Expr::Sub(lhs, rhs) => eval(&lhs.node) - eval(&rhs.node),
        Expr::Mul(lhs, rhs) => eval(&lhs.node) * eval(&rhs.node),
        Expr::Div(lhs, rhs) => eval(&lhs.node) / eval(&rhs.node),
        Expr::Mod(lhs, rhs) => eval(&lhs.node) % eval(&rhs.node),
        Expr::Neg(lhs) => -eval(&lhs.node),
        Expr::Not(lhs) => !eval(&lhs.node),
        Expr::BitNot(lhs) => eval(&lhs.node).bitnot(),
        Expr::BitAnd(lhs, rhs) => eval(&lhs.node) & eval(&rhs.node),
        Expr::BitOr(lhs, rhs) => eval(&lhs.node) | eval(&rhs.node),
        Expr::BitXor(lhs, rhs) => eval(&lhs.node) ^ eval(&rhs.node),
        Expr::LShift(lhs, rhs) => eval(&lhs.node) << eval(&rhs.node),
        Expr::RShift(lhs, rhs) => eval(&lhs.node) >> eval(&rhs.node),
        // Comparison operators
        Expr::Equals(lhs, rhs) => Value::from(eval(&lhs.node) == eval(&rhs.node)),
        Expr::NotEquals(lhs, rhs) => Value::from(eval(&lhs.node) != eval(&rhs.node)),
        Expr::LessThan(lhs, rhs) => Value::from(eval(&lhs.node) < eval(&rhs.node)),
        Expr::GreaterThan(lhs, rhs) => Value::from(eval(&lhs.node) > eval(&rhs.node)),
        Expr::LessEq(lhs, rhs) => Value::from(eval(&lhs.node) <= eval(&rhs.node)),
        Expr::GreaterEq(lhs, rhs) => Value::from(eval(&lhs.node) >= eval(&rhs.node)),
        // Short-circuiting operators
        Expr::And(lhs, rhs) => {
            let left = eval(&lhs.node);
            if left.to_bool() {
                eval(&rhs.node)
            } else {
                left
            }
        }
        Expr::Or(lhs, rhs) => {
            let left = eval(&lhs.node);
            if left.to_bool() {
                left
            } else {
                eval(&rhs.node)
            }
        }
        // Nesting
        Expr::BlockExpr(BlockExpr::Block(block)) => eval_block(block),
        Expr::BlockExpr(BlockExpr::IfThenElse(cond, if_true, if_false)) => {
            if eval(&cond.node).to_bool() {
                eval_block(if_true)
            } else if let Some(if_false) = if_false {
                eval_block(if_false)
            } else {
                Value::None
            }
        }
        _ => todo!(),
    }
}

#[test]
fn eval_math() {
    assert_eq!(
        eval(&Expr::Add(
            Box::new(Spanned::none(Expr::PrimaryExpr(
                FieldAccessExpression::PrimaryExpr(PrimaryExpr::Number {
                    literal: 17,
                    ty: None
                })
            ))),
            Box::new(Spanned::none(Expr::PrimaryExpr(
                FieldAccessExpression::PrimaryExpr(PrimaryExpr::Number {
                    literal: 33,
                    ty: None
                })
            ))),
        )),
        Value::Number(50)
    );
}

fn eval_field_access(expr: &FieldAccessExpression) -> Value {
    match expr {
        FieldAccessExpression::PrimaryExpr(PrimaryExpr::Number { literal, ty: _ }) => {
            Value::from(*literal)
        }
        FieldAccessExpression::PrimaryExpr(PrimaryExpr::Bool(bool)) => Value::from(*bool),
        FieldAccessExpression::PrimaryExpr(_) => todo!(),
        FieldAccessExpression::FieldAccess { base, field } => {
            let lhs = eval_field_access(base);
            todo!()
        }
    }
}

// ----------------------------------------------------------------------------
// Value semantics

#[derive(Debug, Clone)]
pub enum Value {
    None,
    // Primitive values
    Number(i32),
    Boolean(bool),
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Number(i32::try_from(value).unwrap())
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
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
