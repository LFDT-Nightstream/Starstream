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
        // Non-control-flow operators
        Expr::Equals(lhs, rhs) => Value::from(eval(&lhs.node) == eval(&rhs.node)),
        Expr::NotEquals(lhs, rhs) => Value::from(eval(&lhs.node) != eval(&rhs.node)),
        Expr::LessThan(lhs, rhs) => Value::from(eval(&lhs.node) < eval(&rhs.node)),
        Expr::GreaterThan(lhs, rhs) => Value::from(eval(&lhs.node) > eval(&rhs.node)),
        Expr::LessEq(lhs, rhs) => Value::from(eval(&lhs.node) <= eval(&rhs.node)),
        Expr::GreaterEq(lhs, rhs) => Value::from(eval(&lhs.node) >= eval(&rhs.node)),
        Expr::Add(lhs, rhs) => eval(&lhs.node) + eval(&rhs.node),
        Expr::Sub(lhs, rhs) => eval(&lhs.node) - eval(&rhs.node),
        Expr::Mul(lhs, rhs) => eval(&lhs.node) * eval(&rhs.node),
        Expr::Div(lhs, rhs) => eval(&lhs.node) / eval(&rhs.node),
        Expr::Mod(lhs, rhs) => eval(&lhs.node) % eval(&rhs.node),
        Expr::Neg(lhs) => -eval(&lhs.node),
        Expr::BitNot(lhs) => eval(&lhs.node).bitnot(),
        Expr::BitAnd(lhs, rhs) => eval(&lhs.node) & eval(&rhs.node),
        Expr::BitOr(lhs, rhs) => eval(&lhs.node) | eval(&rhs.node),
        Expr::BitXor(lhs, rhs) => eval(&lhs.node) ^ eval(&rhs.node),
        Expr::LShift(lhs, rhs) => eval(&lhs.node) << eval(&rhs.node),
        Expr::RShift(lhs, rhs) => eval(&lhs.node) >> eval(&rhs.node),
        Expr::Not(lhs) => !eval(&lhs.node),
        // Control flow
        Expr::BlockExpr(BlockExpr::Block(block)) => eval_block(block),
        Expr::BlockExpr(BlockExpr::IfThenElse(cond, if_true, if_false)) => {
            if eval(&cond.node).to_bool() {
                eval_block(if_true)
            } else if let Some(if_false) = if_false {
                eval_block(if_false)
            } else {
                Value::None
            }
        },
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
        },
    }
}

fn eval_field_access(expr: &FieldAccessExpression) -> Value {
    match expr {
        FieldAccessExpression::PrimaryExpr(PrimaryExpr::Number { literal, ty: _ }) => Value::from(*literal),
        FieldAccessExpression::PrimaryExpr(PrimaryExpr::Bool(bool)) => Value::from(*bool),
        FieldAccessExpression::PrimaryExpr(_) => todo!(),
        FieldAccessExpression::FieldAccess { base, field } => {
            let lhs = eval_field_access(base);
            todo!()
        },
    }
}

// ----------------------------------------------------------------------------
// Value semantics

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    None,
    // Primitive values
    Number(i64),
    Boolean(bool),
    // Linear handle types
    Utxo(u64),
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Number(i64::from(value))
    }
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
        todo!()
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        todo!()
    }
}

impl Value {
    pub fn bitnot(self) -> Value {
        todo!()
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        todo!()
    }
}

impl BitAnd for Value {
    type Output = Value;

    fn bitand(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl BitOr for Value {
    type Output = Value;

    fn bitor(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl BitXor for Value {
    type Output = Value;

    fn bitxor(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Shl for Value {
    type Output = Value;

    fn shl(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Shr for Value {
    type Output = Value;

    fn shr(self, rhs: Self) -> Self::Output {
        todo!()
    }
}
