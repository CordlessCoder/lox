use thiserror::Error;

use crate::{interpreter::Lox, types::Value};

use super::expr::{BinaryExpr, BinaryOperator, Expr, Literal, UnaryExpr, UnaryOperator};

#[derive(Debug, Clone, Error)]
pub enum EvalError {
    #[error("Invalid unary operation {operator} on {val}")]
    InvalidUnary { val: Value, operator: UnaryOperator },
    #[error("Invalid binary operation {lhs} {operator} {rhs}")]
    InvalidBinary {
        lhs: Value,
        operator: BinaryOperator,
        rhs: Value,
    },
    #[error("Negation operation -{0} would overflow")]
    NegOverflow(i64),
    #[error("{lhs} {operator} {rhs} would overflow")]
    BinaryOverflow {
        lhs: Value,
        operator: BinaryOperator,
        rhs: Value,
    },
    #[error("Attempt to perform nil {0} {1}")]
    BinaryWithNilRhs(BinaryOperator, Value),
    #[error("Attempt to perform {1} {0} nil")]
    BinaryWithNilLhs(BinaryOperator, Value),
}

pub trait Eval {
    fn eval(self, lox: &mut Lox) -> Result<Value, EvalError>;
}

impl Eval for Expr {
    fn eval(self, lox: &mut Lox) -> Result<Value, EvalError> {
        use Expr::*;
        match self {
            Literal(lit) => lit.eval(lox),
            Unary(unary) => unary.eval(lox),
            Grouped(group) => group.eval(lox),
            Binary(bin) => bin.eval(lox),
        }
    }
}

impl Eval for BinaryExpr {
    fn eval(self, lox: &mut Lox) -> Result<Value, EvalError> {
        fn try_int_float_casts<R>(
            lhs: Value,
            operator: BinaryOperator,
            rhs: Value,
            int_cb: impl FnOnce(i64, i64) -> R,
            float_cb: impl FnOnce(f64, f64) -> R,
        ) -> Result<R, EvalError> {
            if let (Some(lhs), Some(rhs)) = (lhs.as_int(), rhs.as_int()) {
                return Ok(int_cb(lhs, rhs));
            }
            if let (Some(lhs), Some(rhs)) = (lhs.as_float(), rhs.as_float()) {
                return Ok(float_cb(lhs, rhs));
            }
            Err(EvalError::InvalidBinary { lhs, operator, rhs })
        }
        use Value::*;
        let Self {
            left,
            operator,
            right,
        } = self;
        let lhs = left.eval(lox)?;
        let rhs = right.eval(lox)?;
        Ok(match operator {
            BinaryOperator::Plus => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(operator, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(operator, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_add(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        operator,
                        rhs: rhs.into(),
                    })?)
                }
                (String(a), String(b)) => String(a + &b),
                (Float(a), Float(b)) => Float(a + b),
                (Bool(a), Bool(b)) => Integer(a as i64 + b as i64),

                (Float(f), Integer(i)) | (Integer(i), Float(f)) => Float(f + i as f64),
                (Integer(i), Bool(b)) | (Bool(b), Integer(i)) => Integer(i + b as i64),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, operator, rhs }),
            },
            BinaryOperator::Minus => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(operator, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(operator, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_sub(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        operator,
                        rhs: rhs.into(),
                    })?)
                }
                (Float(a), Float(b)) => Float(a - b),
                (Bool(a), Bool(b)) => Integer(a as i64 - b as i64),

                (Float(lhs), Integer(rhs)) => Float(lhs - rhs as f64),
                (Integer(lhs), Float(rhs)) => Float(lhs as f64 - rhs),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, operator, rhs }),
            },
            BinaryOperator::Mul => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(operator, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(operator, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_mul(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        operator,
                        rhs: rhs.into(),
                    })?)
                }

                (Float(a), Float(b)) => Float(a * b),
                (Bool(a), Bool(b)) => Integer(a as i64 * b as i64),

                (Float(lhs), Integer(rhs)) => Float(lhs * rhs as f64),
                (Integer(lhs), Float(rhs)) => Float(lhs as f64 * rhs),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, operator, rhs }),
            },
            BinaryOperator::Div => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(operator, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(operator, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_div(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        operator,
                        rhs: rhs.into(),
                    })?)
                }

                (Float(a), Float(b)) => Float(a / b),

                (Float(lhs), Integer(rhs)) => Float(lhs / rhs as f64),
                (Integer(lhs), Float(rhs)) => Float(lhs as f64 / rhs),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, operator, rhs }),
            },
            BinaryOperator::LessThan => Bool(try_int_float_casts(
                lhs,
                operator,
                rhs,
                |a, b| a < b,
                |a, b| a < b,
            )?),
            BinaryOperator::GreaterThan => Bool(try_int_float_casts(
                lhs,
                operator,
                rhs,
                |a, b| a > b,
                |a, b| a > b,
            )?),
            BinaryOperator::LessThanOrEqual => Bool(try_int_float_casts(
                lhs,
                operator,
                rhs,
                |a, b| a <= b,
                |a, b| a <= b,
            )?),
            BinaryOperator::GreaterThanOrEqual => Bool(try_int_float_casts(
                lhs,
                operator,
                rhs,
                |a, b| a >= b,
                |a, b| a >= b,
            )?),
            BinaryOperator::Equal => Bool(lhs == rhs),
            BinaryOperator::NotEqual => Bool(lhs != rhs),
        })
    }
}

impl Eval for UnaryExpr {
    fn eval(self, lox: &mut Lox) -> Result<Value, EvalError> {
        let UnaryExpr { operator, expr } = self;
        use Value::*;
        let val = expr.eval(lox)?;
        Ok(match operator {
            UnaryOperator::Not => match val {
                Integer(n) => Integer(!n),
                Bool(b) => Bool(!b),
                Nil => Bool(true),
                _ => return Err(EvalError::InvalidUnary { val, operator }),
            },
            UnaryOperator::Minus => match val {
                Nil => Integer(0),
                Integer(n) => {
                    let Some(n) = n.checked_neg() else {
                        return Err(EvalError::NegOverflow(n));
                    };
                    Integer(n)
                }
                Bool(true) => Integer(-1),
                Bool(false) => Integer(0),
                Float(f) => Float(-f),
                _ => return Err(EvalError::InvalidUnary { val, operator }),
            },
        })
    }
}

impl Eval for Literal {
    fn eval(self, _: &mut Lox) -> Result<Value, EvalError> {
        Ok(match self {
            Literal::Nil => Value::Nil,
            Literal::Bool(b) => Value::Bool(b),
            Literal::Float(f) => Value::Float(f),
            Literal::Integer(n) => Value::Integer(n),
            Literal::String(s) => Value::String(s),
        })
    }
}
