use crate::LoxVm;
use ast::{BinaryExpr, BinaryOperator, Expr, LiteralExpression, Stmt, UnaryExpr, UnaryOperator};
use thiserror::Error;
use vm_types::Value;

#[derive(Debug, Clone, Error)]
pub enum EvalError {
    #[error("Invalid unary operation {op:?} on {val:?}")]
    InvalidUnary { val: Value, op: UnaryOperator },
    #[error("Invalid binary operation {lhs:?} {op} {rhs:?}")]
    InvalidBinary {
        lhs: Value,
        op: BinaryOperator,
        rhs: Value,
    },
    #[error("Negation operation -{0:?} would overflow")]
    NegOverflow(i64),
    #[error("{lhs} {op} {rhs} would overflow")]
    BinaryOverflow {
        lhs: Value,
        op: BinaryOperator,
        rhs: Value,
    },
    #[error("Attempt to perform nil {0} {1:?}")]
    BinaryWithNilLhs(BinaryOperator, Value),
    #[error("Attempt to perform {1:?} {0} nil")]
    BinaryWithNilRhs(BinaryOperator, Value),
}

pub trait Eval {
    fn eval(self, lox: &mut LoxVm) -> Result<Value, EvalError>;
}

impl Eval for Stmt<'_> {
    fn eval(self, lox: &mut LoxVm) -> Result<Value, EvalError> {
        use Stmt::*;
        match self {
            Expr(v) => {
                v.eval(lox)?;
                Ok(Value::Nil)
            }
            Print { value } => {
                let val = value.eval(lox)?;
                println!("{val}");
                Ok(Value::Nil)
            }
            other => {
                unimplemented!("evaluation of {other:?} is not yet supported")
            }
        }
    }
}

impl Eval for Expr<'_> {
    fn eval(self, lox: &mut LoxVm) -> Result<Value, EvalError> {
        use Expr::*;
        match self {
            Ident(i) => unimplemented!(),
            Lit(lit) => lit.eval(lox),
            Unary(unary) => unary.eval(lox),
            Grouped(group) => group.eval(lox),
            Binary(bin) => bin.eval(lox),
        }
    }
}

impl Eval for BinaryExpr<'_> {
    fn eval(self, lox: &mut LoxVm) -> Result<Value, EvalError> {
        fn try_int_float_casts<R>(
            lhs: Value,
            op: BinaryOperator,
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
            Err(EvalError::InvalidBinary { lhs, op, rhs })
        }
        use Value::*;
        let Self { lhs, op, rhs } = self;
        let lhs = lhs.eval(lox)?;
        let rhs = rhs.eval(lox)?;
        Ok(match op {
            BinaryOperator::Add => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_add(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    })?)
                }
                (String(a), String(b)) => String(a + &b),
                (Float(a), Float(b)) => Float(a + b),
                (Bool(a), Bool(b)) => Integer(a as i64 + b as i64),

                (Float(f), Integer(i)) | (Integer(i), Float(f)) => Float(f + i as f64),
                (Integer(i), Bool(b)) | (Bool(b), Integer(i)) => Integer(i + b as i64),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
            },
            BinaryOperator::Sub => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_sub(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    })?)
                }
                (Float(a), Float(b)) => Float(a - b),
                (Bool(a), Bool(b)) => Integer(a as i64 - b as i64),

                (Float(lhs), Integer(rhs)) => Float(lhs - rhs as f64),
                (Integer(lhs), Float(rhs)) => Float(lhs as f64 - rhs),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
            },
            BinaryOperator::Mul => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_mul(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    })?)
                }

                (Float(a), Float(b)) => Float(a * b),
                (Bool(a), Bool(b)) => Integer(a as i64 * b as i64),

                (Float(lhs), Integer(rhs)) => Float(lhs * rhs as f64),
                (Integer(lhs), Float(rhs)) => Float(lhs as f64 * rhs),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
            },
            BinaryOperator::Div => match (lhs, rhs) {
                (Nil, Nil) => Nil,
                (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                (Integer(lhs), Integer(rhs)) => {
                    Integer(lhs.checked_div(rhs).ok_or(EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    })?)
                }

                (Float(a), Float(b)) => Float(a / b),

                (Float(lhs), Integer(rhs)) => Float(lhs / rhs as f64),
                (Integer(lhs), Float(rhs)) => Float(lhs as f64 / rhs),

                (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
            },
            BinaryOperator::Lt => Bool(try_int_float_casts(
                lhs,
                op,
                rhs,
                |a, b| a < b,
                |a, b| a < b,
            )?),
            BinaryOperator::Gt => Bool(try_int_float_casts(
                lhs,
                op,
                rhs,
                |a, b| a > b,
                |a, b| a > b,
            )?),
            BinaryOperator::Le => Bool(try_int_float_casts(
                lhs,
                op,
                rhs,
                |a, b| a <= b,
                |a, b| a <= b,
            )?),
            BinaryOperator::Ge => Bool(try_int_float_casts(
                lhs,
                op,
                rhs,
                |a, b| a >= b,
                |a, b| a >= b,
            )?),
            BinaryOperator::Eq => Bool(lhs == rhs),
            BinaryOperator::Ne => Bool(lhs != rhs),
            BinaryOperator::And => {
                Value::Bool(lhs.as_bool().unwrap_or_default() && rhs.as_bool().unwrap_or_default())
            }
            BinaryOperator::Or => {
                Value::Bool(lhs.as_bool().unwrap_or_default() || rhs.as_bool().unwrap_or_default())
            }
        })
    }
}

impl Eval for UnaryExpr<'_> {
    fn eval(self, lox: &mut LoxVm) -> Result<Value, EvalError> {
        let UnaryExpr { op, val } = self;
        use Value::*;
        let val = val.eval(lox)?;
        Ok(match op {
            UnaryOperator::Not => match val {
                Integer(n) => Integer(!n),
                Bool(b) => Bool(!b),
                Nil => Bool(true),
                _ => return Err(EvalError::InvalidUnary { val, op }),
            },
            UnaryOperator::Neg => match val {
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
                _ => return Err(EvalError::InvalidUnary { val, op }),
            },
        })
    }
}

impl Eval for LiteralExpression<'_> {
    fn eval(self, _: &mut LoxVm) -> Result<Value, EvalError> {
        use LiteralExpression::*;
        Ok(match self {
            Nil => Value::Nil,
            Bool(b) => Value::Bool(b),
            Float(f) => Value::Float(f),
            // TODO: Proper char runtime types
            Char(c) => Value::Integer(c as i64),
            Int(n) => Value::Integer(n),
            Str(s) => Value::String(s.into_owned()),
        })
    }
}
