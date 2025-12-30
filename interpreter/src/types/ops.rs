use ast::BinaryOperator;
use std::{
    fmt::Write,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

use crate::{eval::EvalError, types::Value};

fn try_int_float_casts<'s, R, E>(
    lhs: &Value<'s>,
    rhs: &Value<'s>,
    int_cb: impl FnOnce(i64, i64) -> R,
    float_cb: impl FnOnce(f64, f64) -> R,
    err: impl FnOnce() -> E,
) -> Result<R, E> {
    if let (Some(lhs), Some(rhs)) = (lhs.as_int(), rhs.as_int()) {
        return Ok(int_cb(lhs, rhs));
    }
    if let (Some(lhs), Some(rhs)) = (lhs.as_float(), rhs.as_float()) {
        return Ok(float_cb(lhs, rhs));
    }
    Err(err())
}

impl<'s> Add for Value<'s> {
    type Output = Result<Self, EvalError<'s>>;

    fn add(self, rhs: Self) -> Self::Output {
        use Value::*;
        let op = BinaryOperator::Add;
        Ok(match (self, rhs) {
            (Nil, Nil) => Nil,
            (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
            (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

            (Integer(lhs), Integer(rhs)) => {
                return lhs.checked_add(rhs).map(Integer).ok_or_else(|| {
                    EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    }
                });
            }
            (String(mut a), rhs) => {
                _ = write!(Rc::make_mut(&mut a), "{rhs}");
                String(a)
            }
            (lhs, String(b)) => String(format!("{lhs}{b}").into()),
            (Float(a), Float(b)) => Float(a + b),
            (Bool(a), Bool(b)) => Integer(i64::from(a) + i64::from(b)),

            (Float(f), Integer(i)) | (Integer(i), Float(f)) => Float(f + i as f64),
            (Integer(i), Bool(b)) | (Bool(b), Integer(i)) => Integer(i + i64::from(b)),

            (lhs, rhs) => {
                return Err(EvalError::InvalidBinary { lhs, op, rhs });
            }
        })
    }
}
impl<'s> Sub for Value<'s> {
    type Output = Result<Self, EvalError<'s>>;

    fn sub(self, rhs: Self) -> Self::Output {
        use Value::*;
        let op = BinaryOperator::Sub;
        Ok(match (self, rhs) {
            (Nil, Nil) => Nil,
            (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
            (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

            (Integer(lhs), Integer(rhs)) => {
                return lhs.checked_sub(rhs).map(Integer).ok_or_else(|| {
                    EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    }
                });
            }
            (Float(a), Float(b)) => Float(a - b),
            (Bool(a), Bool(b)) => Integer(i64::from(a) - i64::from(b)),

            (Float(lhs), Integer(rhs)) => Float(lhs - rhs as f64),
            (Integer(lhs), Float(rhs)) => Float(lhs as f64 - rhs),

            (lhs, rhs) => {
                return Err(EvalError::InvalidBinary { lhs, op, rhs });
            }
        })
    }
}
impl<'s> Mul for Value<'s> {
    type Output = Result<Self, EvalError<'s>>;

    fn mul(self, rhs: Self) -> Self::Output {
        use Value::*;
        let op = BinaryOperator::Mul;
        Ok(match (self, rhs) {
            (Nil, Nil) => Nil,
            (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
            (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

            (Integer(lhs), Integer(rhs)) => {
                return lhs.checked_mul(rhs).map(Integer).ok_or_else(|| {
                    EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    }
                });
            }

            (Float(a), Float(b)) => Float(a * b),
            (Bool(a), Bool(b)) => Integer(i64::from(a) * i64::from(b)),

            (Float(lhs), Integer(rhs)) => Float(lhs * rhs as f64),
            (Integer(lhs), Float(rhs)) => Float(lhs as f64 * rhs),

            (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
        })
    }
}
impl<'s> Div for Value<'s> {
    type Output = Result<Self, EvalError<'s>>;

    fn div(self, rhs: Self) -> Self::Output {
        use Value::*;
        let op = BinaryOperator::Div;
        Ok(match (self, rhs) {
            (Nil, Nil) => Nil,
            (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
            (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

            (Integer(lhs), Integer(rhs)) => {
                return lhs.checked_div(rhs).map(Integer).ok_or_else(|| {
                    EvalError::BinaryOverflow {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                    }
                });
            }

            (Float(a), Float(b)) => Float(a / b),

            (Float(lhs), Integer(rhs)) => Float(lhs / rhs as f64),
            (Integer(lhs), Float(rhs)) => Float(lhs as f64 / rhs),

            (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
        })
    }
}
impl PartialOrd for Value<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Instant(a), Value::Instant(b)) => return a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => return a.partial_cmp(b),
            (Value::Callable(a), Value::Callable(b)) => return a.partial_cmp(b),
            (Value::Bool(a), Value::Bool(b)) => return a.partial_cmp(b),
            (Value::Integer(a), Value::Integer(b)) => return a.partial_cmp(b),
            (Value::Float(a), Value::Float(b)) => return a.partial_cmp(b),
            _ => (),
        }
        try_int_float_casts(
            self,
            other,
            |a, b| a.partial_cmp(&b),
            |a, b| a.partial_cmp(&b),
            || (),
        )
        .ok()
        .flatten()
    }
}

//     BinaryOperator::Lt => {
//         return try_int_float_casts(lhs, op, rhs!(), |a, b| a < b, |a, b| a < b)
//             .map(Bool)
//             .into();
//     }
//     BinaryOperator::Gt => {
//         return try_int_float_casts(lhs, op, rhs!(), |a, b| a > b, |a, b| a > b)
//             .map(Bool)
//             .into();
//     }
//     BinaryOperator::Le => {
//         return try_int_float_casts(lhs, op, rhs!(), |a, b| a <= b, |a, b| a <= b)
//             .map(Bool)
//             .into();
//     }
//     BinaryOperator::Ge => {
//         return try_int_float_casts(lhs, op, rhs!(), |a, b| a >= b, |a, b| a >= b)
//             .map(Bool)
//             .into();
//     }
//     BinaryOperator::Eq => Bool(lhs == rhs!()),
//     BinaryOperator::Ne => Bool(lhs != rhs!()),
//     BinaryOperator::And if lhs.truthy() => rhs!(),
//     BinaryOperator::And => lhs,
//     BinaryOperator::Or if lhs.truthy() => lhs,
//     BinaryOperator::Or => rhs!(),
// })
