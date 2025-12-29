use crate::LoxVm;
use ast::{
    BinaryExpr, BinaryOperator, Block, Decl, Expr, For, LiteralExpression, Stmt, UnaryExpr,
    UnaryOperator,
};
use thiserror::Error;
use vm_types::Value;

#[derive(Debug, Clone, Error)]
pub enum EvalError {
    #[error("Identifier {0} is not defined")]
    UndefinedIdent(String),
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

pub trait Eval<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError>;
}

impl<'s> Eval<'s> for Decl<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
        use Decl::*;
        match self {
            Stmt(s) => s.eval(lox),
            VarDecl { name, init } => {
                let init = init.eval(lox)?;
                lox.env.define(name, init);
                Ok(Value::Nil)
            }
        }
    }
}

impl<'s> Eval<'s> for Stmt<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
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
            Block(block) => block.eval(lox),
            If {
                cond,
                then_body,
                else_body,
            } => {
                let cond = cond.eval(lox)?;
                if cond.as_bool().unwrap_or(false) {
                    then_body.eval(lox)
                } else {
                    let Some(else_body) = else_body else {
                        return Ok(Value::Nil);
                    };
                    else_body.eval(lox)
                }
            }
            While { cond, body } => {
                while cond.eval(lox)?.as_bool().unwrap_or_default() {
                    body.eval(lox)?;
                }
                Ok(Value::Nil)
            }
            For(f) => f.eval(lox),
            other => {
                unimplemented!("evaluation of {other:?} is not yet supported")
            }
        }
    }
}

impl<'s> Eval<'s> for Expr<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
        use Expr::*;
        match self {
            Ident(name) => {
                let Some(val) = lox.env.get(name) else {
                    return Err(EvalError::UndefinedIdent(name.to_string()));
                };
                Ok(val.clone())
            }
            Lit(lit) => lit.eval(lox),
            Unary(unary) => unary.eval(lox),
            Grouped(group) => group.eval(lox),
            Binary(bin) => bin.eval(lox),
            Assignment(assignment) => {
                let ast::Assignment { target, val } = &mut **assignment;
                let val = val.eval(lox)?;
                lox.env.assign(target, val.clone())?;
                Ok(val)
            }
        }
    }
}

impl<'s> Eval<'s> for For<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
        let Self {
            initializer,
            cond,
            increment,
            body,
        } = self;
        lox.env.push_scope();
        initializer.eval(lox)?;
        let Some(cond) = cond else {
            loop {
                body.eval(lox)?;
                increment.eval(lox)?;
            }
        };
        while cond.eval(lox)?.as_bool().unwrap_or_default() {
            body.eval(lox)?;
            increment.eval(lox)?;
        }
        lox.env.pop_scope();
        Ok(Value::Nil)
    }
}

impl<'s> Eval<'s> for Block<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
        lox.env.push_scope();
        let mut error = None;
        for stmt in &mut self.0 {
            if let Err(err) = stmt.eval(lox) {
                error = Some(err);
                break;
            };
        }
        lox.env.pop_scope();
        error.map_or_else(|| Ok(Value::Nil), Err)
    }
}

impl<'s> Eval<'s> for BinaryExpr<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
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
        let op = *op;
        let lhs = lhs.eval(lox)?;
        let mut rhs = || rhs.eval(lox);
        Ok(match op {
            BinaryOperator::Add => {
                match (lhs, rhs()?) {
                    (Nil, Nil) => Nil,
                    (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                    (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                    (Integer(lhs), Integer(rhs)) => Integer(lhs.checked_add(rhs).ok_or_else(
                        || EvalError::BinaryOverflow {
                            lhs: lhs.into(),
                            op,
                            rhs: rhs.into(),
                        },
                    )?),
                    (String(a), String(b)) => String(a + &b),
                    (Float(a), Float(b)) => Float(a + b),
                    (Bool(a), Bool(b)) => Integer(i64::from(a) + i64::from(b)),

                    (Float(f), Integer(i)) | (Integer(i), Float(f)) => Float(f + i as f64),
                    (Integer(i), Bool(b)) | (Bool(b), Integer(i)) => Integer(i + i64::from(b)),

                    (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
                }
            }
            BinaryOperator::Sub => {
                match (lhs, rhs()?) {
                    (Nil, Nil) => Nil,
                    (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                    (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                    (Integer(lhs), Integer(rhs)) => Integer(lhs.checked_sub(rhs).ok_or_else(
                        || EvalError::BinaryOverflow {
                            lhs: lhs.into(),
                            op,
                            rhs: rhs.into(),
                        },
                    )?),
                    (Float(a), Float(b)) => Float(a - b),
                    (Bool(a), Bool(b)) => Integer(i64::from(a) - i64::from(b)),

                    (Float(lhs), Integer(rhs)) => Float(lhs - rhs as f64),
                    (Integer(lhs), Float(rhs)) => Float(lhs as f64 - rhs),

                    (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
                }
            }
            BinaryOperator::Mul => {
                match (lhs, rhs()?) {
                    (Nil, Nil) => Nil,
                    (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                    (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                    (Integer(lhs), Integer(rhs)) => Integer(lhs.checked_mul(rhs).ok_or_else(
                        || EvalError::BinaryOverflow {
                            lhs: lhs.into(),
                            op,
                            rhs: rhs.into(),
                        },
                    )?),

                    (Float(a), Float(b)) => Float(a * b),
                    (Bool(a), Bool(b)) => Integer(i64::from(a) * i64::from(b)),

                    (Float(lhs), Integer(rhs)) => Float(lhs * rhs as f64),
                    (Integer(lhs), Float(rhs)) => Float(lhs as f64 * rhs),

                    (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
                }
            }
            BinaryOperator::Div => {
                match (lhs, rhs()?) {
                    (Nil, Nil) => Nil,
                    (Nil, right) => return Err(EvalError::BinaryWithNilLhs(op, right)),
                    (left, Nil) => return Err(EvalError::BinaryWithNilRhs(op, left)),

                    (Integer(lhs), Integer(rhs)) => Integer(lhs.checked_div(rhs).ok_or_else(
                        || EvalError::BinaryOverflow {
                            lhs: lhs.into(),
                            op,
                            rhs: rhs.into(),
                        },
                    )?),

                    (Float(a), Float(b)) => Float(a / b),

                    (Float(lhs), Integer(rhs)) => Float(lhs / rhs as f64),
                    (Integer(lhs), Float(rhs)) => Float(lhs as f64 / rhs),

                    (lhs, rhs) => return Err(EvalError::InvalidBinary { lhs, op, rhs }),
                }
            }
            BinaryOperator::Lt => Bool(try_int_float_casts(
                lhs,
                op,
                rhs()?,
                |a, b| a < b,
                |a, b| a < b,
            )?),
            BinaryOperator::Gt => Bool(try_int_float_casts(
                lhs,
                op,
                rhs()?,
                |a, b| a > b,
                |a, b| a > b,
            )?),
            BinaryOperator::Le => Bool(try_int_float_casts(
                lhs,
                op,
                rhs()?,
                |a, b| a <= b,
                |a, b| a <= b,
            )?),
            BinaryOperator::Ge => Bool(try_int_float_casts(
                lhs,
                op,
                rhs()?,
                |a, b| a >= b,
                |a, b| a >= b,
            )?),
            BinaryOperator::Eq => Bool(lhs == rhs()?),
            BinaryOperator::Ne => Bool(lhs != rhs()?),
            BinaryOperator::And if lhs.as_bool().unwrap_or_default() => rhs()?,
            BinaryOperator::And => lhs,
            BinaryOperator::Or if lhs.as_bool().unwrap_or_default() => lhs,
            BinaryOperator::Or => rhs()?,
        })
    }
}

impl<'s> Eval<'s> for UnaryExpr<'s> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
        use Value::*;
        let UnaryExpr { op, val } = self;
        let op = *op;
        let val = val.eval(lox)?;
        Ok(match op {
            UnaryOperator::Not => match val {
                Integer(n) => Integer(!n),
                Bool(b) => Bool(!b),
                Nil => Bool(true),
                _ => return Err(EvalError::InvalidUnary { val, op }),
            },
            UnaryOperator::Neg => match val {
                Nil | Bool(false) => Integer(0),
                Integer(n) => {
                    let Some(n) = n.checked_neg() else {
                        return Err(EvalError::NegOverflow(n));
                    };
                    Integer(n)
                }
                Bool(true) => Integer(-1),
                Float(f) => Float(-f),
                _ => return Err(EvalError::InvalidUnary { val, op }),
            },
        })
    }
}

impl<'s> Eval<'s> for LiteralExpression<'s> {
    fn eval(&mut self, _: &mut LoxVm<'s>) -> Result<Value, EvalError> {
        use LiteralExpression::*;
        Ok(match self {
            Nil => Value::Nil,
            &mut Bool(b) => Value::Bool(b),
            &mut Float(f) => Value::Float(f),
            // TODO: Proper char runtime types
            &mut Char(c) => Value::Integer(i64::from(u32::from(c))),
            &mut Int(n) => Value::Integer(n),
            Str(s) => Value::String(s.clone().into_owned()),
        })
    }
}

impl<'s, T: Eval<'s>> Eval<'s> for Option<T> {
    fn eval(&mut self, lox: &mut LoxVm<'s>) -> Result<Value, EvalError> {
        let Some(value) = self else {
            return Ok(Value::Nil);
        };
        value.eval(lox)
    }
}
