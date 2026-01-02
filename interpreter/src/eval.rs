use std::{cell::RefCell, rc::Rc};

use crate::{
    LoxVm, require_no_cf,
    types::{
        Callable, Closure, Value,
        control_flow::{ControlFlow, cf_err, no_cf},
    },
};
use ast::{
    BinaryExpr, BinaryOperator, Block, Call, Decl, Expr, For, LiteralExpression, LogicalExpr,
    LogicalOperator, Stmt, UnaryExpr, UnaryOperator,
};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum EvalError<'s> {
    #[error("Invalid use of {kind} in {context}")]
    InvalidCF { kind: &'s str, context: &'s str },
    #[error("Identifier {0} is not defined")]
    UndefinedIdent(String),
    #[error("Invalid unary operation {op} on {val}")]
    InvalidUnary { val: Value<'s>, op: UnaryOperator },
    #[error("Cannot compare {lhs} and {rhs}")]
    Incomparable { lhs: Value<'s>, rhs: Value<'s> },
    #[error("Invalid binary operation {lhs} {op} {rhs}")]
    InvalidBinary {
        lhs: Value<'s>,
        op: BinaryOperator,
        rhs: Value<'s>,
    },
    #[error("Invalid argument for callee {callee}. Expected {expected}, but got {got}")]
    InvalidArgument {
        callee: Callable<'s>,
        expected: &'static str,
        got: Value<'s>,
    },
    #[error(
        "Invalid number of arguments for callee {callee}, expected {arity}, but got {count} arguments. The arguments were {args:?}",
        arity = callee.arity().unwrap(),
        count = args.len()
    )]
    InvalidArgumentCount {
        callee: Callable<'s>,
        args: Vec<Value<'s>>,
    },
    #[error("Invalid callee {0}")]
    InvalidCallee(Value<'s>),
    #[error("Negation operation -{0} would overflow")]
    NegOverflow(i64),
    #[error("{lhs} {op} {rhs} would overflow")]
    BinaryOverflow {
        lhs: Value<'s>,
        op: BinaryOperator,
        rhs: Value<'s>,
    },
    #[error("Attempt to perform nil {0} {1}")]
    BinaryWithNilLhs(BinaryOperator, Value<'s>),
    #[error("Attempt to perform {1} {0} nil")]
    BinaryWithNilRhs(BinaryOperator, Value<'s>),
}

pub type EvalResult<'s> = ControlFlow<Value<'s>, Option<Value<'s>>, EvalError<'s>>;

pub trait Eval<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s>;
}

impl<'s> Eval<'s> for Decl<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        use Decl::*;
        match self {
            Stmt(s) => s.eval(lox),
            VarDecl { name, init } => {
                let init = require_no_cf!(init.eval(lox));
                lox.env.define(name, init);
                no_cf(Value::default())
            }
            Fun(fun) => {
                let captured = lox.env.current_scope().clone();
                let callable = Value::Callable(Callable::Closure(Closure {
                    fun: Rc::clone(fun),
                    environment: Rc::new(RefCell::new(captured)),
                }));
                lox.env.define(fun.name, callable);
                no_cf(Value::default())
            }
        }
    }
}

impl<'s> Eval<'s> for Stmt<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        use Stmt::*;
        match self {
            Expr(v) => {
                require_no_cf!(v.eval(lox));
                no_cf(Value::default())
            }
            Block(block) => block.eval(lox),
            If {
                cond,
                then_body,
                else_body,
            } => {
                let cond = require_no_cf!(cond.eval(lox));
                if cond.truthy() {
                    then_body.eval(lox)
                } else {
                    let Some(else_body) = else_body else {
                        return no_cf(Value::default());
                    };
                    else_body.eval(lox)
                }
            }
            While { cond, body } => {
                while {
                    let cond = require_no_cf!(cond.eval(lox));
                    cond.truthy()
                } {
                    match body.eval(lox) {
                        ControlFlow::Break => break,
                        ControlFlow::Value(_) | ControlFlow::Continue => (),
                        cf @ (ControlFlow::Error(_) | ControlFlow::Return(_)) => return cf,
                    }
                }
                no_cf(Value::default())
            }
            For(f) => f.eval(lox),
            Return(None) => ControlFlow::Return(None),
            Return(Some(ret)) => {
                let ret = require_no_cf!(ret.eval(lox));
                ControlFlow::Return(Some(ret))
            }
            Break => ControlFlow::Break,
            Continue => ControlFlow::Continue,
        }
    }
}

impl<'s> Eval<'s> for Expr<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        use Expr::*;
        match self {
            Ident(name) => {
                let Some(val) = lox.env.get(name) else {
                    return cf_err(EvalError::UndefinedIdent(name.to_string()));
                };
                no_cf(val.clone())
            }
            Lit(lit) => lit.eval(lox),
            Unary(unary) => unary.eval(lox),
            Grouped(group) => group.eval(lox),
            Binary(bin) => bin.eval(lox),
            Logical(logical) => logical.eval(lox),
            Assignment(assignment) => {
                let ast::Assignment { target, val } = &**assignment;
                let val = require_no_cf!(val.eval(lox));
                require_no_cf!(result lox.env.assign(target, val.clone()));
                no_cf(val)
            }
            Call(call) => call.eval(lox),
        }
    }
}

impl<'s> Eval<'s> for For<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        // TODO: Handle Break, Continue
        let Self {
            initializer,
            cond,
            increment,
            body,
        } = self;
        let _scope = lox.env.scope_guard();
        require_no_cf!(initializer.eval(lox));
        let cond = cond
            .as_ref()
            .unwrap_or(&Expr::Lit(LiteralExpression::Bool(true)));
        while {
            let cond = require_no_cf!(cond.eval(lox));
            cond.truthy()
        } {
            match body.eval(lox) {
                ControlFlow::Break => break,
                ControlFlow::Value(_) | ControlFlow::Continue => (),
                cf @ (ControlFlow::Error(_) | ControlFlow::Return(_)) => return cf,
            }
            require_no_cf!(increment.eval(lox));
        }
        no_cf(Value::default())
    }
}

impl<'s> Eval<'s> for Block<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        let _scope = lox.env.scope_guard();
        let mut error = None;
        for stmt in &self.0 {
            match stmt.eval(lox) {
                ControlFlow::Error(err) => {
                    error = Some(err);
                    break;
                }
                other => {
                    require_no_cf!(other);
                }
            }
        }
        if let Some(err) = error {
            return cf_err(err);
        }
        no_cf(Value::default())
    }
}

impl<'s> Eval<'s> for BinaryExpr<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        use Value::*;
        let Self { lhs, op, rhs } = self;
        let op = *op;
        let lhs = require_no_cf!(lhs.eval(lox));
        let rhs = require_no_cf!(rhs.eval(lox));
        no_cf(match op {
            BinaryOperator::Add => return (lhs + rhs).into(),
            BinaryOperator::Sub => return (lhs - rhs).into(),
            BinaryOperator::Mul => return (lhs * rhs).into(),
            BinaryOperator::Div => return (lhs / rhs).into(),
            BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Le | BinaryOperator::Ge => {
                use std::cmp::Ordering;
                Bool(match (op, lhs.partial_cmp(&rhs)) {
                    (_, None) => return cf_err(EvalError::Incomparable { lhs, rhs }),
                    (BinaryOperator::Gt | BinaryOperator::Ge, Some(Ordering::Greater)) => true,
                    (BinaryOperator::Lt | BinaryOperator::Le, Some(Ordering::Less)) => true,
                    (BinaryOperator::Ge | BinaryOperator::Le, Some(Ordering::Equal)) => true,
                    _ => false,
                })
            }
            BinaryOperator::Eq => Bool(lhs == rhs),
            BinaryOperator::Ne => Bool(lhs != rhs),
        })
    }
}

impl<'s> Eval<'s> for LogicalExpr<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        let Self { lhs, op, rhs } = self;
        let op = *op;
        let lhs = require_no_cf!(lhs.eval(lox));
        no_cf(match op {
            LogicalOperator::And if lhs.truthy() => require_no_cf!(rhs.eval(lox)),
            LogicalOperator::And => lhs,
            LogicalOperator::Or if lhs.truthy() => lhs,
            LogicalOperator::Or => require_no_cf!(rhs.eval(lox)),
        })
    }
}

impl<'s> Eval<'s> for Call<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        let Self { callee, arguments } = self;
        let mut callee = match require_no_cf!(callee.eval(lox)) {
            Value::Callable(callee) => callee,
            other => return cf_err(EvalError::InvalidCallee(other)),
        };
        let mut arg_values = Vec::new();
        for argument in arguments {
            let val = require_no_cf!(argument.eval(lox));
            arg_values.push(val);
        }
        if let Some(arity) = callee.arity()
            && arg_values.len() != arity
        {
            return cf_err(EvalError::InvalidArgumentCount {
                callee,
                args: arg_values,
            });
        }
        callee.call(lox, &arg_values)
    }
}

impl<'s> Eval<'s> for UnaryExpr<'s> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        use Value::*;
        let UnaryExpr { op, val } = self;
        let op = *op;
        let val = require_no_cf!(val.eval(lox));
        no_cf(match op {
            UnaryOperator::Not => match val {
                Integer(n) => Integer(!n),
                Bool(b) => Bool(!b),
                Nil => Bool(true),
                _ => return cf_err(EvalError::InvalidUnary { val, op }),
            },
            UnaryOperator::Neg => match val {
                Nil | Bool(false) => Integer(0),
                Integer(n) => {
                    let Some(n) = n.checked_neg() else {
                        return cf_err(EvalError::NegOverflow(n));
                    };
                    Integer(n)
                }
                Bool(true) => Integer(-1),
                Float(f) => Float(-f),
                _ => return cf_err(EvalError::InvalidUnary { val, op }),
            },
        })
    }
}

impl<'s> Eval<'s> for LiteralExpression<'s> {
    fn eval(&self, _: &mut LoxVm<'s>) -> EvalResult<'s> {
        use LiteralExpression::*;
        no_cf(match self {
            Nil => Value::Nil,
            &Bool(b) => Value::Bool(b),
            &Float(f) => Value::Float(f),
            // TODO: Proper char runtime types
            &Char(c) => Value::Integer(i64::from(u32::from(c))),
            &Int(n) => Value::Integer(n),
            Str(s) => Value::String(Rc::new(s.clone().into_owned())),
        })
    }
}

impl<'s, T: Eval<'s>> Eval<'s> for Option<T> {
    fn eval(&self, lox: &mut LoxVm<'s>) -> EvalResult<'s> {
        let Some(value) = self else {
            return no_cf(Value::default());
        };
        value.eval(lox)
    }
}
