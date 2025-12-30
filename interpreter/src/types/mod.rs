use std::{fmt::Display, rc::Rc, time::Instant};

use crate::{
    LoxVm,
    eval::{Eval, EvalError, EvalResult},
    types::control_flow::{ControlFlow, cf_err, no_cf},
};
pub mod control_flow;
mod conversions;
mod ops;

#[derive(Debug, Clone)]
pub enum Callable<'s> {
    Print,
    Now,
    Elapsed,
    Fun(Rc<ast::Fun<'s>>),
}

impl PartialEq for Callable<'_> {
    fn eq(&self, other: &Self) -> bool {
        use Callable::*;
        match (self, other) {
            (Print, Print) | (Now, Now) | (Elapsed, Elapsed) => true,
            (Fun(a), Fun(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}
impl PartialOrd for Callable<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Callable::*;
        use std::cmp::Ordering;
        Some(match (self, other) {
            (Print, Print) | (Now, Now) | (Elapsed, Elapsed) => Ordering::Equal,
            (Fun(a), Fun(b)) => return a.name.partial_cmp(b.name),
            _ => return None,
        })
    }
}

impl Display for Callable<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Callable::*;
        let name = match self {
            Print => "print",
            Now => "now",
            Elapsed => "elapsed",
            Fun(fun) => return write!(f, "fun {}", fun.name),
        };
        f.write_str(name)
    }
}

impl<'s> Callable<'s> {
    pub fn arity(&self) -> Option<usize> {
        use Callable::*;
        match self {
            Print => None,
            Now => Some(0),
            Elapsed => Some(1),
            Fun(fun) => Some(fun.parameters.len()),
        }
    }
    pub fn call(&self, lox: &mut LoxVm<'s>, args: &[Value<'s>]) -> EvalResult<'s> {
        use Callable::*;
        match self {
            Print => {
                let mut args = args.iter();
                if let Some(first) = args.next() {
                    print!("{first}");
                }
                for arg in args {
                    print!(" {arg}");
                }
                println!();
                no_cf(Value::Nil)
            }
            Now => no_cf(Instant::now().into()),
            Elapsed => {
                let [arg] = args else { unreachable!() };
                let Value::Instant(time) = arg else {
                    return cf_err(EvalError::InvalidArgument {
                        callee: self.clone(),
                        expected: "an instant",
                        got: arg.clone(),
                    });
                };
                no_cf(time.elapsed().as_secs_f64().into())
            }
            Fun(fun) => {
                assert_eq!(args.len(), fun.parameters.len());
                let _scope = lox.env.scope_guard();
                for (parameter, argument) in fun.parameters.iter().zip(args) {
                    lox.env.define(parameter, argument.clone());
                }
                match fun.body.eval(lox) {
                    ControlFlow::Return(ret) => ControlFlow::Value(ret.unwrap_or_default()),
                    ControlFlow::Value(_) => ControlFlow::Value(Value::default()),
                    ControlFlow::Break => ControlFlow::Error(EvalError::InvalidCF {
                        kind: "break",
                        context: "without an enclosing loop",
                    }),
                    ControlFlow::Continue => ControlFlow::Error(EvalError::InvalidCF {
                        kind: "continue",
                        context: "without an enclosing loop",
                    }),
                    ControlFlow::Error(err) => ControlFlow::Error(err),
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Value<'s> {
    Callable(Callable<'s>),
    Instant(Instant),
    String(Rc<String>),
    Integer(i64),
    Float(f64),
    Bool(bool),
    #[default]
    Nil,
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Callable(_) => f.write_str("callable"),
            String(s) => s.fmt(f),
            Integer(n) => n.fmt(f),
            Float(n) => n.fmt(f),
            Bool(n) => n.fmt(f),
            Instant(_) => f.write_str("Instant"),
            Nil => f.write_str("nil"),
        }
    }
}
