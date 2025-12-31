use std::{fmt::Display, rc::Rc, time::Instant};

use crate::{
    LoxVm,
    environment::Scope,
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
    Closure(Closure<'s>),
}

#[derive(Debug, Clone)]
pub struct Closure<'s> {
    pub fun: Rc<ast::Fun<'s>>,
    pub environment: Scope<'s>,
}

impl PartialEq for Closure<'_> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.fun, &other.fun) && self.environment == other.environment
    }
}

impl PartialEq for Callable<'_> {
    fn eq(&self, other: &Self) -> bool {
        use Callable::*;
        match (self, other) {
            (Print, Print) | (Now, Now) | (Elapsed, Elapsed) => true,
            (Closure(a), Closure(b)) => a == b,
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
            (Closure(a), Closure(b)) => return a.fun.name.partial_cmp(b.fun.name),
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
            Closure(closure) => return write!(f, "fun {}", closure.fun.name),
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
            Closure(closure) => Some(closure.fun.parameters.len()),
        }
    }
    pub fn call(&mut self, lox: &mut LoxVm<'s>, args: &[Value<'s>]) -> EvalResult<'s> {
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
            Closure(closure) => {
                assert_eq!(args.len(), closure.fun.parameters.len());
                // TODO; Do we need to inject the closure into itself?
                // TODO: Proper name resolution pass to get rid of runtime environments
                // This is where lox shows its runtime reference semantics, that I don't like.
                // Next step: My own language, and proper compilation?
                let callable = Callable::Closure(closure.clone());
                let crate::types::Closure { fun, environment } = closure;
                let _scope = lox.env.replace_guard(environment);
                let _scope = lox.env.scope_guard();
                // Make sure the function can resolve to itself
                lox.env.define(fun.name, Value::Callable(callable));
                for (parameter, argument) in closure.fun.parameters.iter().zip(args) {
                    lox.env.define(parameter, argument.clone());
                }
                match closure.fun.body.eval(lox) {
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
