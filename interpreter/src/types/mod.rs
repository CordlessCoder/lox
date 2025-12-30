use std::{fmt::Display, rc::Rc, time::Instant};

use crate::{LoxVm, eval::EvalError};
mod conversions;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Callable {
    Print,
    Time,
}

impl Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Callable::*;
        let name = match self {
            Print => "print",
            Time => "time",
        };
        f.write_str(name)
    }
}

impl Callable {
    pub fn arity(&self) -> Option<usize> {
        use Callable::*;
        match self {
            Print => None,
            Time => Some(0),
        }
    }
    pub fn call(&self, _lox: &mut LoxVm<'_>, args: &[Value]) -> Result<Value, EvalError> {
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
                Ok(Value::Nil)
            }
            Time => Ok(Instant::now().into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Callable(Callable),
    Instant(Instant),
    String(Rc<String>),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

impl Display for Value {
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
