use std::fmt::Display;
mod conversions;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            String(s) => s.fmt(f),
            Integer(n) => n.fmt(f),
            Float(n) => n.fmt(f),
            Bool(n) => n.fmt(f),
            Nil => f.write_str("nil"),
        }
    }
}
