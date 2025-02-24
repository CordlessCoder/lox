use super::Value;

impl Value {
    pub fn as_bool(&self) -> Option<bool> {
        use Value::*;
        Some(match self {
            &Bool(b) => b,
            &Integer(n) => n != 0,
            &Float(n) => n != 0.0,
            Nil => false,
            _ => return None,
        })
    }
    pub fn as_int(&self) -> Option<i64> {
        use Value::*;
        Some(match self {
            &Bool(b) => b as i64,
            &Integer(n) => n,
            Nil => 0,
            _ => return None,
        })
    }
    pub fn as_float(&self) -> Option<f64> {
        use Value::*;
        Some(match self {
            &Bool(b) => b as u32 as f64,
            &Integer(n) => n as f64,
            &Float(n) => n,
            Nil => 0.0,
            _ => return None,
        })
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(value: Option<T>) -> Self {
        value.map(Into::into).unwrap_or(Self::Nil)
    }
}
