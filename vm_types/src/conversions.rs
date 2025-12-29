use super::Value;

impl Value {
    #[must_use]
    pub const fn as_bool(&self) -> Option<bool> {
        use Value::*;
        Some(match self {
            &Bool(b) => b,
            &Integer(n) => n != 0,
            &Float(n) => n != 0.0,
            Nil => false,
            String(s) => !s.is_empty(),
        })
    }
    #[must_use]
    pub fn as_int(&self) -> Option<i64> {
        use Value::*;
        Some(match self {
            &Bool(b) => i64::from(b),
            &Integer(n) => n,
            Nil => 0,
            _ => return None,
        })
    }
    #[must_use]
    pub const fn as_float(&self) -> Option<f64> {
        use Value::*;
        Some(match self {
            &Bool(true) => 1.0,
            Nil | &Bool(false) => 0.0,
            &Integer(n) => n as f64,
            &Float(n) => n,
            String(_) => return None,
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

impl<T: Into<Self>> From<Option<T>> for Value {
    fn from(value: Option<T>) -> Self {
        value.map_or(Self::Nil, Into::into)
    }
}
