#[must_use = "Ignoring control flow is likely a bug"]
pub enum ControlFlow<T, R, E> {
    Break,
    Continue,
    Return(R),
    Value(T),
    Error(E),
}

#[macro_export]
macro_rules! require_no_cf {
    (result $cf:expr) => {{
        use $crate::types::control_flow::ControlFlow;
        match $cf {
            Ok(val) => val,
            Err(err) => return ControlFlow::Error(err),
        }
    }};
    (erase $cf:expr) => {{
        use $crate::types::control_flow::ControlFlow;
        match $cf {
            ControlFlow::Value(val) => val,
            ControlFlow::Break => return ControlFlow::Break,
            ControlFlow::Continue => return ControlFlow::Continue,
            ControlFlow::Error(err) => return ControlFlow::Error(err),
            ControlFlow::Return(ret) => return ControlFlow::Return(ret),
        }
    }};
    ($cf:expr) => {{
        use $crate::types::control_flow::ControlFlow;
        match $cf {
            ControlFlow::Value(val) => val,
            other => return other,
        }
    }};
}

#[inline(always)]
pub const fn no_cf<T, R, E>(val: T) -> ControlFlow<T, R, E> {
    ControlFlow::Value(val)
}

#[inline(always)]
pub const fn cf_err<T, R, E>(err: E) -> ControlFlow<T, R, E> {
    ControlFlow::Error(err)
}

#[expect(unused)]
pub trait OptionCFExt<T> {
    fn ok_or_else_err<E, F, R>(self, err: F) -> ControlFlow<T, R, E>
    where
        F: FnOnce() -> E;
}
impl<T> OptionCFExt<T> for Option<T> {
    fn ok_or_else_err<E, F, R>(self, err: F) -> ControlFlow<T, R, E>
    where
        F: FnOnce() -> E,
    {
        self.map_or_else(|| ControlFlow::Error(err()), ControlFlow::Value)
    }
}

impl<T, R, E> From<Result<T, E>> for ControlFlow<T, R, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(val) => Self::Value(val),
            Err(err) => Self::Error(err),
        }
    }
}

impl<T, R, E> ControlFlow<T, R, E> {
    pub fn map<U>(self, apply: impl FnOnce(T) -> U) -> ControlFlow<U, R, E> {
        let val = require_no_cf!(erase  self);
        ControlFlow::Value(apply(val))
    }
    pub fn and_then(self, then: impl FnOnce(T) -> Self) -> Self {
        let val = require_no_cf!(self);
        then(val)
    }
}
