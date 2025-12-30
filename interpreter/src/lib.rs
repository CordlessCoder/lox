mod environment;
mod eval;
mod types;

use ast::Program;
use environment::Environment;
use eval::{Eval, EvalError};

use crate::types::{Value, control_flow::ControlFlow};

#[derive(Debug, Clone)]
pub struct LoxVm<'s> {
    env: Environment<'s>,
}

impl<'s> LoxVm<'s> {
    #[must_use]
    pub fn new() -> Self {
        let mut vm = Self {
            env: Environment::new(),
        };
        vm.reset();
        vm
    }
    pub fn reset(&mut self) {
        use types::{Callable, Value};

        self.env.clear();
        self.env.define("print", Value::Callable(Callable::Print));
        self.env.define("now", Value::Callable(Callable::Now));
        self.env
            .define("elapsed", Value::Callable(Callable::Elapsed));
    }
    pub fn run(&mut self, program: &'s Program<'s>) -> Result<Option<Value<'s>>, EvalError<'s>> {
        let invalid_main_cf = |kind| {
            Err(EvalError::InvalidCF {
                kind,
                context: "in the main function",
            })
        };
        for decl in &program.declarations {
            use ControlFlow::*;
            match decl.eval(self) {
                Value(_) => Ok(()),
                Error(err) => Err(err),
                // TODO: Handle returns as exit codes?
                Return(ret) => return Ok(ret),
                Break => invalid_main_cf("break"),
                Continue => invalid_main_cf("continue"),
            }?;
        }
        Ok(None)
    }
}

impl Default for LoxVm<'_> {
    fn default() -> Self {
        Self::new()
    }
}
