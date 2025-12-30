mod environment;
mod eval;
mod types;

use ast::Program;
use environment::Environment;
use eval::{Eval, EvalError};

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
        vm.populate_builtins();
        vm
    }
    pub fn populate_builtins(&mut self) {
        use types::Callable;
        use types::Value;
        self.env.define("print", Value::Callable(Callable::Print));
    }
    pub fn run(&mut self, program: &mut Program<'s>) -> Result<(), Vec<EvalError>> {
        let mut errors = Vec::new();
        for decl in &mut program.declarations {
            if let Err(err) = decl.eval(self) {
                errors.push(err);
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }
}

impl<'s> Default for LoxVm<'s> {
    fn default() -> Self {
        Self::new()
    }
}
