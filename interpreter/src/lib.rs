mod environment;
mod eval;

use ast::Program;
use environment::Environment;
use eval::{Eval, EvalError};

#[derive(Debug, Clone, Default)]
pub struct LoxVm<'s> {
    env: Environment<'s>,
}

impl<'s> LoxVm<'s> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
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
