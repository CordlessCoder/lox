mod eval;
use ast::Program;

use crate::eval::{Eval, EvalError};

#[derive(Debug, Default)]
pub struct LoxVm {}

impl LoxVm {
    pub fn run(&mut self, program: Program<'_>) -> Result<(), Vec<EvalError>> {
        let mut errors = Vec::new();
        for stmt in program.statements {
            if let Err(err) = stmt.eval(self) {
                errors.push(err);
            };
        }
        if !errors.is_empty() {
            return Err(errors);
        };
        Ok(())
    }
}
