mod eval;
use std::collections::HashMap;

use ast::Program;
use vm_types::Value;

use crate::eval::{Eval, EvalError};

#[derive(Debug, Default)]
pub struct LoxVm<'s> {
    global_variables: HashMap<&'s str, Value>,
}

impl<'s> LoxVm<'s> {
    pub fn run(&mut self, program: Program<'s>) -> Result<(), Vec<EvalError>> {
        let mut errors = Vec::new();
        for decl in program.declarations {
            if let Err(err) = decl.eval(self) {
                errors.push(err);
            };
        }
        if !errors.is_empty() {
            return Err(errors);
        };
        Ok(())
    }
}
