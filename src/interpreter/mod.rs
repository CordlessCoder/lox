use crate::{lex::Scanner, parse::Parser, tree::eval::Eval};
mod error;
pub use error::*;
use owo_colors::OwoColorize;

#[derive(Debug, Default)]
pub struct Lox {}

impl Lox {
    pub fn run(&mut self, source: &str) -> Result<(), Vec<LoxError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        for res in Scanner::new(source) {
            match res {
                Ok(token) => tokens.push(token),
                Err(error) => errors.push(error),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }
        eprint!("tokens: ");
        for token in &tokens {
            eprint!("{}; ", token.blue());
        }
        eprintln!();
        let mut parser = Parser::new(tokens);
        let mut errors = Vec::new();
        while let Some(expr) = parser.expression() {
            let expr = match expr {
                Err(err) => {
                    errors.push(LoxError::from(err));
                    parser.post_error_sync();
                    continue;
                }
                Ok(expr) => expr,
            };
            eprintln!("Expr: {}", expr.green());
            let result = expr
                .eval(self)
                .map_err(LoxError::from)
                .map_err(|err| vec![err])?;
            eprintln!("result: {}", result.bright_blue());
        }
        if !errors.is_empty() {
            return Err(errors);
        };
        Ok(())
    }
}
