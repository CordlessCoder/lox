use crate::{lex::Scanner, parse::Parser};
mod error;
pub use error::*;

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
            eprint!("{token}; ");
        }
        eprintln!();
        let mut parser = Parser::new(tokens);
        let mut errors = Vec::new();
        while let Some(expr) = parser.expression() {
            let expr = match expr {
                Err(err) => {
                    errors.push(LoxError::from_parser(err));
                    parser.post_error_sync();
                    continue;
                }
                Ok(expr) => expr,
            };
            eprintln!("{expr}");
        }
        if !errors.is_empty() {
            return Err(errors);
        };
        Ok(())
    }
}
