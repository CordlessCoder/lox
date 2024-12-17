use std::fmt::Display;

use crate::parse::ParserError;

#[derive(Debug, Clone)]
pub struct LoxError {
    line: usize,
    column: Option<usize>,
    location: Option<String>,
    message: String,
}
impl LoxError {
    pub fn simple_error(line: usize, message: impl Into<String>) -> Self {
        LoxError {
            line,
            column: None,
            location: None,
            message: message.into(),
        }
    }
    pub fn from_parser(mut parser_error: ParserError) -> Self {
        let mut l = 0;
        if let ParserError::WithLine { error, line } = parser_error {
            l = line;
            parser_error = *error;
        };
        LoxError {
            line: l,
            column: None,
            location: None,
            message: parser_error.to_string(),
        }
    }
}
impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            line,
            column,
            location,
            message,
        } = self;
        write!(f, "[line {line}")?;
        if let Some(column) = column {
            write!(f, " col {column}")?;
        };
        write!(f, "] Error")?;
        if let Some(location) = location {
            write!(f, " {location}")?;
        };
        write!(f, ": {message}")
    }
}
