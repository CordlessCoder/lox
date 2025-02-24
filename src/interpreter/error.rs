use std::fmt::Display;

use crate::{parse::ParserError, source::FilePosition, tree::eval::EvalError};

#[derive(Debug, Clone)]
pub struct LoxError {
    pos: Option<FilePosition>,
    message: String,
}
impl LoxError {
    pub fn simple_error(message: impl Into<String>, pos: impl Into<Option<FilePosition>>) -> Self {
        LoxError {
            pos: pos.into(),
            message: message.into(),
        }
    }
}

impl From<ParserError> for LoxError {
    fn from(mut parser_error: ParserError) -> Self {
        let mut line = None;
        if let ParserError::WithLine { error, line: l } = parser_error {
            line = Some(l);
            parser_error = *error;
        };
        LoxError {
            pos: line.map(|line| FilePosition::new().with_line(line)),
            message: parser_error.to_string(),
        }
    }
}

impl From<EvalError> for LoxError {
    fn from(value: EvalError) -> Self {
        LoxError {
            message: value.to_string(),
            pos: None,
        }
    }
}
impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { pos, message } = self;
        if let Some(FilePosition {
            line,
            col,
            filepath,
        }) = pos
        {
            write!(f, "[")?;
            if let Some(path) = filepath {
                write!(f, "file {path:?} ")?;
            }
            write!(f, "line {line} col {col}] ")?;
        }
        write!(f, "Error: {message}")
    }
}
