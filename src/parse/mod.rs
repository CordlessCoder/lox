use std::{borrow::Cow, ops::Range};

use thiserror::Error;

use crate::{
    lex::{Token, TokenValue},
    tree::expr::{BinaryExpr, BinaryOperator, Expr, Literal, UnaryExpr, UnaryOperator},
};

pub struct Parser<'t> {
    tokens: Cow<'t, [Token]>,
    remaining: Range<usize>,
}

impl<'t> Parser<'t> {
    pub fn new(source: impl Into<Cow<'t, [Token]>>) -> Self {
        let source = source.into();
        let remaining = 0..source.len();
        Parser {
            tokens: source,
            remaining,
        }
    }
    fn remaining(&self) -> &[Token] {
        self.tokens
            .get(self.remaining.clone())
            .expect("remaining range out of bounds for source")
    }
    fn advance(&mut self) -> Option<Token> {
        let first = self.remaining().first().cloned();
        if first.is_some() {
            self.remaining.start += 1;
        }
        first
    }
    fn unconsume_one(&mut self) {
        self.remaining.start = self.remaining.start.saturating_sub(1);
    }
    fn last_consumed(&mut self) -> Option<Token> {
        if self.remaining.start == 0 {
            return None;
        }
        self.unconsume_one();
        self.advance()
    }
    fn peek(&self) -> Option<&Token> {
        self.remaining().first()
    }
    fn check(&self, predicate: impl FnOnce(&Token) -> bool) -> bool {
        self.peek().is_some_and(predicate)
    }
    fn advance_if(&mut self, predicate: impl FnOnce(&Token) -> bool) -> Option<Token> {
        let advance = self.check(predicate);
        if advance {
            return self.advance();
        }
        None
    }
    fn consume_if(&mut self, predicate: impl FnOnce(&Token) -> bool) -> bool {
        self.advance_if(predicate).is_some()
    }
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token {0}")]
    Unexpected(Token),
    #[error("Unexpected token {0}")]
    UnexpectedValue(TokenValue),
    #[error("Unterminated {0} token")]
    Unterminated(String),
    #[error("Expected a value on the right-hand-side of the {0}")]
    BinaryOperatorRhsMissing(BinaryOperator),
    #[error("error {error} on line {line}")]
    WithLine {
        error: Box<ParserError>,
        line: usize,
    },
}
impl ParserError {
    pub fn with_line(self, line: usize) -> Self {
        ParserError::WithLine {
            error: Box::new(self),
            line,
        }
    }
}

impl Parser<'_> {
    pub fn post_error_sync(&mut self) {
        let Some(mut prev) = self.advance() else {
            return;
        };
        while let Some(tok) = self.advance() {
            use TokenValue::*;
            if matches!(prev.value, TokenValue::Semicolon)
                || matches!(tok.value, Fn | Var | For | If | While | Print | Return)
            {
                self.unconsume_one();
                return;
            }
            prev = tok;
        }
    }
    pub fn expression(&mut self) -> Option<Result<Expr, ParserError>> {
        self.equality()
    }
    fn equality(&mut self) -> Option<Result<Expr, ParserError>> {
        let mut expr = match self.comparison()? {
            Ok(expr) => expr,
            Err(err) => return Some(Err(err)),
        };
        while let Some(operator) =
            self.advance_if(|t| matches!(t.value, TokenValue::Equal | TokenValue::BangEqual))
        {
            let line = operator.line;
            let operator = match operator.value {
                TokenValue::Equal => BinaryOperator::Equal,
                TokenValue::BangEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            let Some(right) = self.comparison() else {
                return Some(Err(
                    ParserError::BinaryOperatorRhsMissing(operator).with_line(line)
                ));
            };
            let right = match right {
                Ok(expr) => expr,
                Err(err) => return Some(Err(err)),
            };
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Some(Ok(expr))
    }
    fn comparison(&mut self) -> Option<Result<Expr, ParserError>> {
        let mut expr = match self.term()? {
            Ok(expr) => expr,
            Err(err) => return Some(Err(err)),
        };
        while let Some(operator) = self.advance_if(|t| {
            matches!(
                t.value,
                TokenValue::Greater
                    | TokenValue::GreaterEqual
                    | TokenValue::Less
                    | TokenValue::LessEqual
            )
        }) {
            let line = operator.line;
            let operator = match operator.value {
                TokenValue::Greater => BinaryOperator::GreaterThan,
                TokenValue::GreaterEqual => BinaryOperator::GreaterThanOrEqual,
                TokenValue::Less => BinaryOperator::LessThan,
                TokenValue::LessEqual => BinaryOperator::LessThanOrEqual,
                _ => unreachable!(),
            };
            let Some(right) = self.term() else {
                return Some(Err(
                    ParserError::BinaryOperatorRhsMissing(operator).with_line(line)
                ));
            };
            let right = match right {
                Ok(expr) => expr,
                Err(err) => return Some(Err(err)),
            };
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Some(Ok(expr))
    }
    fn term(&mut self) -> Option<Result<Expr, ParserError>> {
        let mut expr = match self.factor()? {
            Ok(expr) => expr,
            Err(err) => return Some(Err(err)),
        };
        while let Some(operator) =
            self.advance_if(|t| matches!(t.value, TokenValue::Minus | TokenValue::Plus))
        {
            let line = operator.line;
            let operator = match operator.value {
                TokenValue::Minus => BinaryOperator::Minus,
                TokenValue::Plus => BinaryOperator::Plus,
                _ => unreachable!(),
            };
            let Some(right) = self.factor() else {
                return Some(Err(
                    ParserError::BinaryOperatorRhsMissing(operator).with_line(line)
                ));
            };
            let right = match right {
                Ok(expr) => expr,
                Err(err) => return Some(Err(err)),
            };
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Some(Ok(expr))
    }
    fn factor(&mut self) -> Option<Result<Expr, ParserError>> {
        let mut expr = match self.unary()? {
            Ok(expr) => expr,
            Err(err) => return Some(Err(err)),
        };
        while let Some(operator) =
            self.advance_if(|t| matches!(t.value, TokenValue::Slash | TokenValue::Star))
        {
            let line = operator.line;
            let operator = match operator.value {
                TokenValue::Slash => BinaryOperator::Div,
                TokenValue::Star => BinaryOperator::Mul,
                _ => unreachable!(),
            };
            let Some(right) = self.unary() else {
                return Some(Err(ParserError::BinaryOperatorRhsMissing(operator)));
            };
            let right = match right {
                Ok(expr) => expr,
                Err(err) => return Some(Err(err.with_line(line))),
            };
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        Some(Ok(expr))
    }
    fn unary(&mut self) -> Option<Result<Expr, ParserError>> {
        use crate::lex::TokenValue::*;
        let operator = match self.peek()?.value {
            Minus => UnaryOperator::Minus,
            Bang => UnaryOperator::Not,
            _ => return self.primary(),
        };
        // consume the value we just peeked
        _ = self.advance();
        Some(match self.unary() {
            None => Err(ParserError::Unterminated(operator.to_string())),
            Some(Err(err)) => Err(err),
            Some(Ok(expr)) => Ok(Expr::Unary(UnaryExpr {
                operator,
                expr: Box::new(expr),
            })),
        })
    }
    fn primary(&mut self) -> Option<Result<Expr, ParserError>> {
        use crate::lex::TokenValue::*;
        let Token { value, line, .. } = self.advance()?;
        Some(Ok(match value {
            Int(n) => Expr::Literal(Literal::Integer(n)),
            Float(f) => Expr::Literal(Literal::Float(f)),
            String(s) => Expr::Literal(Literal::String(s)),
            Bool(b) => Expr::Literal(Literal::Bool(b)),
            Nil => Expr::Literal(Literal::Nil),
            LeftParen => {
                let Some(expr) = self.expression() else {
                    return Some(Err(
                        ParserError::Unterminated("(".to_string()).with_line(line)
                    ));
                };
                let expr = match expr {
                    Ok(expr) => expr,
                    Err(err) => return Some(Err(err)),
                };
                let Some(tok) = self.advance() else {
                    return Some(Err(
                        ParserError::Unterminated("(".to_string()).with_line(line)
                    ));
                };
                if !matches!(tok.value, RightParen) {
                    return Some(Err(ParserError::Unexpected(tok).with_line(line)));
                }
                Expr::Grouped(Box::new(expr))
            }
            unexpected => {
                return Some(Err(ParserError::UnexpectedValue(unexpected).with_line(line)))
            }
        }))
    }
}
