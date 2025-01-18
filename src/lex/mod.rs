use std::{borrow::Cow, ops::Range};
pub struct Scanner<'s> {
    source: Cow<'s, str>,
    remaining: Range<usize>,
    line: usize,
}
mod token;
pub use token::*;

use crate::interpreter::LoxError;

impl<'s> Scanner<'s> {
    pub fn new(source: impl Into<Cow<'s, str>>) -> Self {
        let source = source.into();
        let remaining = 0..source.len();
        Scanner {
            source,
            line: 1,
            remaining,
        }
    }
    fn remaining(&self) -> &str {
        self.source
            .get(self.remaining.clone())
            .expect("remaining range out of bounds for source")
    }
    fn advance(&mut self) -> Option<char> {
        let first = self.remaining().chars().next()?;
        self.remaining.start += first.len_utf8();
        Some(first)
    }
    fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }
    fn advance_if(&mut self, predicate: impl FnOnce(char) -> bool) -> Option<char> {
        let advance = predicate(self.peek()?);
        if advance {
            return self.advance();
        }
        None
    }
    fn consume_if(&mut self, predicate: impl FnOnce(char) -> bool) -> bool {
        self.advance_if(predicate).is_some()
    }
    fn match_literal(&mut self, literal: &str) -> bool {
        let matches = self.peek_literal(literal);
        if matches {
            self.remaining.start += literal.len();
        }
        matches
    }
    fn peek_literal(&mut self, literal: &str) -> bool {
        self.remaining().starts_with(literal)
    }
    pub fn span(&self) -> Span {
        Span {
            line: self.line,
            start: self.remaining.start,
        }
    }
    pub fn consumed_from_range(&self, span: &Span) -> Range<usize> {
        span.start..self.remaining.start
    }
    pub fn consumed_from(&self, span: &Span) -> &str {
        self.source
            .get(self.consumed_from_range(span))
            .expect("span start or remaining range start out of bounds")
    }
}
#[derive(Debug, Clone, Copy)]
pub struct Span {
    line: usize,
    start: usize,
}

impl Iterator for Scanner<'_> {
    type Item = Result<Token, LoxError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn capture_digits(scanner: &mut Scanner<'_>) {
            while scanner.consume_if(|c| matches!(c, '0'..='9' | 'a'..='f' | '.')) {}
        }
        fn try_parse_int(
            scanner: &Scanner<'_>,
            body: &str,
            radix: u32,
            kind: &str,
        ) -> Result<TokenValue, LoxError> {
            match i64::from_str_radix(body, radix) {
                Ok(n) => Ok(Int(n)),
                Err(err) => Err(LoxError::simple_error(
                    scanner.line,
                    format!("Error while attempting to parse {kind} integer: {err}",),
                )),
            }
        }
        use TokenValue::*;
        let (span, value) = loop {
            let span = self.span();

            let value = match self.advance()? {
                '(' => LeftParen,
                ')' => RightParen,
                '{' => LeftBrace,
                '}' => RightBrace,
                ',' => Comma,
                '.' => Dot,
                '-' => Minus,
                '+' => Plus,
                ';' => Semicolon,
                '*' => Star,

                '!' if self.match_literal("=") => BangEqual,
                '!' => Bang,
                '=' if self.match_literal("=") => EqualEqual,
                '=' => Equal,
                '<' if self.match_literal("=") => LessEqual,
                '<' => Less,
                '>' if self.match_literal("=") => GreaterEqual,
                '>' => Greater,

                '/' if self.match_literal("*") => {
                    match self.remaining().find("*/") {
                        Some(star) => {
                            self.remaining.start += star + 2;
                        }
                        None => {
                            self.remaining.start = self.remaining.end;
                            return Some(Err(LoxError::simple_error(
                                self.line,
                                "Unterminated block comment(/* ... */)",
                            )));
                        }
                    }
                    continue;
                }
                '/' if !self.peek_literal("/") => Slash,
                '/' if self.match_literal("/") => {
                    while self.consume_if(|c| c != '\n') {}
                    self.line += 1;
                    continue;
                }
                quote @ ('"' | '\'') => {
                    loop {
                        let Some(c) = self.advance() else {
                            return Some(Err(LoxError::simple_error(
                                self.line,
                                "Unterminated string literal.",
                            )));
                        };
                        match c {
                            '\n' => self.line += 1,
                            c if c == quote => break,
                            _ => (),
                        }
                    }

                    let consumed = self.consumed_from(&span);
                    let text = &consumed[1..consumed.len() - 1];
                    return Some(Ok(Token {
                        value: TokenValue::String(text.to_string()),
                        lexeme: consumed.to_string(),
                        line: span.line,
                    }));
                }
                '\r' | ' ' | '\t' => continue,
                '\n' => {
                    self.line += 1;
                    continue;
                }
                '0' if self.match_literal("b") => {
                    let body = self.span();
                    capture_digits(self);
                    match try_parse_int(self, self.consumed_from(&body), 2, "binary") {
                        Ok(n) => n,
                        Err(err) => return Some(Err(err)),
                    }
                }
                '0' if self.match_literal("o") => {
                    let body = self.span();
                    capture_digits(self);
                    match try_parse_int(self, self.consumed_from(&body), 8, "octal") {
                        Ok(n) => n,
                        Err(err) => return Some(Err(err)),
                    }
                }
                '0' if self.match_literal("x") => {
                    let body = self.span();
                    capture_digits(self);
                    match try_parse_int(self, self.consumed_from(&body), 16, "hex") {
                        Ok(n) => n,
                        Err(err) => return Some(Err(err)),
                    }
                }
                '0'..='9' => {
                    capture_digits(self);
                    let digits = self.consumed_from(&span);
                    match try_parse_int(self, digits, 10, "decimal") {
                        Ok(n) => n,
                        Err(err) => {
                            if let Ok(float) = digits.parse() {
                                Float(float)
                            } else {
                                return Some(Err(err));
                            }
                        }
                    }
                }
                'a'..='z' | 'A'..='Z' => {
                    while self.consume_if(|c| c.is_alphanumeric()) {}
                    let ident = self.consumed_from(&span);
                    KEYWORDS
                        .get(ident)
                        .cloned()
                        .unwrap_or_else(|| Identifier(ident.to_string()))
                }
                c => {
                    return Some(Err(LoxError::simple_error(
                        self.line,
                        format!("Unexpected character: {c:?}"),
                    )));
                }
            };
            break (span, value);
        };
        Some(Ok(Token::from_span(self, &span, value)))
    }
}
