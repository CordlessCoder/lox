use std::{borrow::Cow, ops::Range, path::PathBuf};

pub struct Lexer<'s> {
    source: Cow<'s, str>,
    remaining: Range<usize>,
    pos: FilePosition,
}
mod token;
pub use token::*;

use crate::{interpreter::LoxError, source::FilePosition};

impl<'s> Lexer<'s> {
    pub fn new(source: impl Into<Cow<'s, str>>, path: Option<PathBuf>) -> Self {
        let source = source.into();
        let remaining = 0..source.len();
        Lexer {
            source,
            remaining,
            pos: path
                .map(|path| FilePosition::new().with_path(path))
                .unwrap_or_default(),
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
        match first {
            '\n' => {
                self.pos.line += 1;
                self.pos.col = 1;
            }
            _ => self.pos.col += 1,
        }
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
            pos: self.pos.clone(),
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
    fn current_position(&self) -> FilePosition {
        self.pos.clone()
    }
    fn escape(c: char) -> Option<char> {
        Some(match c {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '0' => '\0',
            '\'' => '\'',
            '\"' => '\"',
            '\\' => '\\',
            _ => return None,
        })
    }
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LoxError> {
        self.collect()
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    pos: FilePosition,
    start: usize,
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LoxError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn capture_digits(scanner: &mut Lexer<'_>) {
            while scanner.consume_if(|c| matches!(c, '0'..='9' | 'a'..='f')) {}
        }
        fn try_parse_int(
            scanner: &Lexer<'_>,
            body: &str,
            radix: u32,
            kind: &str,
        ) -> Result<TokenValue, LoxError> {
            match i64::from_str_radix(body, radix) {
                Ok(n) => Ok(Int(n)),
                Err(err) => Err(LoxError::simple_error(
                    format!("Error while attempting to parse {kind} integer: {err}",),
                    scanner.pos.clone(),
                )),
            }
        }
        use TokenValue::*;
        let (span, value) = loop {
            let span = self.span();

            let value = match self.advance()? {
                '/' if self.match_literal("*") => {
                    match self.remaining().find("*/") {
                        Some(star) => {
                            self.remaining.start += star + 2;
                        }
                        None => {
                            self.remaining.start = self.remaining.end;
                            return Some(Err(LoxError::simple_error(
                                "Unterminated block comment(/* ... */)",
                                self.pos.clone(),
                            )));
                        }
                    }
                    continue;
                }
                '/' if !self.peek_literal("/") => Slash,
                '/' if self.match_literal("/") => {
                    while self.consume_if(|c| c != '\n') {}
                    continue;
                }
                quote @ ('"' | '\'') => {
                    let mut body = String::new();
                    loop {
                        let Some(c) = self.advance() else {
                            return Some(Err(LoxError::simple_error(
                                "Unterminated string literal.",
                                self.pos.clone(),
                            )));
                        };
                        match c {
                            c if c == quote => break,
                            '\\' => {
                                let Some(c) = self.advance() else {
                                    // Ignore \ without following symbol as that means the string
                                    // is unterminated
                                    continue;
                                };
                                // TODO: Hex ASCII/BYTE escapes using \x41 syntax?
                                let Some(escaped) = Self::escape(c) else {
                                    return Some(Err(LoxError::simple_error(
                                        "Invalid escape sequence in string literral.",
                                        self.pos.clone(),
                                    )));
                                    // return Some(Err(NovaError::Lexing {
                                    //     msg: "Invalid escape sequence in string literal.".into(),
                                    //     note: format!("Attempted to use escape sequence \\{c}")
                                    //         .into(),
                                    //     position: self.current_position(),
                                    // }));
                                };
                                body.push(escaped);
                            }
                            c => body.push(c),
                        }
                    }

                    let consumed = self.consumed_from(&span);
                    return Some(Ok(Token {
                        value: TokenValue::Text(body),
                        lexeme: consumed.to_string(),
                        pos: span.pos,
                    }));
                }
                '\r' | ' ' | '\t' | '\n' => continue,
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
                c @ ('0'..='9' | '.')
                    if c != '.' || self.peek().is_some_and(|c| c.is_ascii_digit()) =>
                {
                    capture_digits(self);
                    let int_part = self.consumed_from(&span);

                    let float = self.remaining().starts_with('.')
                        && self.remaining()[1..]
                            .chars()
                            .next()
                            .is_none_or(|c| !c.is_alphabetic() && c != '.');

                    let float = float || c == '.';
                    if float {
                        // Capture .
                        self.advance_if(|c| c == '.');
                        // Capture rest of the digits
                        capture_digits(self);
                        let float = self.consumed_from(&span);
                        match float.parse() {
                            Ok(f) => Float(f),
                            Err(err) => {
                                return Some(Err(LoxError::simple_error(
                                    format!("Invalid float literal {float}. Parsing error: {err}"),
                                    self.pos.clone(),
                                )));
                            }
                        }
                    } else {
                        match try_parse_int(self, int_part, 10, "decimal") {
                            Ok(n) => n,
                            Err(err) => {
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
                c => {
                    return Some(Err(LoxError::simple_error(
                        format!("Unexpected character: {c:?}"),
                        self.pos.clone(),
                    )));
                }
            };
            break (span, value);
        };
        Some(Ok(Token::from_span(self, &span, value)))
    }
}
