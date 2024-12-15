#![allow(dead_code)]
use std::{
    borrow::Cow,
    fmt::Display,
    io::{stdin, stdout, Write},
    ops::Range,
    path::Path,
};

#[derive(Debug, Clone)]
pub enum TokenValue {
    // Single-character tokens.
    /// (
    LeftParen,
    /// )
    RightParen,
    /// [
    LeftBrace,
    /// ]
    RightBrace,
    /// ,
    Comma,
    /// .
    Dot,
    /// -
    Minus,
    /// +
    Plus,
    /// ;
    Semicolon,
    /// /
    Slash,
    /// *
    Star,

    // One or two character tokens.
    /// !
    Bang,
    /// !=
    BangEqual,
    /// =
    Equal,
    /// ==
    EqualEqual,
    /// >
    Greater,
    /// >=
    GreaterEqual,
    /// <
    Less,
    /// <=
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Int(i64),
    Float(f64),

    // Keywords.
    /// and
    And,
    /// class
    Class,
    /// else
    Else,
    /// fn
    Fn,
    /// for
    For,
    /// if
    If,
    /// nil
    Nil,
    /// or
    Or,
    /// print
    Print,
    /// return
    Return,
    /// super
    Super,
    /// this
    This,
    /// var
    Var,
    /// while
    While,

    /// true
    /// false
    Bool(bool),
}
mod keywords {
    use super::TokenValue::*;
    pub const KEYWORDS: phf::Map<&'static str, super::TokenValue> = phf::phf_map! {
        "and" => And,
        "class" => Class,
        "else" => Else,
        "for" => For,
        "fn" => Fn,
        "if" => If,
        "nil" => Nil,
        "or" => Or,
        "print" => Print,
        "return" => Return,
        "super" => Super,
        "this" => This,
        "true" => Bool(true),
        "false" => Bool(false),
        "var" => Var,
        "while" => While,
    };
}
use keywords::KEYWORDS;
#[derive(Debug, Clone)]
pub struct Token {
    value: TokenValue,
    lexeme: String,
    line: usize,
}

impl Token {
    fn from_span(scanner: &Scanner, span: &Span, value: TokenValue) -> Self {
        let lexeme = scanner.consumed_from(span).to_string();
        Token {
            line: span.line,
            lexeme,
            value,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { value, lexeme, .. } = self;
        write!(f, "{lexeme} => {value:?}")
    }
}

struct Scanner<'s> {
    source: Cow<'s, str>,
    remaining: Range<usize>,
    line: usize,
}

impl<'s> Scanner<'s> {
    pub fn new(source: impl Into<Cow<'s, str>>) -> Self {
        let source = source.into();
        let remaining = 0..source.len();
        Scanner {
            source,
            line: 0,
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
                    continue;
                }
                '"' => {
                    loop {
                        let Some(c) = self.advance() else {
                            return Some(Err(LoxError::simple_error(
                                self.line,
                                "Unterminated string literal.",
                            )));
                        };
                        match c {
                            '\n' => self.line += 1,
                            '"' => break,
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
                c if c.is_alphabetic() => {
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

#[derive(Debug, Clone)]
struct LoxError {
    line: usize,
    column: Option<usize>,
    location: Option<String>,
    message: String,
}
impl LoxError {
    fn simple_error(line: usize, message: impl Into<String>) -> Self {
        LoxError {
            line,
            column: None,
            location: None,
            message: message.into(),
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

#[derive(Debug, Default)]
struct Lox {}

impl Lox {
    fn run(&mut self, source: &str) -> Result<(), LoxError> {
        let mut found_err = false;
        let tokens: Vec<Token> = Scanner::new(source)
            .filter_map(|item| match item {
                Ok(token) => Some(token),
                Err(err) => {
                    found_err = true;
                    println!("{err}");
                    None
                }
            })
            .collect();

        println!("{tokens:?}");
        todo!()
    }
}

fn repl() {
    let mut output = stdout();
    let input = stdin();
    let mut buf = String::new();
    let mut lox = Lox::default();

    loop {
        write!(&mut output, "> ").unwrap();
        output.flush().unwrap();

        buf.clear();
        input.read_line(&mut buf).unwrap();
        if buf.is_empty() {
            break;
        }
        lox.run(&buf).unwrap();
    }
}

fn run_file(path: impl AsRef<Path>) {
    let data = std::fs::read_to_string(path).expect("Failed to read input file");
    let mut lox = Lox::default();
    lox.run(&data).unwrap();
}

fn main() {
    let mut args = std::env::args().skip(1);
    match args.len() {
        0 => repl(),
        1 => run_file(args.next().unwrap()),
        _ => {
            eprintln!("Usage: rlox [script]");
            std::process::exit(64)
        }
    }
}
