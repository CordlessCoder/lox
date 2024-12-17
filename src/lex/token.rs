use std::fmt::Display;

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
pub use keywords::KEYWORDS;

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
    /// true
    /// false
    Bool(bool),

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
}

impl Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenValue::*;
        let buf;
        match self {
            LeftParen => "(",
            RightParen => ")",
            LeftBrace => "[",
            RightBrace => "]",
            Comma => ",",
            Dot => ".",
            Minus => "-",
            Plus => "+",
            Semicolon => ";",
            Slash => "/",
            Star => "*",
            // One or two character tokens.
            Bang => "!",
            BangEqual => "!=",
            Equal => "=",
            EqualEqual => "==",
            Greater => ">",
            GreaterEqual => ">=",
            Less => "<",
            LessEqual => "<=",
            // Literals.
            Identifier(text) => text,
            String(text) => {
                buf = format!("{text:?}");
                &buf
            }
            Int(n) => {
                buf = format!("{n}");
                &buf
            }
            Float(n) => {
                buf = format!("{n}");
                &buf
            }
            Bool(b) => {
                buf = format!("{b}");
                &buf
            }
            // Keywords.
            And => "and",
            Class => "class",
            Else => "else",
            Fn => "fn",
            For => "for",
            If => "if",
            Nil => "nil",
            Or => "or",
            Print => "print",
            Return => "return",
            Super => "super",
            This => "this",
            Var => "var",
            While => "while",
        }
        .fmt(f)
    }
}

use super::{Scanner, Span};
#[derive(Debug, Clone)]
pub struct Token {
    pub(crate) value: TokenValue,
    pub(crate) lexeme: String,
    pub(crate) line: usize,
}

impl Token {
    pub fn from_span(scanner: &Scanner, span: &Span, value: TokenValue) -> Self {
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
        write!(f, "{lexeme} => {value}")
    }
}
