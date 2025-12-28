use crate::escapes::{unescape, unescape_string};
use crate::int::parse_int;
pub use logos::{Lexer, Logos};
use std::borrow::Cow;
use utils::Spanned;

mod display;
mod escapes;
mod int;

pub type SToken<'s> = Spanned<Token<'s>>;

#[derive(Debug, Clone, Logos, PartialEq)]
#[logos(skip "[ \r\n\t]+")]
#[logos(skip "//[^\n]*")]
pub enum Token<'s> {
    // keywords
    #[token("and")]
    /// and
    And,
    #[token("class")]
    /// class
    Class,
    #[token("else")]
    /// else
    Else,
    #[token("fun")]
    /// fun
    Fun,
    #[token("for")]
    /// for
    For,
    #[token("if")]
    /// if
    If,
    #[token("nil")]
    /// nil
    Nil,
    #[token("or")]
    /// or
    Or,
    #[token("print")]
    /// print
    Print,
    #[token("return")]
    /// return
    Return,
    #[token("super")]
    /// super
    Super,
    #[token("this")]
    /// this
    This,
    #[token("var")]
    /// var
    Var,
    #[token("while")]
    /// while
    While,
    #[token("break")]
    /// break
    Break,
    #[token("continue")]
    /// continue
    Continue,

    // literals
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    BoolLit(bool),
    #[regex("\"", unescape_string)]
    StringLit(Cow<'s, str>),
    #[regex(r"'[^']'", |lex| {
        let text = lex.slice();
        text[1..].chars().next().unwrap()
    })]
    #[regex(r"'\\[^']'", (|lex: &mut Lexer<'s, Token<'s>>| -> Option<char> {
        let text = lex.slice();
        let text = &text[2..text.len() - 1];
        let mut chars = text.chars();
        let c = chars.next().unwrap();
        unescape(c)
    }))]
    CharLiteral(char),
    #[regex(r"0x[0-9a-fA-F][0-9a-fA-F_]*", |lex| parse_int(16, &lex.slice()[2..]))]
    #[regex(r"0o[0-9a-fA-F][0-9a-fA-F_]*", |lex| parse_int(8, &lex.slice()[2..]))]
    #[regex(r"0p[0-9a-fA-F][0-9a-fA-F_]*", |lex| parse_int(2, &lex.slice()[2..]))]
    #[regex(r"[0-9][0-9a-fA-F_]*", |lex| parse_int(10, lex.slice()))]
    IntLit(i64),
    #[regex(r"\.\d+", |lex| lex.slice().parse().ok())]
    #[regex(r"\d+\.\d+", |lex| lex.slice().parse().ok())]
    FloatLit(f64),
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*")]
    Ident(&'s str),

    #[token("==")]
    /// ==
    EqEq,
    #[token("!=")]
    /// !=
    Ne,
    #[token("<=")]
    /// <=
    Le,
    #[token(">=")]
    /// =>
    Ge,
    #[token("<")]
    /// <
    Lt,
    #[token(">")]
    /// >
    Gt,
    #[token("+")]
    /// +
    Plus,
    #[token("-")]
    /// -
    Minus,
    #[token("*")]
    /// *
    Star,
    #[token("/")]
    /// /
    Slash,
    #[token("(")]
    /// (
    LParen,
    #[token(")")]
    /// )
    RParen,
    #[token("{")]
    /// {
    LBrace,
    #[token("}")]
    /// }
    RBrace,
    #[token(",")]
    /// ,
    Comma,
    #[token(";")]
    /// ;
    Semicolon,
    #[token(".")]
    /// .
    Dot,
    #[token("=")]
    /// =
    Eq,
    #[token("!")]
    /// !
    Not,
}

#[cfg(test)]
mod tests {
    use logos::Logos;
    use pretty_assertions::assert_eq;
    use std::borrow::Cow;

    use crate::Token;

    fn string<'s>(text: impl Into<Cow<'s, str>>) -> Token<'s> {
        Token::StringLit(text.into())
    }

    #[test]
    fn lex_example() {
        use Token::*;
        let example = include_str!("../../examples/hello.lox");
        let tokens: Vec<_> = Token::lexer(example).collect::<Result<_, _>>().unwrap();
        assert_eq!(
            &tokens,
            [
                // Your first Lox program!
                // print "\"Hello, world\"!";
                Print,
                string("\"Hello, world\"!"),
                Semicolon
            ]
            .as_slice()
        );
    }
}
