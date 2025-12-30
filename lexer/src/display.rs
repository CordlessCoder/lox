use crate::Token;
use std::fmt::Display;

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        let lit = match self {
            And => "and",
            Class => "class",
            Else => "else",
            Fun => "fun",
            For => "for",
            If => "if",
            Nil => "nil",
            Or => "or",
            Return => "return",
            Super => "super",
            This => "this",
            Var => "var",
            While => "while",
            Break => "break",
            Continue => "continue",

            BoolLit(true) => "true",
            BoolLit(false) => "false",
            StringLit(s) => return write!(f, "{s:?}"),
            CharLiteral(c) => return c.fmt(f),
            IntLit(v) => return v.fmt(f),
            FloatLit(v) => return v.fmt(f),
            Ident(i) => i,

            EqEq => "==",
            Ne => "!=",
            Le => "<=",
            Ge => ">=",
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            LParen => "(",
            RParen => ")",
            LBrace => "{",
            RBrace => "}",
            Comma => ",",
            Semicolon => ";",
            Dot => ".",
            Eq => "=",
            Not => "!",
            Gt => ">",
            Lt => "<",
        };
        f.write_str(lit)
    }
}
