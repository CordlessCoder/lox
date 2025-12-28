pub use logos::Lexer;
use memchr::memchr2_iter;
use std::borrow::Cow;

use crate::Token;

pub fn unescape(c: char) -> Option<char> {
    Some(match c {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '0' => '\0',
        '\'' => '\'',
        '\"' => '\"',
        _ => return None,
    })
}

pub fn unescape_string<'s>(lex: &mut Lexer<'s, Token<'s>>) -> Option<Cow<'s, str>> {
    let remainder = lex.remainder();
    let mut important_iter = memchr2_iter(b'\\', b'"', remainder.as_bytes());
    let first_important = important_iter.next()?;
    // Early return on string without escapes
    if remainder.as_bytes()[first_important] == b'"' {
        lex.bump(first_important + 1);
        return Some(Cow::Borrowed(&remainder[..first_important]));
    }
    let mut out = String::from(&remainder[..first_important]);
    let mut start = first_important + 1;
    let escaped = remainder[start..].chars().next()?;
    if escaped == '"' {
        // We know the next important character has been escaped.
        _ = important_iter.next();
    }
    out.push(unescape(escaped)?);
    start += escaped.len_utf8();
    loop {
        let important = important_iter.next()?;
        // Capture string component before escape/quote
        out.push_str(&remainder[start..important]);
        if remainder.as_bytes()[important] == b'"' {
            lex.bump(important + 1);
            return Some(Cow::Owned(out));
        }
        let escaped = remainder[important + 1..].chars().next()?;
        if escaped == '"' {
            // We know the next important character has been escaped.
            _ = important_iter.next();
        }
        out.push(unescape(escaped)?);
        start = important + 1 + escaped.len_utf8();
    }
}
