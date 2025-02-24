use std::borrow::Cow;

fn remove_underscores<'s>(s: &'s str) -> Cow<'s, str> {
    s.split('_')
        .map(Cow::from)
        .reduce(|a, b| (a.into_owned() + &b).into())
        .unwrap_or_default()
}

pub fn parse_int(base: u32, text: &str) -> Option<i64> {
    i64::from_str_radix(&remove_underscores(text), base).ok()
}
