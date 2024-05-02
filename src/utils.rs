use std::{iter::Peekable, str::Chars};

pub fn take_series_where<F>(chars: &mut Peekable<Chars>, predicate: F) -> Option<Box<str>>
where
    F: Fn(&char) -> bool,
{
    let mut result = String::new();

    while let Some(c) = chars.peek() {
        match predicate(c) {
            true => {
                result.push(*c);
                chars.next();
            }
            false => break,
        }
    }

    match result.is_empty() {
        true => None,
        false => Some(result.into_boxed_str()),
    }
}
