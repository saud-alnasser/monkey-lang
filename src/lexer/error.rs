use chumsky::error::Simple;
use std::fmt::Display;

#[derive(Debug, thiserror::Error)]
pub struct Error(Simple<char>);

impl From<Simple<char>> for Error {
    fn from(value: Simple<char>) -> Self {
        Self(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error = &self.0;
        let span = error.span();
        let start = span.start;
        let end = span.end;

        if let Some(found) = error.found() {
            write!(f, "unexpected token got={} at {}:{}", found, start, end)
        } else {
            write!(f, "unexpected token got=EOF at {}:{}", start, end)
        }
    }
}

pub type Result<T> = std::result::Result<T, Vec<Error>>;
