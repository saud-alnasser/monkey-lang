use std::fmt::Display;

use super::{Spanned, Token};
use crate::DataType;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    ObjectKeyTypeMismatch(DataType, Spanned<Token>),
    BuiltinFunction(#[from] crate::builtins::error::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ObjectKeyTypeMismatch(value, (_, span)) => {
                write!(
                    f,
                    "object key type mismatch got={} want=STRING|INTEGER|BOOLEAN at {}:{}",
                    value.name_of(),
                    span.start,
                    span.end
                )
            }
            Error::BuiltinFunction(error) => write!(f, "{}", error),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
