use crate::ast::Token;
use std::fmt::Display;

use super::datatype::DataType;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    TypeMismatch(DataType, Token, DataType),
    IndexTypeMismatch(DataType, Token),
    UnknownOperator(Token),
    UndefinedVariable(DataType),
    Builtin(crate::builtins::error::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeMismatch(left, token, right) => write!(
                f,
                "type mismatch {:?} {} {:?} at {}:{}",
                left, token.literal, right, token.span.line, token.span.column
            ),
            Error::IndexTypeMismatch(value, token) => {
                write!(
                    f,
                    "index type mismatch got={:?}, want=INT at {}:{}",
                    value, token.span.line, token.span.column
                )
            }
            Error::UnknownOperator(token) => {
                write!(
                    f,
                    "unknown operator {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                )
            }
            Error::UndefinedVariable(value) => {
                write!(f, "unknown identifier {}", value)
            }
            Error::Builtin(error) => write!(f, "{}", error),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
