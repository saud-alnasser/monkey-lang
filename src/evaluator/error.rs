use crate::ast::Token;
use std::fmt::Display;

use super::datatype::DataType;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    IllegalType(Token),
    TypeMismatch(DataType, Token, DataType),
    IndexTypeMismatch(DataType, Token),
    UnknownOperator(Token),
    UndefinedVariable(DataType),
    Builtin(crate::builtins::error::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::IllegalType(token) => write!(
                f,
                "illegal type {:?} at {}:{}",
                token.literal, token.line, token.column
            ),
            Error::TypeMismatch(left, token, right) => write!(
                f,
                "type mismatch {:?} {} {:?} at {}:{}",
                left, token.literal, right, token.line, token.column
            ),
            Error::IndexTypeMismatch(value, token) => {
                write!(
                    f,
                    "index type mismatch got={:?} want=INT at {}:{}",
                    value, token.line, token.column
                )
            }
            Error::UnknownOperator(token) => {
                write!(
                    f,
                    "unknown operator {} at {}:{}",
                    token.literal, token.line, token.column
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
