use std::fmt::Display;

use super::DataType;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    ExtraArguments {
        builtin: Box<str>,
        expected: Box<str>,
        got: String,
    },
    NotSupportedArgument {
        builtin: Box<str>,
        expected: Box<str>,
        position: usize,
        got: DataType,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ExtraArguments {
                builtin,
                expected,
                got,
            } => write!(
                f,
                r#"extra arguments are passed to BUILTIN("{}"). got={}, want={}"#,
                builtin, got, expected
            ),
            Error::NotSupportedArgument {
                builtin,
                expected,
                position,
                got,
            } => write!(
                f,
                r#"argument[{}] passed to BUILTIN("{}") is not supported. got={}, want={}"#,
                position, builtin, got, expected
            ),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

