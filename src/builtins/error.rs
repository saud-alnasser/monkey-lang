use std::fmt::Display;

use crate::DataType;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    ExtraArguments {
        builtin: &'static str,
        expected: &'static str,
        got: String,
    },
    NotSupportedArgument {
        builtin: &'static str,
        expected: &'static str,
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
                r#"argument[{}] passed to BUILTIN("{}") is not supported. got={:?}, want={}"#,
                position, builtin, got, expected
            ),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
