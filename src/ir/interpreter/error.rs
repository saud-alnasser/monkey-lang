use internment::Intern;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::ir::program::{Label, Temp};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("invalid label: {0:?}")]
    InvalidLabel(Label),

    #[error("undefined temporary: {0:?}")]
    UndefinedTemp(Temp),

    #[error("undefined variable: {0}")]
    UndefinedVariable(Intern<String>),

    #[error("type mismatch for operator {0}: {1} and {2}")]
    TypeMismatch(BinaryOperator, String, String),

    #[error("unary type mismatch for operator {0}: {1}")]
    UnaryTypeMismatch(UnaryOperator, String),

    #[error("index type mismatch: {0}")]
    IndexTypeMismatch(String),

    #[error("not indexable: {0}")]
    NotIndexable(String),

    #[error("not callable: {0}")]
    NotCallable(String),

    #[error("builtin error: {0}")]
    BuiltinError(#[from] crate::runtime::builtins::error::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
