use std::fmt::Display;

use internment::Intern;

use super::{BinaryOperator, DataType, Expression, UnaryOperator};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    UndefinedIdentifier(Intern<String>),
    IndexOutOfBounds(usize),
    IndexTypeMismatch(DataType),
    NonCallable(DataType),
    ObjectKeyTypeMismatch(Expression),
    PrefixIntegerTypeMismatch(UnaryOperator, DataType),
    PrefixBooleanTypeMismatch(UnaryOperator, DataType),
    InfixTypeMismatch(BinaryOperator, DataType, DataType),
    BuiltinFunction(#[from] crate::builtins::error::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UndefinedIdentifier(identifier) => {
                write!(f, "undefined identifier={}", identifier)
            }
            Error::IndexTypeMismatch(datatype) => {
                write!(
                    f,
                    "index type mismatch got={} want=STRING|INTEGER|BOOLEAN",
                    datatype.name_of(),
                )
            }
            Error::IndexOutOfBounds(index) => {
                write!(f, "index out of bounds at position={}", index,)
            }
            Error::NonCallable(datatype) => {
                write!(
                    f,
                    "calling a non-callable got={} want=FUNCTION",
                    datatype.name_of(),
                )
            }
            Error::ObjectKeyTypeMismatch(expr) => {
                write!(
                    f,
                    "object key type mismatch got={} want=STRING|INTEGER|BOOLEAN",
                    expr.name_of(),
                )
            }
            Error::PrefixIntegerTypeMismatch(operator, datatype) => {
                write!(
                    f,
                    "operator {} cannot be applied type mismatch got={}",
                    operator,
                    datatype.name_of(),
                )
            }
            Error::PrefixBooleanTypeMismatch(operator, datatype) => {
                write!(
                    f,
                    "operator {} cannot be applied type mismatch got={}",
                    operator,
                    datatype.name_of(),
                )
            }
            Error::InfixTypeMismatch(operator, left, right) => {
                write!(
                    f,
                    "operator {} cannot be applied type mismatch got={{ left = {}, right = {} }} want=INTEGER|BOOLEAN|STRING pairs of same type",
                    operator,
                    left.name_of(),
                    right.name_of(),
                )
            }
            Error::BuiltinFunction(error) => write!(f, "{}", error),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
