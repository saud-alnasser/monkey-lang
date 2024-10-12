mod array;
mod boolean;
mod function;
mod integer;
mod object;
mod string;

use std::fmt::Display;

pub use self::{array::*, boolean::*, function::*, integer::*, object::*, string::*};

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    Integer(Integer),
    String(String),
    Boolean(Boolean),
    Array(Array),
    Function(Function),
    Object(Object),
    Undefined,
    Null,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Integer(value) => write!(f, "{}", value),
            DataType::String(value) => write!(f, "{}", value),
            DataType::Boolean(value) => write!(f, "{}", value),
            DataType::Array(value) => write!(f, "{}", value),
            DataType::Function(value) => write!(f, "{}", value),
            DataType::Object(value) => write!(f, "{}", value),
            DataType::Undefined => write!(f, ""),
            DataType::Null => write!(f, "null"),
        }
    }
}

impl DataType {
    pub fn name_of(&self) -> &'static str {
        match self {
            DataType::Integer(_) => "INTEGER",
            DataType::String(_) => "STRING",
            DataType::Boolean(_) => "BOOLEAN",
            DataType::Array(_) => "ARRAY",
            DataType::Function(_) => "FUNCTION",
            DataType::Object(_) => "OBJECT",
            DataType::Undefined => "UNDEFINED",
            DataType::Null => "NULL",
        }
    }
}
