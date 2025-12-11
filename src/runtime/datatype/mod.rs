mod array;
mod boolean;
mod callable;
mod integer;
mod object;
mod string;

use std::{fmt::Display, rc::Rc};

pub use self::{array::*, boolean::*, callable::*, integer::*, object::*, string::*};

#[derive(Debug, Clone)]
pub enum DataType {
    Integer(Integer),
    String(String),
    Boolean(Boolean),
    Array(Array),
    Function(Rc<dyn Callable>),
    Object(Object),
    Undefined,
    Null,
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DataType::Integer(a), DataType::Integer(b)) => a == b,
            (DataType::String(a), DataType::String(b)) => a == b,
            (DataType::Boolean(a), DataType::Boolean(b)) => a == b,
            (DataType::Array(a), DataType::Array(b)) => a == b,
            (DataType::Object(a), DataType::Object(b)) => a == b,
            (DataType::Undefined, DataType::Undefined) => true,
            (DataType::Null, DataType::Null) => true,
            (DataType::Function(_), DataType::Function(_)) => false,
            _ => false,
        }
    }
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
