use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::builtins::error::Result;
use crate::{BlockStatement, Environment, IdentExpression};

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    INT(i64),
    STRING(Box<str>),
    BOOLEAN(bool),
    ARRAY(Vec<DataType>),
    RETURN(Box<DataType>),
    IDENT(Box<str>),
    FUNCTION {
        parameters: Vec<IdentExpression>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    },
    BUILTIN {
        func: fn(Vec<DataType>) -> Result<DataType>,
    },
    UNDEFINED,
    NULL,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::INT(value) => write!(f, "{}", value),
            DataType::STRING(value) => write!(f, "{}", value),
            DataType::BOOLEAN(value) => write!(f, "{}", value),
            DataType::ARRAY(value) => write!(
                f,
                "[{}]",
                value
                    .iter()
                    .map(|element| element.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            DataType::RETURN(value) => write!(f, "{}", value),
            DataType::IDENT(value) => write!(f, "{}", value),
            DataType::FUNCTION { .. } => write!(f, ""),
            DataType::BUILTIN { .. } => write!(f, ""),
            DataType::UNDEFINED => write!(f, ""),
            DataType::NULL => write!(f, "null"),
        }
    }
}
