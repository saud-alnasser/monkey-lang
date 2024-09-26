use std::collections::HashMap;
use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::builtins::error::Result;
use crate::{BlockExpression, Environment, IdentExpression};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum HashKey {
    String(Box<str>),
    Int(i64),
    Boolean(bool),
}

impl Display for HashKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HashKey::String(value) => write!(f, "{}", value),
            HashKey::Int(value) => write!(f, "{}", value),
            HashKey::Boolean(value) => write!(f, "{}", value),
        }
    }
}

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
        body: BlockExpression,
        env: Rc<RefCell<Environment>>,
    },
    HASH {
        pairs: HashMap<HashKey, DataType>,
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
            DataType::HASH { pairs: map } => {
                let mut output = String::new();

                let mut pairs = String::new();

                for (i, (key, value)) in map.iter().enumerate() {
                    pairs.push_str(&format!(" {}: {}", key, value));

                    if i < pairs.len() - 1 {
                        pairs.push_str(", ");
                    }
                }

                output.push_str(&format!("{{{}}}", pairs));

                write!(f, "{}", output)
            }
            DataType::BUILTIN { .. } => write!(f, ""),
            DataType::UNDEFINED => write!(f, ""),
            DataType::NULL => write!(f, "null"),
        }
    }
}
