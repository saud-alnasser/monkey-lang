use std::{cell::RefCell, fmt::Display, rc::Rc};

use internment::Intern;

use crate::{ast::BlockExpression, builtins::error::Result, datatype::DataType, Environment};

#[derive(Debug, PartialEq, Clone)]
pub enum Function {
    Native(fn(Vec<DataType>) -> Result<DataType>),
    Normal {
        parameters: Vec<Intern<String>>,
        body: Box<BlockExpression>,
        env: Rc<RefCell<Environment>>,
    },
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
