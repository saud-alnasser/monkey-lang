use std::fmt::Display;

use crate::runtime::{builtins::error::Result, datatype::DataType};

#[derive(Debug, Clone)]
pub struct Function(pub fn(Vec<DataType>) -> Result<DataType>);

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

impl Function {
    pub const fn new(f: fn(Vec<DataType>) -> Result<DataType>) -> Self {
        Self(f)
    }
}
