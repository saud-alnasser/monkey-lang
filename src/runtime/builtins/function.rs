use std::fmt::Display;

use super::error::Error;
use crate::runtime::{Callable, DataType};

#[derive(Debug, Clone)]
pub struct Function(pub fn(Vec<DataType>) -> Result<DataType, Error>);

impl Callable for Function {}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin>")
    }
}

impl Function {
    pub fn new(f: fn(Vec<DataType>) -> Result<DataType, Error>) -> Self {
        Self(f)
    }
}
