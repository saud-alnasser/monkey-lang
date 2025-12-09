use std::{cell::RefCell, rc::Rc};

use internment::Intern;

use crate::ir::program::Label;
use crate::runtime::environment::Environment;

/// a function is a list of parameters, a body label, and an environment
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<Intern<std::string::String>>,
    pub body_label: Label,
    pub env: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(
        parameters: Vec<Intern<std::string::String>>,
        body_label: Label,
        env: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            parameters,
            body_label,
            env,
        }
    }
}
