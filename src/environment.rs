use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::DataType;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, DataType>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(outer: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer,
        }
    }

    pub fn get(&self, key: &str) -> Option<DataType> {
        match self.store.get(key) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: &str, value: DataType) {
        self.store.insert(key.to_string(), value);
    }
}
