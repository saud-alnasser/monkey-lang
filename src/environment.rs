use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{builtins, DataType};

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, DataType>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    fn builtins() -> Environment {
        let mut env = Environment {
            store: HashMap::new(),
            outer: None,
        };

        for (key, value) in builtins() {
            env.set(key, value);
        }

        env
    }

    pub fn new(outer: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: match outer {
                Some(outer) => Some(outer),
                None => Some(Rc::new(RefCell::new(Environment::builtins()))),
            },
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
