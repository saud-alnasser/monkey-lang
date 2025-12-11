use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::runtime::{DataType, builtins};

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<Box<str>, DataType>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(outer: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: match outer {
                Some(outer) => Some(outer),
                None => {
                    let mut outer = Environment {
                        store: HashMap::new(),
                        outer: None,
                    };

                    for (key, value) in builtins::definitions() {
                        outer.set(key, value);
                    }

                    Some(Rc::new(RefCell::new(outer)))
                }
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
        self.store.insert(key.into(), value);
    }
}
