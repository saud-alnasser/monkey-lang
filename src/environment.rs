use std::collections::HashMap;

use crate::DataType;

pub struct Environment {
    store: HashMap<String, DataType>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&DataType> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: &str, value: DataType) {
        self.store.insert(key.to_string(), value);
    }
}
