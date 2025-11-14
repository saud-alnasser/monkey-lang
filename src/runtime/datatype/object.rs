use std::{collections::HashMap, fmt::Display};

use super::{DataType, boolean::Boolean, integer::Integer, string::String};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ObjectKey {
    String(String),
    Integer(Integer),
    Boolean(Boolean),
}

impl Display for ObjectKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectKey::String(value) => write!(f, "{}", value),
            ObjectKey::Integer(value) => write!(f, "{}", value),
            ObjectKey::Boolean(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Object {
    pairs: HashMap<ObjectKey, DataType>,
}

impl Object {
    pub fn new(pairs: HashMap<ObjectKey, DataType>) -> Self {
        Self { pairs }
    }

    pub fn set(&mut self, key: ObjectKey, value: DataType) {
        self.pairs.insert(key, value);
    }

    pub fn get(&self, key: &ObjectKey) -> Option<&DataType> {
        self.pairs.get(key)
    }

    pub fn get_mut(&mut self, key: &ObjectKey) -> Option<&mut DataType> {
        self.pairs.get_mut(key)
    }
}

impl Default for Object {
    fn default() -> Self {
        Self::new(HashMap::new())
    }
}

impl std::ops::Index<&ObjectKey> for Object {
    type Output = DataType;

    fn index(&self, index: &ObjectKey) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl std::ops::IndexMut<&ObjectKey> for Object {
    fn index_mut(&mut self, index: &ObjectKey) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = std::string::String::new();

        let mut pairs = std::string::String::new();

        for (i, (key, value)) in self.pairs.iter().enumerate() {
            pairs.push_str(&format!(" {}: {}", key, value));

            if i < self.pairs.len() - 1 {
                pairs.push_str(", ");
            }
        }

        output.push_str(&format!("{{{}}}", pairs));

        write!(f, "{}", output)
    }
}

