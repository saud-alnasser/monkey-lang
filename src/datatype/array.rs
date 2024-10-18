use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use super::DataType;

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    elements: Vec<DataType>,
}

impl Array {
    pub fn new(elements: Vec<DataType>) -> Self {
        Self { elements }
    }
}

impl Default for Array {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl Deref for Array {
    type Target = Vec<DataType>;

    fn deref(&self) -> &Self::Target {
        &self.elements
    }
}

impl DerefMut for Array {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.elements
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = std::string::String::new();

        let mut elements = std::string::String::new();

        for (i, element) in self.elements.iter().enumerate() {
            elements.push_str(&format!("{}", element));

            if i < self.elements.len() - 1 {
                elements.push_str(", ");
            }
        }

        output.push_str(&format!("[{}]", elements));

        write!(f, "{}", output)
    }
}
