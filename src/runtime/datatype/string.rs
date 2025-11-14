use std::{fmt::Display, ops::Deref};

use internment::Intern;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct String {
    value: Intern<std::string::String>,
}

impl String {
    pub fn new(value: Intern<std::string::String>) -> Self {
        Self { value }
    }
}

impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Deref for String {
    type Target = Intern<std::string::String>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

