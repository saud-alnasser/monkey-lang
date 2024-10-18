use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Boolean {
    value: bool,
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Deref for Boolean {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for Boolean {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
