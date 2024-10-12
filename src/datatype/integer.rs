use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Integer {
    value: i64,
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Deref for Integer {
    type Target = i64;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for Integer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
