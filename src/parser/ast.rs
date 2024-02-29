use crate::Token;

pub enum Statement {
    Let {
        token: Token,
        identifier: Box<str>,
        expression: Expression,
    },
}

pub enum Expression {
    INT { value: Box<str> },
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}
