use std::iter::Peekable;

use crate::{Lexer, Token, TokenKind};

pub enum Statement {
    Let {
        token: Token,
        identifier: Box<str>,
        expression: Expression,
    },
    Return {
        token: Token,
        expression: Expression,
    },
}

pub enum Expression {
    INT { value: Box<str> },
}

impl Expression {
    pub fn process(lexer: &mut Peekable<Lexer>) {}
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
