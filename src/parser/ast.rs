use std::{error::Error, iter::Peekable};

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
    Expression {
        token: Token,
        expression: Expression,
    },
}

pub enum Expression {
    IDENT {
        token: Token,
        value: Box<str>,
    },
    INT {
        token: Token,
        value: i64,
    },
    PREFIX {
        token: Token,
        operator: TokenKind,
        right: Box<Expression>,
    },
}

impl Expression {
    pub fn parse(lexer: &mut Peekable<Lexer>) -> Result<Self, Box<dyn Error>> {
        match lexer.peek() {
            Some(token) if token.kind == TokenKind::IDENT => {
                let token = lexer.next().unwrap();
                let literal = token.literal.clone();

                Ok(Expression::IDENT {
                    token: token,
                    value: literal,
                })
            }
            Some(token) if token.kind == TokenKind::INT => {
                let token = lexer.next().unwrap();
                let literal = token.literal.parse::<i64>()?;

                Ok(Expression::INT {
                    token: token,
                    value: literal,
                })
            }
            _ => Err("Expected expression".into()),
        }
    }
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
