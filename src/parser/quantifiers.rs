use std::{error::Error, iter::Peekable};

use crate::{Lexer, Token};

use super::{Expression, Statement};

pub trait Quantifier {
    fn is_applicable(&self, lexer: &mut Peekable<Lexer>) -> bool;

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>>;
}

pub struct LetStatementQuantifier;

impl Quantifier for LetStatementQuantifier {
    fn is_applicable(&self, lexer: &mut Peekable<Lexer>) -> bool {
        match lexer.peek() {
            Some(Token::LET { .. }) => true,
            _ => false,
        }
    }

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>> {
        let token = match lexer.next() {
            Some(token) => token,
            None => return Err("Expected keyword let".into()),
        };

        let identifier = match lexer.next() {
            Some(Token::IDENT { value, .. }) => value,
            _ => return Err("Expected identifier".into()),
        };

        match lexer.next() {
            Some(Token::ASSIGN { .. }) => (),
            _ => return Err("Expected assignment operator".into()),
        }

        // TODO: manage all kinds of expressions

        let expression = match lexer.next() {
            Some(Token::INT { value, .. }) => Expression::INT { value },
            _ => return Err("Expected expression".into()),
        };

        match lexer.next() {
            Some(Token::SEMICOLON { .. }) => (),
            _ => return Err("Expected semicolon".into()),
        }

        Ok(Statement::Let {
            token,
            identifier,
            expression,
        })
    }
}
