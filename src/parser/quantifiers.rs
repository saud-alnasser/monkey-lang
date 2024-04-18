use std::{error::Error, iter::Peekable};

use crate::{lexer::TokenKind, Lexer, Token};

use super::{Expression, Statement};

pub trait Quantifier {
    fn is_applicable(&self, lexer: &mut Peekable<Lexer>) -> bool;

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>>;
}

pub struct LetStatementQuantifier;

impl Quantifier for LetStatementQuantifier {
    fn is_applicable(&self, lexer: &mut Peekable<Lexer>) -> bool {
        match lexer.peek() {
            Some(Token {
                kind: TokenKind::LET,
                ..
            }) => true,
            _ => false,
        }
    }

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>> {
        let token = match lexer.next() {
            Some(token) => token,
            None => return Err("Expected keyword let".into()),
        };

        let identifier = match lexer.next() {
            Some(Token {
                kind: TokenKind::IDENT,
                literal,
                ..
            }) => literal,
            _ => return Err("Expected identifier".into()),
        };

        match lexer.next() {
            Some(Token {
                kind: TokenKind::ASSIGN,
                ..
            }) => (),
            _ => return Err("Expected assignment operator".into()),
        }

        // TODO: manage all kinds of expressions

        let expression = match lexer.next() {
            Some(Token {
                kind: TokenKind::INT,
                literal,
                ..
            }) => Expression::INT { value: literal },
            _ => return Err("Expected expression".into()),
        };

        match lexer.next() {
            Some(Token {
                kind: TokenKind::SEMICOLON,
                ..
            }) => (),
            _ => return Err("Expected semicolon".into()),
        }

        Ok(Statement::Let {
            token,
            identifier,
            expression,
        })
    }
}

pub struct ReturnStatementQuantifier;

impl Quantifier for ReturnStatementQuantifier {
    fn is_applicable(&self, lexer: &mut Peekable<Lexer>) -> bool {
        match lexer.peek() {
            Some(Token {
                kind: TokenKind::RETURN,
                ..
            }) => true,
            _ => false,
        }
    }

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>> {
        let token = match lexer.next() {
            Some(token) => token,
            None => return Err("Expected keyword return".into()),
        };

        // TODO: manage all kinds of expressions

        let expression = match lexer.next() {
            Some(Token {
                kind: TokenKind::INT,
                literal,
                ..
            }) => Expression::INT { value: literal },
            _ => return Err("Expected expression".into()),
        };

        match lexer.next() {
            Some(Token {
                kind: TokenKind::SEMICOLON,
                ..
            }) => (),
            _ => return Err("Expected semicolon".into()),
        }

        Ok(Statement::Return { token, expression })
    }
}
