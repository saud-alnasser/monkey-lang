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
            Some(token) if token.kind == TokenKind::LET => true,
            _ => false,
        }
    }

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>> {
        let token = match lexer.next() {
            Some(token) if token.kind == TokenKind::LET => token,
            _ => return Err("Expected keyword let".into()),
        };

        let identifier = match lexer.next() {
            Some(token) if token.kind == TokenKind::IDENT => token.literal,
            _ => return Err("Expected identifier".into()),
        };

        match lexer.next() {
            Some(token) if token.kind == TokenKind::ASSIGN => (),
            _ => return Err("Expected assignment operator".into()),
        }

        let expression = Expression::parse(lexer)?;

        match lexer.next() {
            Some(token) if token.kind == TokenKind::SEMICOLON => (),
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
            Some(token) if token.kind == TokenKind::RETURN => true,
            _ => false,
        }
    }

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>> {
        let token = match lexer.next() {
            Some(token) if token.kind == TokenKind::RETURN => token,
            _ => return Err("Expected keyword return".into()),
        };

        let expression = Expression::parse(lexer)?;

        match lexer.next() {
            Some(token) if token.kind == TokenKind::SEMICOLON => (),
            _ => return Err("Expected semicolon".into()),
        }

        Ok(Statement::Return { token, expression })
    }
}

pub struct ExpressionStatementQuantifier;

impl Quantifier for ExpressionStatementQuantifier {
    fn is_applicable(&self, lexer: &mut Peekable<Lexer>) -> bool {
        match lexer.peek() {
            Some(token) if token.kind == TokenKind::LET || token.kind == TokenKind::RETURN => false,
            _ => true,
        }
    }

    fn process(&self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Box<dyn Error>> {
        let token = match lexer.next() {
            Some(token) if token.kind != TokenKind::LET && token.kind != TokenKind::RETURN => token,
            _ => return Err("Expected a token".into()),
        };

        let expression = Expression::parse(lexer)?;

        match lexer.next() {
            Some(token) if token.kind == TokenKind::SEMICOLON => (),
            _ => return Err("Expected semicolon".into()),
        }

        Ok(Statement::Expression { token, expression })
    }
}
