mod cursor;
mod rules;

use cursor::Cursor;

use crate::{
    lexer::rules::{Rule, RULES},
    Token, TokenKind,
};

#[derive(Debug)]
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    rules: [Rule; RULES.len()],
    peeked: Option<Token>,
    exhausted: bool,
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            cursor: Cursor::new(input),
            rules: RULES,
            peeked: None,
            exhausted: false,
        }
    }

    fn capture(&mut self) -> Option<Token> {
        if self.exhausted {
            return None;
        }

        for rule in &self.rules {
            if let Some(token) = (rule.consume)(&mut self.cursor) {
                return Some(token);
            }
        }

        self.exhausted = true;

        self.cursor.capture(TokenKind::EOF)
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.capture();
        }

        self.peeked.as_ref()
    }

    pub fn next(&mut self) -> Option<Token> {
        match self.peeked.take() {
            Some(token) => Some(token),
            None => self.capture(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}
