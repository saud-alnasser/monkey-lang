use std::str::Chars;

use crate::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    current: Option<char>,
    next: Option<char>,
    line: usize,
    column: usize,
    literal: Option<String>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut cursor = Self {
            chars: input.chars(),
            current: None,
            next: None,
            line: 1,
            column: 0,
            literal: None,
        };

        cursor.advance();

        cursor
    }

    pub fn advance(&mut self) {
        if let Some(ch) = self.current {
            match ch {
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                }
                _ => {
                    self.column += 1;
                }
            }

            match self.literal.as_mut() {
                Some(literal) => literal.push(ch),
                None => self.literal = Some(ch.to_string()),
            }
        }

        self.current = match self.next.take() {
            Some(c) => Some(c),
            None => self.chars.next(),
        };

        self.next = self.chars.next();
    }

    pub fn advance_where<F>(&mut self, predicate: F)
    where
        F: Fn(&char) -> bool,
    {
        while let Some(c) = self.current {
            match predicate(&c) {
                true => self.advance(),
                false => break,
            }
        }
    }

    pub fn capture(&mut self, kind: TokenKind) -> Option<Token> {
        if kind == TokenKind::WHITESPACE {
            self.literal.take();

            return None;
        }

        if kind == TokenKind::EOF {
            return Some(Token {
                kind,
                line: self.line,
                column: self.column,
                literal: "\0".into(),
            });
        }

        if let Some(literal) = self.literal.take().map(|literal| literal.into_boxed_str()) {
            return Some(Token {
                kind,
                line: self.line,
                column: self.column - literal.len() + 1,
                literal,
            });
        }

        None
    }

    pub const fn first(&self) -> Option<char> {
        self.current
    }

    pub const fn second(&self) -> Option<char> {
        self.next
    }
}
