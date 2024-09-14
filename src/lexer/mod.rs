mod rules;
mod utils;

use crate::{
    lexer::{
        rules::{Rule, RULES},
        utils::SpanTracker,
    },
    Token, TokenKind,
};
use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    span: SpanTracker,
    rules: Vec<Rule>,
    peeked: Option<Token>,
    exhausted: bool,
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.trim_start().chars().peekable(),
            span: SpanTracker::new(),
            rules: RULES.to_vec(),
            peeked: None,
            exhausted: false,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if self.exhausted {
            return None;
        }

        for rule in &self.rules {
            if let Some(token) = (rule.consume)(&mut self.chars, &mut self.span) {
                return Some(token);
            }
        }

        self.exhausted = true;

        if self.chars.peek().is_some() {
            return Some(Token {
                span: self.span.capture(),
                kind: TokenKind::ILLEGAL,
                literal: match self.chars.next() {
                    Some(c) => c.to_string().into_boxed_str(),
                    None => "\0".into(),
                },
            });
        }

        Some(Token {
            span: self.span.capture(),
            kind: TokenKind::EOF,
            literal: "\0".into(),
        })
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.advance();
        }

        self.peeked.as_ref()
    }

    pub fn next(&mut self) -> Option<Token> {
        match self.peeked.take() {
            Some(token) => Some(token),
            None => self.advance(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}
