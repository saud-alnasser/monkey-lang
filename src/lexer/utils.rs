use crate::TokenSpan;
use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct SpanTracker {
    line: usize,
    column_start: usize,
    column_end: usize,
}

impl SpanTracker {
    pub fn new() -> Self {
        Self {
            line: 1,
            column_start: 0,
            column_end: 0,
        }
    }

    pub fn advance(&mut self, literal: &str) {
        let mut chars = literal.chars().peekable();

        if chars.peek().is_some() {
            self.column_start = self.column_end + 1;
        }

        while let Some(c) = chars.next() {
            match c {
                '\n' => {
                    self.line += 1;
                    self.column_start = 0;
                    self.column_end = 0;
                }
                _ => {
                    self.column_end += 1;
                }
            }
        }
    }

    pub fn capture(&self) -> TokenSpan {
        let line = self.line;

        let column = match self.column_start {
            0 => 1,
            _ => self.column_start,
        };

        let length = match self.column_end >= self.column_start {
            true => self.column_end - self.column_start + 1,
            false => 0,
        };

        TokenSpan {
            line,
            column,
            length,
        }
    }
}

pub fn take_series_where<F>(chars: &mut Peekable<Chars>, predicate: F) -> Option<Box<str>>
where
    F: Fn(&char) -> bool,
{
    let mut result = String::new();

    while let Some(c) = chars.peek() {
        match predicate(c) {
            true => {
                result.push(*c);
                chars.next();
            }
            false => break,
        }
    }

    match result.is_empty() {
        true => None,
        false => Some(result.into_boxed_str()),
    }
}
