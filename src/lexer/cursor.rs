use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    current: Option<char>,
    next: Option<char>,
    exhausted: bool,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut cursor = Self {
            chars: input.chars(),
            current: None,
            next: None,
            exhausted: false,
        };

        cursor.advance();

        cursor
    }

    pub fn advance(&mut self) {
        if self.exhausted {
            return;
        }

        self.current = match self.next.take() {
            Some(c) => Some(c),
            None => self.chars.next(),
        };

        self.next = self.chars.next();

        if self.current.is_none() {
            self.exhausted = true;
        }
    }

    pub const fn current(&self) -> Option<char> {
        self.current
    }

    pub fn next(&self) -> Option<char> {
        self.next
    }
}
