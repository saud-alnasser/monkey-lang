#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    line: usize,
    column: usize,
    length: usize,
}

impl Span {
    pub fn new(line: usize, column: usize, length: usize) -> Self {
        Self {
            line,
            column,
            length,
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn length(&self) -> usize {
        self.length
    }
}

#[derive(Debug)]
pub struct FramedSpan {
    line: usize,
    column_start: usize,
    column_end: usize,
}

impl FramedSpan {
    pub fn new() -> Self {
        Self {
            line: 1,
            column_start: 0,
            column_end: 0,
        }
    }

    pub fn advance(&mut self, literal: &str) {
        let mut chars = literal.chars().peekable();

        if let Some(_) = chars.peek() {
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

    pub fn capture(&self) -> Span {
        let line = self.line;

        let column = match self.column_start {
            0 => 1,
            _ => self.column_start,
        };

        let length = match self.column_end >= self.column_start {
            true => self.column_end - self.column_start + 1,
            false => 0,
        };

        Span::new(line, column, length)
    }
}
