mod quantifiers;
mod token;
mod utils;

use quantifiers::*;
pub use token::*;

use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct TokenSpanTracker {
    line: usize,
    column_start: usize,
    column_end: usize,
}

impl TokenSpanTracker {
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

pub struct Lexer<'a> {
    quantifiers: Vec<Box<dyn Quantifier>>,
    chars: Peekable<Chars<'a>>,
    tracker: TokenSpanTracker,
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            // NOTE: order of quantifiers is important
            quantifiers: vec![
                Box::new(WhitespaceQuantifier),
                Box::new(OperatorsQuantifier),
                Box::new(DelimitersQuantifier),
                Box::new(BracketsQuantifier),
                Box::new(LiteralsQuantifier),
                Box::new(KeywordAndIdentifiersQuantifier),
            ],
            chars: input.trim().chars().peekable(),
            tracker: TokenSpanTracker::new(),
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(_) = self.chars.peek() {
            for quantifier in &self.quantifiers {
                if let Some(token) = quantifier.process(&mut self.chars, &mut self.tracker) {
                    return token;
                }
            }

            return Token {
                span: self.tracker.capture(),
                kind: TokenKind::ILLEGAL,
                literal: match self.chars.next() {
                    Some(c) => c.to_string().into_boxed_str(),
                    None => "\0".into(),
                },
            };
        }

        Token {
            span: self.tracker.capture(),
            kind: TokenKind::EOF,
            literal: "\0".into(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next() {
            Token {
                kind: TokenKind::EOF,
                ..
            } => None,
            token => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbols() {
        let input = "= + ( ) { } , ; ! - / * < > == != <= >=";

        let tokens = vec![
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1,
                },
                kind: TokenKind::ASSIGN,
                literal: "=".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 3,
                    length: 1,
                },
                kind: TokenKind::PLUS,
                literal: "+".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 5,
                    length: 1,
                },
                kind: TokenKind::LPAREN,
                literal: "(".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 7,
                    length: 1,
                },
                kind: TokenKind::RPAREN,
                literal: ")".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 9,
                    length: 1,
                },
                kind: TokenKind::LBRACE,
                literal: "{".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 11,
                    length: 1,
                },
                kind: TokenKind::RBRACE,
                literal: "}".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 13,
                    length: 1,
                },
                kind: TokenKind::COMMA,
                literal: ",".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 15,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 17,
                    length: 1,
                },
                kind: TokenKind::BANG,
                literal: "!".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 19,
                    length: 1,
                },
                kind: TokenKind::MINUS,
                literal: "-".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 21,
                    length: 1,
                },
                kind: TokenKind::SLASH,
                literal: "/".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 23,
                    length: 1,
                },
                kind: TokenKind::ASTERISK,
                literal: "*".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 25,
                    length: 1,
                },
                kind: TokenKind::LT,
                literal: "<".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 27,
                    length: 1,
                },
                kind: TokenKind::GT,
                literal: ">".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 29,
                    length: 2,
                },
                kind: TokenKind::EQ,
                literal: "==".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 32,
                    length: 2,
                },
                kind: TokenKind::NEQ,
                literal: "!=".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 35,
                    length: 2,
                },
                kind: TokenKind::LTE,
                literal: "<=".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 38,
                    length: 2,
                },
                kind: TokenKind::GTE,
                literal: ">=".into(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for expected in tokens {
            let actual = lexer.next();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_let_statement() {
        let input = "let five = 5;";

        let tokens = vec![
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 3,
                },
                kind: TokenKind::LET,
                literal: "let".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 5,
                    length: 4,
                },
                kind: TokenKind::IDENT,
                literal: "five".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 10,
                    length: 1,
                },
                kind: TokenKind::ASSIGN,
                literal: "=".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 12,
                    length: 1,
                },
                kind: TokenKind::INT,
                literal: "5".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 13,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for expected in tokens {
            let actual = lexer.next();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_function_statement() {
        let input = "let add = fn(x, y) { x + y; };\nadd(5, 10);";

        let tokens = vec![
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 3,
                },
                kind: TokenKind::LET,
                literal: "let".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 5,
                    length: 3,
                },
                kind: TokenKind::IDENT,
                literal: "add".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 9,
                    length: 1,
                },
                kind: TokenKind::ASSIGN,
                literal: "=".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 11,
                    length: 2,
                },
                kind: TokenKind::FUNCTION,
                literal: "fn".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 13,
                    length: 1,
                },
                kind: TokenKind::LPAREN,
                literal: "(".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 14,
                    length: 1,
                },
                kind: TokenKind::IDENT,
                literal: "x".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 15,
                    length: 1,
                },
                kind: TokenKind::COMMA,
                literal: ",".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 17,
                    length: 1,
                },
                kind: TokenKind::IDENT,
                literal: "y".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 18,
                    length: 1,
                },
                kind: TokenKind::RPAREN,
                literal: ")".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 20,
                    length: 1,
                },
                kind: TokenKind::LBRACE,
                literal: "{".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 22,
                    length: 1,
                },
                kind: TokenKind::IDENT,
                literal: "x".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 24,
                    length: 1,
                },
                kind: TokenKind::PLUS,
                literal: "+".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 26,
                    length: 1,
                },
                kind: TokenKind::IDENT,
                literal: "y".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 27,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 29,
                    length: 1,
                },
                kind: TokenKind::RBRACE,
                literal: "}".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 30,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
            Token {
                span: TokenSpan {
                    line: 2,
                    column: 1,
                    length: 3,
                },
                kind: TokenKind::IDENT,
                literal: "add".into(),
            },
            Token {
                span: TokenSpan {
                    line: 2,
                    column: 4,
                    length: 1,
                },
                kind: TokenKind::LPAREN,
                literal: "(".into(),
            },
            Token {
                span: TokenSpan {
                    line: 2,
                    column: 5,
                    length: 1,
                },
                kind: TokenKind::INT,
                literal: "5".into(),
            },
            Token {
                span: TokenSpan {
                    line: 2,
                    column: 6,
                    length: 1,
                },
                kind: TokenKind::COMMA,
                literal: ",".into(),
            },
            Token {
                span: TokenSpan {
                    line: 2,
                    column: 8,
                    length: 2,
                },
                kind: TokenKind::INT,
                literal: "10".into(),
            },
            Token {
                span: TokenSpan {
                    line: 2,
                    column: 10,
                    length: 1,
                },
                kind: TokenKind::RPAREN,
                literal: ")".into(),
            },
            Token {
                span: TokenSpan {
                    line: 2,
                    column: 11,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for expected in tokens {
            let actual = lexer.next();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_conditional_statement() {
        let input = "if (5 < 10) { return true; } else { return false; }";

        let tokens = vec![
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 2,
                },
                kind: TokenKind::IF,
                literal: "if".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 4,
                    length: 1,
                },
                kind: TokenKind::LPAREN,
                literal: "(".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 5,
                    length: 1,
                },
                kind: TokenKind::INT,
                literal: "5".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 7,
                    length: 1,
                },
                kind: TokenKind::LT,
                literal: "<".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 9,
                    length: 2,
                },
                kind: TokenKind::INT,
                literal: "10".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 11,
                    length: 1,
                },
                kind: TokenKind::RPAREN,
                literal: ")".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 13,
                    length: 1,
                },
                kind: TokenKind::LBRACE,
                literal: "{".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 15,
                    length: 6,
                },
                kind: TokenKind::RETURN,
                literal: "return".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 22,
                    length: 4,
                },
                kind: TokenKind::TRUE,
                literal: "true".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 26,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 28,
                    length: 1,
                },
                kind: TokenKind::RBRACE,
                literal: "}".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 30,
                    length: 4,
                },
                kind: TokenKind::ELSE,
                literal: "else".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 35,
                    length: 1,
                },
                kind: TokenKind::LBRACE,
                literal: "{".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 37,
                    length: 6,
                },
                kind: TokenKind::RETURN,
                literal: "return".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 44,
                    length: 5,
                },
                kind: TokenKind::FALSE,
                literal: "false".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 49,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 51,
                    length: 1,
                },
                kind: TokenKind::RBRACE,
                literal: "}".into(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for expected in tokens {
            let actual = lexer.next();
            assert_eq!(actual, expected);
        }
    }
}
