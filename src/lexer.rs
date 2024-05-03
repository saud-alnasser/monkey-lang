mod quantifiers;
mod utils;

use std::{iter::Peekable, str::Chars};

use crate::{Token, TokenKind, TokenSpan};

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

pub trait LexerQuantifier {
    fn consume(&mut self, chars: &mut Peekable<Chars>, span: &mut SpanTracker) -> Option<Token>;
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    span: SpanTracker,
    quantifiers: Vec<Box<dyn LexerQuantifier>>,
    peeked: Option<Token>,
    exhausted: bool,
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.trim_start().chars().peekable(),
            span: SpanTracker::new(),
            quantifiers: vec![
                Box::new(quantifiers::WhitespaceQuantifier),
                Box::new(quantifiers::OperatorsQuantifier),
                Box::new(quantifiers::DelimitersQuantifier),
                Box::new(quantifiers::BracketsQuantifier),
                Box::new(quantifiers::StringsQuantifier),
                Box::new(quantifiers::IntegersQuantifier),
                Box::new(quantifiers::KeywordsAndIdentifiersQuantifier),
            ],
            peeked: None,
            exhausted: false,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if self.exhausted {
            return None;
        }

        for quantifier in &mut self.quantifiers {
            if let Some(token) = quantifier.consume(&mut self.chars, &mut self.span) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbols() {
        let input = "= + ( ) { } [ ] , ; ! - / * < > == != <= >=";

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
                kind: TokenKind::LBRACKET,
                literal: "[".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 15,
                    length: 1,
                },
                kind: TokenKind::RBRACKET,
                literal: "]".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 17,
                    length: 1,
                },
                kind: TokenKind::COMMA,
                literal: ",".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 19,
                    length: 1,
                },
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 21,
                    length: 1,
                },
                kind: TokenKind::BANG,
                literal: "!".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 23,
                    length: 1,
                },
                kind: TokenKind::MINUS,
                literal: "-".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 25,
                    length: 1,
                },
                kind: TokenKind::SLASH,
                literal: "/".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 27,
                    length: 1,
                },
                kind: TokenKind::ASTERISK,
                literal: "*".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 29,
                    length: 1,
                },
                kind: TokenKind::LT,
                literal: "<".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 31,
                    length: 1,
                },
                kind: TokenKind::GT,
                literal: ">".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 33,
                    length: 2,
                },
                kind: TokenKind::EQ,
                literal: "==".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 36,
                    length: 2,
                },
                kind: TokenKind::NEQ,
                literal: "!=".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 39,
                    length: 2,
                },
                kind: TokenKind::LTE,
                literal: "<=".into(),
            },
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 42,
                    length: 2,
                },
                kind: TokenKind::GTE,
                literal: ">=".into(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for expected in tokens {
            let actual = lexer.next().unwrap();
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
            let actual = lexer.next().unwrap();

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
            let actual = lexer.next().unwrap();
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
            let actual = lexer.next().unwrap();
            assert_eq!(actual, expected);
        }
    }
}
