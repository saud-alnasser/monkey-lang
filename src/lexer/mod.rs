mod quantifiers;
mod span;
mod tokens;
mod utils;

use quantifiers::*;
pub use span::*;
pub use tokens::*;

use std::{iter::Peekable, str::Chars};

pub struct Lexer<'a> {
    quantifiers: Vec<Box<dyn Quantifier>>,
    chars: Peekable<Chars<'a>>,
    span: FramedSpan,
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
            span: FramedSpan::new(),
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(_) = self.chars.peek() {
            for quantifier in &self.quantifiers {
                if let Some(token) = quantifier.process(&mut self.chars, &mut self.span) {
                    return token;
                }
            }

            return Token::ILLEGAL {
                span: self.span.capture(),
                literal: match self.chars.next() {
                    Some(c) => c.to_string().into_boxed_str(),
                    None => "\0".into(),
                },
            };
        }

        Token::EOF
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next() {
            Token::EOF => None,
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
            Token::ASSIGN {
                span: Span::new(1, 1, 1),
            },
            Token::PLUS {
                span: Span::new(1, 3, 1),
            },
            Token::LPAREN {
                span: Span::new(1, 5, 1),
            },
            Token::RPAREN {
                span: Span::new(1, 7, 1),
            },
            Token::LBRACE {
                span: Span::new(1, 9, 1),
            },
            Token::RBRACE {
                span: Span::new(1, 11, 1),
            },
            Token::COMMA {
                span: Span::new(1, 13, 1),
            },
            Token::SEMICOLON {
                span: Span::new(1, 15, 1),
            },
            Token::BANG {
                span: Span::new(1, 17, 1),
            },
            Token::MINUS {
                span: Span::new(1, 19, 1),
            },
            Token::SLASH {
                span: Span::new(1, 21, 1),
            },
            Token::ASTERISK {
                span: Span::new(1, 23, 1),
            },
            Token::LT {
                span: Span::new(1, 25, 1),
            },
            Token::GT {
                span: Span::new(1, 27, 1),
            },
            Token::EQ {
                span: Span::new(1, 29, 2),
            },
            Token::NEQ {
                span: Span::new(1, 32, 2),
            },
            Token::LTE {
                span: Span::new(1, 35, 2),
            },
            Token::GTE {
                span: Span::new(1, 38, 2),
            },
            Token::EOF,
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
            Token::LET {
                span: Span::new(1, 1, 3),
            },
            Token::IDENT {
                span: Span::new(1, 5, 4),
                value: "five".into(),
            },
            Token::ASSIGN {
                span: Span::new(1, 10, 1),
            },
            Token::INT {
                span: Span::new(1, 12, 1),
                value: "5".into(),
            },
            Token::SEMICOLON {
                span: Span::new(1, 13, 1),
            },
            Token::EOF,
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
            Token::LET {
                span: Span::new(1, 1, 3),
            },
            Token::IDENT {
                span: Span::new(1, 5, 3),
                value: "add".into(),
            },
            Token::ASSIGN {
                span: Span::new(1, 9, 1),
            },
            Token::FUNCTION {
                span: Span::new(1, 11, 2),
            },
            Token::LPAREN {
                span: Span::new(1, 13, 1),
            },
            Token::IDENT {
                span: Span::new(1, 14, 1),
                value: "x".into(),
            },
            Token::COMMA {
                span: Span::new(1, 15, 1),
            },
            Token::IDENT {
                span: Span::new(1, 17, 1),
                value: "y".into(),
            },
            Token::RPAREN {
                span: Span::new(1, 18, 1),
            },
            Token::LBRACE {
                span: Span::new(1, 20, 1),
            },
            Token::IDENT {
                span: Span::new(1, 22, 1),
                value: "x".into(),
            },
            Token::PLUS {
                span: Span::new(1, 24, 1),
            },
            Token::IDENT {
                span: Span::new(1, 26, 1),
                value: "y".into(),
            },
            Token::SEMICOLON {
                span: Span::new(1, 27, 1),
            },
            Token::RBRACE {
                span: Span::new(1, 29, 1),
            },
            Token::SEMICOLON {
                span: Span::new(1, 30, 1),
            },
            Token::IDENT {
                span: Span::new(2, 1, 3),
                value: "add".into(),
            },
            Token::LPAREN {
                span: Span::new(2, 4, 1),
            },
            Token::INT {
                span: Span::new(2, 5, 1),
                value: "5".into(),
            },
            Token::COMMA {
                span: Span::new(2, 6, 1),
            },
            Token::INT {
                span: Span::new(2, 8, 2),
                value: "10".into(),
            },
            Token::RPAREN {
                span: Span::new(2, 10, 1),
            },
            Token::SEMICOLON {
                span: Span::new(2, 11, 1),
            },
            Token::EOF,
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
            Token::IF {
                span: Span::new(1, 1, 2),
            },
            Token::LPAREN {
                span: Span::new(1, 4, 1),
            },
            Token::INT {
                span: Span::new(1, 5, 1),
                value: "5".into(),
            },
            Token::LT {
                span: Span::new(1, 7, 1),
            },
            Token::INT {
                span: Span::new(1, 9, 2),
                value: "10".into(),
            },
            Token::RPAREN {
                span: Span::new(1, 11, 1),
            },
            Token::LBRACE {
                span: Span::new(1, 13, 1),
            },
            Token::RETURN {
                span: Span::new(1, 15, 6),
            },
            Token::TRUE {
                span: Span::new(1, 22, 4),
            },
            Token::SEMICOLON {
                span: Span::new(1, 26, 1),
            },
            Token::RBRACE {
                span: Span::new(1, 28, 1),
            },
            Token::ELSE {
                span: Span::new(1, 30, 4),
            },
            Token::LBRACE {
                span: Span::new(1, 35, 1),
            },
            Token::RETURN {
                span: Span::new(1, 37, 6),
            },
            Token::FALSE {
                span: Span::new(1, 44, 5),
            },
            Token::SEMICOLON {
                span: Span::new(1, 49, 1),
            },
            Token::RBRACE {
                span: Span::new(1, 51, 1),
            },
        ];

        let mut lexer = Lexer::new(input);

        for expected in tokens {
            let actual = lexer.next();
            assert_eq!(actual, expected);
        }
    }
}
