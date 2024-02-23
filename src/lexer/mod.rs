mod quantifiers;
mod tokens;
mod utils;

use self::quantifiers::*;
use std::{iter::Peekable, str::Chars};
pub use tokens::Token;

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.trim().chars().peekable(),
        }
    }

    pub fn next(&mut self) -> Token {
        // NOTE: order of quantifiers is important.
        // NOTE: direct usage of quantifiers and not using dynamic dispatch is for performance.
        if let Some(_) = self.chars.peek() {
            if let Some(token) = WhitespaceQuantifier::process(&mut self.chars) {
                return token;
            }

            if let Some(token) = OperatorsQuantifier::process(&mut self.chars) {
                return token;
            }

            if let Some(token) = DelimitersQuantifier::process(&mut self.chars) {
                return token;
            }

            if let Some(token) = BracketsQuantifier::process(&mut self.chars) {
                return token;
            }

            if let Some(token) = LiteralsQuantifier::process(&mut self.chars) {
                return token;
            }

            if let Some(token) = KeywordAndIdentifiersQuantifier::process(&mut self.chars) {
                return token;
            }

            return Token::ILLEGAL;
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
        let input = " = + ( )  { } , ; ! - / * < >";

        let tokens = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::LT,
            Token::GT,
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
            Token::LET,
            Token::IDENT("five".into()),
            Token::ASSIGN,
            Token::INT("5".into()),
            Token::SEMICOLON,
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
        let input = "let add = fn(x, y) { x + y; }; add(5, 10);";

        let tokens = vec![
            Token::LET,
            Token::IDENT("add".into()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".into()),
            Token::COMMA,
            Token::IDENT("y".into()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".into()),
            Token::PLUS,
            Token::IDENT("y".into()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::IDENT("add".into()),
            Token::LPAREN,
            Token::INT("5".into()),
            Token::COMMA,
            Token::INT("10".into()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for expected in tokens {
            let actual = lexer.next();
            assert_eq!(actual, expected);
        }
    }
}
