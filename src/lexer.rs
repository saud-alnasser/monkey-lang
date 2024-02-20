use std::{iter::Peekable, str::Chars};

use crate::token::{self, Token};

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.trim().chars().peekable(),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if c.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    pub fn next(&mut self) -> Token {
        self.skip_whitespace();

        while let Some(c) = self.chars.next() {
            let token = Token::from_symbol(&c);

            if token == Token::ILLEGAL {
                if c.is_digit(10) {
                    let mut num = String::new();

                    num.push(c);

                    while let Some(c) = self.chars.peek() {
                        if c.is_digit(10) {
                            num.push(*c);
                            self.chars.next();
                        } else {
                            break;
                        }
                    }

                    return Token::from_numeric(&num);
                }

                if c.is_alphabetic() || c == '_' {
                    let mut ident = String::new();
                    ident.push(c);

                    while let Some(c) = self.chars.peek() {
                        if c.is_alphanumeric() {
                            ident.push(*c);
                            self.chars.next();
                        } else {
                            break;
                        }
                    }

                    return Token::from_literal(&ident);
                }
            }

            return token;
        }

        Token::EOF
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = " = + ( )  { } , ; ";

        let tokens = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
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
}
