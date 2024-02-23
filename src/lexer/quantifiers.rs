use super::{tokens::Token, utils};
use std::{iter::Peekable, str::Chars};

pub trait Quantifier {
    fn process(chars: &mut Peekable<Chars>) -> Option<Token>;
}

pub struct WhitespaceQuantifier;

impl Quantifier for WhitespaceQuantifier {
    fn process(chars: &mut Peekable<Chars>) -> Option<Token> {
        while let Some(c) = chars.peek() {
            if !c.is_ascii_whitespace() {
                break;
            }

            chars.next();
        }

        None
    }
}

pub struct OperatorsQuantifier;

impl Quantifier for OperatorsQuantifier {
    fn process(chars: &mut Peekable<Chars>) -> Option<Token> {
        let token = match chars.peek()? {
            '=' => Some(Token::ASSIGN),
            '+' => Some(Token::PLUS),
            _ => None,
        };

        if token.is_some() {
            chars.next();
        }

        token
    }
}

pub struct DelimitersQuantifier;

impl Quantifier for DelimitersQuantifier {
    fn process(chars: &mut Peekable<Chars>) -> Option<Token> {
        let token = match chars.peek()? {
            ',' => Some(Token::COMMA),
            ';' => Some(Token::SEMICOLON),
            _ => None,
        };

        if token.is_some() {
            chars.next();
        }

        token
    }
}

pub struct BracketsQuantifier;

impl Quantifier for BracketsQuantifier {
    fn process(chars: &mut Peekable<Chars>) -> Option<Token> {
        let token = match chars.peek()? {
            '(' => Some(Token::LPAREN),
            ')' => Some(Token::RPAREN),
            '{' => Some(Token::LBRACE),
            '}' => Some(Token::RBRACE),
            _ => None,
        };

        if token.is_some() {
            chars.next();
        }

        token
    }
}

pub struct LiteralsQuantifier;

impl Quantifier for LiteralsQuantifier {
    fn process(chars: &mut Peekable<Chars>) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_digit()) {
            Some(literal) => Some(Token::INT(literal)),
            None => None,
        }
    }
}

pub struct KeywordAndIdentifiersQuantifier;

impl Quantifier for KeywordAndIdentifiersQuantifier {
    fn process(chars: &mut Peekable<Chars>) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_alphanumeric() || *c == '_') {
            Some(keyword) => match &keyword[..] {
                "fn" => Some(Token::FUNCTION),
                "let" => Some(Token::LET),
                identifier => Some(Token::IDENT(Box::from(identifier))),
            },
            None => None,
        }
    }
}
