#[derive(Debug, PartialEq)]
pub enum Token {
    // special
    ILLEGAL,
    EOF,

    // identifiers
    IDENT(Box<str>),
    INT(Box<str>),

    // keywords
    LET,

    // operators
    ASSIGN,
    PLUS,

    // delimiters
    COMMA,
    SEMICOLON,

    // brackets
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
}

impl Token {
    pub fn from_symbol(symbol: &char) -> Token {
        match symbol {
            '=' => Token::ASSIGN,
            '+' => Token::PLUS,
            ',' => Token::COMMA,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            _ => Token::ILLEGAL,
        }
    }

    pub fn from_numeric(num: &str) -> Token {
        Token::INT(num.into())
    }

    pub fn from_literal(ident: &str) -> Token {
        match ident {
            "let" => Token::LET,
            _ => Token::IDENT(ident.into()),
        }
    }
}
