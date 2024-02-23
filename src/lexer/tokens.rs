#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum Token {
    // special
    ILLEGAL,
    EOF,

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

    // literals
    INT(Box<str>),

    // keywords & identifiers
    LET,
    FUNCTION,
    IDENT(Box<str>),
}
