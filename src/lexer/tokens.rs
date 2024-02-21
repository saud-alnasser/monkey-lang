#[derive(Debug, PartialEq)]
pub enum Token {
    // special
    ILLEGAL,
    EOF,

    // operators
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    BANG,
    EQ,
    NEQ,
    LT,
    GT,
    LTE,
    GTE,

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
    RETURN,
    IF,
    ELSE,
    TRUE,
    FALSE,
    IDENT(Box<str>),
}
