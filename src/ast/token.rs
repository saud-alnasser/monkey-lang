#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // special
    WHITESPACE,
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
    COLON,
    SEMICOLON,

    // brackets
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    RBRACKET,
    LBRACKET,

    // literals
    INT,
    STRING,

    // keywords & identifiers
    LET,
    FUNCTION,
    RETURN,
    IF,
    ELSE,
    TRUE,
    FALSE,
    IDENT,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
    pub literal: Box<str>,
}
