#[derive(Debug, PartialEq, Clone)]
pub struct TokenSpan {
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
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
    INT,

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

#[derive(Debug, PartialEq)]
pub struct Token {
    pub span: TokenSpan,
    pub kind: TokenKind,
    pub literal: Box<str>,
}

pub enum Expression {
    IDENT {
        token: Token,
        value: Box<str>,
    },
    INT {
        token: Token,
        value: i64,
    },
    PREFIX {
        token: Token,
        operator: TokenKind,
        right: Box<Expression>,
    },
}

pub enum Statement {
    Let {
        token: Token,
        identifier: Box<str>,
        expression: Expression,
    },
    Return {
        token: Token,
        expression: Expression,
    },
    Expression {
        token: Token,
        expression: Expression,
    },
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}
