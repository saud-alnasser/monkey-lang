use internment::Intern;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Token {
    // special
    ILLEGAL(char),

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
    LBRACKET,
    RBRACKET,

    // keywords
    LET,
    FUNCTION,
    RETURN,
    IF,
    ELSE,
    TRUE,
    FALSE,

    // literals
    INT(i64),
    STRING(Intern<String>),
    IDENTIFIER(Intern<String>),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::ILLEGAL(ch) => write!(f, "{}", ch),
            Token::ASSIGN => write!(f, "="),
            Token::PLUS => write!(f, "+"),
            Token::MINUS => write!(f, "-"),
            Token::ASTERISK => write!(f, "*"),
            Token::SLASH => write!(f, "/"),
            Token::BANG => write!(f, "!"),
            Token::GT => write!(f, ">"),
            Token::LT => write!(f, "<"),
            Token::EQ => write!(f, "=="),
            Token::NEQ => write!(f, "!="),
            Token::LTE => write!(f, "<="),
            Token::GTE => write!(f, ">="),
            Token::COMMA => write!(f, ","),
            Token::COLON => write!(f, ":"),
            Token::SEMICOLON => write!(f, ";"),
            Token::LPAREN => write!(f, "("),
            Token::RPAREN => write!(f, ")"),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),
            Token::LBRACKET => write!(f, "["),
            Token::RBRACKET => write!(f, "]"),
            Token::LET => write!(f, "let"),
            Token::FUNCTION => write!(f, "fn"),
            Token::RETURN => write!(f, "return"),
            Token::IF => write!(f, "if"),
            Token::ELSE => write!(f, "else"),
            Token::TRUE => write!(f, "true"),
            Token::FALSE => write!(f, "false"),
            Token::INT(value) => write!(f, "{}", value),
            Token::STRING(value) => write!(f, "\"{}\"", value),
            Token::IDENTIFIER(value) => write!(f, "{}", value),
        }
    }
}
