#[derive(Debug, PartialEq)]
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
}
