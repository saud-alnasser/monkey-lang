use std::str::Chars;

use crate::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    first: Option<char>,
    second: Option<char>,
    line: usize,
    column: usize,
    literal: Option<String>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut cursor = Self {
            chars: input.chars(),
            first: None,
            second: None,
            line: 1,
            column: 0,
            literal: None,
        };

        cursor.advance();

        cursor
    }

    pub fn advance(&mut self) {
        if let Some(ch) = self.first {
            match ch {
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                }
                _ => {
                    self.column += 1;
                }
            }

            match self.literal.as_mut() {
                Some(literal) => literal.push(ch),
                None => self.literal = Some(ch.to_string()),
            }
        }

        self.first = match self.second.take() {
            Some(c) => Some(c),
            None => self.chars.next(),
        };

        self.second = self.chars.next();
    }

    pub fn advance_where<F>(&mut self, predicate: F)
    where
        F: Fn(&char) -> bool,
    {
        while let Some(c) = self.first {
            match predicate(&c) {
                true => self.advance(),
                false => break,
            }
        }
    }

    pub fn capture(&mut self, kind: TokenKind) -> Option<Token> {
        if kind == TokenKind::WHITESPACE {
            self.literal.take();

            return None;
        }

        if kind == TokenKind::EOF {
            return Some(Token {
                kind,
                line: self.line,
                column: self.column,
                literal: "\0".into(),
            });
        }

        if let Some(literal) = self.literal.take().map(|literal| literal.into_boxed_str()) {
            return Some(Token {
                kind,
                line: self.line,
                column: self.column - literal.len() + 1,
                literal,
            });
        }

        None
    }

    pub const fn first(&self) -> Option<char> {
        self.first
    }

    pub const fn second(&self) -> Option<char> {
        self.second
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    peeked: Option<Token>,
    exhausted: bool,
}

impl Lexer<'_> {
    pub fn new<'a>(input: &'a str) -> Lexer<'a> {
        Lexer {
            cursor: Cursor::new(input),
            peeked: None,
            exhausted: false,
        }
    }

    fn capture(&mut self) -> Option<Token> {
        if self.exhausted {
            return None;
        }

        // consume whitespace
        self.cursor.advance_where(|ch| ch.is_whitespace());
        self.cursor.capture(TokenKind::WHITESPACE);

        // consume eq_operator
        if let Some('=') = self.cursor.first() {
            if let Some('=') = self.cursor.second() {
                self.cursor.advance();
                self.cursor.advance();

                return self.cursor.capture(TokenKind::EQ);
            }
        }

        // consume neq_operator
        if let Some('!') = self.cursor.first() {
            if let Some('=') = self.cursor.second() {
                self.cursor.advance();
                self.cursor.advance();

                return self.cursor.capture(TokenKind::NEQ);
            }
        }

        // consume lte_operator
        if let Some('<') = self.cursor.first() {
            if let Some('=') = self.cursor.second() {
                self.cursor.advance();
                self.cursor.advance();

                return self.cursor.capture(TokenKind::LTE);
            }
        }

        // consume gte_operator
        if let Some('>') = self.cursor.first() {
            if let Some('=') = self.cursor.second() {
                self.cursor.advance();
                self.cursor.advance();

                return self.cursor.capture(TokenKind::GTE);
            }
        }

        // consume assignment_operator
        if let Some('=') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::ASSIGN);
        }

        // consume plus_operator
        if let Some('+') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::PLUS);
        }

        // consume minus_operator
        if let Some('-') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::MINUS);
        }

        // consume asterisk_operator
        if let Some('*') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::ASTERISK);
        }

        // consume slash_operator
        if let Some('/') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::SLASH);
        }

        // consume bang_operator
        if let Some('!') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::BANG);
        }

        // consume gt_operator
        if let Some('>') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::GT);
        }

        // consume lt_operator
        if let Some('<') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::LT);
        }

        // consume comma_delimiter
        if let Some(',') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::COMMA);
        }

        // consume semicolon_delimiter
        if let Some(';') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::SEMICOLON);
        }

        // consume left parentheses
        if let Some('(') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::LPAREN);
        }

        // consume right parentheses
        if let Some(')') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::RPAREN);
        }

        // consume left braces
        if let Some('{') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::LBRACE);
        }

        // consume right braces
        if let Some('}') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::RBRACE);
        }

        // consume left brackets
        if let Some('[') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::LBRACKET);
        }

        // consume right brackets
        if let Some(']') = self.cursor.first() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::RBRACKET);
        }

        // consume string_literal
        if let Some('"') = self.cursor.first() {
            self.cursor.advance();
            self.cursor.advance_where(|c| *c != '"');
            self.cursor.advance();

            return self.cursor.capture(TokenKind::STRING);
        }

        // consume integer_literal
        self.cursor.advance_where(|ch| ch.is_ascii_digit());

        if let Some(token) = self.cursor.capture(TokenKind::INT) {
            return Some(token);
        }

        // consume identifier
        self.cursor
            .advance_where(|ch| ch.is_ascii_alphabetic() || *ch == '_');

        if let Some(mut token) = self.cursor.capture(TokenKind::IDENT) {
            // consume keywords
            if "let" == &*token.literal {
                token.kind = TokenKind::LET;
            }

            if "fn" == &*token.literal {
                token.kind = TokenKind::FUNCTION;
            }

            if "return" == &*token.literal {
                token.kind = TokenKind::RETURN;
            }

            if "if" == &*token.literal {
                token.kind = TokenKind::IF;
            }

            if "else" == &*token.literal {
                token.kind = TokenKind::ELSE;
            }

            if "true" == &*token.literal {
                token.kind = TokenKind::TRUE;
            }

            if "false" == &*token.literal {
                token.kind = TokenKind::FALSE;
            }

            return Some(token);
        }

        self.exhausted = true;

        if self.cursor.first().is_some() {
            self.cursor.advance();

            return self.cursor.capture(TokenKind::ILLEGAL);
        }

        self.cursor.capture(TokenKind::EOF)
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.capture();
        }

        self.peeked.as_ref()
    }

    pub fn next(&mut self) -> Option<Token> {
        match self.peeked.take() {
            Some(token) => Some(token),
            None => self.capture(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn consume_whitespace() {
        let mut lexer = Lexer::new("  \n ");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::EOF);
    }

    #[test]
    fn consume_eq_operator() {
        let mut lexer = Lexer::new("==");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::EQ);
    }

    #[test]
    fn consume_not_eq_operator() {
        let mut lexer = Lexer::new("!=");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::NEQ);
    }

    #[test]
    fn consume_lte_operator() {
        let mut lexer = Lexer::new("<=");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::LTE);
    }

    #[test]
    fn consume_gte_operator() {
        let mut lexer = Lexer::new(">=");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::GTE);
    }

    #[test]
    fn consume_assignment_operator() {
        let mut lexer = Lexer::new("=");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::ASSIGN);
    }

    #[test]
    fn consume_plus_operator() {
        let mut lexer = Lexer::new("+");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::PLUS);
    }

    #[test]
    fn consume_minus_operator() {
        let mut lexer = Lexer::new("-");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::MINUS);
    }

    #[test]
    fn consume_asterisk_operator() {
        let mut lexer = Lexer::new("*");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::ASTERISK);
    }

    #[test]
    fn consume_slash_operator() {
        let mut lexer = Lexer::new("/");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::SLASH);
    }

    #[test]
    fn consume_bang_operator() {
        let mut lexer = Lexer::new("!");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::BANG);
    }

    #[test]
    fn consume_gt_operator() {
        let mut lexer = Lexer::new(">");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::GT);
    }

    #[test]
    fn consume_lt_operator() {
        let mut lexer = Lexer::new("<");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::LT);
    }

    #[test]
    fn consume_comma_delimiter() {
        let mut lexer = Lexer::new(",");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::COMMA);
    }

    #[test]
    fn consume_semicolon_delimiter() {
        let mut lexer = Lexer::new(";");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::SEMICOLON);
    }

    #[test]
    fn consume_parentheses() {
        let mut lexer = Lexer::new("()");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::LPAREN);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::RPAREN);
    }

    #[test]
    fn consume_braces() {
        let mut lexer = Lexer::new("{}");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::LBRACE);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::RBRACE);
    }

    #[test]
    fn consume_brackets() {
        let mut lexer = Lexer::new("[]");

        assert_eq!(lexer.next().unwrap().kind, TokenKind::LBRACKET);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::RBRACKET);
    }

    #[test]
    fn consume_string_literal() {
        let mut lexer = Lexer::new("\"hello, world!\"");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::STRING);
    }

    #[test]
    fn consume_integer_literal() {
        let mut lexer = Lexer::new("123");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::INT);
    }

    #[test]
    fn consume_ident() {
        let mut lexer = Lexer::new("abc");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::IDENT);
    }

    #[test]
    fn consume_let_keyword() {
        let mut lexer = Lexer::new("let");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::LET);
    }

    #[test]
    fn consume_fn_keyword() {
        let mut lexer = Lexer::new("fn");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::FUNCTION);
    }

    #[test]
    fn consume_return_keyword() {
        let mut lexer = Lexer::new("return");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::RETURN);
    }

    #[test]
    fn consume_if_keyword() {
        let mut lexer = Lexer::new("if");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::IF);
    }

    #[test]
    fn consume_else_keyword() {
        let mut lexer = Lexer::new("else");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::ELSE);
    }

    #[test]
    fn consume_true_keyword() {
        let mut lexer = Lexer::new("true");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::TRUE);
    }

    #[test]
    fn consume_false_keyword() {
        let mut lexer = Lexer::new("false");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::FALSE);
    }

    #[test]
    fn consume_illegal_literal() {
        let mut lexer = Lexer::new("$");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::ILLEGAL);
    }

    #[test]
    fn consume_eof() {
        let mut lexer = Lexer::new("");

        let token = lexer.next().unwrap();

        assert_eq!(token.kind, TokenKind::EOF);
    }
}
