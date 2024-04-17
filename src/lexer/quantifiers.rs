use super::{utils, Token, TokenKind, TokenSpanTracker};
use std::{iter::Peekable, str::Chars};

pub trait Quantifier {
    fn process(&self, chars: &mut Peekable<Chars>, tracker: &mut TokenSpanTracker)
        -> Option<Token>;
}

pub struct WhitespaceQuantifier;

impl Quantifier for WhitespaceQuantifier {
    fn process(
        &self,
        chars: &mut Peekable<Chars>,
        tracker: &mut TokenSpanTracker,
    ) -> Option<Token> {
        if let Some(whitespace) = utils::take_series_where(chars, |c| c.is_whitespace()) {
            tracker.advance(&whitespace);
        }

        None
    }
}

pub struct OperatorsQuantifier;

impl Quantifier for OperatorsQuantifier {
    fn process(
        &self,
        chars: &mut Peekable<Chars>,
        tracker: &mut TokenSpanTracker,
    ) -> Option<Token> {
        match chars.peek()? {
            '=' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        tracker.advance("==");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::EQ,
                            literal: "==".into(),
                        })
                    }
                    _ => {
                        tracker.advance("=");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::ASSIGN,
                            literal: "=".into(),
                        })
                    }
                }
            }
            '+' => {
                chars.next();
                tracker.advance("+");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::PLUS,
                    literal: "+".into(),
                })
            }
            '-' => {
                chars.next();
                tracker.advance("-");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::MINUS,
                    literal: "-".into(),
                })
            }
            '*' => {
                chars.next();
                tracker.advance("*");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::ASTERISK,
                    literal: "*".into(),
                })
            }
            '/' => {
                chars.next();
                tracker.advance("/");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::SLASH,
                    literal: "/".into(),
                })
            }
            '!' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        tracker.advance("!=");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::NEQ,
                            literal: "!=".into(),
                        })
                    }
                    _ => {
                        tracker.advance("!");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::BANG,
                            literal: "!".into(),
                        })
                    }
                }
            }
            '<' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        tracker.advance("<=");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::LTE,
                            literal: "<=".into(),
                        })
                    }
                    _ => {
                        tracker.advance("<");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::LT,
                            literal: "<".into(),
                        })
                    }
                }
            }
            '>' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        tracker.advance(">=");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::GTE,
                            literal: ">=".into(),
                        })
                    }
                    _ => {
                        tracker.advance(">");
                        Some(Token {
                            span: tracker.capture(),
                            kind: TokenKind::GT,
                            literal: ">".into(),
                        })
                    }
                }
            }
            _ => None,
        }
    }
}

pub struct DelimitersQuantifier;

impl Quantifier for DelimitersQuantifier {
    fn process(
        &self,
        chars: &mut Peekable<Chars>,
        tracker: &mut TokenSpanTracker,
    ) -> Option<Token> {
        match chars.peek()? {
            ',' => {
                chars.next();
                tracker.advance(",");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::COMMA,
                    literal: ",".into(),
                })
            }
            ';' => {
                chars.next();
                tracker.advance(";");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::SEMICOLON,
                    literal: ";".into(),
                })
            }
            _ => None,
        }
    }
}

pub struct BracketsQuantifier;

impl Quantifier for BracketsQuantifier {
    fn process(
        &self,
        chars: &mut Peekable<Chars>,
        tracker: &mut TokenSpanTracker,
    ) -> Option<Token> {
        match chars.peek()? {
            '(' => {
                chars.next();
                tracker.advance("(");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::LPAREN,
                    literal: "(".into(),
                })
            }
            ')' => {
                chars.next();
                tracker.advance(")");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::RPAREN,
                    literal: ")".into(),
                })
            }
            '{' => {
                chars.next();
                tracker.advance("{");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::LBRACE,
                    literal: "{".into(),
                })
            }
            '}' => {
                chars.next();
                tracker.advance("}");
                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::RBRACE,
                    literal: "}".into(),
                })
            }
            _ => None,
        }
    }
}

pub struct LiteralsQuantifier;

impl Quantifier for LiteralsQuantifier {
    fn process(
        &self,
        chars: &mut Peekable<Chars>,
        tracker: &mut TokenSpanTracker,
    ) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_digit()) {
            Some(literal) => {
                tracker.advance(&literal);

                Some(Token {
                    span: tracker.capture(),
                    kind: TokenKind::INT,
                    literal,
                })
            }
            None => None,
        }
    }
}

pub struct KeywordAndIdentifiersQuantifier;

impl Quantifier for KeywordAndIdentifiersQuantifier {
    fn process(
        &self,
        chars: &mut Peekable<Chars>,
        tracker: &mut TokenSpanTracker,
    ) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_alphanumeric() || *c == '_') {
            Some(keyword) => {
                tracker.advance(&keyword);

                match &keyword[..] {
                    "let" => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::LET,
                        literal: "let".into(),
                    }),
                    "fn" => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::FUNCTION,
                        literal: "fn".into(),
                    }),
                    "return" => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::RETURN,
                        literal: "return".into(),
                    }),
                    "if" => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::IF,
                        literal: "if".into(),
                    }),
                    "else" => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::ELSE,
                        literal: "else".into(),
                    }),
                    "true" => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::TRUE,
                        literal: "true".into(),
                    }),
                    "false" => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::FALSE,
                        literal: "false".into(),
                    }),
                    identifier => Some(Token {
                        span: tracker.capture(),
                        kind: TokenKind::IDENT,
                        literal: identifier.into(),
                    }),
                }
            }
            None => None,
        }
    }
}
