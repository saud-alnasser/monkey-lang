use super::{utils, FramedSpan, Token};
use std::{iter::Peekable, str::Chars};

pub trait Quantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut FramedSpan) -> Option<Token>;
}

pub struct WhitespaceQuantifier;

impl Quantifier for WhitespaceQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut FramedSpan) -> Option<Token> {
        if let Some(whitespace) = utils::take_series_where(chars, |c| c.is_whitespace()) {
            span.advance(&whitespace);
        }

        None
    }
}

pub struct OperatorsQuantifier;

impl Quantifier for OperatorsQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut FramedSpan) -> Option<Token> {
        match chars.peek()? {
            '=' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("==");
                        Some(Token::EQ {
                            span: span.capture(),
                        })
                    }
                    _ => {
                        span.advance("=");
                        Some(Token::ASSIGN {
                            span: span.capture(),
                        })
                    }
                }
            }
            '+' => {
                chars.next();
                span.advance("+");
                Some(Token::PLUS {
                    span: span.capture(),
                })
            }
            '-' => {
                chars.next();
                span.advance("-");
                Some(Token::MINUS {
                    span: span.capture(),
                })
            }
            '*' => {
                chars.next();
                span.advance("*");
                Some(Token::ASTERISK {
                    span: span.capture(),
                })
            }
            '/' => {
                chars.next();
                span.advance("/");
                Some(Token::SLASH {
                    span: span.capture(),
                })
            }
            '!' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("!=");
                        Some(Token::NEQ {
                            span: span.capture(),
                        })
                    }
                    _ => {
                        span.advance("!");
                        Some(Token::BANG {
                            span: span.capture(),
                        })
                    }
                }
            }
            '<' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("<=");
                        Some(Token::LTE {
                            span: span.capture(),
                        })
                    }
                    _ => {
                        span.advance("<");
                        Some(Token::LT {
                            span: span.capture(),
                        })
                    }
                }
            }
            '>' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance(">=");
                        Some(Token::GTE {
                            span: span.capture(),
                        })
                    }
                    _ => {
                        span.advance(">");
                        Some(Token::GT {
                            span: span.capture(),
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
    fn process(chars: &mut Peekable<Chars>, span: &mut FramedSpan) -> Option<Token> {
        match chars.peek()? {
            ',' => {
                chars.next();
                span.advance(",");
                Some(Token::COMMA {
                    span: span.capture(),
                })
            }
            ';' => {
                chars.next();
                span.advance(";");
                Some(Token::SEMICOLON {
                    span: span.capture(),
                })
            }
            _ => None,
        }
    }
}

pub struct BracketsQuantifier;

impl Quantifier for BracketsQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut FramedSpan) -> Option<Token> {
        match chars.peek()? {
            '(' => {
                chars.next();
                span.advance("(");
                Some(Token::LPAREN {
                    span: span.capture(),
                })
            }
            ')' => {
                chars.next();
                span.advance(")");
                Some(Token::RPAREN {
                    span: span.capture(),
                })
            }
            '{' => {
                chars.next();
                span.advance("{");
                Some(Token::LBRACE {
                    span: span.capture(),
                })
            }
            '}' => {
                chars.next();
                span.advance("}");
                Some(Token::RBRACE {
                    span: span.capture(),
                })
            }
            _ => None,
        }
    }
}

pub struct LiteralsQuantifier;

impl Quantifier for LiteralsQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut FramedSpan) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_digit()) {
            Some(literal) => {
                span.advance(&literal);

                Some(Token::INT {
                    span: span.capture(),
                    value: Box::from(literal),
                })
            }
            None => None,
        }
    }
}

pub struct KeywordAndIdentifiersQuantifier;

impl Quantifier for KeywordAndIdentifiersQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut FramedSpan) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_alphanumeric() || *c == '_') {
            Some(keyword) => {
                span.advance(&keyword);

                match &keyword[..] {
                    "let" => Some(Token::LET {
                        span: span.capture(),
                    }),
                    "fn" => Some(Token::FUNCTION {
                        span: span.capture(),
                    }),
                    "return" => Some(Token::RETURN {
                        span: span.capture(),
                    }),
                    "if" => Some(Token::IF {
                        span: span.capture(),
                    }),
                    "else" => Some(Token::ELSE {
                        span: span.capture(),
                    }),
                    "true" => Some(Token::TRUE {
                        span: span.capture(),
                    }),
                    "false" => Some(Token::FALSE {
                        span: span.capture(),
                    }),
                    identifier => Some(Token::IDENT {
                        span: span.capture(),
                        value: Box::from(identifier),
                    }),
                }
            }
            None => None,
        }
    }
}
