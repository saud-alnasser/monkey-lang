use super::{utils, Span, Token};
use std::{iter::Peekable, str::Chars};

pub trait Quantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut Span) -> Option<Token>;
}

pub struct WhitespaceQuantifier;

impl Quantifier for WhitespaceQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut Span) -> Option<Token> {
        if let Some(whitespace) = utils::take_series_where(chars, |c| c.is_whitespace()) {
            span.advance(&whitespace);
        }

        None
    }
}

pub struct OperatorsQuantifier;

impl Quantifier for OperatorsQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut Span) -> Option<Token> {
        match chars.peek()? {
            '=' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("==");
                        Some(Token::EQ { span: span.clone() })
                    }
                    _ => {
                        span.advance("=");
                        Some(Token::ASSIGN { span: span.clone() })
                    }
                }
            }
            '+' => {
                chars.next();
                span.advance("+");
                Some(Token::PLUS { span: span.clone() })
            }
            '-' => {
                chars.next();
                span.advance("-");
                Some(Token::MINUS { span: span.clone() })
            }
            '*' => {
                chars.next();
                span.advance("*");
                Some(Token::ASTERISK { span: span.clone() })
            }
            '/' => {
                chars.next();
                span.advance("/");
                Some(Token::SLASH { span: span.clone() })
            }
            '!' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("!=");
                        Some(Token::NEQ { span: span.clone() })
                    }
                    _ => {
                        span.advance("!");
                        Some(Token::BANG { span: span.clone() })
                    }
                }
            }
            '<' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("<=");
                        Some(Token::LTE { span: span.clone() })
                    }
                    _ => {
                        span.advance("<");
                        Some(Token::LT { span: span.clone() })
                    }
                }
            }
            '>' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance(">=");
                        Some(Token::GTE { span: span.clone() })
                    }
                    _ => {
                        span.advance(">");
                        Some(Token::GT { span: span.clone() })
                    }
                }
            }
            _ => None,
        }
    }
}

pub struct DelimitersQuantifier;

impl Quantifier for DelimitersQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut Span) -> Option<Token> {
        match chars.peek()? {
            ',' => {
                chars.next();
                span.advance(",");
                Some(Token::COMMA { span: span.clone() })
            }
            ';' => {
                chars.next();
                span.advance(";");
                Some(Token::SEMICOLON { span: span.clone() })
            }
            _ => None,
        }
    }
}

pub struct BracketsQuantifier;

impl Quantifier for BracketsQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut Span) -> Option<Token> {
        match chars.peek()? {
            '(' => {
                chars.next();
                span.advance("(");
                Some(Token::LPAREN { span: span.clone() })
            }
            ')' => {
                chars.next();
                span.advance(")");
                Some(Token::RPAREN { span: span.clone() })
            }
            '{' => {
                chars.next();
                span.advance("{");
                Some(Token::LBRACE { span: span.clone() })
            }
            '}' => {
                chars.next();
                span.advance("}");
                Some(Token::RBRACE { span: span.clone() })
            }
            _ => None,
        }
    }
}

pub struct LiteralsQuantifier;

impl Quantifier for LiteralsQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut Span) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_digit()) {
            Some(literal) => {
                span.advance(&literal);

                Some(Token::INT {
                    span: span.clone(),
                    value: Box::from(literal),
                })
            }
            None => None,
        }
    }
}

pub struct KeywordAndIdentifiersQuantifier;

impl Quantifier for KeywordAndIdentifiersQuantifier {
    fn process(chars: &mut Peekable<Chars>, span: &mut Span) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_alphanumeric() || *c == '_') {
            Some(keyword) => {
                span.advance(&keyword);

                match &keyword[..] {
                    "let" => Some(Token::LET { span: span.clone() }),
                    "fn" => Some(Token::FUNCTION { span: span.clone() }),
                    "return" => Some(Token::RETURN { span: span.clone() }),
                    "if" => Some(Token::IF { span: span.clone() }),
                    "else" => Some(Token::ELSE { span: span.clone() }),
                    "true" => Some(Token::TRUE { span: span.clone() }),
                    "false" => Some(Token::FALSE { span: span.clone() }),
                    identifier => Some(Token::IDENT {
                        span: span.clone(),
                        value: Box::from(identifier),
                    }),
                }
            }
            None => None,
        }
    }
}
