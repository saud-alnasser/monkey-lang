use crate::{LexerQuantifier, SpanTracker, Token, TokenKind};

pub struct OperatorsQuantifier;

impl LexerQuantifier for OperatorsQuantifier {
    fn consume(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: &mut SpanTracker,
    ) -> Option<Token> {
        match chars.peek()? {
            '=' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("==");
                        Some(Token {
                            span: span.capture(),
                            kind: TokenKind::EQ,
                            literal: "==".into(),
                        })
                    }
                    _ => {
                        span.advance("=");
                        Some(Token {
                            span: span.capture(),
                            kind: TokenKind::ASSIGN,
                            literal: "=".into(),
                        })
                    }
                }
            }
            '+' => {
                chars.next();
                span.advance("+");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::PLUS,
                    literal: "+".into(),
                })
            }
            '-' => {
                chars.next();
                span.advance("-");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::MINUS,
                    literal: "-".into(),
                })
            }
            '*' => {
                chars.next();
                span.advance("*");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::ASTERISK,
                    literal: "*".into(),
                })
            }
            '/' => {
                chars.next();
                span.advance("/");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::SLASH,
                    literal: "/".into(),
                })
            }
            '!' => {
                chars.next();
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        span.advance("!=");
                        Some(Token {
                            span: span.capture(),
                            kind: TokenKind::NEQ,
                            literal: "!=".into(),
                        })
                    }
                    _ => {
                        span.advance("!");
                        Some(Token {
                            span: span.capture(),
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
                        span.advance("<=");
                        Some(Token {
                            span: span.capture(),
                            kind: TokenKind::LTE,
                            literal: "<=".into(),
                        })
                    }
                    _ => {
                        span.advance("<");
                        Some(Token {
                            span: span.capture(),
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
                        span.advance(">=");
                        Some(Token {
                            span: span.capture(),
                            kind: TokenKind::GTE,
                            literal: ">=".into(),
                        })
                    }
                    _ => {
                        span.advance(">");
                        Some(Token {
                            span: span.capture(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SpanTracker, TokenSpan};

    #[test]
    fn consume_assign() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "=".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::ASSIGN,
                literal: "=".into()
            }
        );
    }

    #[test]
    fn consume_eq() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "==".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 2
                },
                kind: TokenKind::EQ,
                literal: "==".into()
            }
        );
    }

    #[test]
    fn consume_plus() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "+".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::PLUS,
                literal: "+".into()
            }
        );
    }

    #[test]
    fn consume_minus() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "-".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::MINUS,
                literal: "-".into()
            }
        );
    }

    #[test]
    fn consume_asterisk() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "*".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::ASTERISK,
                literal: "*".into()
            }
        );
    }

    #[test]
    fn consume_slash() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "/".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::SLASH,
                literal: "/".into()
            }
        );
    }

    #[test]
    fn consume_bang() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "!".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::BANG,
                literal: "!".into()
            }
        );
    }

    #[test]
    fn consume_neq() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "!=".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 2
                },
                kind: TokenKind::NEQ,
                literal: "!=".into()
            }
        );
    }

    #[test]
    fn consume_lt() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "<".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::LT,
                literal: "<".into()
            }
        );
    }

    #[test]
    fn consume_lte() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "<=".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 2
                },
                kind: TokenKind::LTE,
                literal: "<=".into()
            }
        );
    }

    #[test]
    fn consume_gt() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = ">".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 1
                },
                kind: TokenKind::GT,
                literal: ">".into()
            }
        );
    }

    #[test]
    fn consume_gte() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = ">=".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token,
            Token {
                span: TokenSpan {
                    line: 1,
                    column: 1,
                    length: 2
                },
                kind: TokenKind::GTE,
                literal: ">=".into()
            }
        );
    }

    #[test]
    fn consume_no_operator() {
        let mut quantifier = OperatorsQuantifier;
        let mut chars = "a".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span);

        assert_eq!(token, None);
    }
}
