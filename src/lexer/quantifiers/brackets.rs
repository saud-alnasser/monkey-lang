use crate::{LexerQuantifier, SpanTracker, Token, TokenKind};

pub struct BracketsQuantifier;

impl LexerQuantifier for BracketsQuantifier {
    fn consume(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: &mut SpanTracker,
    ) -> Option<Token> {
        match chars.peek()? {
            '(' => {
                chars.next();
                span.advance("(");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::LPAREN,
                    literal: "(".into(),
                })
            }
            ')' => {
                chars.next();
                span.advance(")");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::RPAREN,
                    literal: ")".into(),
                })
            }
            '{' => {
                chars.next();
                span.advance("{");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::LBRACE,
                    literal: "{".into(),
                })
            }
            '}' => {
                chars.next();
                span.advance("}");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::RBRACE,
                    literal: "}".into(),
                })
            }
            '[' => {
                chars.next();
                span.advance("[");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::LBRACKET,
                    literal: "[".into(),
                })
            }
            ']' => {
                chars.next();
                span.advance("]");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::RBRACKET,
                    literal: "]".into(),
                })
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
    fn consume_parentheses() {
        let mut quantifier = BracketsQuantifier;
        let mut chars = "()".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::LPAREN);
        assert_eq!(token.literal, "(".into());

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 2,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::RPAREN);
        assert_eq!(token.literal, ")".into());
    }

    #[test]
    fn consume_braces() {
        let mut quantifier = BracketsQuantifier;
        let mut chars = "{}".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::LBRACE);
        assert_eq!(token.literal, "{".into());

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 2,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::RBRACE);
        assert_eq!(token.literal, "}".into());
    }

    #[test]
    fn consume_brackets() {
        let mut quantifier = BracketsQuantifier;
        let mut chars = "[]".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::LBRACKET);
        assert_eq!(token.literal, "[".into());

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 2,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::RBRACKET);
        assert_eq!(token.literal, "]".into());
    }
}
