use crate::{lexer::utils, LexerQuantifier, SpanTracker, Token, TokenKind};

pub struct KeywordsAndIdentifiersQuantifier;

impl LexerQuantifier for KeywordsAndIdentifiersQuantifier {
    fn consume(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: &mut SpanTracker,
    ) -> Option<Token> {
        if let Some(keyword) =
            utils::take_series_where(chars, |c| c.is_ascii_alphanumeric() || *c == '_')
        {
            span.advance(&keyword);

            return match &keyword[..] {
                "let" => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::LET,
                    literal: "let".into(),
                }),
                "fn" => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::FUNCTION,
                    literal: "fn".into(),
                }),
                "return" => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::RETURN,
                    literal: "return".into(),
                }),
                "if" => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::IF,
                    literal: "if".into(),
                }),
                "else" => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::ELSE,
                    literal: "else".into(),
                }),
                "true" => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::TRUE,
                    literal: "true".into(),
                }),
                "false" => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::FALSE,
                    literal: "false".into(),
                }),
                identifier => Some(Token {
                    span: span.capture(),
                    kind: TokenKind::IDENT,
                    literal: identifier.into(),
                }),
            };
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SpanTracker, TokenSpan};

    #[test]
    fn consume_keyword_fn() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "fn".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 2
            }
        );
        assert_eq!(token.kind, TokenKind::FUNCTION);
        assert_eq!(token.literal, "fn".into());
    }

    #[test]
    fn consume_keyword_let() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "let".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 3
            }
        );
        assert_eq!(token.kind, TokenKind::LET);
        assert_eq!(token.literal, "let".into());
    }

    #[test]
    fn consume_keyword_if() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "if".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 2
            }
        );
        assert_eq!(token.kind, TokenKind::IF);
        assert_eq!(token.literal, "if".into());
    }

    #[test]
    fn consume_keyword_else() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "else".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 4
            }
        );
        assert_eq!(token.kind, TokenKind::ELSE);
        assert_eq!(token.literal, "else".into());
    }

    #[test]
    fn consume_keyword_return() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "return".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 6
            }
        );
        assert_eq!(token.kind, TokenKind::RETURN);
        assert_eq!(token.literal, "return".into());
    }

    #[test]
    fn consume_keyword_true() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "true".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 4
            }
        );
        assert_eq!(token.kind, TokenKind::TRUE);
        assert_eq!(token.literal, "true".into());
    }

    #[test]
    fn consume_keyword_false() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "false".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 5
            }
        );
        assert_eq!(token.kind, TokenKind::FALSE);
        assert_eq!(token.literal, "false".into());
    }

    #[test]
    fn consume_identifier() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "identifier".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 10
            }
        );
        assert_eq!(token.kind, TokenKind::IDENT);
        assert_eq!(token.literal, "identifier".into());
    }

    #[test]
    fn consume_identifier_with_underscores() {
        let mut quantifier = KeywordsAndIdentifiersQuantifier;
        let mut chars = "identifier_with_underscores".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 27
            }
        );
        assert_eq!(token.kind, TokenKind::IDENT);
        assert_eq!(token.literal, "identifier_with_underscores".into());
    }
}
