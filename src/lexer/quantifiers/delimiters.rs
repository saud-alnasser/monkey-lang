use crate::{LexerQuantifier, SpanTracker, Token, TokenKind};

pub struct DelimitersQuantifier;

impl LexerQuantifier for DelimitersQuantifier {
    fn consume(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: &mut SpanTracker,
    ) -> Option<Token> {
        match chars.peek()? {
            ',' => {
                chars.next();
                span.advance(",");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::COMMA,
                    literal: ",".into(),
                })
            }
            ';' => {
                chars.next();
                span.advance(";");
                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::SEMICOLON,
                    literal: ";".into(),
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
    fn consume_comma() {
        let mut quantifier = DelimitersQuantifier;
        let mut chars = ",".chars().peekable();
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
        assert_eq!(token.kind, TokenKind::COMMA);
        assert_eq!(token.literal, ",".into());
    }

    #[test]
    fn consume_semicolon() {
        let mut quantifier = DelimitersQuantifier;
        let mut chars = ";".chars().peekable();
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
        assert_eq!(token.kind, TokenKind::SEMICOLON);
        assert_eq!(token.literal, ";".into());
    }

    #[test]
    fn consume_no_delimiter() {
        let mut quantifier = DelimitersQuantifier;
        let mut chars = "abc".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span);

        assert_eq!(token, None);
    }
}
