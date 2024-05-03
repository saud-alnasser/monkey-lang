use crate::{lexer::utils, LexerQuantifier, SpanTracker, Token, TokenKind};

pub struct StringsQuantifier;

impl LexerQuantifier for StringsQuantifier {
    fn consume(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: &mut SpanTracker,
    ) -> Option<Token> {
        if let Some('"') = chars.peek() {
            let literal = {
                chars.next();
                span.advance("\"");

                let literal = utils::take_series_where(chars, |c| *c != '"').unwrap_or_default();
                span.advance(&literal);

                chars.next();
                span.advance("\"");

                literal
            };

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::STRING,
                literal,
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SpanTracker, TokenSpan};

    #[test]
    fn consume_string() {
        let mut quantifier = StringsQuantifier;
        let mut chars = "\"hello, world!\"".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 15,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::STRING);
        assert_eq!(token.literal, "hello, world!".into());
    }

    #[test]
    fn consume_no_string() {
        let mut quantifier = StringsQuantifier;
        let mut chars = "abc".chars().peekable();
        let mut span = SpanTracker::new();

        let token = quantifier.consume(&mut chars, &mut span);

        assert_eq!(token, None);
    }
}
