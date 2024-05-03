use crate::{lexer::utils, LexerQuantifier, SpanTracker, Token, TokenKind};

pub struct IntegersQuantifier;

impl LexerQuantifier for IntegersQuantifier {
    fn consume(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: &mut SpanTracker,
    ) -> Option<Token> {
        match utils::take_series_where(chars, |c| c.is_ascii_digit()) {
            Some(literal) => {
                span.advance(&literal);

                Some(Token {
                    span: span.capture(),
                    kind: TokenKind::INT,
                    literal,
                })
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SpanTracker, TokenSpan};

    #[test]
    fn consume_integer() {
        let mut quantifier = IntegersQuantifier;
        let mut chars = "123".chars().peekable();
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
        assert_eq!(token.kind, TokenKind::INT);
        assert_eq!(token.literal, "123".into());
    }
}
