use crate::{lexer::utils, LexerQuantifier, SpanTracker, Token};

pub struct WhitespaceQuantifier;

impl LexerQuantifier for WhitespaceQuantifier {
    fn consume(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: &mut SpanTracker,
    ) -> Option<Token> {
        if let Some(whitespace) = utils::take_series_where(chars, |c| c.is_whitespace()) {
            span.advance(&whitespace);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SpanTracker, TokenSpan};

    #[test]
    fn consume_whitespace() {
        let mut quantifier = WhitespaceQuantifier;
        let mut chars = "  \t\n".chars().peekable();
        let mut span = SpanTracker::new();

        quantifier.consume(&mut chars, &mut span);

        assert_eq!(
            span.capture(),
            TokenSpan {
                line: 2,
                column: 1,
                length: 1
            }
        );
    }

    #[test]
    fn consume_no_whitespace() {
        let mut quantifier = WhitespaceQuantifier;
        let mut chars = "abc".chars().peekable();
        let mut span = SpanTracker::new();

        quantifier.consume(&mut chars, &mut span);

        assert_eq!(
            span.capture(),
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
    }
}
