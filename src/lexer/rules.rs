use super::{
    utils::{self, SpanTracker},
    Cursor,
};
use crate::{Token, TokenKind};

#[derive(Debug, PartialEq, Clone)]
pub struct Rule {
    pub kind: TokenKind,
    pub consume: fn(&mut Cursor, &mut SpanTracker) -> Option<Token>,
}

// order matters
// should be from most specific (more symbols) to least specific (less symbols)
// with taking in account of the type of token.
pub const RULES: [Rule; 28] = [
    // whitespace
    WHITESPACE_RULE,
    // operators with more than one symbol
    EQ_OPERATOR_RULE,
    NEQ_OPERATOR_RULE,
    LTE_OPERATOR_RULE,
    GTE_OPERATOR_RULE,
    // operators with one symbol
    ASSIGN_OPERATOR_RULE,
    PLUS_OPERATOR_RULE,
    MINUS_OPERATOR_RULE,
    ASTERISK_OPERATOR_RULE,
    SLASH_OPERATOR_RULE,
    BANG_OPERATOR_RULE,
    GT_OPERATOR_RULE,
    LT_OPERATOR_RULE,
    // delimiters
    COMMA_DELIMITER_RULE,
    SEMICOLON_DELIMITER_RULE,
    // grouping
    PARENTHESES_RULE,
    BRACES_RULE,
    BRACKETS_RULE,
    // literals
    STRINGS_RULE,
    INTEGERS_RULE,
    // keywords
    LET_KEYWORD_RULE,
    FUNCTION_KEYWORD_RULE,
    RETURN_KEYWORD_RULE,
    IF_KEYWORD_RULE,
    ELSE_KEYWORD_RULE,
    TRUE_KEYWORD_RULE,
    FALSE_KEYWORD_RULE,
    // identifier
    IDENTIFIER_RULE,
];

const WHITESPACE_RULE: Rule = Rule {
    kind: TokenKind::WHITESPACE,
    consume: |cursor, span| {
        if let Some(whitespace) = utils::take_series_where(cursor, |c| c.is_whitespace()) {
            span.advance(&whitespace);
        }

        None
    },
};

#[cfg(test)]
mod whitespace_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_whitespace() {
        let mut cursor = Cursor::new("  \t\n");
        let mut span = SpanTracker::new();

        (WHITESPACE_RULE.consume)(&mut cursor, &mut span);

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
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        (WHITESPACE_RULE.consume)(&mut cursor, &mut span);

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

const ASSIGN_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::ASSIGN,
    consume: |cursor, span| {
        if let '=' = cursor.current()? {
            cursor.advance();
            span.advance("=");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::ASSIGN,
                literal: "=".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod assign_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_assign_operator() {
        let mut cursor = Cursor::new("=");
        let mut span = SpanTracker::new();

        let token = (ASSIGN_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::ASSIGN);
        assert_eq!(token.literal, "=".into());
    }

    #[test]
    fn consume_no_assign_operator() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (ASSIGN_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const PLUS_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::PLUS,
    consume: |cursor, span| {
        if let '+' = cursor.current()? {
            cursor.advance();
            span.advance("+");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::PLUS,
                literal: "+".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod plus_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_plus() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (PLUS_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::PLUS);
        assert_eq!(token.literal, "+".into());
    }

    #[test]
    fn consume_no_plus() {
        let mut cursor = Cursor::new("=");
        let mut span = SpanTracker::new();

        let token = (PLUS_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const MINUS_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::MINUS,
    consume: |cursor, span| {
        if let '-' = cursor.current()? {
            cursor.advance();
            span.advance("-");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::MINUS,
                literal: "-".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod minus_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_minus() {
        let mut cursor = Cursor::new("-");
        let mut span = SpanTracker::new();

        let token = (MINUS_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::MINUS);
        assert_eq!(token.literal, "-".into());
    }

    #[test]
    fn consume_no_minus() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (MINUS_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const ASTERISK_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::ASTERISK,
    consume: |cursor, span| {
        if let '*' = cursor.current()? {
            cursor.advance();
            span.advance("*");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::ASTERISK,
                literal: "*".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod asterisk_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_asterisk() {
        let mut cursor = Cursor::new("*");
        let mut span = SpanTracker::new();

        let token = (ASTERISK_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::ASTERISK);
        assert_eq!(token.literal, "*".into());
    }

    #[test]
    fn consume_no_asterisk() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (ASTERISK_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const SLASH_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::SLASH,
    consume: |chars, span| {
        if let '/' = chars.current()? {
            chars.advance();
            span.advance("/");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::SLASH,
                literal: "/".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod slash_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_slash() {
        let mut cursor = Cursor::new("/");
        let mut span = SpanTracker::new();

        let token = (SLASH_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::SLASH);
        assert_eq!(token.literal, "/".into());
    }

    #[test]
    fn consume_no_slash() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (SLASH_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const BANG_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::BANG,
    consume: |cursor, span| {
        if let '!' = cursor.current()? {
            cursor.advance();
            span.advance("!");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::BANG,
                literal: "!".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod bang_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_bang() {
        let mut cursor = Cursor::new("!");
        let mut span = SpanTracker::new();

        let token = (BANG_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::BANG);
        assert_eq!(token.literal, "!".into());
    }

    #[test]
    fn consume_no_bang() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (BANG_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const GT_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::GT,
    consume: |cursor, span| {
        if let '>' = cursor.current()? {
            cursor.advance();
            span.advance(">");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::GT,
                literal: ">".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod gt_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_gt() {
        let mut cursor = Cursor::new(">");
        let mut span = SpanTracker::new();

        let token = (GT_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::GT);
        assert_eq!(token.literal, ">".into());
    }

    #[test]
    fn consume_no_gt() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (GT_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const LT_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::LT,
    consume: |cursor, span| {
        if let '<' = cursor.current()? {
            cursor.advance();
            span.advance("<");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::LT,
                literal: "<".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod lt_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_lt() {
        let mut cursor = Cursor::new("<");
        let mut span = SpanTracker::new();

        let token = (LT_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 1
            }
        );
        assert_eq!(token.kind, TokenKind::LT);
        assert_eq!(token.literal, "<".into());
    }

    #[test]
    fn consume_no_lt() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (LT_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const EQ_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::EQ,
    consume: |cursor, span| {
        if let '=' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                span.advance("==");

                return Some(Token {
                    span: span.capture(),
                    kind: TokenKind::EQ,
                    literal: "==".into(),
                });
            }
        }

        None
    },
};

#[cfg(test)]
mod eq_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_eq() {
        let mut cursor = Cursor::new("==");
        let mut span = SpanTracker::new();

        let token = (EQ_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 2
            }
        );
        assert_eq!(token.kind, TokenKind::EQ);
        assert_eq!(token.literal, "==".into());
    }

    #[test]
    fn consume_no_eq() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (EQ_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const NEQ_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::NEQ,
    consume: |cursor, span| {
        if let '!' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                span.advance("!=");

                return Some(Token {
                    span: span.capture(),
                    kind: TokenKind::NEQ,
                    literal: "!=".into(),
                });
            }
        }

        None
    },
};

#[cfg(test)]
mod neq_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_neq() {
        let mut cursor = Cursor::new("!=");
        let mut span = SpanTracker::new();

        let token = (NEQ_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 2
            }
        );
        assert_eq!(token.kind, TokenKind::NEQ);
        assert_eq!(token.literal, "!=".into());
    }

    #[test]
    fn consume_no_neq() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (NEQ_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const LTE_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::LTE,
    consume: |cursor, span| {
        if let '<' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                span.advance("<=");

                return Some(Token {
                    span: span.capture(),
                    kind: TokenKind::LTE,
                    literal: "<=".into(),
                });
            }
        }

        None
    },
};

#[cfg(test)]
mod lte_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_lte() {
        let mut cursor = Cursor::new("<=");
        let mut span = SpanTracker::new();

        let token = (LTE_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 2
            }
        );
        assert_eq!(token.kind, TokenKind::LTE);
        assert_eq!(token.literal, "<=".into());
    }

    #[test]
    fn consume_no_lte() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (LTE_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const GTE_OPERATOR_RULE: Rule = Rule {
    kind: TokenKind::GTE,
    consume: |cursor, span| {
        if let '>' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                span.advance(">=");

                return Some(Token {
                    span: span.capture(),
                    kind: TokenKind::GTE,
                    literal: ">=".into(),
                });
            }
        }

        None
    },
};

#[cfg(test)]
mod gte_operator_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_gte() {
        let mut cursor = Cursor::new(">=");
        let mut span = SpanTracker::new();

        let token = (GTE_OPERATOR_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 2
            }
        );
        assert_eq!(token.kind, TokenKind::GTE);
        assert_eq!(token.literal, ">=".into());
    }

    #[test]
    fn consume_no_gte() {
        let mut cursor = Cursor::new("+");
        let mut span = SpanTracker::new();

        let token = (GTE_OPERATOR_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const COMMA_DELIMITER_RULE: Rule = Rule {
    kind: TokenKind::COMMA,
    consume: |cursor, span| {
        if let ',' = cursor.current()? {
            cursor.advance();
            span.advance(",");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::COMMA,
                literal: ",".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod comma_delimiter_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_comma() {
        let mut cursor = Cursor::new(",");
        let mut span = SpanTracker::new();

        let token = (COMMA_DELIMITER_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_comma() {
        let mut cursor = Cursor::new(";");
        let mut span = SpanTracker::new();

        let token = (COMMA_DELIMITER_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const SEMICOLON_DELIMITER_RULE: Rule = Rule {
    kind: TokenKind::SEMICOLON,
    consume: |cursor, span| {
        if let ';' = cursor.current()? {
            cursor.advance();
            span.advance(";");

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::SEMICOLON,
                literal: ";".into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod semicolon_delimiter_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_semicolon() {
        let mut cursor = Cursor::new(";");
        let mut span = SpanTracker::new();

        let token = (SEMICOLON_DELIMITER_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_semicolon() {
        let mut cursor = Cursor::new(",");
        let mut span = SpanTracker::new();

        let token = (SEMICOLON_DELIMITER_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const PARENTHESES_RULE: Rule = Rule {
    kind: TokenKind::LPAREN,
    consume: |cursor, span| match cursor.current()? {
        '(' => {
            cursor.advance();
            span.advance("(");

            Some(Token {
                span: span.capture(),
                kind: TokenKind::LPAREN,
                literal: "(".into(),
            })
        }
        ')' => {
            cursor.advance();
            span.advance(")");

            Some(Token {
                span: span.capture(),
                kind: TokenKind::RPAREN,
                literal: ")".into(),
            })
        }
        _ => None,
    },
};

#[cfg(test)]
mod parentheses_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_parentheses() {
        let mut cursor = Cursor::new("()");
        let mut span = SpanTracker::new();

        let token = (PARENTHESES_RULE.consume)(&mut cursor, &mut span).unwrap();

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

        let token = (PARENTHESES_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_parentheses() {
        let mut cursor = Cursor::new("{}");
        let mut span = SpanTracker::new();

        let token = (PARENTHESES_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const BRACES_RULE: Rule = Rule {
    kind: TokenKind::LBRACE,
    consume: |cursor, span| match cursor.current()? {
        '{' => {
            cursor.advance();
            span.advance("{");

            Some(Token {
                span: span.capture(),
                kind: TokenKind::LBRACE,
                literal: "{".into(),
            })
        }
        '}' => {
            cursor.advance();
            span.advance("}");

            Some(Token {
                span: span.capture(),
                kind: TokenKind::RBRACE,
                literal: "}".into(),
            })
        }
        _ => None,
    },
};

#[cfg(test)]
mod braces_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_braces() {
        let mut cursor = Cursor::new("{}");
        let mut span = SpanTracker::new();

        let token = (BRACES_RULE.consume)(&mut cursor, &mut span).unwrap();

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

        let token = (BRACES_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_braces() {
        let mut cursor = Cursor::new("()");
        let mut span = SpanTracker::new();

        let token = (BRACES_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const BRACKETS_RULE: Rule = Rule {
    kind: TokenKind::LBRACKET,
    consume: |cursor, span| match cursor.current()? {
        '[' => {
            cursor.advance();
            span.advance("[");

            Some(Token {
                span: span.capture(),
                kind: TokenKind::LBRACKET,
                literal: "[".into(),
            })
        }
        ']' => {
            cursor.advance();
            span.advance("]");

            Some(Token {
                span: span.capture(),
                kind: TokenKind::RBRACKET,
                literal: "]".into(),
            })
        }
        _ => None,
    },
};

#[cfg(test)]
mod brackets_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_brackets() {
        let mut cursor = Cursor::new("[]");
        let mut span = SpanTracker::new();

        let token = (BRACKETS_RULE.consume)(&mut cursor, &mut span).unwrap();

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

        let token = (BRACKETS_RULE.consume)(&mut cursor, &mut span).unwrap();

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

    #[test]
    fn consume_no_brackets() {
        let mut cursor = Cursor::new("()");
        let mut span = SpanTracker::new();

        let token = (BRACKETS_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const STRINGS_RULE: Rule = Rule {
    kind: TokenKind::STRING,
    consume: |cursor, span| {
        if let '"' = cursor.current()? {
            let literal = {
                cursor.advance();
                span.advance("\"");

                let literal = utils::take_series_where(cursor, |c| *c != '"').unwrap_or_default();
                span.advance(&literal);

                cursor.advance();
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
    },
};

#[cfg(test)]
mod strings_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_string() {
        let mut cursor = Cursor::new("\"hello, world!\"");
        let mut span = SpanTracker::new();

        let token = (STRINGS_RULE.consume)(&mut cursor, &mut span).unwrap();

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
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (STRINGS_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const INTEGERS_RULE: Rule = Rule {
    kind: TokenKind::INT,
    consume: |cursor, span| match utils::take_series_where(cursor, |c| c.is_ascii_digit()) {
        Some(literal) => {
            span.advance(&literal);

            Some(Token {
                span: span.capture(),
                kind: TokenKind::INT,
                literal,
            })
        }
        None => None,
    },
};

#[cfg(test)]
mod integers_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_integer() {
        let mut cursor = Cursor::new("123");
        let mut span = SpanTracker::new();

        let token = (INTEGERS_RULE.consume)(&mut cursor, &mut span).unwrap();

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

    #[test]
    fn consume_no_integer() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (INTEGERS_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const LET_KEYWORD_RULE: Rule = Rule {
    kind: TokenKind::LET,
    consume: |cursor, span| {
        let mut cloned = cursor.clone();

        let keyword = "let";
        let literal = utils::take_series_where(&mut cloned, |c| c.is_ascii_alphabetic())?;

        if &*literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            span.advance(&literal);

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::LET,
                literal: literal.into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod let_keyword_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_let_keyword() {
        let mut cursor = Cursor::new("let");
        let mut span = SpanTracker::new();

        let token = (LET_KEYWORD_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_let_keyword() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (LET_KEYWORD_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const FUNCTION_KEYWORD_RULE: Rule = Rule {
    kind: TokenKind::FUNCTION,
    consume: |cursor, span| {
        let mut cloned = cursor.clone();

        let keyword = "fn";
        let literal = utils::take_series_where(&mut cloned, |c| c.is_ascii_alphabetic())?;

        if &*literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            span.advance(&literal);

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::FUNCTION,
                literal: literal.into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod function_keyword_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_function_keyword() {
        let mut cursor = Cursor::new("fn");
        let mut span = SpanTracker::new();

        let token = (FUNCTION_KEYWORD_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_function_keyword() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (FUNCTION_KEYWORD_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const RETURN_KEYWORD_RULE: Rule = Rule {
    kind: TokenKind::RETURN,
    consume: |cursor, span| {
        let mut cloned = cursor.clone();

        let keyword = "return";
        let literal = utils::take_series_where(&mut cloned, |c| c.is_ascii_alphabetic())?;

        if &*literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            span.advance(&literal);

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::RETURN,
                literal: literal.into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod return_keyword_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_return_keyword() {
        let mut cursor = Cursor::new("return");
        let mut span = SpanTracker::new();

        let token = (RETURN_KEYWORD_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_return_keyword() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (RETURN_KEYWORD_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const IF_KEYWORD_RULE: Rule = Rule {
    kind: TokenKind::IF,
    consume: |cursor, span| {
        let mut cloned = cursor.clone();

        let keyword = "if";
        let literal = utils::take_series_where(&mut cloned, |c| c.is_ascii_alphabetic())?;

        if &*literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            span.advance(&literal);

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::IF,
                literal: literal.into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod if_keyword_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_if_keyword() {
        let mut cursor = Cursor::new("if");
        let mut span = SpanTracker::new();

        let token = (IF_KEYWORD_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_if_keyword() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (IF_KEYWORD_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const ELSE_KEYWORD_RULE: Rule = Rule {
    kind: TokenKind::ELSE,
    consume: |cursor, span| {
        let mut cloned = cursor.clone();

        let keyword = "else";
        let literal = utils::take_series_where(&mut cloned, |c| c.is_ascii_alphabetic())?;

        if &*literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            span.advance(&literal);

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::ELSE,
                literal: literal.into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod else_keyword_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_else_keyword() {
        let mut cursor = Cursor::new("else");
        let mut span = SpanTracker::new();

        let token = (ELSE_KEYWORD_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_else_keyword() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (ELSE_KEYWORD_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const TRUE_KEYWORD_RULE: Rule = Rule {
    kind: TokenKind::TRUE,
    consume: |cursor, span| {
        let mut cloned = cursor.clone();

        let keyword = "true";
        let literal = utils::take_series_where(&mut cloned, |c| c.is_ascii_alphabetic())?;

        if &*literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            span.advance(&literal);

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::TRUE,
                literal: literal.into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod true_keyword_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_true_keyword() {
        let mut cursor = Cursor::new("true");
        let mut span = SpanTracker::new();

        let token = (TRUE_KEYWORD_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_true_keyword() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (TRUE_KEYWORD_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const FALSE_KEYWORD_RULE: Rule = Rule {
    kind: TokenKind::FALSE,
    consume: |cursor, span| {
        let mut cloned = cursor.clone();

        let keyword = "false";
        let literal = utils::take_series_where(&mut cloned, |c| c.is_ascii_alphabetic())?;

        if &*literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            span.advance(&literal);

            return Some(Token {
                span: span.capture(),
                kind: TokenKind::FALSE,
                literal: literal.into(),
            });
        }

        None
    },
};

#[cfg(test)]
mod false_keyword_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_false_keyword() {
        let mut cursor = Cursor::new("false");
        let mut span = SpanTracker::new();

        let token = (FALSE_KEYWORD_RULE.consume)(&mut cursor, &mut span).unwrap();

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
    fn consume_no_false_keyword() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (FALSE_KEYWORD_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}

const IDENTIFIER_RULE: Rule = Rule {
    kind: TokenKind::IDENT,
    consume: |cursor, span| {
        let literal = utils::take_series_where(cursor, |c| c.is_ascii_alphabetic() || *c == '_')?;

        span.advance(&literal);

        Some(Token {
            span: span.capture(),
            kind: TokenKind::IDENT,
            literal,
        })
    },
};

#[cfg(test)]
mod identifier_tests {
    use super::*;
    use crate::TokenSpan;

    #[test]
    fn consume_identifier() {
        let mut cursor = Cursor::new("abc");
        let mut span = SpanTracker::new();

        let token = (IDENTIFIER_RULE.consume)(&mut cursor, &mut span).unwrap();

        assert_eq!(
            token.span,
            TokenSpan {
                line: 1,
                column: 1,
                length: 3
            }
        );
        assert_eq!(token.kind, TokenKind::IDENT);
        assert_eq!(token.literal, "abc".into());
    }

    #[test]
    fn consume_no_identifier() {
        let mut cursor = Cursor::new("123");
        let mut span = SpanTracker::new();

        let token = (IDENTIFIER_RULE.consume)(&mut cursor, &mut span);

        assert_eq!(token, None);
    }
}
