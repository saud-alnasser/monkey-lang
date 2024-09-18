use super::Cursor;
use crate::{Token, TokenKind};

#[derive(Debug)]
pub struct Rule {
    pub consume: fn(&mut Cursor) -> Option<Token>,
}

// order matters
// should be from most specific (more symbols) to least specific (less symbols)
// with taking in account of the type of token.
pub const RULES: [Rule; 29] = [
    // whitespace (ignored)
    WHITESPACE_RULE,
    // operators with two symbols
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
    // illegal
    ILLEGAL_RULE,
];

const WHITESPACE_RULE: Rule = Rule {
    consume: |cursor| {
        cursor.advance_where(|ch| ch.is_whitespace());
        cursor.capture(TokenKind::WHITESPACE)
    },
};

#[cfg(test)]
mod whitespace_tests {
    use super::*;

    #[test]
    fn consume_whitespace() {
        let mut cursor = Cursor::new("  \n ");

        (WHITESPACE_RULE.consume)(&mut cursor);

        let token = cursor.capture(TokenKind::WHITESPACE);

        assert_eq!(token, None)
    }

    #[test]
    fn consume_no_whitespace() {
        let mut cursor = Cursor::new("abc");

        (WHITESPACE_RULE.consume)(&mut cursor);

        let token = cursor.capture(TokenKind::IDENT);

        assert_eq!(token, None)
    }
}

const ASSIGN_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '=' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::ASSIGN);
        }

        None
    },
};

#[cfg(test)]
mod assign_operator_tests {
    use super::*;

    #[test]
    fn consume_assign_operator() {
        let mut cursor = Cursor::new("=");

        let token = (ASSIGN_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::ASSIGN,
                line: 1,
                column: 1,
                literal: "=".into(),
            }
        );
        assert_eq!(token.kind, TokenKind::ASSIGN);
        assert_eq!(token.literal, "=".into());
    }

    #[test]
    fn consume_no_assign_operator() {
        let mut cursor = Cursor::new("+");

        let token = (ASSIGN_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const PLUS_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '+' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::PLUS);
        }

        None
    },
};

#[cfg(test)]
mod plus_operator_tests {
    use super::*;

    #[test]
    fn consume_plus() {
        let mut cursor = Cursor::new("+");

        let token = (PLUS_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::PLUS,
                line: 1,
                column: 1,
                literal: "+".into(),
            }
        );
    }

    #[test]
    fn consume_no_plus() {
        let mut cursor = Cursor::new("=");

        let token = (PLUS_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const MINUS_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '-' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::MINUS);
        }

        None
    },
};

#[cfg(test)]
mod minus_operator_tests {
    use super::*;

    #[test]
    fn consume_minus() {
        let mut cursor = Cursor::new("-");

        let token = (MINUS_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::MINUS,
                line: 1,
                column: 1,
                literal: "-".into(),
            }
        );
    }

    #[test]
    fn consume_no_minus() {
        let mut cursor = Cursor::new("+");

        let token = (MINUS_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const ASTERISK_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '*' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::ASTERISK);
        }

        None
    },
};

#[cfg(test)]
mod asterisk_operator_tests {
    use super::*;

    #[test]
    fn consume_asterisk() {
        let mut cursor = Cursor::new("*");

        let token = (ASTERISK_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::ASTERISK,
                line: 1,
                column: 1,
                literal: "*".into(),
            }
        );
    }

    #[test]
    fn consume_no_asterisk() {
        let mut cursor = Cursor::new("+");

        let token = (ASTERISK_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const SLASH_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '/' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::SLASH);
        }

        None
    },
};

#[cfg(test)]
mod slash_operator_tests {
    use super::*;

    #[test]
    fn consume_slash() {
        let mut cursor = Cursor::new("/");

        let token = (SLASH_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::SLASH,
                line: 1,
                column: 1,
                literal: "/".into(),
            }
        );
    }

    #[test]
    fn consume_no_slash() {
        let mut cursor = Cursor::new("+");

        let token = (SLASH_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const BANG_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '!' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::BANG);
        }

        None
    },
};

#[cfg(test)]
mod bang_operator_tests {
    use super::*;

    #[test]
    fn consume_bang() {
        let mut cursor = Cursor::new("!");

        let token = (BANG_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::BANG,
                line: 1,
                column: 1,
                literal: "!".into(),
            }
        )
    }

    #[test]
    fn consume_no_bang() {
        let mut cursor = Cursor::new("+");

        let token = (BANG_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const GT_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '>' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::GT);
        }

        None
    },
};

#[cfg(test)]
mod gt_operator_tests {
    use super::*;

    #[test]
    fn consume_gt() {
        let mut cursor = Cursor::new(">");

        let token = (GT_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::GT,
                line: 1,
                column: 1,
                literal: ">".into(),
            }
        );
    }

    #[test]
    fn consume_no_gt() {
        let mut cursor = Cursor::new("+");

        let token = (GT_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const LT_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '<' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::LT);
        }

        None
    },
};

#[cfg(test)]
mod lt_operator_tests {
    use super::*;

    #[test]
    fn consume_lt() {
        let mut cursor = Cursor::new("<");

        let token = (LT_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::LT,
                line: 1,
                column: 1,
                literal: "<".into(),
            }
        );
    }

    #[test]
    fn consume_no_lt() {
        let mut cursor = Cursor::new("+");

        let token = (LT_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const EQ_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '=' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                return cursor.capture(TokenKind::EQ);
            }
        }

        None
    },
};

#[cfg(test)]
mod eq_operator_tests {
    use super::*;

    #[test]
    fn consume_eq() {
        let mut cursor = Cursor::new("==");

        let token = (EQ_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::EQ,
                line: 1,
                column: 1,
                literal: "==".into(),
            }
        );
    }

    #[test]
    fn consume_no_eq() {
        let mut cursor = Cursor::new("+");

        let token = (EQ_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const NEQ_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '!' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                return cursor.capture(TokenKind::NEQ);
            }
        }

        None
    },
};

#[cfg(test)]
mod neq_operator_tests {
    use super::*;

    #[test]
    fn consume_neq() {
        let mut cursor = Cursor::new("!=");

        let token = (NEQ_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::NEQ,
                line: 1,
                column: 1,
                literal: "!=".into(),
            }
        );
    }

    #[test]
    fn consume_no_neq() {
        let mut cursor = Cursor::new("+");

        let token = (NEQ_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const LTE_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '<' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                return cursor.capture(TokenKind::LTE);
            }
        }

        None
    },
};

#[cfg(test)]
mod lte_operator_tests {
    use super::*;

    #[test]
    fn consume_lte() {
        let mut cursor = Cursor::new("<=");

        let token = (LTE_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::LTE,
                line: 1,
                column: 1,
                literal: "<=".into(),
            }
        );
    }

    #[test]
    fn consume_no_lte() {
        let mut cursor = Cursor::new("+");

        let token = (LTE_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const GTE_OPERATOR_RULE: Rule = Rule {
    consume: |cursor| {
        if let '>' = cursor.current()? {
            if let '=' = cursor.next()? {
                cursor.advance();
                cursor.advance();

                return cursor.capture(TokenKind::GTE);
            }
        }

        None
    },
};

#[cfg(test)]
mod gte_operator_tests {
    use super::*;

    #[test]
    fn consume_gte() {
        let mut cursor = Cursor::new(">=");

        let token = (GTE_OPERATOR_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::GTE,
                line: 1,
                column: 1,
                literal: ">=".into(),
            }
        );
    }

    #[test]
    fn consume_no_gte() {
        let mut cursor = Cursor::new("+");

        let token = (GTE_OPERATOR_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const COMMA_DELIMITER_RULE: Rule = Rule {
    consume: |cursor| {
        if let ',' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::COMMA);
        }

        None
    },
};

#[cfg(test)]
mod comma_delimiter_tests {
    use super::*;

    #[test]
    fn consume_comma() {
        let mut cursor = Cursor::new(",");

        let token = (COMMA_DELIMITER_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::COMMA,
                line: 1,
                column: 1,
                literal: ",".into(),
            }
        );
    }

    #[test]
    fn consume_no_comma() {
        let mut cursor = Cursor::new(";");

        let token = (COMMA_DELIMITER_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const SEMICOLON_DELIMITER_RULE: Rule = Rule {
    consume: |cursor| {
        if let ';' = cursor.current()? {
            cursor.advance();

            return cursor.capture(TokenKind::SEMICOLON);
        }

        None
    },
};

#[cfg(test)]
mod semicolon_delimiter_tests {
    use super::*;

    #[test]
    fn consume_semicolon() {
        let mut cursor = Cursor::new(";");

        let token = (SEMICOLON_DELIMITER_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::SEMICOLON,
                line: 1,
                column: 1,
                literal: ";".into(),
            }
        )
    }

    #[test]
    fn consume_no_semicolon() {
        let mut cursor = Cursor::new(",");

        let token = (SEMICOLON_DELIMITER_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const PARENTHESES_RULE: Rule = Rule {
    consume: |cursor| match cursor.current()? {
        '(' => {
            cursor.advance();
            cursor.capture(TokenKind::LPAREN)
        }
        ')' => {
            cursor.advance();
            cursor.capture(TokenKind::RPAREN)
        }
        _ => None,
    },
};

#[cfg(test)]
mod parentheses_tests {
    use super::*;

    #[test]
    fn consume_parentheses() {
        let mut cursor = Cursor::new("()");

        let token = (PARENTHESES_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::LPAREN,
                line: 1,
                column: 1,
                literal: "(".into(),
            }
        );

        let token = (PARENTHESES_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::RPAREN,
                line: 1,
                column: 2,
                literal: ")".into(),
            }
        );
    }

    #[test]
    fn consume_no_parentheses() {
        let mut cursor = Cursor::new("{}");

        let token = (PARENTHESES_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const BRACES_RULE: Rule = Rule {
    consume: |cursor| match cursor.current()? {
        '{' => {
            cursor.advance();
            cursor.capture(TokenKind::LBRACE)
        }
        '}' => {
            cursor.advance();
            cursor.capture(TokenKind::RBRACE)
        }
        _ => None,
    },
};

#[cfg(test)]
mod braces_tests {
    use super::*;

    #[test]
    fn consume_braces() {
        let mut cursor = Cursor::new("{}");

        let token = (BRACES_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::LBRACE,
                line: 1,
                column: 1,
                literal: r#"{"#.into(),
            }
        );

        let token = (BRACES_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::RBRACE,
                line: 1,
                column: 2,
                literal: r#"}"#.into(),
            }
        )
    }

    #[test]
    fn consume_no_braces() {
        let mut cursor = Cursor::new("()");

        let token = (BRACES_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const BRACKETS_RULE: Rule = Rule {
    consume: |cursor| match cursor.current()? {
        '[' => {
            cursor.advance();
            cursor.capture(TokenKind::LBRACKET)
        }
        ']' => {
            cursor.advance();
            cursor.capture(TokenKind::RBRACKET)
        }
        _ => None,
    },
};

#[cfg(test)]
mod brackets_tests {
    use super::*;

    #[test]
    fn consume_brackets() {
        let mut cursor = Cursor::new("[]");

        let token = (BRACKETS_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::LBRACKET,
                line: 1,
                column: 1,
                literal: "[".into(),
            }
        );

        let token = (BRACKETS_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::RBRACKET,
                line: 1,
                column: 2,
                literal: "]".into(),
            }
        )
    }

    #[test]
    fn consume_no_brackets() {
        let mut cursor = Cursor::new("()");

        let token = (BRACKETS_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const STRINGS_RULE: Rule = Rule {
    consume: |cursor| {
        if let '"' = cursor.current()? {
            cursor.advance();
            cursor.advance_where(|c| *c != '"');
            cursor.advance();

            return cursor.capture(TokenKind::STRING);
        }

        None
    },
};

#[cfg(test)]
mod strings_tests {
    use super::*;

    #[test]
    fn consume_string() {
        let mut cursor = Cursor::new("\"hello, world!\"");

        let token = (STRINGS_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::STRING,
                line: 1,
                column: 1,
                literal: "\"hello, world!\"".into(),
            }
        );
    }

    #[test]
    fn consume_no_string() {
        let mut cursor = Cursor::new("abc");

        let token = (STRINGS_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const INTEGERS_RULE: Rule = Rule {
    consume: |cursor| {
        cursor.advance_where(|ch| ch.is_ascii_digit());
        cursor.capture(TokenKind::INT)
    },
};

#[cfg(test)]
mod integers_tests {
    use super::*;

    #[test]
    fn consume_integer() {
        let mut cursor = Cursor::new("123");

        let token = (INTEGERS_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::INT,
                line: 1,
                column: 1,
                literal: "123".into(),
            }
        )
    }

    #[test]
    fn consume_no_integer() {
        let mut cursor = Cursor::new("abc");

        let token = (INTEGERS_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const LET_KEYWORD_RULE: Rule = Rule {
    consume: |cursor| {
        let mut cloned = cursor.clone();

        let keyword = "let";

        cloned.advance_where(|ch| ch.is_ascii_alphabetic());

        let token = cloned.capture(TokenKind::IDENT)?;

        if &*token.literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            return cursor.capture(TokenKind::LET);
        }

        None
    },
};

#[cfg(test)]
mod let_keyword_tests {
    use super::*;

    #[test]
    fn consume_let_keyword() {
        let mut cursor = Cursor::new("let");

        let token = (LET_KEYWORD_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::LET,
                line: 1,
                column: 1,
                literal: "let".into(),
            }
        )
    }

    #[test]
    fn consume_no_let_keyword() {
        let mut cursor = Cursor::new("abc");

        let token = (LET_KEYWORD_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const FUNCTION_KEYWORD_RULE: Rule = Rule {
    consume: |cursor| {
        let mut cloned = cursor.clone();

        let keyword = "fn";

        cloned.advance_where(|ch| ch.is_ascii_alphabetic());

        let token = cloned.capture(TokenKind::IDENT)?;

        if &*token.literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            return cursor.capture(TokenKind::FUNCTION);
        }

        None
    },
};

#[cfg(test)]
mod function_keyword_tests {
    use super::*;

    #[test]
    fn consume_function_keyword() {
        let mut cursor = Cursor::new("fn");

        let token = (FUNCTION_KEYWORD_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::FUNCTION,
                line: 1,
                column: 1,
                literal: "fn".into(),
            }
        );
    }

    #[test]
    fn consume_no_function_keyword() {
        let mut cursor = Cursor::new("abc");

        let token = (FUNCTION_KEYWORD_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const RETURN_KEYWORD_RULE: Rule = Rule {
    consume: |cursor| {
        let mut cloned = cursor.clone();

        let keyword = "return";

        cloned.advance_where(|ch| ch.is_ascii_alphabetic());

        let token = cloned.capture(TokenKind::IDENT)?;

        if &*token.literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            return cursor.capture(TokenKind::RETURN);
        }

        None
    },
};

#[cfg(test)]
mod return_keyword_tests {
    use super::*;

    #[test]
    fn consume_return_keyword() {
        let mut cursor = Cursor::new("return");

        let token = (RETURN_KEYWORD_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::RETURN,
                line: 1,
                column: 1,
                literal: "return".into()
            }
        );
    }

    #[test]
    fn consume_no_return_keyword() {
        let mut cursor = Cursor::new("abc");

        let token = (RETURN_KEYWORD_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const IF_KEYWORD_RULE: Rule = Rule {
    consume: |cursor| {
        let mut cloned = cursor.clone();

        let keyword = "if";

        cloned.advance_where(|ch| ch.is_ascii_alphabetic());

        let token = cloned.capture(TokenKind::IDENT)?;

        if &*token.literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            return cursor.capture(TokenKind::IF);
        }

        None
    },
};

#[cfg(test)]
mod if_keyword_tests {
    use super::*;

    #[test]
    fn consume_if_keyword() {
        let mut cursor = Cursor::new("if");

        let token = (IF_KEYWORD_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::IF,
                line: 1,
                column: 1,
                literal: "if".into(),
            }
        );
    }

    #[test]
    fn consume_no_if_keyword() {
        let mut cursor = Cursor::new("abc");

        let token = (IF_KEYWORD_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const ELSE_KEYWORD_RULE: Rule = Rule {
    consume: |cursor| {
        let mut cloned = cursor.clone();

        let keyword = "else";

        cloned.advance_where(|ch| ch.is_ascii_alphabetic());

        let token = cloned.capture(TokenKind::IDENT)?;

        if &*token.literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            return cursor.capture(TokenKind::ELSE);
        }

        None
    },
};

#[cfg(test)]
mod else_keyword_tests {
    use super::*;

    #[test]
    fn consume_else_keyword() {
        let mut cursor = Cursor::new("else");

        let token = (ELSE_KEYWORD_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::ELSE,
                line: 1,
                column: 1,
                literal: "else".into(),
            }
        );
    }

    #[test]
    fn consume_no_else_keyword() {
        let mut cursor = Cursor::new("abc");

        let token = (ELSE_KEYWORD_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const TRUE_KEYWORD_RULE: Rule = Rule {
    consume: |cursor| {
        let mut cloned = cursor.clone();

        let keyword = "true";

        cloned.advance_where(|ch| ch.is_ascii_alphabetic());

        let token = cloned.capture(TokenKind::IDENT)?;

        if &*token.literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            return cursor.capture(TokenKind::TRUE);
        }

        None
    },
};

#[cfg(test)]
mod true_keyword_tests {
    use super::*;

    #[test]
    fn consume_true_keyword() {
        let mut cursor = Cursor::new("true");

        let token = (TRUE_KEYWORD_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::TRUE,
                line: 1,
                column: 1,
                literal: "true".into(),
            }
        );
    }

    #[test]
    fn consume_no_true_keyword() {
        let mut cursor = Cursor::new("abc");

        let token = (TRUE_KEYWORD_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const FALSE_KEYWORD_RULE: Rule = Rule {
    consume: |cursor| {
        let mut cloned = cursor.clone();

        let keyword = "false";

        cloned.advance_where(|ch| ch.is_ascii_alphabetic());

        let token = cloned.capture(TokenKind::IDENT)?;

        if &*token.literal == keyword {
            for _ in 0..keyword.len() {
                cursor.advance();
            }

            return cursor.capture(TokenKind::FALSE);
        }

        None
    },
};

#[cfg(test)]
mod false_keyword_tests {
    use super::*;

    #[test]
    fn consume_false_keyword() {
        let mut cursor = Cursor::new("false");

        let token = (FALSE_KEYWORD_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::FALSE,
                line: 1,
                column: 1,
                literal: "false".into(),
            }
        );
    }

    #[test]
    fn consume_no_false_keyword() {
        let mut cursor = Cursor::new("abc");

        let token = (FALSE_KEYWORD_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

const IDENTIFIER_RULE: Rule = Rule {
    consume: |cursor| {
        cursor.advance_where(|ch| ch.is_ascii_alphabetic() || *ch == '_');
        cursor.capture(TokenKind::IDENT)
    },
};

#[cfg(test)]
mod identifier_tests {
    use super::*;

    #[test]
    fn consume_identifier() {
        let mut cursor = Cursor::new("abc");

        let token = (IDENTIFIER_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::IDENT,
                line: 1,
                column: 1,
                literal: "abc".into(),
            }
        );
    }

    #[test]
    fn consume_no_identifier() {
        let mut cursor = Cursor::new("123");

        let token = (IDENTIFIER_RULE.consume)(&mut cursor);

        assert_eq!(token, None);
    }
}

pub const ILLEGAL_RULE: Rule = Rule {
    consume: |cursor| {
        if cursor.current().is_none() {
            return None;
        }

        cursor.advance();
        cursor.capture(TokenKind::ILLEGAL)
    },
};

#[cfg(test)]
mod illegal_tests {
    use super::*;

    #[test]
    fn consume_illegal() {
        let mut cursor = Cursor::new("$let x = 5;");

        let token = (ILLEGAL_RULE.consume)(&mut cursor).unwrap();

        assert_eq!(
            token,
            Token {
                kind: TokenKind::ILLEGAL,
                line: 1,
                column: 1,
                literal: "$".into(),
            }
        );
    }
}
