use super::Span;

#[derive(Debug, PartialEq)]
pub enum Token {
    // special
    ILLEGAL { span: Span },
    EOF,

    // operators
    ASSIGN { span: Span },
    PLUS { span: Span },
    MINUS { span: Span },
    ASTERISK { span: Span },
    SLASH { span: Span },
    BANG { span: Span },
    EQ { span: Span },
    NEQ { span: Span },
    LT { span: Span },
    GT { span: Span },
    LTE { span: Span },
    GTE { span: Span },

    // delimiters
    COMMA { span: Span },
    SEMICOLON { span: Span },

    // brackets
    LPAREN { span: Span },
    RPAREN { span: Span },
    LBRACE { span: Span },
    RBRACE { span: Span },

    // literals
    INT { span: Span, value: Box<str> },

    // keywords & identifiers
    LET { span: Span },
    FUNCTION { span: Span },
    RETURN { span: Span },
    IF { span: Span },
    ELSE { span: Span },
    TRUE { span: Span },
    FALSE { span: Span },
    IDENT { span: Span, value: Box<str> },
}
