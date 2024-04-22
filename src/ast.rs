use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub struct TokenSpan {
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // special
    ILLEGAL,
    EOF,

    // operators
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    BANG,
    EQ,
    NEQ,
    LT,
    GT,
    LTE,
    GTE,

    // delimiters
    COMMA,
    SEMICOLON,

    // brackets
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // literals
    INT,

    // keywords & identifiers
    LET,
    FUNCTION,
    RETURN,
    IF,
    ELSE,
    TRUE,
    FALSE,
    IDENT,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub span: TokenSpan,
    pub kind: TokenKind,
    pub literal: Box<str>,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
pub enum Precedence {
    LOWEST = 1,
    EQUIVALENCE = 2, // equivalence: == or !=
    COMPARISON = 3,  // comparison: > or <
    SUM = 4,         // sum: + or -
    PRODUCT = 5,     // product: * or /
    PREFIX = 6,      // prefix: -x or !x
    CALL = 7,        // call: func(x)
}

impl From<&TokenKind> for Precedence {
    fn from(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::EQ | TokenKind::NEQ => Precedence::EQUIVALENCE,
            TokenKind::LT | TokenKind::GT => Precedence::COMPARISON,
            TokenKind::PLUS | TokenKind::MINUS => Precedence::SUM,
            TokenKind::ASTERISK | TokenKind::SLASH => Precedence::PRODUCT,
            _ => Precedence::LOWEST,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    IDENT {
        token: Token,
        value: Box<str>,
    },
    INT {
        token: Token,
        value: i64,
    },
    BOOLEAN {
        token: Token,
        value: bool,
    },
    PREFIX {
        operator: Token,
        right: Box<Expression>,
    },
    INFIX {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IDENT { value, .. } => write!(f, "{}", value),
            Expression::INT { value, .. } => write!(f, "{}", value),
            Expression::BOOLEAN { value, .. } => write!(f, "{}", value),
            Expression::PREFIX { operator, right } => write!(f, "({}{})", operator.literal, right),
            Expression::INFIX {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator.literal, right),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let {
        token: Token,
        identifier: Box<str>,
        expression: Expression,
    },
    Return {
        token: Token,
        expression: Expression,
    },
    Expression {
        token: Token,
        expression: Expression,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let {
                identifier,
                expression,
                ..
            } => write!(f, "let {} = {};", identifier, expression),
            Statement::Return { expression, .. } => write!(f, "return {};", expression),
            Statement::Expression { expression, .. } => write!(f, "{};", expression),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        for statement in &self.statements {
            output.push_str(&format!("{}", statement));
        }

        write!(f, "{}", output)
    }
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}
