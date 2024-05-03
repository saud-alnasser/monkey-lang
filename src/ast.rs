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
    WHITESPACE,
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
    RBRACKET,
    LBRACKET,

    // literals
    INT,
    STRING,

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

#[derive(Debug, PartialEq, Clone)]
pub struct IdentExpression {
    pub token: Token,
    pub value: Box<str>,
}

impl Display for IdentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntExpression {
    pub token: Token,
    pub value: i64,
}

impl Display for IntExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringExpression {
    pub token: Token,
    pub value: Box<str>,
}

impl Display for StringExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayExpression {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl Display for ArrayExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        let mut elements = String::new();

        for (i, element) in self.elements.iter().enumerate() {
            elements.push_str(&format!("{}", element));

            if i < self.elements.len() - 1 {
                elements.push_str(", ");
            }
        }

        output.push_str(&format!("[{}]", elements));

        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        output.push_str(&format!("if {} {}", self.condition, self.consequence));

        if let Some(alternative) = &self.alternative {
            output.push_str(&format!(" else {}", alternative));
        }

        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression {
    pub token: Token,
    pub parameters: Vec<IdentExpression>,
    pub body: BlockStatement,
}

impl Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        let mut parameters = String::new();

        for (i, parameter) in self.parameters.iter().enumerate() {
            parameters.push_str(&format!("{}", parameter));

            if i < self.parameters.len() - 1 {
                parameters.push_str(", ");
            }
        }

        output.push_str(&format!("fn({}) {}", parameters, self.body));

        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        let mut arguments = String::new();

        for (i, argument) in self.arguments.iter().enumerate() {
            arguments.push_str(&format!("{}", argument));

            if i < self.arguments.len() - 1 {
                arguments.push_str(", ");
            }
        }

        output.push_str(&format!("{}({})", self.function, arguments));

        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator.literal, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left, self.operator.literal, self.right
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    IDENT(IdentExpression),
    INT(IntExpression),
    STRING(StringExpression),
    BOOLEAN(BooleanExpression),
    ARRAY(ArrayExpression),
    INDEX(IndexExpression),
    IF(IfExpression),
    FUNCTION(FunctionExpression),
    CALL(CallExpression),
    PREFIX(PrefixExpression),
    INFIX(InfixExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IDENT(expression) => write!(f, "{}", expression),
            Expression::INT(expression) => write!(f, "{}", expression),
            Expression::STRING(expression) => write!(f, "{}", expression),
            Expression::BOOLEAN(expression) => write!(f, "{}", expression),
            Expression::ARRAY(expression) => write!(f, "{}", expression),
            Expression::INDEX(expression) => write!(f, "{}", expression),
            Expression::IF(expression) => write!(f, "{}", expression),
            Expression::FUNCTION(expression) => write!(f, "{}", expression),
            Expression::CALL(expression) => write!(f, "{}", expression),
            Expression::PREFIX(expression) => write!(f, "{}", expression),
            Expression::INFIX(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        for statement in &self.statements {
            output.push_str(&format!("{}", statement));
        }

        write!(f, "{{{}}}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: Box<str>,
    pub expression: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.identifier, self.expression)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.expression)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.expression)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Block(BlockStatement),
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Block(statement) => write!(f, "{}", statement),
            Statement::Let(statement) => write!(f, "{}", statement),
            Statement::Return(statement) => write!(f, "{}", statement),
            Statement::Expression(statement) => write!(f, "{}", statement),
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
