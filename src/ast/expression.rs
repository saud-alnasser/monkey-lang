use std::fmt::Display;

use internment::Intern;

use super::{BinaryOperator, Statement, UnaryOperator};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Block(BlockExpression),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Function(FunctionExpression),
    If(IfExpression),
    Call(CallExpression),
    Index(IndexExpression),
    Int(IntExpression),
    String(StringExpression),
    Boolean(BooleanExpression),
    Identifier(IdentifierExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl Expression {
    pub fn name_of(&self) -> &'static str {
        match self {
            Expression::Block(_) => "BLOCK",
            Expression::Array(_) => "ARRAY",
            Expression::Object(_) => "OBJECT",
            Expression::Function(_) => "FUNCTION",
            Expression::If(_) => "IF",
            Expression::Call(_) => "CALL",
            Expression::Index(_) => "INDEX",
            Expression::Int(_) => "INT",
            Expression::String(_) => "STRING",
            Expression::Boolean(_) => "BOOLEAN",
            Expression::Identifier(_) => "IDENTIFIER",
            Expression::Prefix(_) => "EXPRESSION",
            Expression::Infix(_) => "EXPRESSION",
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Block(expression) => write!(f, "{}", expression),
            Expression::Array(expression) => write!(f, "{}", expression),
            Expression::Object(expression) => write!(f, "{}", expression),
            Expression::Function(expression) => write!(f, "{}", expression),
            Expression::If(expression) => write!(f, "{}", expression),
            Expression::Call(expression) => write!(f, "{}", expression),
            Expression::Index(expression) => write!(f, "{}", expression),
            Expression::Int(expression) => write!(f, "{}", expression),
            Expression::String(expression) => write!(f, "{}", expression),
            Expression::Boolean(expression) => write!(f, "{}", expression),
            Expression::Identifier(expression) => write!(f, "{}", expression),
            Expression::Prefix(expression) => write!(f, "{}", expression),
            Expression::Infix(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockExpression {
    pub statements: Vec<Statement>,
}

impl Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        let mut statements = String::new();

        for (i, statement) in self.statements.iter().enumerate() {
            statements.push_str(&format!("{}", statement));

            if i < self.statements.len() - 1 {
                statements.push_str(", ");
            }
        }

        output.push_str(&format!("{{{}}}", statements));

        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayExpression {
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
pub struct ObjectExpression {
    pub pairs: Vec<(Expression, Expression)>,
}

impl Display for ObjectExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        let mut pairs = String::new();

        for (i, (key, value)) in self.pairs.iter().enumerate() {
            pairs.push_str(&format!("{}: {}", key, value));

            if i < self.pairs.len() - 1 {
                pairs.push_str(", ");
            }
        }

        output.push_str(&format!("{{{}}}", pairs));

        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression {
    pub parameters: Vec<IdentifierExpression>,
    pub body: Box<BlockExpression>,
}

impl Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();

        let parameters: Vec<String> = self
            .parameters
            .iter()
            .map(|param| format!("{}", param))
            .collect();
        output.push_str(&format!("fn({}) ", parameters.join(", ")));
        output.push_str(&format!("{}", self.body));
        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: Box<BlockExpression>,
    pub alternative: Option<Box<BlockExpression>>,
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
pub struct CallExpression {
    pub callable: Box<Expression>,
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

        output.push_str(&format!("{}({})", self.callable, arguments));

        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntExpression {
    pub value: i64,
}

impl Display for IntExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringExpression {
    pub value: Intern<String>,
}

impl Display for StringExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanExpression {
    pub value: bool,
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierExpression {
    pub value: Intern<String>,
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: UnaryOperator,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
