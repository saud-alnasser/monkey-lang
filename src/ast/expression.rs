use super::{Statement, Token};
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub struct BlockExpression {
    pub token: Token,
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
    pub consequence: BlockExpression,
    pub alternative: Option<BlockExpression>,
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
    pub body: BlockExpression,
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

pub type HashPair = (Box<Expression>, Box<Expression>);

#[derive(Debug, PartialEq, Clone)]
pub struct HashExpression {
    pub token: Token,
    pub pairs: Vec<HashPair>,
}

impl Display for HashExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = String::new();
        let mut pairs = String::new();

        for (i, (key, value)) in self.pairs.iter().enumerate() {
            pairs.push_str(&format!(" {}: {}", key, value));

            if i < self.pairs.len() - 1 {
                pairs.push_str(", ");
            }
        }

        output.push_str(&format!("{{{}}}", pairs));

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
    Block(BlockExpression),
    Ident(IdentExpression),
    Int(IntExpression),
    String(StringExpression),
    Boolean(BooleanExpression),
    Array(ArrayExpression),
    Index(IndexExpression),
    If(IfExpression),
    Function(FunctionExpression),
    Hash(HashExpression),
    Call(CallExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Block(expression) => write!(f, "{}", expression),
            Expression::Ident(expression) => write!(f, "{}", expression),
            Expression::Int(expression) => write!(f, "{}", expression),
            Expression::String(expression) => write!(f, "{}", expression),
            Expression::Boolean(expression) => write!(f, "{}", expression),
            Expression::Array(expression) => write!(f, "{}", expression),
            Expression::Index(expression) => write!(f, "{}", expression),
            Expression::If(expression) => write!(f, "{}", expression),
            Expression::Function(expression) => write!(f, "{}", expression),
            Expression::Hash(expression) => write!(f, "{}", expression),
            Expression::Call(expression) => write!(f, "{}", expression),
            Expression::Prefix(expression) => write!(f, "{}", expression),
            Expression::Infix(expression) => write!(f, "{}", expression),
        }
    }
}
