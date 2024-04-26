use std::{cell::RefCell, error::Error, fmt::Display, rc::Rc};

use crate::{
    BlockStatement, BooleanExpression, CallExpression, Environment, Expression, FunctionExpression,
    IdentExpression, IfExpression, InfixExpression, IntExpression, LetStatement, PrefixExpression,
    ReturnStatement, Statement, Token, TokenKind,
};

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    INT(i64),
    BOOLEAN(bool),
    RETURN(Box<DataType>),
    IDENT(Box<str>),
    FUNCTION {
        parameters: Vec<IdentExpression>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    },
    NULL,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::INT(value) => write!(f, "{}", value),
            DataType::BOOLEAN(value) => write!(f, "{}", value),
            DataType::RETURN(value) => write!(f, "{}", value),
            DataType::IDENT(value) => write!(f, "{}", value),
            DataType::FUNCTION { .. } => write!(f, ""),
            DataType::NULL => write!(f, ""),
        }
    }
}

#[derive(Debug)]
pub enum EvaluationError {
    TypeMismatch(DataType, Token, DataType),
    UnknownOperator(Token),
    UndefinedVariable(DataType),
}

impl Error for EvaluationError {
    fn description(&self) -> &str {
        match self {
            EvaluationError::TypeMismatch(_, _, _) => "type mismatch",
            EvaluationError::UnknownOperator(_) => "unknown operator",
            EvaluationError::UndefinedVariable(_) => "undefined variable",
        }
    }
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluationError::TypeMismatch(left, token, right) => write!(
                f,
                "type mismatch {} {} {} at {}:{}",
                match left {
                    DataType::INT(_) => "INTEGER",
                    DataType::BOOLEAN(_) => "BOOLEAN",
                    _ => "UNKNOWN",
                },
                token.literal,
                match right {
                    DataType::INT(_) => "INTEGER",
                    DataType::BOOLEAN(_) => "BOOLEAN",
                    _ => "UNKNOWN",
                },
                token.span.line,
                token.span.column
            ),
            EvaluationError::UnknownOperator(token) => {
                write!(
                    f,
                    "unknown operator {} at {}:{}",
                    token.literal, token.span.line, token.span.column
                )
            }
            EvaluationError::UndefinedVariable(value) => {
                write!(f, "unknown identifier {}", value)
            }
        }
    }
}

pub struct Evaluator;

impl Evaluator {
    fn eval_int(expression: IntExpression) -> Result<DataType, Box<dyn Error>> {
        Ok(DataType::INT(expression.value))
    }

    fn eval_boolean(expression: BooleanExpression) -> Result<DataType, Box<dyn Error>> {
        Ok(DataType::BOOLEAN(expression.value))
    }

    fn eval_prefix(
        expression: PrefixExpression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        let right = Evaluator::eval_expression(*expression.right, Rc::clone(&env))?;

        if expression.operator.kind == TokenKind::BANG {
            if let DataType::BOOLEAN(value) = right {
                return Ok(DataType::BOOLEAN(!value));
            }

            if let DataType::INT(value) = right {
                return Ok(DataType::BOOLEAN(value == 0));
            }
        };

        if expression.operator.kind == TokenKind::MINUS {
            if let DataType::INT(value) = right {
                return Ok(DataType::INT(-value));
            };
        }

        Err(Box::new(EvaluationError::UnknownOperator(
            expression.operator,
        )))
    }

    fn eval_infix(
        expression: InfixExpression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        let left = Evaluator::eval_expression(*expression.left, Rc::clone(&env))?;
        let right = Evaluator::eval_expression(*expression.right, Rc::clone(&env))?;
        let operator = expression.operator;

        if let (DataType::INT(left), DataType::INT(right)) = (&left, &right) {
            match operator.kind {
                TokenKind::PLUS => return Ok(DataType::INT(left + right)),
                TokenKind::MINUS => return Ok(DataType::INT(left - right)),
                TokenKind::ASTERISK => return Ok(DataType::INT(left * right)),
                TokenKind::SLASH => return Ok(DataType::INT(left / right)),
                TokenKind::LT => return Ok(DataType::BOOLEAN(left < right)),
                TokenKind::GT => return Ok(DataType::BOOLEAN(left > right)),
                TokenKind::LTE => return Ok(DataType::BOOLEAN(left <= right)),
                TokenKind::GTE => return Ok(DataType::BOOLEAN(left >= right)),
                TokenKind::EQ => return Ok(DataType::BOOLEAN(left == right)),
                TokenKind::NEQ => return Ok(DataType::BOOLEAN(left != right)),
                _ => return Err(Box::new(EvaluationError::UnknownOperator(operator))),
            }
        }

        if let (DataType::BOOLEAN(left), DataType::BOOLEAN(right)) = (&left, &right) {
            match operator.kind {
                TokenKind::EQ => return Ok(DataType::BOOLEAN(left == right)),
                TokenKind::NEQ => return Ok(DataType::BOOLEAN(left != right)),
                _ => return Err(Box::new(EvaluationError::UnknownOperator(operator))),
            }
        }

        Err(Box::new(EvaluationError::TypeMismatch(
            left, operator, right,
        )))
    }

    fn eval_if_else(
        expression: IfExpression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        let condition = match Evaluator::eval_expression(*expression.condition, Rc::clone(&env))? {
            DataType::BOOLEAN(value) => value,
            DataType::INT(value) => value != 0,
            _ => false,
        };

        match condition {
            true => Ok(Evaluator::eval(
                Statement::Block(expression.consequence),
                Rc::clone(&env),
            )?),
            false => match expression.alternative {
                Some(statement) => Ok(Evaluator::eval(Statement::Block(statement), env)?),
                None => Ok(DataType::NULL),
            },
        }
    }

    fn eval_identifier(
        expression: IdentExpression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        match env.borrow().get(&expression.value) {
            Some(value) => Ok(value.clone()),
            None => Err(Box::new(EvaluationError::UndefinedVariable(
                DataType::IDENT(expression.value),
            ))),
        }
    }

    fn eval_function(
        expression: FunctionExpression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        Ok(DataType::FUNCTION {
            parameters: expression.parameters,
            body: expression.body,
            env: Rc::new(RefCell::new(Environment::new(Some(env)))),
        })
    }

    fn eval_function_call(
        expression: CallExpression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        let function = Evaluator::eval_expression(*expression.function, Rc::clone(&env))?;

        let arguments = expression
            .arguments
            .iter()
            .map(|argument| {
                Evaluator::eval_expression(argument.clone(), Rc::clone(&env))
                    .expect("failed to evaluate function call passed arguments")
            })
            .collect::<Vec<DataType>>();

        if let DataType::FUNCTION {
            parameters,
            body,
            env,
        } = function
        {
            let extended_env = Rc::new(RefCell::new(Environment::new(Some(env))));

            for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                extended_env
                    .borrow_mut()
                    .set(&parameter.value, argument.clone());
            }

            return Evaluator::eval_block(body, extended_env);
        }

        Err(Box::new(EvaluationError::UnknownOperator(expression.token)))
    }

    fn eval_expression(
        expression: Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        match expression {
            Expression::INT(int) => {
                return Evaluator::eval_int(int);
            }
            Expression::BOOLEAN(boolean) => {
                return Evaluator::eval_boolean(boolean);
            }
            Expression::PREFIX(prefix) => {
                return Evaluator::eval_prefix(prefix, Rc::clone(&env));
            }
            Expression::INFIX(infix) => {
                return Evaluator::eval_infix(infix, Rc::clone(&env));
            }
            Expression::IF(expression) => {
                return Evaluator::eval_if_else(expression, Rc::clone(&env));
            }
            Expression::IDENT(expression) => {
                return Evaluator::eval_identifier(expression, Rc::clone(&env));
            }
            Expression::FUNCTION(expression) => {
                return Evaluator::eval_function(expression, Rc::clone(&env));
            }
            Expression::CALL(expression) => {
                return Evaluator::eval_function_call(expression, Rc::clone(&env));
            }
        }
    }

    fn eval_block(
        block: BlockStatement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        let mut result = DataType::NULL;

        for statement in block.statements {
            result = Evaluator::eval(statement, Rc::clone(&env))?;

            if let DataType::RETURN(_) = result {
                return Ok(result);
            }
        }

        Ok(result)
    }

    fn eval_let(
        statement: LetStatement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        let value = Evaluator::eval_expression(statement.expression, Rc::clone(&env))?;
        env.borrow_mut().set(&statement.identifier, value);

        Ok(DataType::NULL)
    }

    fn eval_return(
        statement: ReturnStatement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        Ok(DataType::RETURN(Box::new(Evaluator::eval_expression(
            statement.expression,
            Rc::clone(&env),
        )?)))
    }

    pub fn eval(
        statement: Statement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        match statement {
            Statement::Block(statement) => {
                return Evaluator::eval_block(statement, Rc::clone(&env));
            }
            Statement::Let(statement) => {
                return Evaluator::eval_let(statement, Rc::clone(&env));
            }
            Statement::Return(statement) => {
                return Evaluator::eval_return(statement, Rc::clone(&env));
            }
            Statement::Expression(statement) => {
                return Evaluator::eval_expression(statement.expression, Rc::clone(&env));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Parser};

    use super::*;

    #[test]
    fn test_eval_integer_expressions() {
        let tests = vec![
            ("5;", 5),
            ("10;", 10),
            ("-5;", -5),
            ("-10;", -10),
            ("5 + 5 + 5 + 5 - 10;", 10),
            ("2 * 2 * 2 * 2 * 2;", 32),
            ("-50 + 100 + -50;", 0),
            ("5 * 2 + 10;", 20),
            ("5 + 2 * 10;", 25),
            ("20 + 2 * -10;", 0),
            ("50 / 2 * 2 + 10;", 60),
            ("2 * (5 + 10);", 30),
            ("3 * 3 * 3 + 10;", 37),
            ("3 * (3 * 3) + 10;", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10;", 50),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let env = Rc::new(RefCell::new(Environment::new(None)));

            for statement in program.statements {
                match Evaluator::eval(statement, Rc::clone(&env)) {
                    Ok(DataType::INT(value)) => assert_eq!(value, expected),
                    _ => panic!("expected an integer, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_eval_boolean_expressions() {
        let tests = vec![
            ("true;", true),
            ("false;", false),
            ("!true;", false),
            ("!false;", true),
            ("!!true;", true),
            ("!!false;", false),
            ("!5;", false),
            ("!!5;", true),
            ("!-5;", false),
            ("!!-5;", true),
            ("1 < 2;", true),
            ("1 > 2;", false),
            ("1 < 1;", false),
            ("1 > 1;", false),
            ("1 <= 2;", true),
            ("1 >= 2;", false),
            ("1 <= 1;", true),
            ("1 >= 1;", true),
            ("1 == 1;", true),
            ("1 != 1;", false),
            ("1 == 2;", false),
            ("1 != 2;", true),
            ("true == true;", true),
            ("false == false;", true),
            ("true == false;", false),
            ("true != false;", true),
            ("false != true;", true),
            ("(1 < 2) == true;", true),
            ("(1 < 2) == false;", false),
            ("(1 > 2) == true;", false),
            ("(1 > 2) == false;", true),
            ("1 >= 2 == true;", false),
            ("1 >= 2 == false;", true),
            ("1 <= 1 == true;", true),
            ("1 <= 1 == false;", false),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let env = Rc::new(RefCell::new(Environment::new(None)));

            for statement in program.statements {
                match Evaluator::eval(statement, Rc::clone(&env)) {
                    Ok(DataType::BOOLEAN(value)) => assert_eq!(value, expected),
                    _ => panic!("expected a boolean, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_eval_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10; };", 10),
            ("if (false) { 10; };", 0),
            ("if (1) { 10; };", 10),
            ("if (1 < 2) { 10; };", 10),
            ("if (1 > 2) { 10; };", 0),
            ("if (1 > 2) { 10; } else { 20; };", 20),
            ("if (1 < 2) { 10; } else { 20; };", 10),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let env = Rc::new(RefCell::new(Environment::new(None)));

            for statement in program.statements {
                match Evaluator::eval(statement, Rc::clone(&env)) {
                    Ok(DataType::INT(value)) => assert_eq!(value, expected),
                    Ok(DataType::NULL) => assert_eq!(expected, 0),
                    _ => panic!("expected an integer, got something else"),
                }
            }
        }
    }

    #[test]
    fn test_eval_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    };

                    return 1;
                };",
                10,
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let env = Rc::new(RefCell::new(Environment::new(None)));

            for statement in program.statements {
                if let Ok(DataType::RETURN(value)) = Evaluator::eval(statement, Rc::clone(&env)) {
                    match *value {
                        DataType::INT(value) => assert_eq!(value, expected),
                        _ => panic!("expected an integer, got something else"),
                    }
                }
            }
        }
    }

    #[test]
    fn test_eval_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch INTEGER + BOOLEAN at 1:3"),
            ("5 + true; 5;", "type mismatch INTEGER + BOOLEAN at 1:3"),
            ("-true;", "unknown operator - at 1:1"),
            ("true + false;", "unknown operator + at 1:6"),
            ("5; true + false; 5;", "unknown operator + at 1:9"),
            (
                "if (10 > 1) { true + false; };",
                "unknown operator + at 1:20",
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    };

                    return 1;
                };",
                "unknown operator + at 3:37",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let env = Rc::new(RefCell::new(Environment::new(None)));

            for statement in program.statements {
                if let Err(error) = Evaluator::eval(statement, Rc::clone(&env)) {
                    assert_eq!(error.to_string(), expected);
                }
            }
        }
    }

    #[test]
    fn test_eval_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let env = Rc::new(RefCell::new(Environment::new(None)));

            for statement in program.statements {
                if let Ok(DataType::INT(value)) = Evaluator::eval(statement, Rc::clone(&env)) {
                    assert_eq!(value, expected);
                }
            }
        }
    }

    #[test]
    fn test_eval_function_expressions() {
        let input = "fn(x) { x + 2; };";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();

        let env = Rc::new(RefCell::new(Environment::new(None)));
        let statement = program.statements.first().unwrap().clone();

        match Evaluator::eval(statement, env) {
            Ok(DataType::FUNCTION {
                parameters, body, ..
            }) => {
                assert_eq!(parameters.len(), 1);
                assert_eq!(parameters.first().unwrap().value, "x".into());

                assert_eq!(body.statements.len(), 1);
                assert_eq!(body.statements.first().unwrap().to_string(), "(x + 2);");
            }
            _ => panic!("expected a function, got something else"),
        }
    }

    #[test]
    fn test_eval_function_calls() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5);", 5),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();

            let env = Rc::new(RefCell::new(Environment::new(None)));

            for statement in program.statements {
                if let Ok(DataType::INT(value)) = Evaluator::eval(statement, Rc::clone(&env)) {
                    assert_eq!(value, expected);
                }
            }
        }
    }
}
