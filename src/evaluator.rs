use std::{cell::RefCell, error::Error, fmt::Display, rc::Rc};

use crate::{
    BlockStatement, Environment, Expression, IdentExpression, Program, Statement, Token, TokenKind,
};

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    INT(i64),
    STRING(Box<str>),
    BOOLEAN(bool),
    ARRAY(Vec<DataType>),
    RETURN(Box<DataType>),
    IDENT(Box<str>),
    FUNCTION {
        parameters: Vec<IdentExpression>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    },
    BUILTIN {
        func: fn(Vec<DataType>) -> Result<DataType, Box<dyn Error>>,
    },
    NULL,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::INT(value) => write!(f, "{}", value),
            DataType::STRING(value) => write!(f, "{}", value),
            DataType::BOOLEAN(value) => write!(f, "{}", value),
            DataType::ARRAY(value) => write!(
                f,
                "[{}]",
                value
                    .iter()
                    .map(|element| element.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            DataType::RETURN(value) => write!(f, "{}", value),
            DataType::IDENT(value) => write!(f, "{}", value),
            DataType::FUNCTION { .. } => write!(f, ""),
            DataType::BUILTIN { .. } => write!(f, ""),
            DataType::NULL => write!(f, ""),
        }
    }
}

#[derive(Debug)]
pub enum EvaluationError {
    TypeMismatch(DataType, Token, DataType),
    IndexTypeMismatch(DataType, Token),
    UnknownOperator(Token),
    UndefinedVariable(DataType),
}

impl Error for EvaluationError {
    fn description(&self) -> &str {
        match self {
            EvaluationError::TypeMismatch(_, _, _) => "type mismatch",
            EvaluationError::IndexTypeMismatch(_, _) => "index type mismatch",
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
                "type mismatch {:?} {} {:?} at {}:{}",
                left, token.literal, right, token.span.line, token.span.column
            ),
            EvaluationError::IndexTypeMismatch(value, token) => {
                write!(
                    f,
                    "index type mismatch got={:?}, want=INT at {}:{}",
                    value, token.span.line, token.span.column
                )
            }
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
    fn eval_expression(
        expression: Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        match expression {
            Expression::INT(expression) => {
                return Ok(DataType::INT(expression.value));
            }
            Expression::STRING(expression) => {
                return Ok(DataType::STRING(expression.value));
            }
            Expression::BOOLEAN(expression) => {
                return Ok(DataType::BOOLEAN(expression.value));
            }
            Expression::ARRAY(expression) => {
                let elements = expression
                    .elements
                    .iter()
                    .map(|element| {
                        Evaluator::eval_expression(element.clone(), Rc::clone(&env))
                            .expect("failed to evaluate array element")
                    })
                    .collect::<Vec<DataType>>();

                return Ok(DataType::ARRAY(elements));
            }
            Expression::INDEX(expression) => {
                let left = Evaluator::eval_expression(*expression.left, Rc::clone(&env))?;
                let index = Evaluator::eval_expression(*expression.index, Rc::clone(&env))?;

                if let (DataType::ARRAY(array), DataType::INT(index)) = (&left, &index) {
                    if index < &0 || *index as usize >= array.len() {
                        return Ok(DataType::NULL);
                    }

                    return Ok(array[*index as usize].clone());
                }

                Err(Box::new(EvaluationError::IndexTypeMismatch(
                    index,
                    expression.token,
                )))
            }
            Expression::PREFIX(expression) => {
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
            Expression::INFIX(expression) => {
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

                if let (DataType::STRING(left), DataType::STRING(right)) = (&left, &right) {
                    if operator.kind == TokenKind::PLUS {
                        return Ok(DataType::STRING(
                            format!("{}{}", left, right).into_boxed_str(),
                        ));
                    }
                }

                Err(Box::new(EvaluationError::TypeMismatch(
                    left, operator, right,
                )))
            }
            Expression::IF(expression) => {
                let condition =
                    match Evaluator::eval_expression(*expression.condition, Rc::clone(&env))? {
                        DataType::BOOLEAN(value) => value,
                        DataType::INT(value) => value != 0,
                        _ => false,
                    };

                match condition {
                    true => Ok(Evaluator::eval_statement(
                        Statement::Block(expression.consequence),
                        Rc::clone(&env),
                    )?),
                    false => match expression.alternative {
                        Some(statement) => {
                            Ok(Evaluator::eval_statement(Statement::Block(statement), env)?)
                        }
                        None => Ok(DataType::NULL),
                    },
                }
            }
            Expression::IDENT(expression) => match env.borrow().get(&expression.value) {
                Some(value) => Ok(value.clone()),
                None => Err(Box::new(EvaluationError::UndefinedVariable(
                    DataType::IDENT(expression.value),
                ))),
            },
            Expression::FUNCTION(expression) => Ok(DataType::FUNCTION {
                parameters: expression.parameters,
                body: expression.body,
                env: Rc::new(RefCell::new(Environment::new(Some(env)))),
            }),
            Expression::CALL(expression) => {
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

                    return Evaluator::eval_statement(Statement::Block(body), extended_env);
                }

                if let DataType::BUILTIN { func } = function {
                    return func(arguments);
                }

                Err(Box::new(EvaluationError::UnknownOperator(expression.token)))
            }
        }
    }

    fn eval_statement(
        statement: Statement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        match statement {
            Statement::Block(block) => {
                let mut result = DataType::NULL;

                for statement in block.statements {
                    result = Evaluator::eval_statement(statement, Rc::clone(&env))?;

                    if let DataType::RETURN(_) = result {
                        return Ok(result);
                    }
                }

                Ok(result)
            }
            Statement::Let(statement) => {
                let value = Evaluator::eval_expression(statement.expression, Rc::clone(&env))?;
                env.borrow_mut().set(&statement.identifier, value);

                Ok(DataType::NULL)
            }
            Statement::Return(statement) => Ok(DataType::RETURN(Box::new(
                Evaluator::eval_expression(statement.expression, Rc::clone(&env))?,
            ))),
            Statement::Expression(statement) => {
                Evaluator::eval_expression(statement.expression, Rc::clone(&env))
            }
        }
    }

    pub fn execute(
        program: Program,
        env: Rc<RefCell<Environment>>,
    ) -> Result<DataType, Box<dyn Error>> {
        let mut result = DataType::NULL;

        for statement in program.statements {
            result = Evaluator::eval_statement(statement, Rc::clone(&env))?;

            if let DataType::RETURN(_) = result {
                return Ok(result);
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

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

            match Evaluator::execute(program, env) {
                Ok(DataType::INT(value)) => assert_eq!(value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_string_expressions() {
        let tests = vec![("\"hello world\";", "hello world")];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Ok(DataType::STRING(value)) => assert_eq!(value, expected.into()),
                _ => panic!("expected a string, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_string_concatenation() {
        let tests = vec![(r#""hello" + ", " + "world!";"#, "hello, world!")];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Ok(DataType::STRING(value)) => assert_eq!(value, expected.into()),
                _ => panic!("expected a string, got something else"),
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

            match Evaluator::execute(program, env) {
                Ok(DataType::BOOLEAN(value)) => assert_eq!(value, expected),
                _ => panic!("expected a boolean, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_array_expressions() {
        let tests = vec![
            ("[];", vec![]),
            ("[1, 2, 3];", vec![1, 2, 3]),
            ("[1 + 2, 3 * 4, 5 - 6];", vec![3, 12, -1]),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Ok(DataType::ARRAY(value)) => {
                    assert_eq!(
                        value
                            .iter()
                            .map(|element| match element {
                                DataType::INT(value) => *value,
                                _ => panic!("expected an integer, got something else"),
                            })
                            .collect::<Vec<i64>>(),
                        expected
                    );
                }
                _ => panic!("expected an array, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0];", 1),
            ("[1, 2, 3][1];", 2),
            ("[1, 2, 3][2];", 3),
            ("let i = 0; [1][i];", 1),
            ("[1, 2, 3][1 + 1];", 3),
            ("let array = [1, 2, 3]; array[2];", 3),
            ("let array = [1, 2, 3]; array[0] + array[1] + array[2];", 6),
            ("let array = [1, 2, 3]; let i = array[0]; array[i];", 2),
            ("[1, 2, 3][3];", 0),
            ("[1, 2, 3][-1];", 0),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Ok(DataType::INT(value)) => assert_eq!(value, expected),
                Ok(DataType::NULL) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
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

            match Evaluator::execute(program, env) {
                Ok(DataType::INT(value)) => assert_eq!(value, expected),
                Ok(DataType::NULL) => assert_eq!(expected, 0),
                _ => panic!("expected an integer, got something else"),
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

            match Evaluator::execute(program, env) {
                Ok(DataType::RETURN(value)) => match *value {
                    DataType::INT(value) => assert_eq!(value, expected),
                    _ => panic!("expected an integer, got something else"),
                },
                _ => panic!("expected a return statement, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch INT(5) + BOOLEAN(true) at 1:3"),
            (
                "5 + true; 5;",
                "type mismatch INT(5) + BOOLEAN(true) at 1:3",
            ),
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

            match Evaluator::execute(program, env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
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

            match Evaluator::execute(program, env) {
                Ok(DataType::INT(value)) => assert_eq!(value, expected),
                _ => panic!("expected an integer, got something else"),
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

        match Evaluator::execute(program, env) {
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

            match Evaluator::execute(program, env) {
                Ok(DataType::INT(value)) => assert_eq!(value, expected),
                Ok(DataType::RETURN(value)) => match *value {
                    DataType::INT(value) => assert_eq!(value, expected),
                    _ => panic!("expected an integer, got something else"),
                },
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_closures() {
        let input =
            "let new_adder = fn(x) { fn(y) { x + y; }; }; let add_two = new_adder(2); add_two(2);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse().unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        match Evaluator::execute(program, env) {
            Ok(DataType::INT(value)) => assert_eq!(value, 4),
            _ => panic!("expected an integer, got something else"),
        }
    }

    #[test]
    fn test_eval_builtin_len_function() {
        let tests = vec![(r#"len("");"#, 0), (r#"len("hello, world!");"#, 13)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Ok(DataType::INT(value)) => assert_eq!(value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "len(1);",
                r#"argument passed to BUILTIN("len") is not supported. got=INT(1), want=STRING|ARRAY"#,
            ),
            (
                r#"len("one", "two");"#,
                r#"extra arguments are passed to BUILTIN("len"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_builtin_first_function() {
        let tests = vec![
            ("first([2, 2, 3]);", 2),
            ("first([]);", 0),
            ("first([3]);", 3),
            ("first([5, 2]);", 5),
            ("first([1, 2, 3, 4]);", 1),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Ok(DataType::INT(value)) => assert_eq!(value, expected),
                Ok(DataType::NULL) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "first(1);",
                r#"argument passed to BUILTIN("first") is not supported. got=INT(1), want=ARRAY"#,
            ),
            (
                r#"first([1], [2]);"#,
                r#"extra arguments are passed to BUILTIN("first"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match Evaluator::execute(program, env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }
}
