mod datatype;
pub mod error;

use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

pub use self::datatype::*;
pub use self::error::*;
use crate::Statement;
use crate::{Environment, Expression, Program, TokenKind};

pub struct Evaluator;

impl Evaluator {
    fn eval_expression(expression: Expression, env: Rc<RefCell<Environment>>) -> Result<DataType> {
        match expression {
            Expression::Block(block) => {
                let mut result = DataType::UNDEFINED;

                for statement in block.statements {
                    result = Evaluator::eval_statement(statement, Rc::clone(&env))?;

                    if let DataType::RETURN(_) = result {
                        return Ok(result);
                    }
                }

                Ok(result)
            }
            Expression::Int(expression) => {
                return Ok(DataType::INT(expression.value));
            }
            Expression::String(expression) => {
                return Ok(DataType::STRING(expression.value));
            }
            Expression::Boolean(expression) => {
                return Ok(DataType::BOOLEAN(expression.value));
            }
            Expression::Array(expression) => {
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
            Expression::Index(expression) => {
                let left = Evaluator::eval_expression(*expression.left, Rc::clone(&env))?;
                let index = Evaluator::eval_expression(*expression.index, Rc::clone(&env))?;

                if let (DataType::ARRAY(array), DataType::INT(index)) = (&left, &index) {
                    if index < &0 || *index as usize >= array.len() {
                        return Ok(DataType::NULL);
                    }

                    return Ok(array[*index as usize].clone());
                }

                if let (DataType::HASH { pairs }, DataType::STRING(key)) = (&left, &index) {
                    if let Some(value) = pairs.get(&HashKey::String(key.clone())) {
                        return Ok(value.clone());
                    } else {
                        return Ok(DataType::NULL);
                    }
                }

                if let (DataType::HASH { pairs }, DataType::INT(key)) = (&left, &index) {
                    if let Some(value) = pairs.get(&HashKey::Int(*key)) {
                        return Ok(value.clone());
                    } else {
                        return Ok(DataType::NULL);
                    }
                }

                if let (DataType::HASH { pairs }, DataType::BOOLEAN(key)) = (&left, &index) {
                    if let Some(value) = pairs.get(&HashKey::Boolean(*key)) {
                        return Ok(value.clone());
                    } else {
                        return Ok(DataType::NULL);
                    }
                }

                Err(Error::IndexTypeMismatch(index, expression.token))
            }
            Expression::Prefix(expression) => {
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

                Err(Error::UnknownOperator(expression.operator))
            }
            Expression::Infix(expression) => {
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
                        _ => return Err(Error::UnknownOperator(operator)),
                    }
                }

                if let (DataType::BOOLEAN(left), DataType::BOOLEAN(right)) = (&left, &right) {
                    match operator.kind {
                        TokenKind::EQ => return Ok(DataType::BOOLEAN(left == right)),
                        TokenKind::NEQ => return Ok(DataType::BOOLEAN(left != right)),
                        _ => return Err(Error::UnknownOperator(operator)),
                    }
                }

                if let (DataType::STRING(left), DataType::STRING(right)) = (&left, &right) {
                    if operator.kind == TokenKind::PLUS {
                        return Ok(DataType::STRING(
                            format!("{}{}", left, right).into_boxed_str(),
                        ));
                    }
                }

                Err(Error::TypeMismatch(left, operator, right))
            }
            Expression::If(expression) => {
                let condition =
                    match Evaluator::eval_expression(*expression.condition, Rc::clone(&env))? {
                        DataType::BOOLEAN(value) => value,
                        DataType::INT(value) => value != 0,
                        _ => false,
                    };

                match condition {
                    true => Ok(Evaluator::eval_expression(
                        Expression::Block(expression.consequence),
                        Rc::clone(&env),
                    )?),
                    false => match expression.alternative {
                        Some(statement) => Ok(Evaluator::eval_expression(
                            Expression::Block(statement),
                            env,
                        )?),
                        None => Ok(DataType::UNDEFINED),
                    },
                }
            }
            Expression::Ident(expression) => match env.borrow().get(&expression.value) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::UndefinedVariable(DataType::IDENT(expression.value))),
            },
            Expression::Function(expression) => Ok(DataType::FUNCTION {
                parameters: expression.parameters,
                body: expression.body,
                env: Rc::new(RefCell::new(Environment::new(Some(env)))),
            }),
            Expression::Hash(expression) => {
                let mut pairs = HashMap::<HashKey, DataType>::new();

                for (key, value) in expression.pairs.iter() {
                    let value = Evaluator::eval_expression(*value.clone(), Rc::clone(&env))?;

                    match *key.clone() {
                        Expression::String(expression) => {
                            pairs.insert(HashKey::String(expression.value), value);
                        }
                        Expression::Int(expression) => {
                            pairs.insert(HashKey::Int(expression.value), value);
                        }
                        Expression::Boolean(expression) => {
                            pairs.insert(HashKey::Boolean(expression.value), value);
                        }
                        Expression::Array(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::Block(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::Ident(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::Call(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::Function(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::Hash(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::If(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::Index(expression) => {
                            return Err(Error::IllegalType(expression.token))
                        }
                        Expression::Infix(expression) => {
                            return Err(Error::IllegalType(expression.operator))
                        }
                        Expression::Prefix(expression) => {
                            return Err(Error::IllegalType(expression.operator))
                        }
                    }
                }

                Ok(DataType::HASH { pairs })
            }
            Expression::Call(expression) => {
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

                    return Evaluator::eval_expression(Expression::Block(body), extended_env);
                }

                if let DataType::BUILTIN { func } = function {
                    return func(arguments).map_err(|error| Error::Builtin(error));
                }

                Err(Error::UnknownOperator(expression.token))
            }
        }
    }

    fn eval_statement(statement: Statement, env: Rc<RefCell<Environment>>) -> Result<DataType> {
        match statement {
            Statement::Let(statement) => {
                let value = Evaluator::eval_expression(statement.expression, Rc::clone(&env))?;
                env.borrow_mut().set(&statement.identifier, value);

                Ok(DataType::UNDEFINED)
            }
            Statement::Return(statement) => Ok(DataType::RETURN(Box::new(
                Evaluator::eval_expression(statement.expression, Rc::clone(&env))?,
            ))),
            Statement::Expression(statement) => {
                Evaluator::eval_expression(statement.expression, Rc::clone(&env))
            }
        }
    }

    pub fn execute(program: Program, env: Rc<RefCell<Environment>>) -> Result<DataType> {
        let mut result = DataType::UNDEFINED;

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
                Ok(DataType::UNDEFINED) => assert_eq!(expected, 0),
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
    fn test_eval_hash_expressions() {
        let inputs = vec![
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["one"];"#,
                DataType::INT(1),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["two"];"#,
                DataType::INT(2),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["three"];"#,
                DataType::INT(3),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["four"];"#,
                DataType::NULL,
            ),
        ];

        for (input, expected) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            let result = Evaluator::execute(program, Rc::clone(&env)).unwrap();

            assert_eq!(result, expected);
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
                r#"argument[0] passed to BUILTIN("len") is not supported. got=INT(1), want=STRING|ARRAY"#,
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
                r#"argument[0] passed to BUILTIN("first") is not supported. got=INT(1), want=ARRAY"#,
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

    #[test]
    fn test_eval_builtin_last_function() {
        let tests = vec![
            ("last([2, 2, 3]);", 3),
            ("last([]);", 0),
            ("last([3]);", 3),
            ("last([5, 2]);", 2),
            ("last([1, 2, 3, 4]);", 4),
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
                "last(1);",
                r#"argument[0] passed to BUILTIN("last") is not supported. got=INT(1), want=ARRAY"#,
            ),
            (
                r#"last([1], [2]);"#,
                r#"extra arguments are passed to BUILTIN("last"). got=2, want=1"#,
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
    fn test_eval_builtin_rest_function() {
        let tests = vec![
            ("rest([2, 2, 3]);", vec![2, 3]),
            ("rest([]);", vec![]),
            ("rest([3]);", vec![]),
            ("rest([5, 2]);", vec![2]),
            ("rest([1, 2, 3, 4]);", vec![2, 3, 4]),
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
                Ok(DataType::NULL) => assert_eq!(0, expected.len()),
                _ => panic!("expected an array, got something else"),
            }
        }

        let test_errors = vec![
            (
                "rest(1);",
                r#"argument[0] passed to BUILTIN("rest") is not supported. got=INT(1), want=ARRAY"#,
            ),
            (
                r#"rest([1], [2]);"#,
                r#"extra arguments are passed to BUILTIN("rest"). got=2, want=1"#,
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
    fn test_eval_builtins_push_function() {
        let tests = vec![
            ("push([], 1);", vec![1]),
            ("push([1], 2);", vec![1, 2]),
            ("push([1, 2], 3);", vec![1, 2, 3]),
            ("push([1, 2, 3], 4);", vec![1, 2, 3, 4]),
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

        let test_errors = vec![
            (
                "push(1, 1);",
                r#"argument[0] passed to BUILTIN("push") is not supported. got=INT(1), want=ARRAY"#,
            ),
            (
                "push([1]);",
                r#"extra arguments are passed to BUILTIN("push"). got=1, want=2"#,
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
