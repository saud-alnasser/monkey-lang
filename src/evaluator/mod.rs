mod error;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use self::error::Result;
use crate::{ast::*, datatype::*, Environment};

#[derive(Debug, PartialEq, Clone)]
enum Tag {
    Literal,
    Identifier,
    Return,
}

type Tagged<T> = (T, Tag);

pub fn execute(program: Program, env: Rc<RefCell<Environment>>) -> Result<DataType> {
    let mut result: Tagged<DataType> = (DataType::Undefined, Tag::Literal);

    for stmt in program.statements {
        result = statement(stmt, Rc::clone(&env))?;

        if let Tag::Return = result.1 {
            return Ok(result.0);
        }
    }

    Ok(result.0)
}

fn statement(stmt: Statement, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    match stmt {
        Statement::Let {
            identifier: ident,
            expression: expr,
        } => {
            let ident = &ident.value;
            let value = expression(expr, Rc::clone(&env))?.0;
            env.borrow_mut().set(ident, value);
            Ok((DataType::Undefined, Tag::Literal))
        }
        Statement::Return(expr) => Ok((expression(expr, Rc::clone(&env))?.0, Tag::Return)),
        Statement::Expression(expr) => expression(expr, env),
    }
}

fn expression(expr: Expression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    match expr {
        Expression::Block(expr) => block(expr, env),
        Expression::Array(expr) => array(expr, env),
        Expression::Object(expr) => object(expr, env),
        Expression::Function(expr) => function(expr, env),
        Expression::If(expr) => r#if(expr, env),
        Expression::Call(expr) => call(expr, env),
        Expression::Index(expr) => index(expr, env),
        Expression::Int(expr) => int(expr),
        Expression::String(expr) => string(expr),
        Expression::Boolean(expr) => boolean(expr),
        Expression::Identifier(expr) => identifier(expr, env),
        Expression::Prefix(expr) => prefix(expr, env),
        Expression::Infix(expr) => infix(expr, env),
    }
}

fn block(expr: BlockExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    let mut result: Tagged<DataType> = (DataType::Undefined, Tag::Literal);

    for stmt in expr.statements {
        result = statement(stmt, Rc::clone(&env))?;

        if let Tag::Return = result.1 {
            return Ok(result);
        }
    }

    Ok(result)
}

fn array(expr: ArrayExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    Ok((
        DataType::Array(Array::new(
            expr.elements
                .into_iter()
                .map(|element| {
                    expression(element, Rc::clone(&env))
                        // TODO: replace expected with propagated error
                        .expect("failed to evaluate array element")
                        .0
                })
                .collect(),
        )),
        Tag::Literal,
    ))
}

fn object(expr: ObjectExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    let mut pairs = HashMap::<ObjectKey, DataType>::new();

    for (key, value) in expr.pairs.into_iter() {
        pairs.insert(
            match key {
                Expression::String(expr) => ObjectKey::String(String::new(expr.value)),
                Expression::Int(expr) => ObjectKey::Integer(Integer::new(expr.value)),
                Expression::Boolean(expr) => ObjectKey::Boolean(Boolean::new(expr.value)),
                // TODO: replace panic with propagated error
                _ => panic!("object key type mismatch"),
            },
            expression(value, Rc::clone(&env))?.0,
        );
    }

    Ok((DataType::Object(Object::new(pairs)), Tag::Literal))
}

fn function(expr: FunctionExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    Ok((
        DataType::Function(Function::Normal {
            parameters: expr
                .parameters
                .into_iter()
                .map(|param| param.value)
                .collect(),
            body: expr.body,
            env: Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&env))))),
        }),
        Tag::Literal,
    ))
}

fn r#if(expr: IfExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    let condition = match expression(*expr.condition, Rc::clone(&env))?.0 {
        DataType::Boolean(value) => *value,
        DataType::Integer(value) => *value != 0,
        _ => false,
    };

    Ok(match condition {
        true => block(*expr.consequence, env)?,
        false => match expr.alternative {
            Some(alternative) => block(*alternative, env)?,
            None => (DataType::Undefined, Tag::Literal),
        },
    })
}

fn call(expr: CallExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    let callable = expression(*expr.callable, Rc::clone(&env))?.0;

    let arguments = expr
        .arguments
        .into_iter()
        .map(|argument| expression(argument, Rc::clone(&env)))
        .collect::<Result<Vec<Tagged<DataType>>>>()?;

    match callable {
        DataType::Function(Function::Normal {
            parameters,
            body,
            env,
        }) => {
            let scoped_env = Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&env)))));

            for (parameter, argument) in parameters.into_iter().zip(arguments.into_iter()) {
                scoped_env.borrow_mut().set(&parameter, argument.0);
            }

            return Ok((block(*body, Rc::clone(&scoped_env))?.0, Tag::Literal));
        }
        DataType::Function(Function::Native(function)) => {
            let args = arguments
                .into_iter()
                .map(|argument| argument.0)
                .collect::<Vec<DataType>>();

            return Ok((function(args)?, Tag::Literal));
        }
        // TODO: replace panic with propagated error
        _ => panic!("callable type mismatch"),
    }
}

fn index(expr: IndexExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    let indexable = expression(*expr.left, Rc::clone(&env))?.0;
    let index = expression(*expr.index, Rc::clone(&env))?.0;

    if let DataType::Array(array) = &indexable {
        if let DataType::Integer(value) = index {
            return Ok((
                match *value < 0 || *value as usize >= array.len() {
                    true => DataType::Null,
                    false => {
                        array
                            .get(*value as usize)
                            // TODO: replace panic with propagated error
                            .expect("failed to get array element")
                            .clone()
                    }
                },
                Tag::Literal,
            ));
        }
    }

    if let DataType::Object(object) = indexable {
        if let DataType::String(key) = index {
            return match object.get(&ObjectKey::String(key)) {
                Some(value) => Ok((value.clone(), Tag::Literal)),
                None => Ok((DataType::Null, Tag::Literal)),
            };
        }

        if let DataType::Integer(key) = index {
            return match object.get(&ObjectKey::Integer(key)) {
                Some(value) => Ok((value.clone(), Tag::Literal)),
                None => Ok((DataType::Null, Tag::Literal)),
            };
        }

        if let DataType::Boolean(key) = index {
            return match object.get(&ObjectKey::Boolean(key)) {
                Some(value) => Ok((value.clone(), Tag::Literal)),
                None => Ok((DataType::Null, Tag::Literal)),
            };
        }
    }

    // TODO: replace panic with propagated error
    panic!("index type mismatch");
}

fn boolean(expr: BooleanExpression) -> Result<Tagged<DataType>> {
    Ok((DataType::Boolean(Boolean::new(expr.value)), Tag::Literal))
}

fn int(expr: IntExpression) -> Result<Tagged<DataType>> {
    Ok((DataType::Integer(Integer::new(expr.value)), Tag::Literal))
}

fn string(expr: StringExpression) -> Result<Tagged<DataType>> {
    Ok((DataType::String(String::new(expr.value)), Tag::Literal))
}

fn identifier(
    expr: IdentifierExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<Tagged<DataType>> {
    match env.borrow().get(&expr.value) {
        Some(value) => Ok((value, Tag::Identifier)),
        // TODO: replace panic with propagated error
        None => panic!("unknown identifier {}", expr.value),
    }
}

fn prefix(expr: PrefixExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    let operator = expr.operator;
    let right = expression(*expr.right, Rc::clone(&env))?.0;

    if let DataType::Integer(value) = right {
        return Ok((
            match operator {
                Token::BANG => DataType::Boolean(Boolean::new(*value == 0)),
                Token::MINUS => DataType::Integer(Integer::new(-*value)),
                // TODO: replace panic with propagated error
                _ => panic!("unknown operator {}", operator),
            },
            Tag::Literal,
        ));
    }

    if let DataType::Boolean(value) = right {
        return Ok((
            match operator {
                Token::BANG => DataType::Boolean(Boolean::new(!*value)),
                // TODO: replace panic with propagated error
                _ => panic!("unknown operator {}", operator),
            },
            Tag::Literal,
        ));
    }

    // TODO: replace panic with propagated error
    panic!("unknown operator {}", operator)
}

fn infix(expr: InfixExpression, env: Rc<RefCell<Environment>>) -> Result<Tagged<DataType>> {
    let operator = expr.operator;
    let left = expression(*expr.left, Rc::clone(&env))?.0;
    let right = expression(*expr.right, Rc::clone(&env))?.0;

    if let (DataType::Integer(left), DataType::Integer(right)) = (&left, &right) {
        let left = **left;
        let right = **right;

        return Ok((
            match operator {
                Token::PLUS => DataType::Integer(Integer::new(left + right)),
                Token::MINUS => DataType::Integer(Integer::new(left - right)),
                Token::ASTERISK => DataType::Integer(Integer::new(left * right)),
                Token::SLASH => DataType::Integer(Integer::new(left / right)),
                Token::LT => DataType::Boolean(Boolean::new(left < right)),
                Token::GT => DataType::Boolean(Boolean::new(left > right)),
                Token::LTE => DataType::Boolean(Boolean::new(left <= right)),
                Token::GTE => DataType::Boolean(Boolean::new(left >= right)),
                Token::EQ => DataType::Boolean(Boolean::new(left == right)),
                Token::NEQ => DataType::Boolean(Boolean::new(left != right)),
                // TODO: replace panic with propagated error
                _ => panic!("unknown operator {}", operator),
            },
            Tag::Literal,
        ));
    }

    if let (DataType::Boolean(left), DataType::Boolean(right)) = (&left, &right) {
        let left = **left;
        let right = **right;

        return Ok((
            match operator {
                Token::EQ => DataType::Boolean(Boolean::new(left == right)),
                Token::NEQ => DataType::Boolean(Boolean::new(left != right)),
                // TODO: replace panic with propagated error
                _ => panic!("unknown operator {}", operator),
            },
            Tag::Literal,
        ));
    }

    if let (DataType::String(left), DataType::String(right)) = (&left, &right) {
        if operator == Token::PLUS {
            return Ok((
                DataType::String(String::new(format!("{}{}", left, right).into())),
                Tag::Literal,
            ));
        }
    }

    // TODO: replace panic with propagated error
    panic!("unknown operator {}", operator)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::{lexer, parser};

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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_string_expressions() {
        let tests = vec![("\"hello world\";", "hello world")];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::String(value)) => assert_eq!(&value[..], expected),
                _ => panic!("expected a string, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_string_concatenation() {
        let tests = vec![(r#""hello" + ", " + "world!";"#, "hello, world!")];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::String(value)) => assert_eq!(&value[..], expected),
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Boolean(value)) => assert_eq!(*value, expected),
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Array(value)) => {
                    assert_eq!(
                        value
                            .iter()
                            .map(|element| match element {
                                DataType::Integer(value) => **value,
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Undefined) => assert_eq!(expected, 0),
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected a return statement, got something else"),
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_function_expressions() {
        let input = "fn(x) { x + 2; };";

        let tokens = lexer::parse(input).unwrap();
        let program = parser::parse(tokens).unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        match execute(program, env) {
            Ok(DataType::Function(Function::Normal {
                parameters, body, ..
            })) => {
                assert_eq!(parameters.len(), 1);
                assert_eq!(&**parameters.first().unwrap(), "x");

                assert_eq!(body.statements.len(), 1);
                assert_eq!(body.statements.first().unwrap().to_string(), "(x + 2);");
            }
            _ => panic!("expected a function, got something else"),
        }
    }

    #[test]
    fn test_eval_object_expressions() {
        let inputs = vec![
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["one"];"#,
                DataType::Integer(Integer::new(1)),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["two"];"#,
                DataType::Integer(Integer::new(2)),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["three"];"#,
                DataType::Integer(Integer::new(3)),
            ),
            (
                r#"{ "one": 1, "two": 2, "three": 3 }["four"];"#,
                DataType::Null,
            ),
        ];

        for (input, expected) in inputs {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            let result = execute(program, Rc::clone(&env)).unwrap();

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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_closures() {
        let input =
            "let new_adder = fn(x) { fn(y) { x + y; }; }; let add_two = new_adder(2); add_two(2);";

        let tokens = lexer::parse(input).unwrap();
        let program = parser::parse(tokens).unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        match execute(program, env) {
            Ok(DataType::Integer(value)) => assert_eq!(*value, 4),
            _ => panic!("expected an integer, got something else"),
        }
    }

    #[test]
    fn test_eval_builtin_len_function() {
        let tests = vec![(r#"len("");"#, 0), (r#"len("hello, world!");"#, 13)];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "len(1);",
                r#"argument[0] passed to BUILTIN("len") is not supported. got=1, want=STRING|ARRAY"#,
            ),
            (
                r#"len("one", "two");"#,
                r#"extra arguments are passed to BUILTIN("len"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "first(1);",
                r#"argument[0] passed to BUILTIN("first") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"first([1], [2]);"#,
                r#"extra arguments are passed to BUILTIN("first"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "last(1);",
                r#"argument[0] passed to BUILTIN("last") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"last([1], [2]);"#,
                r#"extra arguments are passed to BUILTIN("last"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Array(value)) => {
                    assert_eq!(
                        value
                            .iter()
                            .map(|element| match element {
                                DataType::Integer(value) => **value,
                                _ => panic!("expected an integer, got something else"),
                            })
                            .collect::<Vec<i64>>(),
                        expected
                    );
                }
                Ok(DataType::Null) => assert_eq!(0, expected.len()),
                _ => panic!("expected an array, got something else"),
            }
        }

        let test_errors = vec![
            (
                "rest(1);",
                r#"argument[0] passed to BUILTIN("rest") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"rest([1], [2]);"#,
                r#"extra arguments are passed to BUILTIN("rest"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
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
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Ok(DataType::Array(value)) => {
                    assert_eq!(
                        value
                            .iter()
                            .map(|element| match element {
                                DataType::Integer(value) => **value,
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
                r#"argument[0] passed to BUILTIN("push") is not supported. got=1, want=ARRAY"#,
            ),
            (
                "push([1]);",
                r#"extra arguments are passed to BUILTIN("push"). got=1, want=2"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(program, env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }
}
