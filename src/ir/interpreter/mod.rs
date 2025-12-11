mod error;

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinaryOperator, UnaryOperator};
use crate::ir::program::*;
use crate::runtime::datatype::*;
use crate::runtime::environment::Environment;

pub use error::{Error, Result};

/// an interpreter executes a program on a given environment and returns a value
pub struct Interpreter {
    program: Program,
    env: Rc<RefCell<Environment>>,
    temps: HashMap<Temp, DataType>,
}

impl Interpreter {
    pub fn new(program: Program, env: Rc<RefCell<Environment>>) -> Self {
        Self {
            program,
            env,
            temps: HashMap::new(),
        }
    }

    /// executes a block of instructions to a data-type
    pub fn execute(&mut self, label: &Label) -> Result<DataType> {
        if let Some(block) = self.program.get(label).cloned() {
            for instruction in &block.instructions {
                instruction.interpret(self)?;
            }

            return block.terminator.interpret(self);
        }

        Ok(DataType::Undefined)
    }

    /// evaluate a value to a data-type
    fn evaluate(&self, value: Value) -> Result<DataType> {
        match value {
            Value::Constant(c) => Ok(c),
            Value::Temp(t) => self
                .temps
                .get(&t)
                .cloned()
                .ok_or_else(|| Error::UndefinedTemp(t)),
            Value::Var(name) => self
                .env
                .borrow()
                .get(&name)
                .ok_or_else(|| Error::UndefinedVariable(name)),
        }
    }
}

/// generic trait for interpreting IR instructions/terminators with a shared `Interpreter` context.
pub trait Interpret<T> {
    /// execute this IR instruction/terminator using the given interpreter and produce a value of type `T`
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<T>;
}

impl Interpret<()> for Assign {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let val = interpreter.evaluate(self.value.clone())?;

        interpreter.temps.insert(self.dest, val);

        Ok(())
    }
}

impl Interpret<()> for Binary {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let op = self.op.clone();
        let left = interpreter.evaluate(self.left.clone())?;
        let right = interpreter.evaluate(self.right.clone())?;

        let result = match (&left, &right) {
            (DataType::Integer(l), DataType::Integer(r)) => {
                let l = **l;
                let r = **r;

                Ok(match op {
                    BinaryOperator::Add => DataType::Integer(Integer::new(l + r)),
                    BinaryOperator::Subtract => DataType::Integer(Integer::new(l - r)),
                    BinaryOperator::Multiply => DataType::Integer(Integer::new(l * r)),
                    BinaryOperator::Divide => DataType::Integer(Integer::new(l / r)),
                    BinaryOperator::LessThan => DataType::Boolean(Boolean::new(l < r)),
                    BinaryOperator::GreaterThan => DataType::Boolean(Boolean::new(l > r)),
                    BinaryOperator::LessThanOrEqual => DataType::Boolean(Boolean::new(l <= r)),
                    BinaryOperator::GreaterThanOrEqual => DataType::Boolean(Boolean::new(l >= r)),
                    BinaryOperator::Equal => DataType::Boolean(Boolean::new(l == r)),
                    BinaryOperator::NotEqual => DataType::Boolean(Boolean::new(l != r)),
                })
            }
            (DataType::Boolean(l), DataType::Boolean(r)) => {
                let l = **l;
                let r = **r;

                Ok(match op {
                    BinaryOperator::Equal => DataType::Boolean(Boolean::new(l == r)),
                    BinaryOperator::NotEqual => DataType::Boolean(Boolean::new(l != r)),
                    _ => {
                        return Err(Error::TypeMismatch(
                            op,
                            left.name_of().to_string(),
                            right.name_of().to_string(),
                        ));
                    }
                })
            }
            (DataType::String(l), DataType::String(r)) => {
                if op == BinaryOperator::Add {
                    Ok(DataType::String(String::new(format!("{}{}", l, r).into())))
                } else {
                    Err(Error::TypeMismatch(
                        op,
                        left.name_of().to_string(),
                        right.name_of().to_string(),
                    ))
                }
            }
            _ => Err(Error::TypeMismatch(
                op,
                left.name_of().to_string(),
                right.name_of().to_string(),
            )),
        }?;

        interpreter.temps.insert(self.dest, result);

        Ok(())
    }
}

impl Interpret<()> for Unary {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let op = self.op.clone();
        let operand = interpreter.evaluate(self.operand.clone())?;

        let result = match &operand {
            DataType::Integer(i) => Ok(match op {
                UnaryOperator::Negate => DataType::Integer(Integer::new(-**i)),
                UnaryOperator::Not => DataType::Boolean(Boolean::new(**i == 0)),
            }),
            DataType::Boolean(b) => Ok(match op {
                UnaryOperator::Not => DataType::Boolean(Boolean::new(!**b)),
                _ => return Err(Error::UnaryTypeMismatch(op, operand.name_of().to_string())),
            }),
            _ => Err(Error::UnaryTypeMismatch(op, operand.name_of().to_string())),
        }?;

        interpreter.temps.insert(self.dest, result);

        Ok(())
    }
}

impl Interpret<()> for StoreVar {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let val = interpreter.evaluate(self.value.clone())?;
        interpreter.env.borrow_mut().set(&self.name, val);

        Ok(())
    }
}

impl Interpret<()> for LoadVar {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let val = interpreter
            .env
            .borrow()
            .get(&self.name)
            .ok_or_else(|| Error::UndefinedVariable(self.name))?;
        interpreter.temps.insert(self.dest, val);

        Ok(())
    }
}

impl Interpret<()> for MakeArray {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let elements = self
            .elements
            .iter()
            .cloned()
            .map(|e| interpreter.evaluate(e))
            .collect::<Result<Vec<_>>>()?;

        interpreter
            .temps
            .insert(self.dest, DataType::Array(Array::new(elements)));

        Ok(())
    }
}

impl Interpret<()> for MakeObject {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let mut map = HashMap::new();

        for (k, v) in &self.pairs {
            let key_val = interpreter.evaluate(k.clone())?;
            let value = interpreter.evaluate(v.clone())?;

            // convert data-type to object-key
            let key = match key_val {
                DataType::String(s) => ObjectKey::String(s),
                DataType::Integer(i) => ObjectKey::Integer(i),
                DataType::Boolean(b) => ObjectKey::Boolean(b),
                _ => return Err(Error::IndexTypeMismatch(key_val.name_of().to_string())),
            };

            map.insert(key, value);
        }

        interpreter
            .temps
            .insert(self.dest, DataType::Object(Object::new(map)));

        Ok(())
    }
}

impl Interpret<()> for MakeFunction {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        use crate::ir::Function;

        interpreter.temps.insert(
            self.dest,
            DataType::Function(Rc::new(Function::new(
                self.params.clone(),
                self.body,
                Rc::clone(&interpreter.env),
            ))),
        );

        Ok(())
    }
}

impl Interpret<()> for Call {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let func = interpreter.evaluate(self.callable.clone())?;

        let args = self
            .args
            .iter()
            .cloned()
            .map(|a| interpreter.evaluate(a))
            .collect::<Result<Vec<_>>>()?;

        let result = match &func {
            DataType::Function(callable) => {
                let any = callable.as_ref() as &dyn Any;

                if let Some(func) = any.downcast_ref::<crate::ir::Function>() {
                    let scoped_env =
                        Rc::new(RefCell::new(Environment::new(Some(Rc::clone(&func.env)))));

                    for (param, arg) in func.parameters.iter().zip(args.iter()) {
                        scoped_env.borrow_mut().set(param, arg.clone());
                    }

                    let saved_env = Rc::clone(&interpreter.env);
                    let saved_temps = interpreter.temps.clone();

                    interpreter.env = scoped_env;
                    interpreter.temps.clear();

                    let result = interpreter.execute(&func.body_label);

                    interpreter.env = saved_env;
                    interpreter.temps = saved_temps;

                    result
                } else if let Some(func) = any.downcast_ref::<crate::runtime::builtins::Function>()
                {
                    (func.0)(args).map_err(Error::BuiltinError)
                } else {
                    Err(Error::NotCallable("FUNCTION".to_string()))
                }
            }

            _ => Err(Error::NotCallable(func.name_of().to_string())),
        }?;

        interpreter.temps.insert(self.dest, result);

        Ok(())
    }
}

impl Interpret<()> for Index {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        let indexable = interpreter.evaluate(self.indexable.clone())?;
        let index = interpreter.evaluate(self.index.clone())?;

        let result = match indexable {
            DataType::Array(arr) => {
                if let DataType::Integer(i) = index {
                    let idx = *i as usize;
                    Ok(arr.get(idx).cloned().unwrap_or(DataType::Null))
                } else {
                    Err(Error::IndexTypeMismatch(index.name_of().to_string()))
                }
            }
            DataType::Object(obj) => {
                // convert data-type to object-key
                let key = match index {
                    DataType::String(s) => ObjectKey::String(s),
                    DataType::Integer(i) => ObjectKey::Integer(i),
                    DataType::Boolean(b) => ObjectKey::Boolean(b),
                    _ => return Err(Error::IndexTypeMismatch(index.name_of().to_string())),
                };

                Ok(obj.get(&key).cloned().unwrap_or(DataType::Null))
            }
            _ => Err(Error::NotIndexable(indexable.name_of().to_string())),
        }?;

        interpreter.temps.insert(self.dest, result);

        Ok(())
    }
}

impl Interpret<()> for Instruction {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<()> {
        match self {
            Instruction::Assign(inner) => inner.interpret(interpreter),
            Instruction::Binary(inner) => inner.interpret(interpreter),
            Instruction::Unary(inner) => inner.interpret(interpreter),
            Instruction::StoreVar(inner) => inner.interpret(interpreter),
            Instruction::LoadVar(inner) => inner.interpret(interpreter),
            Instruction::MakeArray(inner) => inner.interpret(interpreter),
            Instruction::MakeObject(inner) => inner.interpret(interpreter),
            Instruction::MakeFunction(inner) => inner.interpret(interpreter),
            Instruction::Call(inner) => inner.interpret(interpreter),
            Instruction::Index(inner) => inner.interpret(interpreter),
        }
    }
}

impl Interpret<DataType> for Goto {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<DataType> {
        interpreter.execute(&self.target)
    }
}

impl Interpret<DataType> for Branch {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<DataType> {
        let cond = interpreter.evaluate(self.condition.clone())?;

        let is_true = match cond {
            DataType::Boolean(b) => *b,
            DataType::Integer(i) => *i != 0,
            _ => false,
        };

        let target = if is_true {
            self.true_label
        } else {
            self.false_label
        };

        interpreter.execute(&target)
    }
}

impl Interpret<DataType> for Return {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<DataType> {
        match &self.value {
            Some(v) => Ok(interpreter.evaluate(v.clone())?),
            None => Ok(DataType::Undefined),
        }
    }
}

impl Interpret<DataType> for Exit {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<DataType> {
        Ok(interpreter.evaluate(self.value.clone())?)
    }
}

impl Interpret<DataType> for Terminator {
    fn interpret(&self, interpreter: &mut Interpreter) -> Result<DataType> {
        match self {
            Terminator::Goto(inner) => inner.interpret(interpreter),
            Terminator::Branch(inner) => inner.interpret(interpreter),
            Terminator::Return(inner) => inner.interpret(interpreter),
            Terminator::Exit(inner) => inner.interpret(interpreter),
        }
    }
}

/// execute a program starting from the program's entry label
pub fn execute(program: Program, env: Rc<RefCell<Environment>>) -> Result<DataType> {
    Interpreter::new(program, env).execute(&Label(0))
}

/// execute a program starting from the given entry label
pub fn execute_from(
    program: Program,
    env: Rc<RefCell<Environment>>,
    entry: Label,
) -> Result<DataType> {
    Interpreter::new(program, env).execute(&entry)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::{ast::lexer, ast::parser, ir::transpiler};

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
            let statements = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(statements).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }
    }

    #[test]
    fn test_eval_function_expressions() {
        let input = "fn(x) { x + 2; };";

        let tokens = lexer::parse(input).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        match execute(transpiler::transpile(program).unwrap(), env) {
            Ok(DataType::Function(func)) => {
                use crate::ir::Function;
                let any = func.as_ref() as &dyn Any;
                let ir_func = any.downcast_ref::<Function>().unwrap();

                assert_eq!(ir_func.parameters.len(), 1);
                assert_eq!(&**ir_func.parameters.first().unwrap(), "x");
                assert_eq!(ir_func.body_label.0, 1);
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            let result = execute(transpiler::transpile(program).unwrap(), Rc::clone(&env)).unwrap();

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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
        let program = parser::parse(&tokens).unwrap();
        let env = Rc::new(RefCell::new(Environment::new(None)));

        match execute(transpiler::transpile(program).unwrap(), env) {
            Ok(DataType::Integer(value)) => assert_eq!(*value, 4),
            _ => panic!("expected an integer, got something else"),
        }
    }

    #[test]
    fn test_eval_builtin_len_function() {
        let tests = vec![(r#"len("");"#, 0), (r#"len("hello, world!");"#, 13)];

        for (input, expected) in tests {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "len(1);",
                r#"builtin error: argument[0] passed to BUILTIN("len") is not supported. got=1, want=STRING|ARRAY"#,
            ),
            (
                r#"len("one", "two");"#,
                r#"builtin error: extra arguments are passed to BUILTIN("len"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "first(1);",
                r#"builtin error: argument[0] passed to BUILTIN("first") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"first([1], [2]);"#,
                r#"builtin error: extra arguments are passed to BUILTIN("first"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Ok(DataType::Integer(value)) => assert_eq!(*value, expected),
                Ok(DataType::Null) => assert_eq!(0, expected),
                _ => panic!("expected an integer, got something else"),
            }
        }

        let test_errors = vec![
            (
                "last(1);",
                r#"builtin error: argument[0] passed to BUILTIN("last") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"last([1], [2]);"#,
                r#"builtin error: extra arguments are passed to BUILTIN("last"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
                r#"builtin error: argument[0] passed to BUILTIN("rest") is not supported. got=1, want=ARRAY"#,
            ),
            (
                r#"rest([1], [2]);"#,
                r#"builtin error: extra arguments are passed to BUILTIN("rest"). got=2, want=1"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
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
                r#"builtin error: argument[0] passed to BUILTIN("push") is not supported. got=1, want=ARRAY"#,
            ),
            (
                "push([1]);",
                r#"builtin error: extra arguments are passed to BUILTIN("push"). got=1, want=2"#,
            ),
        ];

        for (input, expected) in test_errors {
            let tokens = lexer::parse(input).unwrap();
            let program = parser::parse(&tokens).unwrap();
            let env = Rc::new(RefCell::new(Environment::new(None)));

            match execute(transpiler::transpile(program).unwrap(), env) {
                Err(error) => assert_eq!(error.to_string(), expected),
                _ => panic!("expected an error, got something else"),
            }
        }
    }
}
