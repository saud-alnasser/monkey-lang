use std::rc::Rc;

use super::error::Error;
use crate::runtime::{
    builtins::function::Function,
    datatype::{Array, DataType, Integer},
};

/// list of built-in functions
pub fn definitions<'a>() -> [(&'a str, DataType); 6] {
    [len(), first(), last(), rest(), push(), puts()]
}

/// returns the length of the given string or array
pub fn len<'a>() -> (&'a str, DataType) {
    (
        "len",
        DataType::Function(Rc::new(Function::new(|args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "len".into(),
                    expected: "1".into(),
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::String(s) => Ok(DataType::Integer(Integer::new(s.len() as i64))),
                DataType::Array(a) => Ok(DataType::Integer(Integer::new(a.len() as i64))),
                _ => Err(Error::NotSupportedArgument {
                    builtin: "len".into(),
                    got: args[0].clone(),
                    expected: "STRING|ARRAY".into(),
                    position: 0,
                }),
            }
        }))),
    )
}

/// returns the first element of the given array
pub fn first<'a>() -> (&'a str, DataType) {
    (
        "first",
        DataType::Function(Rc::new(Function::new(|args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "first".into(),
                    expected: "1".into(),
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::Array(array) => {
                    if array.is_empty() {
                        return Ok(DataType::Null);
                    }

                    Ok(array[0].clone())
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "first".into(),
                    got: args[0].clone(),
                    expected: "ARRAY".into(),
                    position: 0,
                }),
            }
        }))),
    )
}

/// returns the last element of the given array
pub fn last<'a>() -> (&'a str, DataType) {
    (
        "last",
        DataType::Function(Rc::new(Function::new(|args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "last".into(),
                    expected: "1".into(),
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::Array(array) => {
                    if array.is_empty() {
                        return Ok(DataType::Null);
                    }

                    Ok(array[array.len() - 1].clone())
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "last".into(),
                    got: args[0].clone(),
                    expected: "ARRAY".into(),
                    position: 0,
                }),
            }
        }))),
    )
}

/// returns all elements except the first of the given array
pub fn rest<'a>() -> (&'a str, DataType) {
    (
        "rest",
        DataType::Function(Rc::new(Function::new(|args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "rest".into(),
                    expected: "1".into(),
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::Array(array) => {
                    if array.is_empty() {
                        return Ok(DataType::Array(Array::default()));
                    }

                    Ok(DataType::Array(Array::new(array[1..].to_vec())))
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "rest".into(),
                    got: args[0].clone(),
                    expected: "ARRAY".into(),
                    position: 0,
                }),
            }
        }))),
    )
}

/// appends an element to the end of the given array
pub fn push<'a>() -> (&'a str, DataType) {
    (
        "push",
        DataType::Function(Rc::new(Function::new(|args| {
            if args.len() != 2 {
                return Err(Error::ExtraArguments {
                    builtin: "push".into(),
                    expected: "2".into(),
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::Array(array) => {
                    let mut array = array.clone();

                    array.push(args[1].clone());

                    Ok(DataType::Array(array))
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "push".into(),
                    got: args[0].clone(),
                    expected: "ARRAY".into(),
                    position: 0,
                }),
            }
        }))),
    )
}

/// prints the given value to the standard output
pub fn puts<'a>() -> (&'a str, DataType) {
    (
        "puts",
        DataType::Function(Rc::new(Function::new(|args| {
            for arg in args {
                println!("{}", arg);
            }

            Ok(DataType::Undefined)
        }))),
    )
}
