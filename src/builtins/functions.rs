use super::error::Error;
use crate::DataType;

pub const LEN: (&'static str, DataType) = (
    "len",
    DataType::BUILTIN {
        func: |args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "len",
                    expected: "1",
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::STRING(s) => Ok(DataType::INT(s.len() as i64)),
                DataType::ARRAY(a) => Ok(DataType::INT(a.len() as i64)),
                _ => Err(Error::NotSupportedArgument {
                    builtin: "len",
                    got: args[0].clone(),
                    expected: "STRING|ARRAY",
                    position: 0,
                }),
            }
        },
    },
);

pub const FIRST: (&'static str, DataType) = (
    "first",
    DataType::BUILTIN {
        func: |args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "first",
                    expected: "1",
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::ARRAY(array) => {
                    if array.is_empty() {
                        return Ok(DataType::NULL);
                    }

                    Ok(array[0].clone())
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "first",
                    got: args[0].clone(),
                    expected: "ARRAY",
                    position: 0,
                }),
            }
        },
    },
);

pub const LAST: (&'static str, DataType) = (
    "last",
    DataType::BUILTIN {
        func: |args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "last",
                    expected: "1",
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::ARRAY(array) => {
                    if array.is_empty() {
                        return Ok(DataType::NULL);
                    }

                    Ok(array[array.len() - 1].clone())
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "last",
                    got: args[0].clone(),
                    expected: "ARRAY",
                    position: 0,
                }),
            }
        },
    },
);

pub const REST: (&'static str, DataType) = (
    "rest",
    DataType::BUILTIN {
        func: |args| {
            if args.len() != 1 {
                return Err(Error::ExtraArguments {
                    builtin: "rest",
                    expected: "1",
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::ARRAY(array) => {
                    if array.is_empty() {
                        return Ok(DataType::ARRAY(vec![]));
                    }

                    Ok(DataType::ARRAY(array[1..].to_vec()))
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "rest",
                    got: args[0].clone(),
                    expected: "ARRAY",
                    position: 0,
                }),
            }
        },
    },
);

pub const PUSH: (&'static str, DataType) = (
    "push",
    DataType::BUILTIN {
        func: |args| {
            if args.len() != 2 {
                return Err(Error::ExtraArguments {
                    builtin: "push",
                    expected: "2",
                    got: args.len().to_string(),
                });
            }

            match &args[0] {
                DataType::ARRAY(array) => {
                    let mut new_array = array.clone();

                    new_array.push(args[1].clone());

                    Ok(DataType::ARRAY(new_array))
                }
                _ => Err(Error::NotSupportedArgument {
                    builtin: "push",
                    got: args[0].clone(),
                    expected: "ARRAY",
                    position: 0,
                }),
            }
        },
    },
);
