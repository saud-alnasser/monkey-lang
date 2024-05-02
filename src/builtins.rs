use crate::DataType;

pub fn builtins() -> Vec<(&'static str, DataType)> {
    vec![
        (
            "len",
            DataType::BUILTIN {
                func: |args| {
                    if args.len() != 1 {
                        return Err(format!(
                            r#"extra arguments are passed to BUILTIN("len"). got={}, want=1"#,
                            args.len()
                        )
                        .into());
                    }

                    match &args[0] {
                        DataType::STRING(s) => Ok(DataType::INT(s.len() as i64)),
                        DataType::ARRAY(a) => Ok(DataType::INT(a.len() as i64)),
                        _ => Err(format!(
                            r#"argument passed to BUILTIN("len") is not supported. got={:?}, want=STRING|ARRAY"#,
                            args[0]
                        )
                        .into()),
                    }
                },
            },
        ),
        (
            "first",
            DataType::BUILTIN {
                func: |args| {
                    if args.len() != 1 {
                        return Err(format!(
                            r#"extra arguments are passed to BUILTIN("first"). got={}, want=1"#,
                            args.len()
                        )
                        .into());
                    }

                    match &args[0] {
                        DataType::ARRAY(array) => {
                            if array.is_empty() {
                                return Ok(DataType::NULL);
                            }

                            Ok(array[0].clone())
                        },
                        _ => Err(format!(
                            r#"argument passed to BUILTIN("first") is not supported. got={:?}, want=ARRAY"#,
                            args[0]
                        )
                        .into()),
                    }
                },
            },
        ),
        (
            "last",
            DataType::BUILTIN {
                func: |args| {
                    if args.len() != 1 {
                        return Err(format!(
                            r#"extra arguments are passed to BUILTIN("last"). got={}, want=1"#,
                            args.len()
                        )
                        .into());
                    }

                    match &args[0] {
                        DataType::ARRAY(array) => {
                            if array.is_empty() {
                                return Ok(DataType::NULL);
                            }

                            Ok(array[array.len() - 1].clone())
                        },
                        _ => Err(format!(
                            r#"argument passed to BUILTIN("last") is not supported. got={:?}, want=ARRAY"#,
                            args[0]
                        )
                        .into()),
                    }
                },
            },
        ),
        (
            "rest",
            DataType::BUILTIN {
                func: |args| {
                    if args.len() != 1 {
                        return Err(format!(
                            r#"extra arguments are passed to BUILTIN("rest"). got={}, want=1"#,
                            args.len()
                        )
                        .into());
                    }

                    match &args[0] {
                        DataType::ARRAY(array) => {
                            if array.is_empty() {
                                return Ok(DataType::ARRAY(vec![]));
                            }

                            Ok(DataType::ARRAY(array[1..].to_vec()))
                        },
                        _ => Err(format!(
                            r#"argument passed to BUILTIN("rest") is not supported. got={:?}, want=ARRAY"#,
                            args[0]
                        )
                        .into()),
                    }
                },
            },
        ),
        (
            "push",
            DataType::BUILTIN {
                func: |args| {
                    if args.len() != 2 {
                        return Err(format!(
                            r#"extra arguments are passed to BUILTIN("push"). got={}, want=2"#,
                            args.len()
                        )
                        .into());
                    }

                    match &args[0] {
                        DataType::ARRAY(array) => {
                            let mut new_array = array.clone();

                            new_array.push(args[1].clone());

                            Ok(DataType::ARRAY(new_array))
                        },
                        _ => Err(format!(
                            r#"argument[0] passed to BUILTIN("push") is not supported. got={:?}, want=ARRAY"#,
                            args[0]
                        )
                        .into()),
                    }
                },
            },
        ),
    ]
}
