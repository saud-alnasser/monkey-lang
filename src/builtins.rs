use crate::DataType;

pub fn builtins() -> [(&'static str, DataType); 1] {
    [(
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
    )]
}
