use crate::{parser::error::Result, Lexer, Token};

#[derive(Debug)]
pub struct Rule<'a, T> {
    pub accept: fn(token: &Token) -> bool,
    pub parse: fn(lexer: &mut Lexer, previous: &Option<T>) -> Result<T>,
    pub dependents: Option<&'a [Rule<'a, T>]>,
}

impl<'a, T> Rule<'a, T> {
    pub fn parse(previous: &Option<T>, rule: &Rule<T>, lexer: &mut Lexer) -> Result<Option<T>> {
        if let Some(token) = lexer.peek() {
            if (rule.accept)(token) {
                let statement = Some((rule.parse)(lexer, previous)?);

                if let Some(dependents) = &rule.dependents {
                    for dependent in dependents.iter() {
                        if let Some(dependent_statement) =
                            Rule::parse(&statement, dependent, lexer)?
                        {
                            return Ok(Some(dependent_statement));
                        }
                    }
                }

                return Ok(statement);
            }
        }

        Ok(None)
    }
}
