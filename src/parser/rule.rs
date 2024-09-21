use crate::{parser::error::Result, Lexer};

#[derive(Debug)]
pub struct Rule<'a, T> {
    pub consume: fn(lexer: &mut Lexer, previous: Option<&T>) -> Result<Option<T>>,
    pub dependents: Option<&'a [Rule<'a, T>]>,
}

impl<'a, T> Rule<'a, T> {
    pub fn parse(self: &Rule<'a, T>, lexer: &mut Lexer, previous: Option<&T>) -> Result<Option<T>> {
        match (self.consume)(lexer, previous)? {
            Some(rule_output) => {
                if let Some(dependents) = self.dependents {
                    for dependent in dependents.iter() {
                        if let Some(dependent_output) =
                            (dependent.consume)(lexer, Some(&rule_output))?
                        {
                            return Ok(Some(dependent_output));
                        }
                    }
                }

                Ok(Some(rule_output))
            }
            None => Ok(None),
        }
    }
}
