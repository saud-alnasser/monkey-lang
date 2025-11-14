pub mod error;
mod functions;

use functions::*;

use crate::runtime::datatype::DataType;

pub fn builtins<'a>() -> [(&'a str, DataType); 6] {
    [len(), first(), last(), rest(), push(), puts()]
}
