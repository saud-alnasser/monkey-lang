pub mod error;
mod functions;

use crate::DataType;
use functions::*;

pub fn builtins<'a>() -> [(&'a str, DataType); 6] {
    [len(), first(), last(), rest(), push(), puts()]
}
