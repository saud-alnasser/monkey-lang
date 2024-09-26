pub mod error;
mod functions;

use functions::*;

pub const BUILTINS: [(&'static str, crate::DataType); 6] = [LEN, FIRST, LAST, REST, PUSH, PUTS];
