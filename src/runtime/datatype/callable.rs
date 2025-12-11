use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;

pub trait Callable: Debug + Display + Any + 'static {}
