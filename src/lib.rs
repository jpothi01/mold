pub mod core;
pub mod stdlib;

pub type Value<'a> = core::Value<'a>;
pub type String = core::eval::types::String;
pub type Number = core::eval::types::Number;
