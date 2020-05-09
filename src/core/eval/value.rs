use super::types;
use super::types::Type;
use crate::core::parse::ast::TypeID;
use std::fmt;
use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Number(types::Number),
    Function(types::Function<'a>),
    String(types::String),
    Bool(types::Bool),
    Unit(types::Unit),
    Enum(types::Enum<'a>),
}

impl<'a> Type for Value<'a> {
    fn type_id(&self) -> TypeID {
        match self {
            Value::Number(n) => n.type_id(),
            Value::Function(f) => f.type_id(),
            Value::String(s) => s.type_id(),
            Value::Bool(b) => b.type_id(),
            Value::Unit(u) => u.type_id(),
            Value::Enum(e) => e.type_id(),
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n.value),
            Value::Bool(b) => write!(f, "{}", b.value),
            Value::Function(function) => {
                write!(f, "(fn ")?;
                for arg in &function.args {
                    write!(f, " ({})", arg)?;
                }

                write!(f, " {:?})", function.body)
            }
            Value::String(s) => write!(f, "{}", s.value),
            Value::Unit(_) => write!(f, "()"),
            Value::Enum(e) => write!(f, "{}", e),
        }
    }
}

impl<'a> types::Function<'a> {
    pub fn is_method(&self) -> bool {
        self.args.get(0).is_some() && self.args[0] == "self"
    }
}
