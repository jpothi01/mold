use crate::core::parse::ast::Expr;
use crate::core::parse::ast::Identifier;
use crate::core::parse::ast::TypeID;
use crate::core::rust::NativeFunction;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

pub trait Type {
    fn type_id(&self) -> TypeID;
}

#[derive(PartialEq, Clone, Debug)]
pub struct String {
    pub value: std::string::String,
}

impl From<std::string::String> for String {
    fn from(s: std::string::String) -> Self {
        String { value: s }
    }
}

impl Display for String {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Type for String {
    fn type_id(&self) -> TypeID {
        TypeID::from("String")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function<'a> {
    pub args: Vec<Identifier>,
    pub body: &'a Expr,
}

impl<'a> Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<function>")
    }
}

impl<'a> Type for Function<'a> {
    fn type_id(&self) -> TypeID {
        TypeID::from("Function")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct RustFunction {
    pub args: Vec<Identifier>,
    pub native_function: NativeFunction,
}

impl Display for RustFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<rust function>")
    }
}

impl<'a> Type for RustFunction {
    fn type_id(&self) -> TypeID {
        TypeID::from("RustFunction")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Bool {
    pub value: bool,
}

impl Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Type for Bool {
    fn type_id(&self) -> TypeID {
        TypeID::from("Bool")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Number {
    pub value: f64,
}

impl From<f64> for Number {
    fn from(n: f64) -> Self {
        Number { value: n }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Type for Number {
    fn type_id(&self) -> TypeID {
        TypeID::from("Number")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Unit;

impl Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "()")
    }
}

impl Type for Unit {
    fn type_id(&self) -> TypeID {
        TypeID::from("Unit")
    }
}
