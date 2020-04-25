use crate::core::parse::ast::Expr;
use crate::core::parse::ast::Identifier;
use crate::core::parse::ast::TypeID;
use std::fmt::Debug;

pub trait Type {
    fn type_id(&self) -> TypeID;
}

#[derive(PartialEq, Clone, Debug)]
pub struct String {
    pub contents: std::string::String,
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

impl<'a> Type for Function<'a> {
    fn type_id(&self) -> TypeID {
        TypeID::from("Function")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Bool {
    pub value: bool,
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

impl Type for Number {
    fn type_id(&self) -> TypeID {
        TypeID::from("Number")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Unit {}

impl Type for Unit {
    fn type_id(&self) -> TypeID {
        TypeID::from("Unit")
    }
}
