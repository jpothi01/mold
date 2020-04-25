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
