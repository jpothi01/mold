use crate::core::parse::ast::TypeID;
use std::fmt::Debug;

pub trait Type {
    fn id(&self) -> TypeID;
}

#[derive(PartialEq, Clone, Debug)]
pub struct String {
    pub contents: std::string::String,
}

impl Type for String {
    fn id(&self) -> TypeID {
        TypeID::from("String")
    }
}
