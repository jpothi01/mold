use crate::Value;
use std::fs;

pub fn print(value: Value) -> Value {
    println!("{}", value);
    Value::Unit(crate::Unit {})
}

pub fn read_to_string(file_path: Value) -> Value {
    match file_path {
        Value::String(s) => {
            let contents = fs::read_to_string(s.value).unwrap();
            Value::String(crate::String::from(contents))
        }
        _ => panic!("Expected value of type 'String'"),
    }
}
