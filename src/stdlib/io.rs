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

pub fn write<'a>(file_path: Value<'a>, content: Value<'a>) -> Value<'a> {
    match file_path {
        Value::String(file_path_string) => match content {
            Value::String(content_string) => {
                fs::write(file_path_string.value, content_string.value).unwrap();
                Value::Unit(crate::Unit {})
            }
            _ => panic!("Expected value of type 'String'"),
        },
        _ => panic!("Expected value of type 'String'"),
    }
}
