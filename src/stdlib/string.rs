use crate::core::eval::types;
use crate::core::Value;

pub fn len(value: Value) -> Value {
    match value {
        Value::String(s) => Value::Number(types::Number {
            value: s.contents.len() as f64,
        }),
        _ => panic!("Cannot call len() on something that's not a string"),
    }
}
