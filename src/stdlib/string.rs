use crate::Value;

pub fn len(value: Value) -> Value {
    match value {
        Value::String(s) => Value::Number(crate::Number {
            value: s.value.len() as f64,
        }),
        _ => panic!("Cannot call len() on something that's not a string"),
    }
}
