use crate::Value;

pub fn print(value: Value) -> Value {
    println!("{}", value);
    Value::Unit(crate::Unit {})
}
