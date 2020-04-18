extern crate mold;
use mold::eval::Value;

fn __mold_sqrt(x: f64) -> f64 {
    x.sqrt() + 1f64
}

fn __mold__adapter_sqrt(value: &Value) -> Value {
    match value {
        Value::Number(n) => Value::Number(__mold_sqrt(*n)),
    }
}

// For now, only one argument for dispatch
#[no_mangle]
pub fn __mold__dispatch(function_name: &str, value: &Value) -> Value {
    if function_name == "sqrt" {
        __mold__adapter_sqrt(value)
    } else {
        panic!("Dispatch failed for function named: {}", function_name)
    }
}
