# Mold

Looks like Rust, feels like Python.

Rust grows on machines, mold grows on beings.

## Strongly, dynamically typed

```
impl Number {
    fn increment(self) {
        self + 1
    }
}

impl String {
    fn increment(self) {
        self + "+ 1"
    }
}

a = 2
print(a.increment()) // Prints "3"

a = "Hello"
print(a.increment()) // Prints "Hello + 1"
```

## Seamless Rust interop

```
rust fn sqrt(s) {
    // This is a rust function, compiled by rustc
    match s {
        mold::Value::Number(n) => {
            mold::Value::Number(mold::Number::from(n.value.sqrt()))
        }
        _ => panic!("Expected a mold::Number")
    }
}

a = 3
b = 6

// Call rust function with mold
c = sqrt(a + b) // c is 3
```

