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
rust fn wget(url) {
    use reqwest;

    match url {
        mold::String(s) => reqwest::blocking::get(s.value.as_str())?.json::<HashMap<String, String>>().unwrap()
        _ => panic!("Type error")
    }
}

json = wget("https://httpbin.org/ip")
print(json)
```

