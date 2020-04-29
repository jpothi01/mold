use mold::core::eval;
use mold::core::eval::Environment;
use mold::core::parse;
use mold::core::rust;
use mold::stdlib;
use std::fmt;
use std::fs;
extern crate dylib;
extern crate term;

// TODO proper error handling for user input
fn read_script_from_file(file_path: &str) -> String {
    fs::read_to_string(file_path).unwrap()
}

fn get_script() -> String {
    let first_argument = std::env::args().nth(1).unwrap();
    match first_argument.as_str() {
        "-e" => String::from(std::env::args().nth(2).unwrap()),
        _ => read_script_from_file(first_argument.as_str()),
    }
}

fn print_error<T: fmt::Display>(error: T) {
    let mut terminal = term::stdout().unwrap();
    terminal.fg(term::color::RED).unwrap();
    terminal.attr(term::Attr::Bold).unwrap();
    println!("{}", error);
    let _ = terminal.reset();
}

fn insert_stdlib(environment: &mut Environment) {
    // Obviously this needs to be magicalized
    environment.rust_functions.insert(
        parse::ast::Identifier::from("print"),
        mold::RustFunction {
            args: vec![parse::ast::Identifier::from("arg1")],
            native_function: rust::NativeFunction::Static1(rust::StaticNativeFunction1 {
                function: stdlib::io::print,
            }),
        },
    );

    environment.rust_functions.insert(
        parse::ast::Identifier::from("read_to_string"),
        mold::RustFunction {
            args: vec![parse::ast::Identifier::from("file_path")],
            native_function: rust::NativeFunction::Static1(rust::StaticNativeFunction1 {
                function: stdlib::io::read_to_string,
            }),
        },
    );
}

fn main() {
    let script = get_script();
    let expr_result = parse::parse(&script);

    if let Err(e) = &expr_result {
        return print_error(e);
    }

    let expr = &expr_result.unwrap();
    println!("{:?}", &expr);

    let mut environment = Environment::new();
    insert_stdlib(&mut environment);
    let eval_result = eval::eval(&expr, &mut environment);
    match eval_result {
        Ok(value) => {
            println!("{}", &value);
        }
        Err(e) => return print_error(e),
    };
}
