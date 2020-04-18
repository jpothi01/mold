use mold::eval;
use mold::eval::Environment;
use mold::parse;
use std::env;
use std::fmt;
use std::fs;
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

fn main() {
    let script = get_script();
    let expr_result = parse::parse(&script);

    if let Err(e) = &expr_result {
        return print_error(e);
    }

    let expr = &expr_result.unwrap();
    println!("{:?}", &expr);

    let mut environment = Environment::new();
    let eval_result = eval::eval(&expr, &mut environment);
    match eval_result {
        Ok(value) => println!("{}", &value),
        Err(e) => return print_error(e),
    }
}
