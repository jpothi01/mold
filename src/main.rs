use bark::eval;
use bark::parse;
use std::env;
extern crate term;

fn main() {
    let user_input = std::env::args().nth(1).unwrap();
    let expr_result = parse::parse(&user_input);

    if let Err(e) = &expr_result {
        let mut terminal = term::stdout().unwrap();
        terminal.fg(term::color::RED).unwrap();
        terminal.attr(term::Attr::Bold).unwrap();
        println!("Parsing failed.");
        println!("{}", e);
        let _ = terminal.reset();
    }

    let expr = &expr_result.unwrap();
    println!("{:?}", &expr);

    println!("{}", eval::eval(&expr))
}
