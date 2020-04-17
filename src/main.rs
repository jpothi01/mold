use bark::parse;
use std::env;
extern crate term;

fn main() {
    let user_input = std::env::args().nth(1).unwrap();
    let expr_result = parse::parse(&user_input);
    match expr_result {
        Err(e) => {
            let mut terminal = term::stdout().unwrap();
            terminal.fg(term::color::RED).unwrap();
            terminal.attr(term::Attr::Bold).unwrap();
            println!("Parsing failed.");
            println!("{}", e);
            let _ = terminal.reset();
        }
        Ok(expr) => {
            println!("{:?}", &expr);
        }
    }
}
