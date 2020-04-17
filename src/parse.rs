use std::fmt;
use std::fmt::Debug;

pub enum Op {
    Plus,
}

impl Op {
    pub fn from_char(c: char) -> Option<Op> {
        match c {
            '+' => Some(Op::Plus),
            _ => None,
        }
    }
}

impl Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Op::Plus => "+",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
pub struct ParseError {
    context: String,
    message: String,
}

#[derive(Debug)]
struct ParserState<'a> {
    original_input: &'a str,
    remaining_input: &'a str,
}

impl<'a> ParserState<'a> {
    fn next_character(&self) -> Option<char> {
        self.remaining_input.chars().nth(0)
    }
    fn consume_character(&mut self) {
        self.remaining_input = &self.remaining_input[1..]
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n {}", &self.message, &self.context)
    }
}

fn make_parse_error(parser_state: &ParserState, msg: &str) -> ParseError {
    let error_character_index =
        parser_state.original_input.len() - parser_state.remaining_input.len();
    let number_of_squiggles = error_character_index;
    let squiggle_string = "~".repeat(number_of_squiggles);
    let context = format!("\t{}\n\t{}^", parser_state.original_input, squiggle_string);
    ParseError {
        context: String::from(context),
        message: String::from(msg),
    }
}

pub enum Expr {
    BinOp {
        op: Op,
        rhs: Box<Expr>,
        lhs: Box<Expr>,
    },
    Number(f64),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::BinOp { op, rhs, lhs } => write!(f, "({:?} {:?} {:?})", op, *rhs, *lhs),
            Expr::Number(n) => write!(f, "({:?})", n),
        }
    }
}

fn parse_number(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    let is_part_of_number = |c: char| c.is_digit(10) || c == '.' || c == '-';
    let maybe_last_nonnumeric_index = parser_state
        .remaining_input
        .chars()
        .position(|c| !is_part_of_number(c));
    let numeric_substring = match maybe_last_nonnumeric_index {
        Some(last_nonnumeric_index) => &parser_state.remaining_input[..last_nonnumeric_index],
        None => parser_state.remaining_input,
    };
    let number_parse_result = numeric_substring.parse::<f64>();
    match number_parse_result {
        Ok(number) => Ok(Expr::Number(number)),
        Err(_) => Err(make_parse_error(parser_state, "Expected number.")),
    }
}

fn parse_primary(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    parse_number(parser_state)
}

pub fn parse(input: &str) -> Result<Expr, ParseError> {
    let mut parser_state = ParserState {
        original_input: input,
        remaining_input: input,
    };

    parse_primary(&mut parser_state)
}
