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

    pub fn is_binop(c: char) -> bool {
        Op::from_char(c).is_some()
    }

    fn precedence(&self) -> i32 {
        match self {
            Op::Plus => 0,
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
    fn consume_until_nonwhitespace(&mut self) {
        while let Some(c) = self.next_character() {
            if c.is_whitespace() {
                self.consume_character();
            } else {
                break;
            }
        }
    }
    fn expect_character_and_consume(&mut self, character: char) -> Result<(), ParseError> {
        if let Some(c) = self.next_character() {
            if c == character {
                self.consume_character();
                return Ok(());
            }
        }

        Err(make_parse_error(
            self,
            format!("Expected '{}'", character).as_str(),
        ))
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

    let mut original_input_substring_start_index = error_character_index;
    while original_input_substring_start_index > 0
        && parser_state
            .original_input
            .chars()
            .nth(original_input_substring_start_index)
            .unwrap()
            != '\n'
    {
        original_input_substring_start_index -= 1;
    }

    let mut original_input_substring_end_index = error_character_index;
    while original_input_substring_end_index < parser_state.original_input.len()
        && parser_state
            .original_input
            .chars()
            .nth(original_input_substring_end_index)
            .unwrap()
            != '\n'
    {
        original_input_substring_end_index += 1;
    }

    let original_input_substring = &parser_state.original_input
        [original_input_substring_start_index..original_input_substring_end_index];
    let context = format!("\t{}\n\t{}^", original_input_substring, squiggle_string);
    ParseError {
        context: String::from(context),
        message: String::from(msg),
    }
}

pub type Identifier = String;

pub enum Expr {
    BinOp {
        op: Op,
        rhs: Box<Expr>,
        lhs: Box<Expr>,
    },
    Number(f64),
    Ident(Identifier),
    Assignment {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        rest: Box<Expr>,
    },
}

impl Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::BinOp { op, rhs, lhs } => write!(f, "({:?} {:?} {:?})", op, *rhs, *lhs),
            Expr::Number(n) => write!(f, "({:?})", n),
            Expr::Ident(i) => write!(f, "(id {:?})", i),
            Expr::Assignment { lhs, rhs, rest } => {
                write!(f, "(assign {:?} {:?} {:?})", *lhs, *rhs, *rest)
            }
        }
    }
}

fn parse_binop_rhs(
    parser_state: &mut ParserState,
    lhs: Expr,
    minimum_precedence: i32,
) -> Result<Expr, ParseError> {
    assert!(parser_state.remaining_input.len() > 0);
    println!("parse_binop_rhs: {:?}", parser_state);

    let mut new_lhs = lhs;
    loop {
        let maybe_next_character = parser_state.next_character();
        if maybe_next_character.is_none() {
            return Ok(new_lhs);
        }

        let next_character = maybe_next_character.unwrap();
        let maybe_op = Op::from_char(next_character);
        if maybe_op.is_none() {
            if next_character != ')' {
                return Err(make_parse_error(parser_state, "Expected operator."));
            }

            return Ok(new_lhs);
        }

        let op = maybe_op.unwrap();
        let op_precedence = op.precedence();

        if op_precedence < minimum_precedence {
            // Our current expression has higher precedence than the next, so we're done collecting
            // the terms for it now.
            return Ok(new_lhs);
        }

        parser_state.consume_character();

        let next_primary_expr = parse_primary(parser_state)?;
        println!("next_primary_expr: {:?}", next_primary_expr);
        println!("{:?}", parser_state);

        // Clean this up
        let rhs = if parser_state.remaining_input.len() > 0
            && Op::is_binop(parser_state.next_character().unwrap())
        {
            let next_op = Op::from_char(parser_state.next_character().unwrap()).unwrap();
            let next_precedence = next_op.precedence();
            if next_precedence > op_precedence {
                parse_binop_rhs(parser_state, next_primary_expr, op_precedence + 1)?
            } else {
                next_primary_expr
            }
        } else {
            next_primary_expr
        };

        new_lhs = Expr::BinOp {
            op: op,
            lhs: Box::new(new_lhs),
            rhs: Box::new(rhs),
        };
        println!("new_lhs: {:?}", new_lhs);
    }
}

fn parse_number(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    println!("parse_number: {:?}", parser_state);

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
    parser_state.remaining_input = match maybe_last_nonnumeric_index {
        None => "",
        Some(last_nonnumeric_index) => &parser_state.remaining_input[last_nonnumeric_index..],
    };
    match number_parse_result {
        Ok(number) => Ok(Expr::Number(number)),
        Err(_) => Err(make_parse_error(parser_state, "Expected number.")),
    }
}

fn parse_identifier(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    println!("parse_identifier: {:?}", parser_state);

    let is_part_of_identifier = |c: char| c.is_alphanumeric() || c == '_';
    let maybe_last_non_identifier_index = parser_state
        .remaining_input
        .chars()
        .position(|c| !is_part_of_identifier(c));
    let identifier_substring = match maybe_last_non_identifier_index {
        Some(last_non_identifier_index) => {
            &parser_state.remaining_input[..last_non_identifier_index]
        }
        None => parser_state.remaining_input,
    };

    if identifier_substring.len() == 0 {
        return Err(make_parse_error(parser_state, "Expected identifier"));
    }

    parser_state.remaining_input = match maybe_last_non_identifier_index {
        None => "",
        Some(last_non_identifier_index) => {
            &parser_state.remaining_input[last_non_identifier_index..]
        }
    };
    Ok(Expr::Ident(String::from(identifier_substring)))
}

fn parse_assignment(parser_state: &mut ParserState, lhs: Expr) -> Result<Expr, ParseError> {
    println!("parse_assignment: {:?}", parser_state);

    assert!(parser_state.next_character() == Some('='));
    parser_state.consume_character();
    parser_state.consume_until_nonwhitespace();

    match &lhs {
        Expr::Ident(_) => {
            let rhs = parse_primary(parser_state)?;
            parser_state.expect_character_and_consume('\n')?;
            let rest = parse_expr(parser_state)?;

            Ok(Expr::Assignment {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                rest: Box::new(rest),
            })
        }
        _ => Err(make_parse_error(
            parser_state,
            "Left-hand side of assignment must be identifier",
        )),
    }
}

fn parse_paren_expr(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    println!("parse_paren_expr: {:?}", parser_state);

    assert!(parser_state.next_character() == Some('('));
    parser_state.consume_character();
    let expr = parse_expr(parser_state)?;
    if parser_state.next_character() != Some(')') {
        return Err(make_parse_error(parser_state, "Expected ')'"));
    }

    parser_state.consume_character();
    Ok(expr)
}

fn parse_primary(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    // Base case: We parse the rhs of a binary or unary operation
    // Recursive case: We need to parse the rhs of a binary operation
    println!("parse_primary: {:?}", parser_state);

    parser_state.consume_until_nonwhitespace();
    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_none() {
        return Err(make_parse_error(
            parser_state,
            "Expected primary expression",
        ));
    }
    let next_character = maybe_next_character.unwrap();

    if next_character == '(' {
        return parse_paren_expr(parser_state);
    }

    if next_character.is_alphabetic() {
        return parse_identifier(parser_state);
    }

    return parse_number(parser_state);
}

fn parse_expr(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    println!("parse_expr: {:?}", parser_state);
    let primary = parse_primary(parser_state)?;

    parser_state.consume_until_nonwhitespace();
    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_some() {
        let next_character = maybe_next_character.unwrap();
        if Op::is_binop(next_character) {
            parse_binop_rhs(parser_state, primary, -1)
        } else if next_character == ')' {
            Ok(primary)
        } else if next_character == '=' {
            parse_assignment(parser_state, primary)
        } else {
            Err(make_parse_error(
                parser_state,
                "Expected a binary operator or ')'",
            ))
        }
    } else {
        Ok(primary)
    }
}

pub fn parse(input: &str) -> Result<Expr, ParseError> {
    let mut parser_state = ParserState {
        original_input: input,
        remaining_input: input,
    };

    parse_expr(&mut parser_state)
}
