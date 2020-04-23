pub mod ast;

use ast::AssignmentLHS;
use ast::Expr;
use ast::Identifier;
use ast::Op;
use ast::Statement;
use std::fmt;
use std::fmt::Debug;

#[derive(Debug, PartialEq)]
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
        self.consume_n_characters(1)
    }
    fn consume_n_characters(&mut self, n: usize) {
        self.remaining_input = &self.remaining_input[n..]
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

// Does this character always terminate an expression?
fn is_expression_terminator(c: char) -> bool {
    c == ')' || c == '}' || c == ','
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n {}", &self.message, &self.context)
    }
}

type ParseResult = Result<Expr, ParseError>;

fn make_parse_error(parser_state: &ParserState, msg: &str) -> ParseError {
    let error_character_index =
        parser_state.original_input.len() - parser_state.remaining_input.len();

    let mut original_input_substring_start_index = error_character_index;
    while original_input_substring_start_index > 0
        && parser_state
            .original_input
            .chars()
            .nth(original_input_substring_start_index - 1)
            != Some('\n')
    {
        original_input_substring_start_index -= 1;
    }

    let mut original_input_substring_end_index = error_character_index;
    while original_input_substring_end_index < parser_state.original_input.len()
        && parser_state
            .original_input
            .chars()
            .nth(original_input_substring_end_index + 1)
            != Some('\n')
    {
        original_input_substring_end_index += 1;
    }

    let number_of_squiggles = error_character_index - original_input_substring_start_index;
    let squiggle_string = "~".repeat(number_of_squiggles);

    let original_input_substring = &parser_state.original_input
        [original_input_substring_start_index..original_input_substring_end_index];
    let context = format!("\n{}\n{}^", original_input_substring, squiggle_string);
    ParseError {
        context: String::from(context),
        message: String::from(msg),
    }
}

fn parse_binop_rhs(
    parser_state: &mut ParserState,
    lhs: Expr,
    minimum_precedence: i32,
) -> ParseResult {
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
            if !is_expression_terminator(next_character) && !next_character.is_whitespace() {
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

fn parse_number(parser_state: &mut ParserState) -> ParseResult {
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

fn parse_identifier(parser_state: &mut ParserState) -> Result<Identifier, ParseError> {
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
    Ok(Identifier::from(identifier_substring))
}

fn parse_assignment(parser_state: &mut ParserState, lhs: AssignmentLHS) -> ParseResult {
    println!("parse_assignment: {:?}", parser_state);

    assert!(parser_state.next_character() == Some('='));
    parser_state.consume_character();
    parser_state.consume_until_nonwhitespace();

    let rhs = parse_primary(parser_state)?;

    // TODO: this is messy. We want to support both with newline and without newline at the
    // last statement of a Unit-returning Statement-expr.
    let rest = if parser_state.remaining_input.len() >= 2 {
        parser_state.expect_character_and_consume('\n')?;
        parse_expr(parser_state)?
    } else {
        Expr::Unit
    };

    Ok(Expr::Statement(
        Statement::Assignment {
            lhs: lhs,
            rhs: Box::new(rhs),
        },
        Box::new(rest),
    ))
}

fn parse_paren_expr(parser_state: &mut ParserState) -> ParseResult {
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

fn parse_function_call_args(parser_state: &mut ParserState) -> Result<Vec<Expr>, ParseError> {
    println!("parse_function_call_args: {:?}", parser_state);

    assert!(parser_state.next_character() == Some('('));
    parser_state.consume_character();

    let mut args: Vec<Expr> = Vec::new();
    while parser_state.next_character() != Some(')') {
        if args.len() > 0 {
            parser_state.expect_character_and_consume(',')?;
        }
        parser_state.consume_until_nonwhitespace();
        args.push(parse_expr(parser_state)?);
        parser_state.consume_until_nonwhitespace();
    }

    parser_state.expect_character_and_consume(')')?;
    Ok(args)
}

fn parse_string_literal(parser_state: &mut ParserState) -> ParseResult {
    println!("parse_string_literal: {:?}", parser_state);

    assert!(parser_state.next_character() == Some('"'));
    parser_state.consume_character();

    let maybe_string_end_index = parser_state.remaining_input.find('"');
    match maybe_string_end_index {
        None => Err(make_parse_error(
            parser_state,
            "Unterminated string literal",
        )),
        Some(string_end_index) => {
            let string_literal = &parser_state.remaining_input[..string_end_index];
            // Consume the string itself
            parser_state.consume_n_characters(string_end_index);
            // Consume the terminating '"'
            parser_state.consume_character();
            Ok(Expr::String(String::from(string_literal)))
        }
    }
}

fn parse_method_call(parser_state: &mut ParserState, target: Expr) -> ParseResult {
    assert!(parser_state.next_character() == Some('.'));
    parser_state.consume_character();
    parser_state.consume_until_nonwhitespace();
    let method_name = parse_identifier(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    let function_call_args = parse_function_call_args(parser_state)?;
    return Ok(Expr::MethodCall {
        name: method_name,
        target: Box::new(target),
        args: function_call_args,
    });
}

fn parse_primary(parser_state: &mut ParserState) -> ParseResult {
    // Base case: We parse the rhs of a binary or unary operation
    // Recursive case: We need to parse the rhs of a binary operation
    println!("parse_primary: {:?}", parser_state);

    parser_state.consume_until_nonwhitespace();
    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_none() {
        return Ok(Expr::Unit);
    }
    let next_character = maybe_next_character.unwrap();

    if next_character == '(' {
        return parse_paren_expr(parser_state);
    }

    if next_character == '"' {
        return parse_string_literal(parser_state);
    }

    if next_character.is_alphabetic() {
        let identifier = parse_identifier(parser_state)?;
        parser_state.consume_until_nonwhitespace();
        let maybe_next_character = parser_state.next_character();
        match maybe_next_character {
            Some('(') => {
                let function_call_args = parse_function_call_args(parser_state)?;
                return Ok(Expr::FunctionCall {
                    name: identifier,
                    args: function_call_args,
                });
            }
            _ => return Ok(Expr::Ident(identifier)),
        }
    }

    return parse_number(parser_state);
}

fn parser_function_definition(parser_state: &mut ParserState) -> ParseResult {
    println!("parser_function_definition: {:?}", parser_state);

    assert!(parser_state.remaining_input.starts_with("fn "));
    parser_state.consume_n_characters(3);
    let name = parse_identifier(parser_state)?;

    parser_state.expect_character_and_consume('(')?;

    let mut args: Vec<Identifier> = Vec::new();
    while parser_state.next_character() != Some(')') {
        if args.len() > 0 {
            parser_state.expect_character_and_consume(',')?;
        }
        parser_state.consume_until_nonwhitespace();
        args.push(parse_identifier(parser_state)?);
        parser_state.consume_until_nonwhitespace();
    }

    parser_state.expect_character_and_consume(')')?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('{')?;

    let body = parse_expr(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('}')?;
    parser_state.consume_until_nonwhitespace();

    Ok(Expr::Statement(
        Statement::FunctionDefinition {
            name: name,
            args: args,
            body: Box::new(body),
        },
        Box::new(parse_expr(parser_state)?),
    ))
}

fn parse_expr(parser_state: &mut ParserState) -> ParseResult {
    println!("parse_expr: {:?}", parser_state);

    parser_state.consume_until_nonwhitespace();
    // An expression either starts with a primary expression or is a function definition
    if parser_state.remaining_input.starts_with("fn ") {
        return parser_function_definition(parser_state);
    }

    let primary = parse_primary(parser_state)?;

    parser_state.consume_until_nonwhitespace();
    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_some() {
        let next_character = maybe_next_character.unwrap();
        if Op::is_binop(next_character) {
            parse_binop_rhs(parser_state, primary, -1)
        } else if is_expression_terminator(next_character) {
            Ok(primary)
        } else if next_character == '=' {
            // TODO: don't parse assignment lhs from primary expression, since this is a hack
            match primary {
                Expr::Ident(identifier) => {
                    parse_assignment(parser_state, AssignmentLHS::Single(identifier))
                }
                _ => Err(make_parse_error(
                    parser_state,
                    "Left-hand side of assignment must be a variable name",
                )),
            }
        } else if next_character == '.' {
            parse_method_call(parser_state, primary)
        } else {
            Err(make_parse_error(
                parser_state,
                "Expected a binary operator, '=', or end of expression",
            ))
        }
    } else {
        Ok(primary)
    }
}

pub fn parse(input: &str) -> ParseResult {
    let mut parser_state = ParserState {
        original_input: input,
        remaining_input: input,
    };

    parse_expr(&mut parser_state)
}

#[cfg(test)]
mod tests {
    use super::ast::*;
    use super::*;
    #[test]
    fn parse_simple_binary_op() {
        assert_eq!(
            parse("1+2"),
            Ok(Expr::BinOp {
                op: Op::Plus,
                lhs: Box::new(Expr::Number(1f64)),
                rhs: Box::new(Expr::Number(2f64))
            })
        );
    }

    #[test]
    fn parse_grouped_ops() {
        assert_eq!(
            parse("1+((2+3)+4)+5"),
            Ok(Expr::BinOp {
                op: Op::Plus,
                lhs: Box::new(Expr::BinOp {
                    op: Op::Plus,
                    lhs: Box::new(Expr::Number(1f64)),
                    rhs: Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::BinOp {
                            op: Op::Plus,
                            lhs: Box::new(Expr::Number(2f64)),
                            rhs: Box::new(Expr::Number(3f64))
                        }),
                        rhs: Box::new(Expr::Number(4f64))
                    }),
                }),
                rhs: Box::new(Expr::Number(5f64)),
            })
        );
    }

    #[test]
    fn parse_non_void_statement() {
        assert_eq!(
            parse("a = 1\nb = 2\na + b"),
            Ok(Expr::Statement(
                Statement::Assignment {
                    lhs: AssignmentLHS::Single(Identifier::from("a")),
                    rhs: Box::new(Expr::Number(1f64))
                },
                Box::new(Expr::Statement(
                    Statement::Assignment {
                        lhs: AssignmentLHS::Single(Identifier::from("b")),
                        rhs: Box::new(Expr::Number(2f64))
                    },
                    Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Ident(Identifier::from("a"))),
                        rhs: Box::new(Expr::Ident(Identifier::from("b")))
                    })
                ))
            ))
        )
    }

    #[test]
    fn parse_void_statement() {
        assert_eq!(
            parse("a = 1"),
            Ok(Expr::Statement(
                Statement::Assignment {
                    lhs: AssignmentLHS::Single(Identifier::from("a")),
                    rhs: Box::new(Expr::Number(1f64))
                },
                Box::new(Expr::Unit)
            ))
        )
    }

    #[test]
    fn parse_function_definition() {
        assert_eq!(
            parse(
                r#"
                fn f(a, b) {
                    a + b
                }
            "#
            ),
            Ok(Expr::Statement(
                Statement::FunctionDefinition {
                    name: Identifier::from("f"),
                    args: vec!(Identifier::from("a"), Identifier::from("b")),
                    body: Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Ident(Identifier::from("a"))),
                        rhs: Box::new(Expr::Ident(Identifier::from("b"))),
                    })
                },
                Box::new(Expr::Unit)
            ))
        )
    }

    #[test]
    fn parse_function_multiple_function_definitions() {
        assert_eq!(
            parse(
                r#"
                fn f(a) {
                    a
                }

                fn g(b) {
                    b
                }
            "#
            ),
            Ok(Expr::Statement(
                Statement::FunctionDefinition {
                    name: Identifier::from("f"),
                    args: vec!(Identifier::from("a")),
                    body: Box::new(Expr::Ident(Identifier::from("a")))
                },
                Box::new(Expr::Statement(
                    Statement::FunctionDefinition {
                        name: Identifier::from("g"),
                        args: vec!(Identifier::from("b")),
                        body: Box::new(Expr::Ident(Identifier::from("b")))
                    },
                    Box::new(Expr::Unit)
                ))
            ))
        )
    }

    #[test]
    fn parse_function_call() {
        assert_eq!(
            parse("a(1, 2)"),
            Ok(Expr::FunctionCall {
                name: Identifier::from("a"),
                args: vec!(Expr::Number(1f64), Expr::Number(2f64),)
            })
        );
    }

    #[test]
    fn parse_function_call_with_complex_expression() {
        assert_eq!(
            parse("a(1+2, (3 + 4) + 5)"),
            Ok(Expr::FunctionCall {
                name: Identifier::from("a"),
                args: vec!(
                    Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Number(1f64)),
                        rhs: Box::new(Expr::Number(2f64)),
                    },
                    Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::BinOp {
                            op: Op::Plus,
                            lhs: Box::new(Expr::Number(3f64)),
                            rhs: Box::new(Expr::Number(4f64)),
                        }),
                        rhs: Box::new(Expr::Number(5f64)),
                    }
                )
            })
        );
    }

    #[test]
    fn parse_function_definition_then_call() {
        assert_eq!(
            parse(
                r#"
            fn f(x) {
                x + 1
            }
            
            f(1)"#
            ),
            Ok(Expr::Statement(
                Statement::FunctionDefinition {
                    name: Identifier::from("f"),
                    args: vec!(Identifier::from("x")),
                    body: Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Ident(Identifier::from("x"))),
                        rhs: Box::new(Expr::Number(1f64))
                    })
                },
                Box::new(Expr::FunctionCall {
                    name: Identifier::from("f"),
                    args: vec!(Expr::Number(1f64))
                })
            ))
        );
    }

    #[test]
    fn parse_string_literal() {
        assert_eq!(
            parse("a = \"hello, mold\""),
            Ok(Expr::Statement(
                Statement::Assignment {
                    lhs: AssignmentLHS::Single(Identifier::from("a")),
                    rhs: Box::new(Expr::String(String::from("hello, mold")))
                },
                Box::new(Expr::Unit)
            ))
        );
    }

    #[test]
    fn parse_method_call() {
        assert_eq!(
            parse("\"hello\".iter()"),
            Ok(Expr::MethodCall {
                name: Identifier::from("iter"),
                target: Box::new(Expr::String(String::from("hello"))),
                args: Vec::new()
            })
        )
    }
}
