pub mod ast;

use ast::AssignmentLHS;
use ast::Expr;
use ast::FunctionDefinition;
use ast::FunctionSignature;
use ast::Identifier;
use ast::IfElse;
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

macro_rules! debug_println {
    ($fmt:expr, $($arg:tt)*) => (if cfg ! ( debug_assertions ) {
        print!(concat!($fmt, "\n"), $($arg)*)
    });
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
            if self.remaining_input.starts_with("//") {
                self.consume_line_comment();
            }
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
    fn consume_line_comment(&mut self) {
        println!("consume_line_comment: {:?}", self);
        debug_assert!(self.remaining_input.starts_with("//"));
        let next_newline = self.remaining_input.find("\n");
        match next_newline {
            None => {
                self.remaining_input = "";
            }
            Some(index_of_newline) => {
                self.remaining_input = &self.remaining_input[index_of_newline + 1..];
            }
        }
    }
}

// Does this character always terminate an expression?
fn is_expression_terminator(c: char) -> bool {
    // Putting '}' here might cause trouble when I add structs
    c == ')' || c == '}' || c == ',' || c == '{'
}

fn starts_with_keyword(s: &str, keyword: &str) -> bool {
    let character_after_keyword = s.chars().nth(keyword.len());
    s.starts_with(keyword)
        && (character_after_keyword.is_none() || character_after_keyword.unwrap().is_whitespace())
}

mod keywords {
    pub const FUNCTION: &'static str = "fn";
    pub const IMPL: &'static str = "impl";
    pub const IF: &'static str = "if";
    pub const ELSE: &'static str = "else";
    pub const TRUE: &'static str = "true";
    pub const FALSE: &'static str = "false";
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

fn parse_binop(parser_state: &mut ParserState, consume: bool) -> Result<Op, ParseError> {
    debug_println!("parse_binop: {:?}", parser_state);
    debug_assert!(parser_state.remaining_input.len() >= 1);
    if let Some(single_character_op) = Op::from_str(&parser_state.remaining_input[0..1]) {
        if consume {
            parser_state.consume_character();
        }
        return Ok(single_character_op);
    }
    if parser_state.remaining_input.len() >= 2 {
        if let Some(double_character_op) = Op::from_str(&parser_state.remaining_input[0..2]) {
            if consume {
                parser_state.consume_n_characters(2);
            }
            return Ok(double_character_op);
        }
    }
    return Err(make_parse_error(parser_state, "Expected binary operator"));
}

// If you ever forget how the hell this works, go to the LLVM tutorial on parsing
// binary expressions. It's really confusing.
fn parse_binop_rhs(
    parser_state: &mut ParserState,
    lhs: Expr,
    minimum_precedence: i32,
) -> ParseResult {
    debug_assert!(parser_state.remaining_input.len() > 0);
    debug_println!("parse_binop_rhs: {:?}", parser_state);

    let mut new_lhs = lhs;
    loop {
        parser_state.consume_until_nonwhitespace();
        let maybe_next_character = parser_state.next_character();
        if maybe_next_character.is_none() {
            return Ok(new_lhs);
        }

        let next_character = maybe_next_character.unwrap();
        if !Op::is_first_char_of_binop(next_character) {
            if !is_expression_terminator(next_character) && !next_character.is_whitespace() {
                return Err(make_parse_error(parser_state, "Expected operator."));
            }

            return Ok(new_lhs);
        }

        let op = parse_binop(parser_state, false)?;
        let op_precedence = op.precedence();

        if op_precedence < minimum_precedence {
            // Our current expression has higher precedence than the next, so we're done collecting
            // the terms for it now.
            return Ok(new_lhs);
        }

        // Must delay consuming the op, because we might have chosen to not parse it in this call to
        // parse_binop_rhs.
        parse_binop(parser_state, true).unwrap();
        parser_state.consume_until_nonwhitespace();
        let next_primary_expr = parse_primary(parser_state)?;
        debug_println!("next_primary_expr: {:?}", next_primary_expr);
        debug_println!("{:?}", parser_state);

        parser_state.consume_until_nonwhitespace();

        let rhs = if parser_state.remaining_input.len() > 0
            && Op::is_first_char_of_binop(parser_state.next_character().unwrap())
        {
            let next_op = parse_binop(parser_state, false)?;
            let next_precedence = next_op.precedence();
            if next_precedence > op_precedence {
                parse_binop_rhs(parser_state, next_primary_expr, next_precedence)?
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
        debug_println!("new_lhs: {:?}", new_lhs);
    }
}

fn parse_number(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_number: {:?}", parser_state);

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
    debug_println!("parse_identifier: {:?}", parser_state);

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
    debug_println!("parse_assignment: {:?}", parser_state);

    debug_assert!(parser_state.next_character() == Some('='));
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
    debug_println!("parse_paren_expr: {:?}", parser_state);

    debug_assert!(parser_state.next_character() == Some('('));
    parser_state.consume_character();
    let expr = parse_expr(parser_state)?;
    if parser_state.next_character() != Some(')') {
        return Err(make_parse_error(parser_state, "Expected ')'"));
    }

    parser_state.consume_character();
    Ok(expr)
}

fn parse_function_call_args(parser_state: &mut ParserState) -> Result<Vec<Expr>, ParseError> {
    debug_println!("parse_function_call_args: {:?}", parser_state);

    debug_assert!(parser_state.next_character() == Some('('));
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
    debug_println!("parse_string_literal: {:?}", parser_state);

    debug_assert!(parser_state.next_character() == Some('"'));
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
    debug_assert!(parser_state.next_character() == Some('.'));
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

fn parse_bool_literal(parser_state: &mut ParserState) -> ParseResult {
    if starts_with_keyword(parser_state.remaining_input, keywords::TRUE) {
        parser_state.consume_n_characters(keywords::TRUE.len());
        return Ok(Expr::Bool(true));
    }

    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::FALSE
    ));
    parser_state.consume_n_characters(keywords::FALSE.len());
    Ok(Expr::Bool(false))
}

fn parse_primary(parser_state: &mut ParserState) -> ParseResult {
    // Base case: We parse the rhs of a binary or unary operation
    // Recursive case: We need to parse the rhs of a binary operation
    debug_println!("parse_primary: {:?}", parser_state);

    parser_state.consume_until_nonwhitespace();

    if starts_with_keyword(parser_state.remaining_input, keywords::IF) {
        return parse_if_else(parser_state);
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::TRUE)
        || starts_with_keyword(parser_state.remaining_input, keywords::FALSE)
    {
        return parse_bool_literal(parser_state);
    }

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

fn parse_function_definition(
    parser_state: &mut ParserState,
) -> Result<FunctionDefinition, ParseError> {
    debug_println!("parser_function_definition: {:?}", parser_state);

    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::FUNCTION
    ));
    parser_state.consume_n_characters(keywords::FUNCTION.len());
    parser_state.consume_until_nonwhitespace();
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

    Ok(FunctionDefinition {
        signature: FunctionSignature {
            name: name,
            args: args,
        },
        body: Box::new(body),
    })
}

fn parse_function_definition_expr(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parser_function_definition_expr: {:?}", parser_state);
    let function_definition = parse_function_definition(parser_state)?;
    let rest = parse_expr(parser_state)?;
    Ok(Expr::Statement(
        Statement::FunctionDefinition(function_definition),
        Box::new(rest),
    ))
}

fn parse_impl(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_impl: {:?}", parser_state);
    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::IMPL
    ));
    parser_state.consume_n_characters(keywords::IMPL.len());
    parser_state.consume_until_nonwhitespace();

    let type_identifier = parse_identifier(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('{')?;

    let mut methods: Vec<FunctionDefinition> = Vec::new();
    while parser_state.next_character().is_some() && parser_state.next_character() != Some('}') {
        parser_state.consume_until_nonwhitespace();
        methods.push(parse_function_definition(parser_state)?);
        parser_state.consume_until_nonwhitespace();
    }

    parser_state.expect_character_and_consume('}')?;
    let rest = parse_expr(parser_state)?;

    Ok(Expr::Statement(
        Statement::Impl {
            tid: type_identifier,
            methods: methods,
        },
        Box::new(rest),
    ))
}

fn parse_if_else(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_if_else: {:?}", parser_state);
    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::IF
    ));
    parser_state.consume_n_characters(keywords::IF.len());

    parser_state.consume_until_nonwhitespace();
    let condition = parse_expr(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('{')?;
    let if_branch = parse_expr(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('}')?;
    parser_state.consume_until_nonwhitespace();

    if !starts_with_keyword(parser_state.remaining_input, keywords::ELSE) {
        return Err(make_parse_error(
            parser_state,
            format!("Expected keyword '{}'", keywords::ELSE).as_str(),
        ));
    }

    parser_state.consume_n_characters(keywords::ELSE.len());
    parser_state.consume_until_nonwhitespace();

    let error_message = || format!("Expected keyword '{}' or '{{'", keywords::IF);
    let else_branch: ParseResult = if parser_state.next_character() == Some('{') {
        // Not "if else"
        parser_state.consume_character();
        let expr = parse_expr(parser_state)?;
        parser_state.consume_until_nonwhitespace();
        parser_state.expect_character_and_consume('}')?;
        Ok(expr)
    } else {
        if !starts_with_keyword(parser_state.remaining_input, keywords::IF) {
            Err(make_parse_error(parser_state, error_message().as_str()))
        } else {
            parse_if_else(parser_state)
        }
    };
    if else_branch.is_err() {
        return else_branch;
    }

    Ok(Expr::IfElse(IfElse {
        condition: Box::new(condition),
        if_branch: Box::new(if_branch),
        else_branch: Box::new(else_branch.unwrap()),
    }))
}

fn parse_expr(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_expr: {:?}", parser_state);

    parser_state.consume_until_nonwhitespace();

    if starts_with_keyword(parser_state.remaining_input, keywords::FUNCTION) {
        return parse_function_definition_expr(parser_state);
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::IMPL) {
        return parse_impl(parser_state);
    }

    let primary = parse_primary(parser_state)?;

    parser_state.consume_until_nonwhitespace();
    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_some() {
        let next_character = maybe_next_character.unwrap();
        if Op::is_first_char_of_binop(next_character) {
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
                format!(
                    "Expected a binary operator, '=', or end of expression, but found {}",
                    next_character
                )
                .as_str(),
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
                Statement::FunctionDefinition(FunctionDefinition {
                    name: Identifier::from("f"),
                    args: vec!(Identifier::from("a"), Identifier::from("b")),
                    body: Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Ident(Identifier::from("a"))),
                        rhs: Box::new(Expr::Ident(Identifier::from("b"))),
                    })
                }),
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
                Statement::FunctionDefinition(FunctionDefinition {
                    name: Identifier::from("f"),
                    args: vec!(Identifier::from("a")),
                    body: Box::new(Expr::Ident(Identifier::from("a")))
                }),
                Box::new(Expr::Statement(
                    Statement::FunctionDefinition(FunctionDefinition {
                        name: Identifier::from("g"),
                        args: vec!(Identifier::from("b")),
                        body: Box::new(Expr::Ident(Identifier::from("b")))
                    }),
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
                Statement::FunctionDefinition(FunctionDefinition {
                    name: Identifier::from("f"),
                    args: vec!(Identifier::from("x")),
                    body: Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Ident(Identifier::from("x"))),
                        rhs: Box::new(Expr::Number(1f64))
                    })
                }),
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

    #[test]
    fn parse_impl() {
        assert_eq!(
            parse(
                r#"impl String {
               fn len(self) {
                   2
               }

               fn g() {
                   3
               }
            }"#
            ),
            Ok(Expr::Statement(
                Statement::Impl {
                    tid: TypeID::from("String"),
                    methods: vec!(
                        FunctionDefinition {
                            name: Identifier::from("len"),
                            args: vec!(Identifier::from("self")),
                            body: Box::new(Expr::Number(2f64))
                        },
                        FunctionDefinition {
                            name: Identifier::from("g"),
                            args: Vec::new(),
                            body: Box::new(Expr::Number(3f64))
                        }
                    )
                },
                Box::new(Expr::Unit)
            ),)
        );
    }

    #[test]
    fn comments() {
        assert_eq!(
            parse(
                r#"fn f(x) {
                // Do the thing
                    x + 1
                }
                
                // Now call the thing
                f(1)
            "#
            ),
            parse(
                r#"fn f(x) {
                x + 1
            }
            
            f(1)
            "#
            )
        );
    }

    #[test]
    fn parse_if_else() {
        assert_eq!(
            parse("if x { 1 } else { 2 }"),
            Ok(Expr::IfElse(IfElse {
                condition: Box::new(Expr::Ident(Identifier::from("x"))),
                if_branch: Box::new(Expr::Number(1f64)),
                else_branch: Box::new(Expr::Number(2f64)),
            }))
        )
    }

    #[test]
    fn parse_if_else_if_else() {
        assert_eq!(
            parse("if x { 1 } else if true { 2 } else { false }"),
            Ok(Expr::IfElse(IfElse {
                condition: Box::new(Expr::Ident(Identifier::from("x"))),
                if_branch: Box::new(Expr::Number(1f64)),
                else_branch: Box::new(Expr::IfElse(IfElse {
                    condition: Box::new(Expr::Bool(true)),
                    if_branch: Box::new(Expr::Number(2f64)),
                    else_branch: Box::new(Expr::Bool(false)),
                })),
            }))
        )
    }

    #[test]
    fn parse_bool_literal() {
        assert_eq!(
            parse(
                r#"
            a = true
            b = false
            "#
            ),
            Ok(Expr::Statement(
                Statement::Assignment {
                    lhs: AssignmentLHS::Single(Identifier::from("a")),
                    rhs: Box::new(Expr::Bool(true))
                },
                Box::new(Expr::Statement(
                    Statement::Assignment {
                        lhs: AssignmentLHS::Single(Identifier::from("b")),
                        rhs: Box::new(Expr::Bool(false))
                    },
                    Box::new(Expr::Unit)
                ))
            ))
        )
    }

    #[test]
    fn parse_complex_binops() {
        assert_eq!(
            parse("true && false || false && true || false"),
            Ok(Expr::BinOp {
                op: Op::Or,
                lhs: Box::new(Expr::BinOp {
                    op: Op::Or,
                    lhs: Box::new(Expr::BinOp {
                        op: Op::And,
                        lhs: Box::new(Expr::Bool(true)),
                        rhs: Box::new(Expr::Bool(false)),
                    }),
                    rhs: Box::new(Expr::BinOp {
                        op: Op::And,
                        lhs: Box::new(Expr::Bool(false)),
                        rhs: Box::new(Expr::Bool(true)),
                    })
                }),
                rhs: Box::new(Expr::Bool(false))
            })
        );
    }
}
