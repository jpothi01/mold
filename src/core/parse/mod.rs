pub mod ast;

use ast::AssignmentLHS;
use ast::EnumAlternative;
use ast::EnumDefinition;
use ast::EnumItem;
use ast::Expr;
use ast::FunctionCall;
use ast::FunctionDefinition;
use ast::FunctionSignature;
use ast::Identifier;
use ast::IfElse;
use ast::Op;
use ast::RustFunctionDefinition;
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
    fn consume_until<P: Fn(char) -> bool>(&mut self, predicate: P) {
        loop {
            while self.remaining_input.starts_with("//") {
                self.consume_line_comment();
            }

            if self.next_character().is_some() && !predicate(self.next_character().unwrap()) {
                self.consume_character();
            } else {
                break;
            }
        }
    }
    fn consume_until_nonwhitespace_or_newline(&mut self) {
        self.consume_until(|c| c == '\n' || !c.is_whitespace())
    }
    fn consume_until_nonwhitespace(&mut self) {
        self.consume_until(|c| !c.is_whitespace())
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

fn is_block_terminator(c: char) -> bool {
    c == '}'
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
    pub const RUST_FUNCTION: &'static str = "rust fn";
    pub const WHILE: &'static str = "while";
    pub const LET: &'static str = "let";
    pub const ENUM: &'static str = "enum";
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

    let mut number_of_backwards_lines_of_context = 1;
    let mut squiggle_count_start_index: Option<usize> = None;
    let mut original_input_substring_start_index = error_character_index;
    while original_input_substring_start_index > 0 && number_of_backwards_lines_of_context >= 0 {
        original_input_substring_start_index -= 1;

        if parser_state
            .original_input
            .chars()
            .nth(original_input_substring_start_index)
            == Some('\n')
        {
            if squiggle_count_start_index.is_none() {
                squiggle_count_start_index = Some(original_input_substring_start_index);
            }
            number_of_backwards_lines_of_context -= 1;
        }
    }

    let mut number_of_forwards_lines_of_context = 1;
    let mut original_input_substring_end_index = error_character_index;
    while original_input_substring_end_index < parser_state.original_input.len()
        && number_of_forwards_lines_of_context >= 0
    {
        original_input_substring_end_index += 1;
        if parser_state
            .original_input
            .chars()
            .nth(original_input_substring_end_index)
            != Some('\n')
        {
            number_of_forwards_lines_of_context -= 1;
        }
    }

    let number_of_squiggles = error_character_index - squiggle_count_start_index.unwrap();
    let squiggle_string = "~".repeat(number_of_squiggles);

    let original_input_substring = &parser_state.original_input
        [original_input_substring_start_index..original_input_substring_end_index];
    let context = format!("\n{}\n{}^", original_input_substring, squiggle_string);
    ParseError {
        context: String::from(context),
        message: String::from(msg),
    }
}

fn parse_binop(parser_state: &mut ParserState, consume: bool) -> Option<Op> {
    debug_assert!(parser_state.remaining_input.len() >= 1);
    if let Some(single_character_op) = Op::from_str(&parser_state.remaining_input[0..1]) {
        if consume {
            parser_state.consume_character();
        }
        return Some(single_character_op);
    }
    if parser_state.remaining_input.len() >= 2 {
        if let Some(double_character_op) = Op::from_str(&parser_state.remaining_input[0..2]) {
            if consume {
                parser_state.consume_n_characters(2);
            }
            return Some(double_character_op);
        }
    }
    return None;
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
        parser_state.consume_until_nonwhitespace_or_newline();
        let maybe_next_character = parser_state.next_character();
        if maybe_next_character.is_none() {
            return Ok(new_lhs);
        }

        let maybe_op = parse_binop(parser_state, false);
        if maybe_op.is_none() {
            let next_character = maybe_next_character.unwrap();
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

        // Must delay consuming the op, because we might have chosen to not parse it in this call to
        // parse_binop_rhs.
        parse_binop(parser_state, true).unwrap();
        parser_state.consume_until_nonwhitespace_or_newline();
        let next_primary_expr = parse_primary(parser_state)?;
        debug_println!("next_primary_expr: {:?}", next_primary_expr);
        debug_println!("{:?}", parser_state);

        parser_state.consume_until_nonwhitespace_or_newline();

        let rhs = if parser_state.remaining_input.len() > 0 {
            let next_op = parse_binop(parser_state, false);
            if next_op.is_some() && next_op.unwrap().precedence() > op_precedence {
                parse_binop_rhs(
                    parser_state,
                    next_primary_expr,
                    next_op.unwrap().precedence(),
                )?
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

    let rhs = parse_expr(parser_state)?;

    // We want to support both with newline and without newline at the
    // last statement of a Unit-returning Statement-expr.

    parser_state.consume_until_nonwhitespace_or_newline();

    if let Some(_) = parser_state.next_character() {
        parser_state.expect_character_and_consume('\n')?;
    }

    let rest = parse_expr(parser_state)?;
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

fn parse_expr_as_statement(parser_state: &mut ParserState, primary_expr: Expr) -> ParseResult {
    match primary_expr {
        Expr::FunctionCall(function_call) => Ok(Expr::Statement(
            Statement::FunctionCall(function_call),
            Box::new(parse_expr(parser_state)?),
        )),
        _ => Ok(primary_expr), // Sometimes, this is wrong, but the error will be caught later
    }
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

fn parse_block(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_block: {:?}", parser_state);
    parser_state.consume_until_nonwhitespace();
    if parser_state.next_character() != Some('{') {
        return Err(make_parse_error(
            parser_state,
            "Expected '{' at beginning of block.",
        ));
    }
    parser_state.consume_character();
    let expr = parse_expr(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('}')?;
    Ok(Expr::Block(Box::new(expr)))
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

    if next_character == '{' {
        return parse_block(parser_state);
    }

    if next_character.is_alphabetic() {
        let identifier = parse_identifier(parser_state)?;

        parser_state.consume_until_nonwhitespace_or_newline();
        match parser_state.next_character() {
            Some('(') => {
                let function_call_args = parse_function_call_args(parser_state)?;
                return Ok(Expr::FunctionCall(FunctionCall {
                    name: identifier,
                    args: function_call_args,
                }));
            }
            Some(':') => return parse_enum_alternative(parser_state, identifier),
            _ => return Ok(Expr::Ident(identifier)),
        }
    }

    return parse_number(parser_state);
}

fn parse_function_signature(
    parser_state: &mut ParserState,
) -> Result<FunctionSignature, ParseError> {
    debug_println!("parse_function_signature: {:?}", parser_state);
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
    Ok(FunctionSignature {
        name: name,
        args: args,
    })
}

fn parse_function_definition(
    parser_state: &mut ParserState,
) -> Result<FunctionDefinition, ParseError> {
    debug_println!("parse_function_definition: {:?}", parser_state);

    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::FUNCTION
    ));
    parser_state.consume_n_characters(keywords::FUNCTION.len());
    parser_state.consume_until_nonwhitespace();
    let signature = parse_function_signature(parser_state)?;

    let body = parse_block(parser_state)?;

    Ok(FunctionDefinition {
        signature: signature,
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

fn parse_rust_function_definition(
    parser_state: &mut ParserState,
) -> Result<RustFunctionDefinition, ParseError> {
    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::RUST_FUNCTION
    ));
    parser_state.consume_n_characters(keywords::RUST_FUNCTION.len());
    parser_state.consume_until_nonwhitespace();
    let signature = parse_function_signature(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('{')?;

    // Dead stupid way to detect the end of the rust block: count curly braces
    // If it ever turns negative, that's where the enclosing '}' is.
    // This breaks if there are comments with braces in them, but whatever, those
    // braces will most likely be balanced.
    let mut number_of_braces = 0i32;
    let mut index_of_enclosing_brace = 0usize;
    for c in parser_state.remaining_input.chars() {
        if c == '{' {
            number_of_braces += 1;
        } else if c == '}' {
            number_of_braces -= 1;
        }

        if number_of_braces < 0 {
            break;
        }

        index_of_enclosing_brace += 1;
    }

    let body = String::from(&parser_state.remaining_input[0..index_of_enclosing_brace]);
    parser_state.remaining_input = &parser_state.remaining_input[index_of_enclosing_brace..];
    debug_assert!(parser_state.next_character() == Some('}'));
    parser_state.expect_character_and_consume('}')?;
    Ok(RustFunctionDefinition {
        signature: signature,
        body: body,
    })
}

fn parse_rust_function_definition_expr(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_rust_function_definition_expr: {:?}", parser_state);
    let function_definition = parse_rust_function_definition(parser_state)?;
    let rest = parse_expr(parser_state)?;
    Ok(Expr::Statement(
        Statement::RustFunctionDefinition(function_definition),
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

fn parse_enum_definition(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_enum_definition: {:?}", parser_state);
    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::ENUM
    ));
    parser_state.consume_n_characters(keywords::ENUM.len());
    parser_state.consume_until_nonwhitespace();

    let type_identifier = parse_identifier(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    parser_state.expect_character_and_consume('{')?;
    parser_state.consume_until_nonwhitespace();

    let mut alternatives: Vec<EnumItem> = Vec::new();
    while parser_state.next_character().is_some() && parser_state.next_character() != Some('}') {
        if alternatives.len() > 0 {
            parser_state.expect_character_and_consume(',')?;
        }

        parser_state.consume_until_nonwhitespace();

        fn parse_enum_alternative(parser_state: &mut ParserState) -> Result<EnumItem, ParseError> {
            let mut associated_values: Vec<Identifier> = Vec::new();
            let tag = parse_identifier(parser_state)?;
            if parser_state.next_character() != Some('(') {
                return Ok(EnumItem {
                    tag: tag,
                    associated_values: associated_values,
                });
            }

            parser_state.consume_character();
            while parser_state.next_character() != Some(')') {
                if associated_values.len() > 0 {
                    parser_state.expect_character_and_consume(',')?;
                }
                parser_state.consume_until_nonwhitespace();
                associated_values.push(parse_identifier(parser_state)?);
                parser_state.consume_until_nonwhitespace();
            }
            parser_state.consume_character();

            return Ok(EnumItem {
                tag: tag,
                associated_values: associated_values,
            });
        }

        alternatives.push(parse_enum_alternative(parser_state)?);
        parser_state.consume_until_nonwhitespace();
    }

    parser_state.expect_character_and_consume('}')?;
    let rest = parse_expr(parser_state)?;

    Ok(Expr::Statement(
        Statement::EnumDefinition(EnumDefinition {
            name: type_identifier,
            alternatives: alternatives,
        }),
        Box::new(rest),
    ))
}

fn parse_enum_alternative(parser_state: &mut ParserState, enum_name: Identifier) -> ParseResult {
    parser_state.expect_character_and_consume(':')?;
    parser_state.expect_character_and_consume(':')?;
    let alternative_name = parse_identifier(parser_state)?;

    if parser_state.next_character() != Some('(') {
        return Ok(Expr::EnumAlternative(EnumAlternative {
            enum_name: enum_name,
            alternative_name: alternative_name,
            associated_values: vec![],
        }));
    }

    parser_state.consume_character();
    let mut associated_values: Vec<Expr> = Vec::new();
    while parser_state.next_character() != Some(')') {
        if associated_values.len() > 0 {
            parser_state.expect_character_and_consume(',')?;
        }
        parser_state.consume_until_nonwhitespace();
        associated_values.push(parse_expr(parser_state)?);
        parser_state.consume_until_nonwhitespace();
    }
    parser_state.consume_character();

    return Ok(Expr::EnumAlternative(EnumAlternative {
        enum_name: enum_name,
        alternative_name: alternative_name,
        associated_values: associated_values,
    }));
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
    let if_branch = parse_block(parser_state)?;
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
        parse_block(parser_state)
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

fn parse_while(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_while: {:?}", parser_state);
    debug_assert!(starts_with_keyword(
        parser_state.remaining_input,
        keywords::WHILE
    ));

    parser_state.consume_n_characters(keywords::WHILE.len());
    parser_state.consume_until_nonwhitespace();
    let condition = parse_expr(parser_state)?;
    parser_state.consume_until_nonwhitespace();
    let body = parse_block(parser_state)?;
    parser_state.consume_until_nonwhitespace_or_newline();
    parser_state.expect_character_and_consume('\n')?;
    let rest = parse_expr(parser_state)?;
    Ok(Expr::Statement(
        Statement::While {
            condition: Box::new(condition),
            body: Box::new(body),
        },
        Box::new(rest),
    ))
}

fn parse_expr(parser_state: &mut ParserState) -> ParseResult {
    debug_println!("parse_expr: {:?}", parser_state);

    parser_state.consume_until_nonwhitespace();

    // Take care of Unit-returning blocks and top-levels
    match parser_state.next_character() {
        None => return Ok(Expr::Unit),
        Some(c) => {
            if is_block_terminator(c) {
                return Ok(Expr::Unit);
            }
        }
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::RUST_FUNCTION) {
        return parse_rust_function_definition_expr(parser_state);
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::FUNCTION) {
        return parse_function_definition_expr(parser_state);
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::IMPL) {
        return parse_impl(parser_state);
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::ENUM) {
        return parse_enum_definition(parser_state);
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::WHILE) {
        return parse_while(parser_state);
    }

    if starts_with_keyword(parser_state.remaining_input, keywords::LET) {
        // TODO: let currently is cosmetic to make typing mold code feel more natural
        // for rust users. Maybe it should be impued with variable declaration semantics as well?
        parser_state.consume_n_characters(keywords::LET.len());
        return parse_expr(parser_state);
    }

    let primary = parse_primary(parser_state)?;

    parser_state.consume_until_nonwhitespace_or_newline();
    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_none() {
        return Ok(primary);
    }

    if let Some(_) = parse_binop(parser_state, false) {
        return parse_binop_rhs(parser_state, primary, -1);
    }

    let next_character = maybe_next_character.unwrap();
    if is_expression_terminator(next_character) {
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
    } else if next_character == '\n' {
        parse_expr_as_statement(parser_state, primary)
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
    fn parse_unit_returning_toplevel() {
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
    fn parse_unit_returning_block() {
        assert_eq!(
            parse(
                r#"
{
    a = 1
    b = 2
}
            "#
            ),
            Ok(Expr::Block(Box::new(Expr::Statement(
                Statement::Assignment {
                    lhs: AssignmentLHS::Single(Identifier::from("a")),
                    rhs: Box::new(Expr::Number(1f64))
                },
                Box::new(Expr::Statement(
                    Statement::Assignment {
                        lhs: AssignmentLHS::Single(Identifier::from("b")),
                        rhs: Box::new(Expr::Number(2f64))
                    },
                    Box::new(Expr::Unit)
                ))
            ))))
        );
    }

    #[test]
    fn parse_empty_block() {
        assert_eq!(
            parse(
                r#"
{

}
            "#
            ),
            Ok(Expr::Block(Box::new(Expr::Unit)))
        )
    }

    #[test]
    fn parse_cascading_assignments() {
        assert_eq!(
            parse("a = 1\nb = a\nb"),
            Ok(Expr::Statement(
                Statement::Assignment {
                    lhs: AssignmentLHS::Single(Identifier::from("a")),
                    rhs: Box::new(Expr::Number(1f64))
                },
                Box::new(Expr::Statement(
                    Statement::Assignment {
                        lhs: AssignmentLHS::Single(Identifier::from("b")),
                        rhs: Box::new(Expr::Ident(Identifier::from("a")))
                    },
                    Box::new(Expr::Ident(Identifier::from("b")))
                ))
            ))
        );
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
                    signature: FunctionSignature {
                        name: Identifier::from("f"),
                        args: vec!(Identifier::from("a"), Identifier::from("b")),
                    },
                    body: Box::new(Expr::Block(Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Ident(Identifier::from("a"))),
                        rhs: Box::new(Expr::Ident(Identifier::from("b"))),
                    })))
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
                    signature: FunctionSignature {
                        name: Identifier::from("f"),
                        args: vec!(Identifier::from("a")),
                    },
                    body: Box::new(Expr::Block(Box::new(Expr::Ident(Identifier::from("a")))))
                }),
                Box::new(Expr::Statement(
                    Statement::FunctionDefinition(FunctionDefinition {
                        signature: FunctionSignature {
                            name: Identifier::from("g"),
                            args: vec!(Identifier::from("b")),
                        },
                        body: Box::new(Expr::Block(Box::new(Expr::Ident(Identifier::from("b")))))
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
            Ok(Expr::FunctionCall(FunctionCall {
                name: Identifier::from("a"),
                args: vec!(Expr::Number(1f64), Expr::Number(2f64),)
            }))
        );
    }

    #[test]
    fn parse_function_call_with_complex_expression() {
        assert_eq!(
            parse("a(1+2, (3 + 4) + 5)"),
            Ok(Expr::FunctionCall(FunctionCall {
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
            }))
        );
    }

    #[test]
    fn parse_function_call_statement() {
        assert_eq!(
            parse("a(1, 2)\n2"),
            Ok(Expr::Statement(
                Statement::FunctionCall(FunctionCall {
                    name: Identifier::from("a"),
                    args: vec!(Expr::Number(1f64), Expr::Number(2f64),)
                }),
                Box::new(Expr::Number(2f64))
            ))
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
                    signature: FunctionSignature {
                        name: Identifier::from("f"),
                        args: vec!(Identifier::from("x")),
                    },
                    body: Box::new(Expr::Block(Box::new(Expr::BinOp {
                        op: Op::Plus,
                        lhs: Box::new(Expr::Ident(Identifier::from("x"))),
                        rhs: Box::new(Expr::Number(1f64))
                    })))
                }),
                Box::new(Expr::FunctionCall(FunctionCall {
                    name: Identifier::from("f"),
                    args: vec!(Expr::Number(1f64))
                }))
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
                            signature: FunctionSignature {
                                name: Identifier::from("len"),
                                args: vec!(Identifier::from("self")),
                            },
                            body: Box::new(Expr::Block(Box::new(Expr::Number(2f64))))
                        },
                        FunctionDefinition {
                            signature: FunctionSignature {
                                name: Identifier::from("g"),
                                args: Vec::new(),
                            },
                            body: Box::new(Expr::Block(Box::new(Expr::Number(3f64))))
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
                if_branch: Box::new(Expr::Block(Box::new(Expr::Number(1f64)))),
                else_branch: Box::new(Expr::Block(Box::new(Expr::Number(2f64)))),
            }))
        )
    }

    #[test]
    fn parse_if_else_if_else() {
        assert_eq!(
            parse("if x { 1 } else if true { 2 } else { false }"),
            Ok(Expr::IfElse(IfElse {
                condition: Box::new(Expr::Ident(Identifier::from("x"))),
                if_branch: Box::new(Expr::Block(Box::new(Expr::Number(1f64)))),
                else_branch: Box::new(Expr::IfElse(IfElse {
                    condition: Box::new(Expr::Bool(true)),
                    if_branch: Box::new(Expr::Block(Box::new(Expr::Number(2f64)))),
                    else_branch: Box::new(Expr::Block(Box::new(Expr::Bool(false)))),
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

    #[test]
    fn parse_rust_function() {
        assert_eq!(
            parse(
                r#"
rust fn print() {
    println!("Hello, world!")
}

print()"#
            ),
            Ok(Expr::Statement(
                Statement::RustFunctionDefinition(RustFunctionDefinition {
                    signature: FunctionSignature {
                        name: Identifier::from("print"),
                        args: vec!()
                    },
                    body: String::from(
                        r#"
    println!("Hello, world!")
"#
                    )
                }),
                Box::new(Expr::FunctionCall(FunctionCall {
                    name: Identifier::from("print"),
                    args: vec!()
                }))
            ))
        )
    }

    #[test]
    fn parse_block() {
        assert_eq!(
            parse("{ a + b }"),
            Ok(Expr::Block(Box::new(Expr::BinOp {
                op: Op::Plus,
                lhs: Box::new(Expr::Ident(Identifier::from("a"))),
                rhs: Box::new(Expr::Ident(Identifier::from("b"))),
            })))
        );
    }

    #[test]
    fn parse_enum_definition() {
        assert_eq!(
            parse(
                r#"
        enum Option {
            None,
            Some(value)
        }"#
            ),
            Ok(Expr::Statement(
                Statement::EnumDefinition(EnumDefinition {
                    name: TypeID::from("Option"),
                    alternatives: vec!(
                        EnumItem {
                            tag: Identifier::from("None"),
                            associated_values: vec!()
                        },
                        EnumItem {
                            tag: Identifier::from("Some"),
                            associated_values: vec!(Identifier::from("value"))
                        }
                    )
                }),
                Box::new(Expr::Unit)
            ))
        );
    }

    #[test]
    fn parse_enum_alternative() {
        assert_eq!(
            parse("a = MyEnum::True\nOtherEnum::Tuple(1 + 2, \"hello\")"),
            Ok(Expr::Statement(
                Statement::Assignment {
                    lhs: AssignmentLHS::Single(Identifier::from("a")),
                    rhs: Box::new(Expr::EnumAlternative(EnumAlternative {
                        enum_name: TypeID::from("MyEnum"),
                        alternative_name: Identifier::from("True"),
                        associated_values: vec!()
                    }))
                },
                Box::new(Expr::EnumAlternative(EnumAlternative {
                    enum_name: TypeID::from("OtherEnum"),
                    alternative_name: Identifier::from("Tuple"),
                    associated_values: vec!(
                        Expr::BinOp {
                            op: Op::Plus,
                            lhs: Box::new(Expr::Number(1f64)),
                            rhs: Box::new(Expr::Number(2f64)),
                        },
                        Expr::String(String::from("hello"))
                    )
                }))
            ))
        )
    }
}
