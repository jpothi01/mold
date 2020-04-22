use crate::parse::ast::AssignmentLHS;
use crate::parse::ast::Expr;
use crate::parse::ast::Identifier;
use crate::parse::ast::Op;
use crate::parse::ast::Statement;
use std::collections::HashMap;
use std::fmt;
use std::ops;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Number(f64),
    Function {
        args: Vec<Identifier>,
        body: &'a Expr,
    },
    String(String),
    Unit,
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Function { args, body } => {
                write!(f, "(fn ")?;
                for arg in args {
                    write!(f, " ({})", arg)?;
                }

                write!(f, " {:?})", body)
            }
            Value::String(s) => write!(f, "{}", s),
            Value::Unit => write!(f, "()"),
        }
    }
}

// Remember, the ast is long-lived and its lifetime strictly exceeds
// that of any evaluation. This is diferent from the lifetime of values
// themselves, which are dynmically created as the program runs.
#[derive(Clone)]
pub struct VariableContent<'a> {
    expr: &'a Expr,
    value: Value<'a>,
}
pub type Environment<'a> = HashMap<Identifier, VariableContent<'a>>;

type EvalResult<'a> = Result<Value<'a>, EvalError>;

// TODO: this needs to return an eval result, I think
impl<'a> ops::Add<Value<'a>> for Value<'a> {
    type Output = Value<'a>;
    fn add(self, rhs: Value) -> Value {
        match self {
            Value::Number(x) => match rhs {
                Value::Number(y) => Value::Number(x + y),
                _ => panic!("Cannot use '+' operator for Unit"),
            },
            _ => panic!("Cannot use '+' operator for Unit"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct EvalError {
    message: String,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

fn make_eval_error(_expr: &Expr, message: &str) -> EvalError {
    EvalError {
        message: String::from(message),
    }
}

pub fn eval<'a>(expr: &'a Expr, environment: &mut Environment<'a>) -> EvalResult<'a> {
    match expr {
        Expr::BinOp { op, lhs, rhs } => match op {
            Op::Plus => {
                let lhs_value = eval(&*lhs, environment)?;
                let rhs_value = eval(&*rhs, environment)?;
                Ok(lhs_value + rhs_value)
            }
        },
        Expr::Number(number) => Ok(Value::Number(*number)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Ident(id) => {
            if environment.contains_key(id.as_str()) {
                eval(environment[id].expr, environment)
            } else {
                Err(make_eval_error(
                    expr,
                    format!("Undefined variable {}", id).as_str(),
                ))
            }
        }
        Expr::Statement(statement, rest) => match statement {
            Statement::Assignment { lhs, rhs } => match lhs {
                AssignmentLHS::Single(identifier) => {
                    let rhs_value = eval(&**rhs, environment)?;
                    let variable_content = VariableContent {
                        expr: &**rhs,
                        value: rhs_value,
                    };

                    if !environment.contains_key(identifier.as_str()) {
                        environment.insert(identifier.clone(), variable_content);
                    } else {
                        *environment.get_mut(identifier.as_str()).unwrap() = variable_content;
                    }

                    eval(rest, environment)
                }
            },
            Statement::FunctionDefinition { name, args, body } => {
                let variable_content = VariableContent {
                    expr: &**body,
                    value: Value::Function {
                        args: args.clone(),
                        body: &**body,
                    },
                };
                if !environment.contains_key(name.as_str()) {
                    environment.insert(name.clone(), variable_content);
                } else {
                    *environment.get_mut(name.as_str()).unwrap() = variable_content;
                }

                eval(rest, environment)
            }
        },
        Expr::FunctionCall { name, args } => {
            if environment.contains_key(name.as_str()) {
                let function_definition = &environment[name.as_str()];
                let call_args = args;
                match function_definition.value.clone() {
                    Value::Function { args, body } => {
                        let def_args = args;
                        if call_args.len() != def_args.len() {
                            Err(make_eval_error(
                                expr,
                                format!(
                                    "Expected {} arguments, got {}",
                                    def_args.len(),
                                    call_args.len()
                                )
                                .as_str(),
                            ))
                        } else {
                            let mut function_environment = environment.clone();
                            let num_args = def_args.len();
                            for i in 0..num_args {
                                let arg_name = def_args[i].clone();
                                let arg_expr = &call_args[i];
                                let variable_content = VariableContent {
                                    expr: arg_expr,
                                    value: eval(arg_expr, environment)?,
                                };

                                // TODO: there's got to be a simpler way to do this
                                if !function_environment.contains_key(arg_name.as_str()) {
                                    function_environment.insert(arg_name.clone(), variable_content);
                                } else {
                                    *function_environment.get_mut(arg_name.as_str()).unwrap() =
                                        variable_content;
                                }
                            }
                            eval(body, &mut function_environment)
                        }
                    }
                    _ => Err(make_eval_error(
                        expr,
                        format!("Expected {} to be a function", function_definition.value).as_str(),
                    )),
                }
            } else {
                Err(make_eval_error(
                    expr,
                    format!("Undefined function {}", name).as_str(),
                ))
            }
        }
        Expr::Unit => Ok(Value::Unit),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn trivial_evaluation() {
        assert_eq!(
            eval(&Expr::Number(1f64), &mut Environment::new()),
            Ok(Value::Number(1f64))
        );
    }
}
