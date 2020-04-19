use crate::parse::ast::AssignmentLHS;
use crate::parse::ast::Expr;
use crate::parse::ast::Identifier;
use crate::parse::ast::Op;
use crate::parse::ast::Statement;
use std::collections::HashMap;
use std::fmt;
use std::ops;

pub enum Value {
    Number(f64),
    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Unit => write!(f, "()"),
        }
    }
}

pub struct VariableContent<'a> {
    expr: &'a Expr,
    value: Value,
}
pub type Environment<'a> = HashMap<Identifier, VariableContent<'a>>;

type EvalResult = Result<Value, EvalError>;

// TODO: this needs to return an eval result, I think
impl ops::Add<&Value> for &Value {
    type Output = Value;
    fn add(self, rhs: &Value) -> Value {
        match self {
            Value::Number(x) => match rhs {
                Value::Number(y) => Value::Number(x + y),
                Value::Unit => panic!("Cannot use '+' operator for Unit"),
            },
            Value::Unit => panic!("Cannot use '+' operator for Unit"),
        }
    }
}

#[derive(Debug)]
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

pub fn eval<'a>(expr: &'a Expr, environment: &mut Environment<'a>) -> EvalResult {
    match expr {
        Expr::BinOp { op, lhs, rhs } => match op {
            Op::Plus => {
                let lhs_value = eval(&*lhs, environment)?;
                let rhs_value = eval(&*rhs, environment)?;
                Ok(&lhs_value + &rhs_value)
            }
        },
        Expr::Number(number) => Ok(Value::Number(*number)),
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
            Statement::Assignment { lhs, rhs } => match &**lhs {
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
        },
        Expr::Unit => Ok(Value::Unit),
    }
}
