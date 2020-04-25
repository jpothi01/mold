pub mod types;

use super::parse::ast::AssignmentLHS;
use super::parse::ast::Expr;
use super::parse::ast::FunctionDefinition;
use super::parse::ast::Identifier;
use super::parse::ast::Op;
use super::parse::ast::Statement;
use super::parse::ast::TypeID;
use std::collections::HashMap;
use std::fmt;
use std::ops;
use types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Number(types::Number),
    Function(types::Function<'a>),
    String(types::String),
    Bool(types::Bool),
    Unit(types::Unit),
}

impl<'a> Type for Value<'a> {
    fn type_id(&self) -> TypeID {
        match self {
            Value::Number(n) => n.type_id(),
            Value::Function(f) => f.type_id(),
            Value::String(s) => s.type_id(),
            Value::Bool(b) => b.type_id(),
            Value::Unit(u) => u.type_id(),
        }
    }
}

impl<'a> Value<'a> {
    fn type_id(&self) -> TypeID {
        match self {
            Value::String(s) => s.type_id(),
            _ => TypeID::from("Value"),
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n.value),
            Value::Bool(b) => write!(f, "{}", b.value),
            Value::Function(function) => {
                write!(f, "(fn ")?;
                for arg in &function.args {
                    write!(f, " ({})", arg)?;
                }

                write!(f, " {:?})", function.body)
            }
            Value::String(s) => write!(f, "{}", s.contents),
            Value::Unit(_) => write!(f, "()"),
        }
    }
}

impl<'a> types::Function<'a> {
    fn is_method(&self) -> bool {
        self.args.get(0).is_some() && self.args[0] == "self"
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

pub type Methods<'a> = HashMap<Identifier, types::Function<'a>>;
#[derive(Clone)]
pub struct TypeContent<'a> {
    methods: Methods<'a>,
}
pub type Variables<'a> = HashMap<Identifier, VariableContent<'a>>;
pub type Types<'a> = HashMap<TypeID, TypeContent<'a>>;

#[derive(Clone)]
pub struct Environment<'a> {
    variables: Variables<'a>,
    types: Types<'a>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        let mut environment = Environment {
            variables: Variables::new(),
            types: Types::new(),
        };

        // Hack to insert built-in types at startup
        let builtin_type_ids = ["String", "Number", "Function", "Bool"];
        for id in builtin_type_ids.iter() {
            environment.types.insert(
                String::from(*id),
                TypeContent {
                    methods: Methods::new(),
                },
            );
        }

        environment
    }
}

type EvalResult<'a> = Result<Value<'a>, EvalError>;

// TODO: this needs to return an eval result, I think
impl<'a> ops::Add<Value<'a>> for Value<'a> {
    type Output = Value<'a>;
    fn add(self, rhs: Value) -> Value {
        match self {
            Value::Number(x) => match rhs {
                Value::Number(y) => Value::Number(types::Number {
                    value: x.value + y.value,
                }),
                _ => panic!("Mismatched types for '+'"),
            },
            Value::String(s1) => match rhs {
                Value::String(s2) => Value::String(types::String {
                    contents: s1.contents + s2.contents.as_str(),
                }),
                _ => panic!("Mismatched types for '+'"),
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

fn eval_function_definition<'a>(
    function_definition: &'a FunctionDefinition,
    environment: &mut Environment<'a>,
) -> Result<types::Function<'a>, EvalError> {
    Ok(types::Function {
        args: function_definition.args.clone(),
        body: &*function_definition.body,
    })
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
        Expr::Number(number) => Ok(Value::Number(types::Number { value: *number })),
        Expr::String(s) => Ok(Value::String(types::String {
            contents: s.clone(),
        })),
        Expr::Ident(id) => {
            if environment.variables.contains_key(id.as_str()) {
                eval(environment.variables[id].expr, environment)
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

                    if !environment.variables.contains_key(identifier.as_str()) {
                        environment
                            .variables
                            .insert(identifier.clone(), variable_content);
                    } else {
                        *environment.variables.get_mut(identifier.as_str()).unwrap() =
                            variable_content;
                    }

                    eval(rest, environment)
                }
            },
            Statement::FunctionDefinition(function_definition) => {
                let variable_content = VariableContent {
                    expr: &*function_definition.body,
                    value: Value::Function(types::Function {
                        args: function_definition.args.clone(),
                        body: &*function_definition.body,
                    }),
                };
                if !environment
                    .variables
                    .contains_key(function_definition.name.as_str())
                {
                    environment
                        .variables
                        .insert(function_definition.name.clone(), variable_content);
                } else {
                    *environment
                        .variables
                        .get_mut(function_definition.name.as_str())
                        .unwrap() = variable_content;
                }

                eval(rest, environment)
            }
            Statement::Impl { tid, methods } => {
                if !environment.types.contains_key(tid.as_str()) {
                    return Err(make_eval_error(
                        expr,
                        format!("Undefined type '{}'", tid).as_str(),
                    ));
                }

                for method in methods {
                    if environment.types[tid.as_str()]
                        .methods
                        .contains_key(method.name.as_str())
                    {
                        return Err(make_eval_error(
                            expr,
                            format!(
                                "Redefinition of method '{}' for type '{}'",
                                method.name, tid
                            )
                            .as_str(),
                        ));
                    }

                    let method_definition = eval_function_definition(method, environment)?;
                    environment
                        .types
                        .get_mut(tid.as_str())
                        .unwrap()
                        .methods
                        .insert(method.name.clone(), method_definition);
                }

                eval(rest, environment)
            }
        },
        Expr::FunctionCall { name, args } => {
            if environment.variables.contains_key(name.as_str()) {
                let function_definition = &environment.variables[name.as_str()];
                let call_args = args;
                match function_definition.value.clone() {
                    Value::Function(function) => {
                        let def_args = function.args;
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
                                if !function_environment
                                    .variables
                                    .contains_key(arg_name.as_str())
                                {
                                    function_environment
                                        .variables
                                        .insert(arg_name.clone(), variable_content);
                                } else {
                                    *function_environment
                                        .variables
                                        .get_mut(arg_name.as_str())
                                        .unwrap() = variable_content;
                                }
                            }
                            eval(function.body, &mut function_environment)
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
        Expr::MethodCall { name, target, args } => {
            let target_value = eval(target, environment)?;
            let target_type = target_value.type_id();
            let call_args = args;

            // This code is quite messy, please clean it up.
            if environment.types.contains_key(target_type.as_str()) {
                if !environment.types[target_type.as_str()]
                    .methods
                    .contains_key(name.as_str())
                {
                    return Err(make_eval_error(
                        expr,
                        format!("Undefined method '{}' for type '{}'", name, target_type).as_str(),
                    ));
                }
                let function_definition =
                    environment.types[target_type.as_str()].methods[name.as_str()].clone();
                if !function_definition.is_method() {
                    return Err(make_eval_error(
                        expr,
                        format!("Expected {} to be a method", name).as_str(),
                    ));
                }

                let def_args = function_definition.args.clone();
                if call_args.len() + 1 != def_args.len() {
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

                    let variable_content = VariableContent {
                        expr: &**target,
                        value: target_value,
                    };

                    // TODO: there's got to be a simpler way to do this
                    if !function_environment.variables.contains_key("self") {
                        function_environment
                            .variables
                            .insert(String::from("self"), variable_content);
                    } else {
                        *function_environment.variables.get_mut("self").unwrap() = variable_content;
                    }

                    for i in 1..num_args {
                        let arg_name = def_args[i].clone();

                        let arg_expr = &call_args[i - 1];
                        let variable_content = VariableContent {
                            expr: arg_expr,
                            value: eval(arg_expr, environment)?,
                        };

                        // TODO: there's got to be a simpler way to do this
                        if !function_environment
                            .variables
                            .contains_key(arg_name.as_str())
                        {
                            function_environment
                                .variables
                                .insert(arg_name.clone(), variable_content);
                        } else {
                            *function_environment
                                .variables
                                .get_mut(arg_name.as_str())
                                .unwrap() = variable_content;
                        }
                    }
                    eval(function_definition.body, &mut function_environment)
                }
            } else {
                Err(make_eval_error(
                    expr,
                    format!("Undefined function {}", name).as_str(),
                ))
            }
        }
        Expr::Unit => Ok(Value::Unit(types::Unit {})),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn trivial_evaluation() {
        assert_eq!(
            eval(&Expr::Number(1f64), &mut Environment::new()),
            Ok(Value::Number(types::Number { value: 1f64 }))
        );
    }

    #[test]
    fn string_concatenation() {
        assert_eq!(
            eval(
                &Expr::BinOp {
                    op: Op::Plus,
                    lhs: Box::new(Expr::String(String::from("hello, "))),
                    rhs: Box::new(Expr::String(String::from("world"))),
                },
                &mut Environment::new()
            ),
            Ok(Value::String(types::String {
                contents: String::from("hello, world")
            }))
        );
    }

    // #[test]
    // fn string_len() {
    //     assert_eq!(
    //         eval(
    //             &Expr::MethodCall {
    //                 name: Identifier::from("len"),
    //                 target: Box::new(Expr::String(String::from("hello"))),
    //                 args: Vec::new()
    //             },
    //             &mut Environment::new()
    //         ),
    //         Ok(Value::Number(5f64))
    //     );
    // }
}
