pub mod types;

use super::parse::ast::AssignmentLHS;
use super::parse::ast::Expr;
use super::parse::ast::FunctionDefinition;
use super::parse::ast::Identifier;
use super::parse::ast::Op;
use super::parse::ast::Statement;
use super::parse::ast::TypeID;
use super::rust;
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
            Value::String(s) => write!(f, "{}", s.value),
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
pub type RustFunctions = HashMap<Identifier, types::RustFunction>;
pub type Types<'a> = HashMap<TypeID, TypeContent<'a>>;

#[derive(Clone)]
pub struct Environment<'a> {
    pub variables: Variables<'a>,
    pub types: Types<'a>,
    pub rust_functions: RustFunctions,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        let mut environment = Environment {
            variables: Variables::new(),
            types: Types::new(),
            rust_functions: RustFunctions::new(),
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
                    value: s1.value + s2.value.as_str(),
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
        args: function_definition.signature.args.clone(),
        body: &*function_definition.body,
    })
}

fn eval_op<'a>(
    op: &'a Op,
    lhs: &'a Expr,
    rhs: &'a Expr,
    environment: &mut Environment<'a>,
) -> EvalResult<'a> {
    let lhs_value = eval(&*lhs, environment)?;
    let rhs_value = eval(&*rhs, environment)?;

    match op {
        Op::Plus => Ok(lhs_value + rhs_value),
        Op::And => match lhs_value {
            Value::Bool(lhs_bool) => match rhs_value {
                Value::Bool(rhs_bool) => Ok(Value::Bool(types::Bool {
                    value: lhs_bool.value && rhs_bool.value,
                })),
                _ => Err(make_eval_error(
                    rhs,
                    format!(
                        "Expected expression of type Bool, got {}",
                        rhs_value.type_id()
                    )
                    .as_str(),
                )),
            },
            _ => Err(make_eval_error(
                lhs,
                format!(
                    "Expected expression of type Bool, got {}",
                    lhs_value.type_id()
                )
                .as_str(),
            )),
        },
        Op::Or => match lhs_value {
            Value::Bool(lhs_bool) => match rhs_value {
                Value::Bool(rhs_bool) => Ok(Value::Bool(types::Bool {
                    value: lhs_bool.value || rhs_bool.value,
                })),
                _ => Err(make_eval_error(
                    rhs,
                    format!(
                        "Expected expression of type Bool, got {}",
                        rhs_value.type_id()
                    )
                    .as_str(),
                )),
            },
            _ => Err(make_eval_error(
                lhs,
                format!(
                    "Expected expression of type Bool, got {}",
                    lhs_value.type_id()
                )
                .as_str(),
            )),
        },
    }
}

fn eval_function_call<'a>(
    expr: &'a Expr,
    name: &'a str,
    args: &'a Vec<Expr>,
    environment: &mut Environment<'a>,
) -> EvalResult<'a> {
    let function_definition = &environment.variables[name];
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

                    function_environment
                        .variables
                        .insert(arg_name.clone(), variable_content);
                }
                eval(function.body, &mut function_environment)
            }
        }
        _ => Err(make_eval_error(
            expr,
            format!("Expected {} to be a function", function_definition.value).as_str(),
        )),
    }
}

fn eval_rust_function_call<'a>(
    expr: &'a Expr,
    name: &'a str,
    args: &'a Vec<Expr>,
    environment: &mut Environment<'a>,
) -> EvalResult<'a> {
    let function_definition = environment.rust_functions[name].clone();
    let call_args = args;
    let def_args = function_definition.args.clone();
    if call_args.len() != def_args.len() {
        return Err(make_eval_error(
            expr,
            format!(
                "Expected {} arguments, got {}",
                def_args.len(),
                call_args.len()
            )
            .as_str(),
        ));
    }

    let num_args = def_args.len();
    let mut arg_values: Vec<Value<'a>> = Vec::new();
    for i in 0..num_args {
        let arg_name = def_args[i].clone();
        let arg_expr = &call_args[i];
        arg_values.push(eval(arg_expr, environment)?);
    }

    match arg_values.len() {
        1 => match function_definition.native_function {
            rust::NativeFunction::Dynamic(dynamic_function) => Ok(rust::external_eval_1(
                &dynamic_function,
                arg_values[0].clone(),
            )),
            rust::NativeFunction::Static1(static_function) => {
                Ok((static_function.function)(arg_values[0].clone()))
            }
            rust::NativeFunction::Static2(_) => panic!("Wrong number of args"),
        },
        2 => match function_definition.native_function {
            rust::NativeFunction::Dynamic(dynamic_function) => panic!("Not implemented"),
            rust::NativeFunction::Static1(_) => panic!("Wrong number of args"),
            rust::NativeFunction::Static2(static_function) => Ok((static_function.function)(
                arg_values[0].clone(),
                arg_values[1].clone(),
            )),
        },
        _ => Err(make_eval_error(
            expr,
            format!("Unsupported number rust fn args: {}", arg_values.len()).as_str(),
        )),
    }
}

pub fn eval<'a>(expr: &'a Expr, environment: &mut Environment<'a>) -> EvalResult<'a> {
    match expr {
        Expr::BinOp { op, lhs, rhs } => eval_op(op, lhs, rhs, environment),
        Expr::Unit => Ok(Value::Unit(types::Unit {})),
        Expr::Number(number) => Ok(Value::Number(types::Number { value: *number })),
        Expr::String(s) => Ok(Value::String(types::String { value: s.clone() })),
        Expr::Bool(b) => Ok(Value::Bool(types::Bool { value: *b })),
        Expr::Ident(id) => {
            if environment.variables.contains_key(id.as_str()) {
                Ok(environment.variables[id].value.clone())
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

                    environment
                        .variables
                        .insert(identifier.clone(), variable_content);

                    eval(rest, environment)
                }
            },
            Statement::FunctionDefinition(function_definition) => {
                let variable_content = VariableContent {
                    expr: &*function_definition.body,
                    value: Value::Function(types::Function {
                        args: function_definition.signature.args.clone(),
                        body: &*function_definition.body,
                    }),
                };
                environment
                    .variables
                    .insert(function_definition.signature.name.clone(), variable_content);

                eval(rest, environment)
            }
            Statement::RustFunctionDefinition(function_definition) => {
                match rust::compile_function(function_definition) {
                    Ok(native_function) => {
                        environment.rust_functions.insert(
                            function_definition.signature.name.clone(),
                            types::RustFunction {
                                args: function_definition.signature.args.clone(),
                                native_function: native_function,
                            },
                        );
                        eval(rest, environment)
                    }
                    Err(e) => Err(make_eval_error(expr, e.message.as_str())),
                }
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
                        .contains_key(method.signature.name.as_str())
                    {
                        return Err(make_eval_error(
                            expr,
                            format!(
                                "Redefinition of method '{}' for type '{}'",
                                method.signature.name, tid
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
                        .insert(method.signature.name.clone(), method_definition);
                }

                eval(rest, environment)
            }
            Statement::IfElse(ifelse) => panic!(),
        },
        Expr::FunctionCall { name, args } => {
            if environment.variables.contains_key(name.as_str()) {
                eval_function_call(expr, name.as_str(), args, environment)
            } else if environment.rust_functions.contains_key(name.as_str()) {
                eval_rust_function_call(expr, name.as_str(), args, environment)
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
            if !environment.types.contains_key(target_type.as_str()) {
                return Err(make_eval_error(
                    expr,
                    format!("Undefined type {}", target_type).as_str(),
                ));
            }

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
                return Err(make_eval_error(
                    expr,
                    format!(
                        "Expected {} arguments, got {}",
                        def_args.len(),
                        call_args.len()
                    )
                    .as_str(),
                ));
            }
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
        Expr::IfElse(ifelse) => {
            let condition_value = eval(&ifelse.condition, environment)?;
            match condition_value {
                Value::Bool(b) => {
                    if b.value {
                        eval(&*ifelse.if_branch, environment)
                    } else {
                        eval(&*ifelse.else_branch, environment)
                    }
                }
                _ => Err(make_eval_error(expr, "Expected expression returning Bool")),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::parse::ast::IfElse;

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
                value: String::from("hello, world")
            }))
        );
    }

    #[test]
    fn simple_if_else() {
        assert_eq!(
            eval(
                &Expr::IfElse(IfElse {
                    condition: Box::new(Expr::Bool(true)),
                    if_branch: Box::new(Expr::Number(1f64)),
                    else_branch: Box::new(Expr::Number(2f64)),
                }),
                &mut Environment::new()
            ),
            Ok(Value::Number(types::Number { value: 1f64 }))
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
