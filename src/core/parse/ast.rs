use std::fmt;

#[derive(PartialEq, Clone)]
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

    pub fn precedence(&self) -> i32 {
        match self {
            Op::Plus => 0,
        }
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Op::Plus => "+",
        };
        write!(f, "{}", s)
    }
}

pub type Identifier = String;
pub type TypeID = String;

#[derive(PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub args: Vec<Identifier>,
    pub body: Box<Expr>,
}

impl fmt::Debug for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(fn {:?} ({:?}) {:?})", self.name, self.args, self.body)
    }
}

#[derive(PartialEq, Clone)]
pub struct IfElse {
    pub condition: Box<Expr>,
    pub if_branch: Box<Expr>,
    pub else_branch: Box<Expr>,
}

impl fmt::Debug for IfElse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(ifelse {:?} {:?} {:?})",
            self.condition, self.if_branch, self.else_branch
        )
    }
}

#[derive(PartialEq, Clone)]
pub enum Statement {
    Assignment {
        lhs: AssignmentLHS,
        rhs: Box<Expr>,
    },
    FunctionDefinition(FunctionDefinition),
    Impl {
        tid: TypeID,
        methods: Vec<FunctionDefinition>,
    },
    IfElse(IfElse),
}

#[derive(PartialEq, Clone)]
pub enum Expr {
    BinOp {
        op: Op,
        rhs: Box<Expr>,
        lhs: Box<Expr>,
    },
    Unit,
    Number(f64),
    String(String),
    Bool(bool),
    Ident(Identifier),
    Statement(Statement, Box<Expr>),
    FunctionCall {
        name: Identifier,
        args: Vec<Expr>,
    },
    MethodCall {
        name: Identifier,
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    IfElse(IfElse),
}

#[derive(PartialEq, Clone)]
pub enum AssignmentLHS {
    Single(Identifier),
}

impl fmt::Debug for AssignmentLHS {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignmentLHS::Single(identifier) => write!(f, "{:?}", identifier),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::BinOp { op, lhs, rhs } => write!(f, "({:?} {:?} {:?})", op, lhs, rhs),
            Expr::Unit => write!(f, "()"),
            Expr::Number(n) => write!(f, "({:?})", n),
            Expr::String(s) => write!(f, "({:?}", s),
            Expr::Bool(b) => write!(f, "({})", if *b { "true" } else { "false" }),
            Expr::Ident(i) => write!(f, "(id {:?})", i),
            Expr::Statement(statement, rest) => {
                match statement {
                    Statement::Assignment { lhs, rhs } => {
                        write!(f, "(assign {:?} {:?} {:?})", lhs, rhs, rest)
                    }
                    Statement::FunctionDefinition(function_definition) => {
                        write!(f, "{:?}", function_definition)
                    }
                    Statement::Impl { tid, methods } => {
                        write!(f, "(impl {:?}", tid)?;
                        for method in methods {
                            write!(f, " {:?}", method)?;
                        }
                        write!(f, ")")
                    }
                    Statement::IfElse(ifelse) => write!(f, "{:?}", ifelse),
                }?;

                write!(f, " {:?}", rest)
            }
            Expr::FunctionCall { name, args } => {
                write!(f, "(call {:?}", name)?;
                for arg in args {
                    write!(f, " {:?}", arg)?;
                }
                write!(f, ")")
            }
            Expr::MethodCall { name, target, args } => {
                write!(f, "(methodcall {:?}", name)?;
                write!(f, " {:?}", target)?;
                for arg in args {
                    write!(f, " {:?}", arg)?;
                }
                write!(f, ")")
            }
            Expr::IfElse(ifelse) => write!(f, "{:?}", ifelse),
        }
    }
}
