use std::fmt;

#[derive(PartialEq)]
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

#[derive(PartialEq)]
pub enum Statement {
    Assignment {
        lhs: Box<AssignmentLHS>,
        rhs: Box<Expr>,
    },
    FunctionDefinition {
        name: Identifier,
        args: Vec<Identifier>,
        body: Box<Expr>,
    },
}

#[derive(PartialEq)]
pub enum Expr {
    BinOp {
        op: Op,
        rhs: Box<Expr>,
        lhs: Box<Expr>,
    },
    Number(f64),
    Ident(Identifier),
    Statement(Statement, Box<Expr>),
    FunctionCall {
        name: Identifier,
        args: Vec<Expr>,
    },
    Unit,
}

#[derive(PartialEq)]
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
            Expr::Number(n) => write!(f, "({:?})", n),
            Expr::Ident(i) => write!(f, "(id {:?})", i),
            Expr::Statement(statement, rest) => {
                match statement {
                    Statement::Assignment { lhs, rhs } => {
                        write!(f, "(assign {:?} {:?} {:?})", lhs, rhs, rest)
                    }
                    Statement::FunctionDefinition { name, args, body } => {
                        write!(f, "(fn {:?} ({:?}) {:?}", name, args, body)
                    }
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
            Expr::Unit => write!(f, "()"),
        }
    }
}
