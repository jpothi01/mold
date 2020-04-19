use std::fmt;

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

pub enum Statement {
    Assignment { lhs: Box<Expr>, rhs: Box<Expr> },
}

pub enum Expr {
    BinOp {
        op: Op,
        rhs: Box<Expr>,
        lhs: Box<Expr>,
    },
    Number(f64),
    Ident(Identifier),
    Statement(Statement, Box<Expr>),
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::BinOp { op, rhs, lhs } => write!(f, "({:?} {:?} {:?})", op, *rhs, *lhs),
            Expr::Number(n) => write!(f, "({:?})", n),
            Expr::Ident(i) => write!(f, "(id {:?})", i),
            Expr::Statement(statement, rest) => match statement {
                Statement::Assignment { lhs, rhs } => {
                    write!(f, "(assign {:?} {:?} {:?})", *lhs, *rhs, *rest)
                }
            },
        }
    }
}
