use crate::parse::Expr;
use crate::parse::Op;

pub fn eval(expr: &Expr) -> f64 {
    match expr {
        Expr::BinOp { op, lhs, rhs } => match op {
            Op::Plus => eval(lhs) + eval(rhs),
        },
        Expr::Number(number) => *number,
    }
}
