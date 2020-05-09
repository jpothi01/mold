use std::fmt;

#[derive(PartialEq, Clone, Copy)]
pub enum Op {
    Plus,
    Minus,
    Times,
    DividedBy,
    And,
    Or,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl Op {
    pub fn from_str(s: &str) -> Option<Op> {
        match s {
            "+" => Some(Op::Plus),
            "-" => Some(Op::Minus),
            "*" => Some(Op::Times),
            "/" => Some(Op::DividedBy),
            "&&" => Some(Op::And),
            "||" => Some(Op::Or),
            "==" => Some(Op::Equals),
            "!=" => Some(Op::NotEquals),
            "<" => Some(Op::LessThan),
            "<=" => Some(Op::LessThanOrEqual),
            ">" => Some(Op::GreaterThan),
            ">=" => Some(Op::GreaterThanOrEqual),
            _ => None,
        }
    }

    pub fn precedence(&self) -> i32 {
        match self {
            Op::Plus => 3,
            Op::Minus => 3,
            Op::Times => 4,
            Op::DividedBy => 4,
            Op::And => 1,
            Op::Or => 0,
            Op::Equals => 2,
            Op::NotEquals => 2,
            Op::GreaterThan => 2,
            Op::GreaterThanOrEqual => 2,
            Op::LessThan => 2,
            Op::LessThanOrEqual => 2,
        }
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Op::Plus => "+",
            Op::Minus => "-",
            Op::Times => "*",
            Op::DividedBy => "/",
            Op::And => "&&",
            Op::Or => "||",
            Op::Equals => "==",
            Op::NotEquals => "!=",
            Op::LessThan => "<",
            Op::LessThanOrEqual => "<=",
            Op::GreaterThan => ">",
            Op::GreaterThanOrEqual => ">=",
        };
        write!(f, "{}", s)
    }
}

pub type Identifier = String;
pub type TypeID = String;

#[derive(PartialEq, Clone)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub args: Vec<Identifier>,
}

#[derive(PartialEq, Clone)]
pub struct FunctionDefinition {
    pub signature: FunctionSignature,
    pub body: Box<Expr>,
}

impl fmt::Debug for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(fndef {:?} ({:?}) {:?})",
            self.signature.name, self.signature.args, self.body
        )
    }
}

#[derive(PartialEq, Clone)]
pub struct RustFunctionDefinition {
    pub signature: FunctionSignature,
    pub body: String,
}

impl fmt::Debug for RustFunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(rustfndef {:?} ({:?}) {:?})",
            self.signature.name, self.signature.args, self.body
        )
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
pub struct FunctionCall {
    pub name: Identifier,
    pub args: Vec<Expr>,
}

impl fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(call {:?}", self.name)?;
        for arg in &self.args {
            write!(f, " {:?}", arg)?;
        }
        write!(f, ")")
    }
}

#[derive(PartialEq, Clone)]
pub struct EnumItem {
    pub tag: Identifier,
    // For now, only tuple associated values
    pub associated_values: Vec<Identifier>,
}

impl fmt::Debug for EnumItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(enumitem {:?}", self.tag)?;
        for alternative in &self.associated_values {
            write!(f, " {:?}", alternative)?;
        }
        write!(f, ")")
    }
}

#[derive(PartialEq, Clone)]
pub struct EnumDefinition {
    pub name: TypeID,
    pub alternatives: Vec<EnumItem>,
}

impl fmt::Debug for EnumDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(enumdef {:?}", self.name)?;
        for alternative in &self.alternatives {
            write!(f, " {:?}", alternative)?;
        }
        write!(f, ")")
    }
}

#[derive(PartialEq, Clone)]
pub struct EnumAlternative {
    pub enum_name: TypeID,
    pub alternative_name: Identifier,
    pub associated_values: Vec<Expr>,
}

impl fmt::Debug for EnumAlternative {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(enumalt {:?} {:?}",
            self.enum_name, self.alternative_name
        )?;
        for associated_value in &self.associated_values {
            write!(f, " {:?}", associated_value)?;
        }
        write!(f, ")")
    }
}

#[derive(PartialEq, Clone)]
pub struct EnumDestructure {
    pub enum_name: TypeID,
    pub alternative_name: Identifier,
    pub associated_values: Vec<Identifier>,
}

impl fmt::Debug for EnumDestructure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "(enumdestructure {:?} {:?}",
            self.enum_name, self.alternative_name
        )?;
        for associated_value in &self.associated_values {
            write!(f, " {:?}", associated_value)?;
        }
        write!(f, ")")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum MatchPattern {
    EnumDestructure(EnumDestructure),
}

#[derive(PartialEq, Clone)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub expr: Expr,
}

impl fmt::Debug for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(matcharm {:?} {:?})", self.pattern, self.expr)
    }
}

#[derive(PartialEq, Clone)]
pub struct Match {
    pub match_expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

impl fmt::Debug for Match {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(match {:?}", self.match_expr)?;
        for arm in &self.arms {
            write!(f, " {:?}", arm)?;
        }
        write!(f, ")")
    }
}

#[derive(PartialEq, Clone)]
pub enum Statement {
    Assignment {
        lhs: AssignmentLHS,
        rhs: Box<Expr>,
    },
    EnumDefinition(EnumDefinition),
    FunctionCall(FunctionCall),
    FunctionDefinition(FunctionDefinition),
    RustFunctionDefinition(RustFunctionDefinition),
    Impl {
        tid: TypeID,
        methods: Vec<FunctionDefinition>,
    },
    IfElse(IfElse),
    While {
        condition: Box<Expr>,
        body: Box<Expr>,
    },
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
    EnumAlternative(EnumAlternative),
    Statement(Statement, Box<Expr>),
    FunctionCall(FunctionCall),
    MethodCall {
        name: Identifier,
        target: Box<Expr>,
        args: Vec<Expr>,
    },
    IfElse(IfElse),
    Block(Box<Expr>),
    Match(Match),
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
            Expr::String(s) => write!(f, "({:?})", s),
            Expr::Bool(b) => write!(f, "({})", if *b { "true" } else { "false" }),
            Expr::Ident(i) => write!(f, "(id {:?})", i),
            Expr::EnumAlternative(e) => write!(f, "{:?}", e),
            Expr::Statement(statement, rest) => {
                match statement {
                    Statement::Assignment { lhs, rhs } => write!(f, "(assign {:?} {:?})", lhs, rhs),
                    Statement::EnumDefinition(enum_definition) => {
                        write!(f, "{:?}", enum_definition)
                    }
                    Statement::FunctionCall(function_call) => write!(f, "{:?}", function_call),
                    Statement::FunctionDefinition(function_definition) => {
                        write!(f, "{:?}", function_definition)
                    }
                    Statement::RustFunctionDefinition(function_definition) => {
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
                    Statement::While { condition, body } => {
                        write!(f, "(while {:?} {:?})", condition, body)
                    }
                }?;

                write!(f, " {:?}", rest)
            }
            Expr::FunctionCall(function_call) => write!(f, "{:?}", function_call),
            Expr::MethodCall { name, target, args } => {
                write!(f, "(methodcall {:?}", name)?;
                write!(f, " {:?}", target)?;
                for arg in args {
                    write!(f, " {:?}", arg)?;
                }
                write!(f, ")")
            }
            Expr::IfElse(ifelse) => write!(f, "{:?}", ifelse),
            Expr::Block(expr) => write!(f, "(block {:?})", expr),
            Expr::Match(m) => write!(f, "{:?}", m),
        }
    }
}
