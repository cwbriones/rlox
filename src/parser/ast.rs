use value::Value;

#[cfg(test)]
pub mod dsl {
    use super::*;

    pub fn binary(operator: BinaryOperator, lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(operator, lhs, rhs)
    }

    // TODO: Write test for parsing logical operator
    // pub fn logical(operator: LogicalOperator, lhs: Expr, rhs: Expr) -> Expr {
    //     Expr::logical(operator, lhs, rhs)
    // }

    pub fn unary(operator: UnaryOperator, unary: Expr) -> Expr {
        Expr::unary(operator, unary)
    }

    pub fn number(n: f64) -> Expr {
        Expr::Literal(Value::Number(n))
    }

    pub fn nil() -> Expr {
        Expr::Literal(Value::Nil)
    }

    pub fn truelit() -> Expr {
        Expr::Literal(Value::True)
    }

    // pub fn falselit() -> Expr {
    //     Expr::Literal(Value::False)
    // }

    pub fn string<S: Into<String>>(s: S) -> Expr {
        Expr::Literal(Value::String(s.into()))
    }

    pub fn grouping(expr: Expr) -> Expr {
        Expr::Grouping(Box::new(expr))
    }

    pub fn var(name: &str) -> Expr {
        Expr::Var(name.into())
    }
}

#[derive(PartialEq, Debug)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Break,
}

impl Stmt {
    pub(super) fn if_stmt(cond: Expr, then_clause: Stmt) -> Self {
        Stmt::If(cond, Box::new(then_clause), None)
    }

    pub(super) fn if_else_stmt(cond: Expr, then_clause: Stmt, else_clause: Stmt) -> Self {
        Stmt::If(cond, Box::new(then_clause), Some(Box::new(else_clause)))
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Logical(Logical),
    Binary(Binary),
    Grouping(Box<Expr>),
    Literal(Value),
    Unary(Unary),
    Var(String),
    Assign(String, Box<Expr>),
}

impl Expr {
    pub(super) fn binary(operator: BinaryOperator, lhs: Expr, rhs: Expr) -> Self {
        Expr::Binary(Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn logical(operator: LogicalOperator, lhs: Expr, rhs: Expr) -> Self {
        Expr::Logical(Logical {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn unary(operator: UnaryOperator, unary: Expr) -> Self {
        Expr::Unary(Unary {
            operator: operator,
            unary: Box::new(unary),
        })
    }
}

#[derive(PartialEq, Debug)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: BinaryOperator,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum BinaryOperator {
    Equal,
    BangEq,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
    Minus,
    Plus,
    Slash,
    Star,
}

impl BinaryOperator {
    pub fn to_str(&self) -> &str {
        match *self {
            BinaryOperator::Equal => "=",
            BinaryOperator::BangEq => "!=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEq => ">=",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEq => "<=",
            BinaryOperator::Minus => "-",
            BinaryOperator::Plus => "+",
            BinaryOperator::Slash => "/",
            BinaryOperator::Star => "*",
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Logical {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: LogicalOperator
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}

impl LogicalOperator {
    pub fn to_str(&self) -> &str {
        match *self {
            LogicalOperator::And => "and",
            LogicalOperator::Or  => "or",
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Unary {
    pub operator: UnaryOperator,
    pub unary: Box<Expr>,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl UnaryOperator {
    pub fn to_str(&self) -> &str {
        match *self {
            UnaryOperator::Minus => "-",
            UnaryOperator::Bang  => "!",
        }
    }
}
