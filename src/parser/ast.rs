use value::Value;

#[cfg(test)]
pub mod dsl {
    use super::*;

    pub fn binary<'t>(operator: BinaryOperator, lhs: Expr<'t>, rhs: Expr<'t>) -> Expr<'t> {
        Expr::binary(operator, lhs, rhs)
    }

    // TODO: Write test for parsing logical operator
    // pub fn logical<'t>(operator: LogicalOperator, lhs: Expr<'t>, rhs: Expr<'t>) -> Expr<'t> {
    //     Expr::logical(operator, lhs, rhs)
    // }

    pub fn unary<'t>(operator: UnaryOperator, unary: Expr<'t>) -> Expr<'t> {
        Expr::unary(operator, unary)
    }

    pub fn number<'t>(n: f64) -> Expr<'t> {
        Expr::Literal(Value::Number(n))
    }

    pub fn nil<'t>() -> Expr<'t> {
        Expr::Literal(Value::Nil)
    }

    pub fn truelit<'t>() -> Expr<'t> {
        Expr::Literal(Value::True)
    }

    // pub fn falselit<'t>() -> Expr<'t> {
    //     Expr::Literal(Value::False)
    // }

    pub fn string<'t, S: Into<String>>(s: S) -> Expr<'t> {
        Expr::Literal(Value::String(s.into()))
    }

    pub fn grouping<'t>(expr: Expr<'t>) -> Expr<'t> {
        Expr::Grouping(Box::new(expr))
    }

    pub fn var<'t>(name: &'t str) -> Expr<'t> {
        Expr::Var(name)
    }
}

#[derive(PartialEq, Debug)]
pub enum Stmt<'t> {
    Expr(Expr<'t>),
    Print(Expr<'t>),
    Var(&'t str, Expr<'t>),
    Block(Vec<Stmt<'t>>),
    If(Expr<'t>, Box<Stmt<'t>>, Option<Box<Stmt<'t>>>),
    While(Expr<'t>, Box<Stmt<'t>>),
    Break,
}

impl<'t> Stmt<'t> {
    pub(super) fn if_stmt(cond: Expr<'t>, then_clause: Stmt<'t>) -> Self {
        Stmt::If(cond, Box::new(then_clause), None)
    }

    pub(super) fn if_else_stmt(cond: Expr<'t>, then_clause: Stmt<'t>, else_clause: Stmt<'t>) -> Self {
        Stmt::If(cond, Box::new(then_clause), Some(Box::new(else_clause)))
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr<'t> {
    Logical(Logical<'t>),
    Binary(Binary<'t>),
    Grouping(Box<Expr<'t>>),
    Literal(Value),
    Unary(Unary<'t>),
    Var(&'t str),
    Assign(&'t str, Box<Expr<'t>>),
}

impl<'t> Expr<'t> {
    pub(super) fn binary(operator: BinaryOperator, lhs: Expr<'t>, rhs: Expr<'t>) -> Self {
        Expr::Binary(Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn logical(operator: LogicalOperator, lhs: Expr<'t>, rhs: Expr<'t>) -> Self {
        Expr::Logical(Logical {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn unary(operator: UnaryOperator, unary: Expr<'t>) -> Self {
        Expr::Unary(Unary {
            operator: operator,
            unary: Box::new(unary),
        })
    }
}

#[derive(PartialEq, Debug)]
pub struct Binary<'t> {
    pub lhs: Box<Expr<'t>>,
    pub rhs: Box<Expr<'t>>,
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
pub struct Logical<'t> {
    pub lhs: Box<Expr<'t>>,
    pub rhs: Box<Expr<'t>>,
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
pub struct Unary<'t> {
    pub operator: UnaryOperator,
    pub unary: Box<Expr<'t>>,
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
