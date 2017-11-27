use std::fmt::Debug;
use std::fmt::Write;

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
}

impl<'t> Stmt<'t> {
    pub(super) fn if_stmt(cond: Expr<'t>, then_clause: Stmt<'t>) -> Self {
        Stmt::If(cond, Box::new(then_clause), None)
    }

    pub(super) fn if_else_stmt(cond: Expr<'t>, then_clause: Stmt<'t>, else_clause: Stmt<'t>) -> Self {
        Stmt::If(cond, Box::new(then_clause), Some(Box::new(else_clause)))
    }
}

#[derive(PartialEq)]
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

impl<'t> Debug for Expr<'t> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> FmtResult<()> {
        let pp = PrettyPrinter::new();
        let s = pp.print(self)?;
        write!(f, "{}", s)
    }
}

#[derive(PartialEq)]
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

#[derive(PartialEq)]
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

#[derive(PartialEq)]
pub struct Unary<'t> {
    pub operator: UnaryOperator,
    pub unary: Box<Expr<'t>>,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

struct PrettyPrinter {
    buf: String,
}

type FmtResult<T> = ::std::result::Result<T, ::std::fmt::Error>;

impl PrettyPrinter {
    fn new() -> Self {
        PrettyPrinter {
            buf: String::new(),
        }
    }

    fn print<'t>(mut self, expr: &Expr<'t>) -> FmtResult<String> {
        self.print_inner(expr, 0)?;
        Ok(self.buf)
    }

    fn print_inner<'t>(&mut self, expr: &Expr<'t>, indent: usize) -> FmtResult<()> {
        match *expr {
            Expr::Binary(ref bin) => {
                let op = bin.operator;
                write!(&mut self.buf, "{:indent$}Binary({:?})\n", "", op, indent=indent)?;
                self.print_inner(&*bin.lhs, indent + 2)?;
                self.print_inner(&*bin.rhs, indent + 2)
            },
            Expr::Logical(ref logical) => {
                let op = logical.operator;
                write!(&mut self.buf, "{:indent$}Logical({:?})\n", "", op, indent=indent)?;
                self.print_inner(&*logical.lhs, indent + 2)?;
                self.print_inner(&*logical.rhs, indent + 2)
            },
            Expr::Grouping(ref group) => {
                self.print_inner(group, indent + 2)
            },
            Expr::Literal(ref lit) => {
                write!(&mut self.buf, "{:indent$}{:?}\n", "", lit, indent=indent)
            },
            Expr::Unary(ref unary) => {
                let op = unary.operator;
                write!(&mut self.buf, "{:indent$}Unary({:?})\n", "", op, indent=indent)?;
                self.print_inner(&*unary.unary, indent + 2)
            },
            Expr::Var(ref var) => {
                write!(&mut self.buf, "{:indent$}Var({:?})\n", "", var, indent=indent)
            },
            Expr::Assign(var, ref _expr) => {
                write!(&mut self.buf, "{:indent$}Assign({:?})\n", "", var, indent=indent)
            }
        }
    }
}
