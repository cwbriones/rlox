use std::fmt::Debug;
use std::fmt::Write;

use value::Value;
use super::Token;

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
                let op = bin.operator.value;
                write!(&mut self.buf, "{:indent$}Binary({:?})\n", "", op, indent=indent)?;
                self.print_inner(&*bin.lhs, indent + 2)?;
                self.print_inner(&*bin.rhs, indent + 2)
            },
            Expr::Grouping(ref group) => {
                self.print_inner(group, indent + 2)
            },
            Expr::Literal(ref lit) => {
                write!(&mut self.buf, "{:indent$}{:?}\n", "", lit, indent=indent)
            },
            Expr::Unary(ref unary) => {
                let op = unary.operator.value;
                write!(&mut self.buf, "{:indent$}Unary({:?})\n", "", op, indent=indent)?;
                self.print_inner(&*unary.unary, indent + 2)
            },
            Expr::Var(ref var) => {
                write!(&mut self.buf, "{:indent$}Var({:?})\n", "", var, indent=indent)
            }
        }
    }
}

impl<'t> Debug for Expr<'t> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> FmtResult<()> {
        let pp = PrettyPrinter::new();
        let s = pp.print(self)?;
        write!(f, "{}", s)
    }
}

pub enum Stmt<'t> {
    Expression(Expr<'t>),
    Print(Expr<'t>),
    Var(Token<'t>, Expr<'t>)
}

pub enum Expr<'t> {
    Binary(Binary<'t>),
    Grouping(Box<Expr<'t>>),
    Literal(Value),
    Unary(Unary<'t>),
    Var(Token<'t>),
}

pub struct Binary<'t> {
    pub lhs: Box<Expr<'t>>,
    pub rhs: Box<Expr<'t>>,
    pub operator: Token<'t>,
}

impl<'t> Binary<'t> {
    pub(super) fn new(lhs: Expr<'t>, rhs: Expr<'t>, operator: Token<'t>) -> Self {
        Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        }
    }
}

pub struct Unary<'t> {
    pub operator: Token<'t>,
    pub unary: Box<Expr<'t>>,
}

