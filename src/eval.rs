use parser::Expr;
use parser::Binary;
use parser::Unary;
use parser::Literal;

use scanner::TokenType;

use errors::*;

pub struct Context;

impl Context {
    pub fn new() -> Self { Context }
}

pub trait Eval {
    fn eval(&self, context: &mut Context) -> Result<Expr>;
}

fn is_true(expr: &Expr) -> bool {
    match *expr {
        Expr::Literal(Literal::Nil) => false,
        Expr::Literal(Literal::False) => false,
        _ => true,
    }
}

impl<'t> Eval for Expr<'t> {
    fn eval(&self, context: &mut Context) -> Result<Expr> {
        match *self {
            Expr::Grouping(ref inner) => inner.eval(context),
            Expr::Binary(ref inner) => inner.eval(context),
            Expr::Unary(ref inner) => inner.eval(context),
            Expr::Literal(ref inner) => inner.eval(context),
        }
    }
}

macro_rules! numeric_binary_op (
    ($op:tt, $lhs:ident, $rhs:ident) => (
        match ($lhs, $rhs) {
            (Expr::Literal(Literal::Number(nlhs)), Expr::Literal(Literal::Number(nrhs))) => {
                return Ok(Expr::Literal(Literal::Number(nlhs $op nrhs)));
            },
            (Expr::Literal(Literal::Number(_)), _) => {
                return Err("Invalid operand on lhs, expected number".into())
            },
            (_, Expr::Literal(Literal::Number(_))) => {
                return Err("Invalid operand on rhs, expected number".into())
            },
            _ => {
                return Err("Invalid operands, expected numbers".into())
            },
        }
    );
);

impl<'t> Eval for Binary<'t> {
    fn eval(&self, context: &mut Context) -> Result<Expr> {
        let lhs = self.lhs.eval(context)?;
        let rhs = self.rhs.eval(context)?;
        let op = &self.operator.ty;

        match *op {
            TokenType::Plus => numeric_binary_op!(+, lhs, rhs),
            TokenType::Minus => numeric_binary_op!(-, lhs, rhs),
            TokenType::Star => numeric_binary_op!(*, lhs, rhs),
            TokenType::Slash => numeric_binary_op!(/, lhs, rhs),
            _ => unreachable!(),
        }
    }
}

impl<'t> Eval for Unary<'t> {
    fn eval(&self, context: &mut Context) -> Result<Expr> {
        let operand = self.unary.eval(context)?;
        match self.operator.ty {
            TokenType::Minus => {
                match operand {
                    Expr::Literal(Literal::Number(n)) => {
                        Ok(Expr::Literal(Literal::Number(-1.0 * n)))
                    },
                    _ => {
                        Err("Invalid operand for operator '-', expected number".into())
                    }
                }
            },
            TokenType::Bang => {
                if is_true(&operand) {
                    Ok(Expr::Literal(Literal::False))
                } else {
                    Ok(Expr::Literal(Literal::True))
                }
            },
            _ => unreachable!(),
        }
    }
}

impl Eval for Literal {
    fn eval(&self, _: &mut Context) -> Result<Expr> {
        Ok(Expr::Literal(self.clone()))
    }
}
