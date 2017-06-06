use parser::Parser;
use parser::Expr;
use parser::Binary;
use parser::Unary;
use parser::Literal;

use scanner::Token;
use scanner::TokenType;
use scanner::Scanner;

use errors::*;

pub struct Context;

impl Context {
    pub fn new() -> Self { Context }
}

pub trait Eval {
    fn eval(&self, context: &mut Context) -> Result<Literal>;
}

impl<T> Eval for T
    where
        T: AsRef<str>
{
    fn eval(&self, context: &mut Context) -> Result<Literal> {
        let s = self.as_ref();
        let scanner = Scanner::new(s);
        let tokens = scanner.filter(|t| {
            if let &Ok(Token{ty: TokenType::Comment, ..}) = t {
                false
            } else {
                true
            }
        }).collect::<Result<Vec<Token>>>()?;

        let mut parser = Parser::new(&tokens);
        let expr = parser.expression()?;
        expr.eval(context)
    }
}

impl<'t> Eval for Expr<'t> {
    fn eval(&self, context: &mut Context) -> Result<Literal> {
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
            (Literal::Number(nlhs), Literal::Number(nrhs)) => {
                return Ok(Literal::Number(nlhs $op nrhs));
            },
            (Literal::Number(_), _) => {
                return Err("Invalid operand on lhs, expected number".into())
            },
            (_, Literal::Number(_)) => {
                return Err("Invalid operand on rhs, expected number".into())
            },
            _ => {
                return Err("Invalid operands, expected number or string".into())
            },
        }
    );
);

impl<'t> Eval for Binary<'t> {
    fn eval(&self, context: &mut Context) -> Result<Literal> {
        let lhs = self.lhs.eval(context)?;
        let rhs = self.rhs.eval(context)?;
        let op = &self.operator.ty;

        match *op {
            TokenType::Plus => match (lhs, rhs) {
                (Literal::String(lhs), Literal::String(rhs)) => {
                    let mut res = lhs.clone();
                    res.push_str(&rhs);
                    return Ok(Literal::String(res));
                },
                (lhs, rhs) => numeric_binary_op!(+, lhs, rhs)
            },
            TokenType::Minus => numeric_binary_op!(-, lhs, rhs),
            TokenType::Star => numeric_binary_op!(*, lhs, rhs),
            TokenType::Slash => numeric_binary_op!(/, lhs, rhs),
            _ => unreachable!(),
        }
    }
}

impl<'t> Eval for Unary<'t> {
    fn eval(&self, context: &mut Context) -> Result<Literal> {
        let operand = self.unary.eval(context)?;
        match self.operator.ty {
            TokenType::Minus => {
                match operand {
                    Literal::Number(n) => {
                        Ok(Literal::Number(-1.0 * n))
                    },
                    _ => {
                        Err("Invalid operand for operator '-', expected number".into())
                    }
                }
            },
            TokenType::Bang => {
                if operand.into_bool() {
                    Ok(Literal::False)
                } else {
                    Ok(Literal::True)
                }
            },
            _ => unreachable!(),
        }
    }
}

impl Eval for Literal {
    fn eval(&self, _: &mut Context) -> Result<Literal> {
        Ok(self.clone())
    }
}
