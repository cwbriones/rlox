use parser::Stmt;
use parser::Expr;
use parser::Binary;
use parser::Unary;
use parser::TokenType;

use value::Value;
use environment::Environment;

use errors::*;

pub struct Context {
    pub environment: Environment
}

impl Context {
    pub fn new() -> Self {
        Context {
            environment: Environment::new()
        }
    }
}

pub trait Eval {
    fn eval(&self, context: &mut Context) -> Result<Value>;
}

impl<'t> Eval for Stmt<'t> {
    fn eval(&self, context: &mut Context) -> Result<Value> {
        match *self {
            Stmt::Expression(ref inner) => { inner.eval(context)?; }
            Stmt::Print(ref inner) => {
                let evald = inner.eval(context)?;
                println!("{}", evald);
            },
            Stmt::Var(ref tok, ref expr) => {
                let val = expr.eval(context)?;
                debug!("Set var '{}' to value {}", tok.value, val);
                context.environment.bind(tok.value, val);
            }
        }
        Ok(Value::Void)
    }
}

impl<'t> Eval for Expr<'t> {
    fn eval(&self, context: &mut Context) -> Result<Value> {
        match *self {
            Expr::Grouping(ref inner) => inner.eval(context),
            Expr::Binary(ref inner) => inner.eval(context),
            Expr::Unary(ref inner) => inner.eval(context),
            Expr::Literal(ref inner) => inner.eval(context),
            Expr::Var(ref token) => {
                let var = token.value;
                let env = &context.environment;
                match env.lookup(var) {
                    None => return Err("Could not find var".into()),
                    Some(v) => {
                        return Ok((*v).clone())
                    }
                }
            }
        }
    }
}

macro_rules! numeric_binary_op (
    ($op:tt, $lhs:ident, $rhs:ident) => (
        match ($lhs, $rhs) {
            (Value::Number(nlhs), Value::Number(nrhs)) => {
                return Ok(Value::Number(nlhs $op nrhs));
            },
            (Value::Number(_), _) => {
                return Err("Invalid operand on lhs, expected number".into())
            },
            (_, Value::Number(_)) => {
                return Err("Invalid operand on rhs, expected number".into())
            },
            _ => {
                return Err("Invalid operands, expected number or string".into())
            },
        }
    );
);

impl<'t> Eval for Binary<'t> {
    fn eval(&self, context: &mut Context) -> Result<Value> {
        let lhs = self.lhs.eval(context)?;
        let rhs = self.rhs.eval(context)?;
        let op = &self.operator.ty;

        match *op {
            TokenType::Plus => match (lhs, rhs) {
                (Value::String(lhs), Value::String(rhs)) => {
                    let mut res = lhs.clone();
                    res.push_str(&rhs);
                    return Ok(Value::String(res));
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
    fn eval(&self, context: &mut Context) -> Result<Value> {
        let operand = self.unary.eval(context)?;
        match self.operator.ty {
            TokenType::Minus => {
                match operand {
                    Value::Number(n) => {
                        Ok(Value::Number(-1.0 * n))
                    },
                    _ => {
                        Err("Invalid operand for operator '-', expected number".into())
                    }
                }
            },
            TokenType::Bang => {
                if operand.into() {
                    Ok(Value::False)
                } else {
                    Ok(Value::True)
                }
            },
            _ => unreachable!(),
        }
    }
}

impl Eval for Value {
    fn eval(&self, _: &mut Context) -> Result<Value> {
        Ok(self.clone())
    }
}
