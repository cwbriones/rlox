use environment::Environment;
use errors::*;
use parser::ast::{Expr,Stmt,Binary,Unary};
use parser::{Parser,TokenType};
use value::Value;

pub trait Context {
    fn println(&mut self, &Value);
    fn env(&self) -> &Environment;
    fn env_mut(&mut self) -> &mut Environment;
}

pub struct StandardContext {
    environment: Environment,
}

impl StandardContext {
    pub fn new() -> Self {
        StandardContext {
            environment: Environment::new()
        }
    }
}

impl Context for StandardContext {
    fn env(&self) -> &Environment {
        &self.environment
    }

    fn env_mut(&mut self) -> &mut Environment {
        &mut self.environment
    }

    fn println(&mut self, v: &Value) {
        println!("{}", v);
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
                context.println(&evald);
            },
            Stmt::Var(ref tok, ref expr) => {
                let val = expr.eval(context)?;
                debug!("Set var '{}' to value {}", tok.value, val);
                context.env_mut().bind(tok.value, val);
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
                let env = context.env();
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

impl<T> Eval for T
    where
        T: AsRef<str>
{
    fn eval(&self, context: &mut Context) -> Result<Value> {
        let s = self.as_ref();
        let mut parser = Parser::new(s);
        let statements = parser.parse()?;
        let mut last_value = Value::Void;
        for stmt in statements {
            last_value = stmt.eval(context)?;
        }
        Ok(last_value)
    }
}
