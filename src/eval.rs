use environment::Environment;
use parser::ast::{Expr,Stmt,Binary,Unary,UnaryOperator,BinaryOperator,Logical,LogicalOperator};
use value::Value;

#[derive(Debug, Fail)]
pub enum RuntimeError {
    #[fail(display = "division by zero")]
    DivideByZero,
    #[fail(display = "variable '{}' could not be resolved", _0)]
    UndefinedVariable(String),
    #[fail(display = "invalid operands, expected {}", _0)]
    InvalidOperands(String),
    #[fail(display = "break")]
    Break,
}

pub type Result<T> = ::std::result::Result<T, RuntimeError>;

pub trait Context {
    fn println(&mut self, &Value);
    fn env(&self) -> &Environment;
    fn env_mut(&mut self) -> &mut Environment;
    fn push_env(&mut self);
    fn pop_env(&mut self);
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

    fn push_env(&mut self) {
        self.environment = self.environment.extend();
    }

    fn pop_env(&mut self) {
        if let Some(p) = self.environment.parent() {
            self.environment = p;
        }
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
            Stmt::Expr(ref inner) => { inner.eval(context)?; }
            Stmt::Print(ref inner) => {
                let evald = inner.eval(context)?;
                context.println(&evald);
            },
            Stmt::Var(ref var, ref expr) => {
                let val = expr.eval(context)?;
                debug!("Set var '{}' to value {}", var, val);
                context.env_mut().bind(var, val);
            },
            Stmt::Block(ref stmts) => {
                context.push_env();
                for stmt in stmts.iter() {
                    if let Err(err) = stmt.eval(context) {
                        context.pop_env();
                        return Err(err);
                    }
                }
                context.pop_env();
            },
            Stmt::If(ref cond, ref then_clause, ref else_clause) => {
                if cond.eval(context)?.truthy() {
                    then_clause.eval(context)?;
                } else if let &Some(ref else_clause) = else_clause {
                    else_clause.eval(context)?;
                }
            },
			Stmt::While(ref cond, ref body) => {
				while cond.eval(context)?.truthy() {
					match body.eval(context) {
                        Err(RuntimeError::Break) => break,
                        val => val,
                    }?;
				}
			},
            Stmt::Break => return Err(RuntimeError::Break),
        }
        Ok(Value::Void)
    }
}

impl<'t> Eval for Expr<'t> {
    fn eval(&self, context: &mut Context) -> Result<Value> {
        match *self {
            Expr::Grouping(ref inner) => inner.eval(context),
            Expr::Logical(ref inner) => inner.eval(context),
            Expr::Binary(ref inner) => inner.eval(context),
            Expr::Unary(ref inner) => inner.eval(context),
            Expr::Literal(ref inner) => inner.eval(context),
            Expr::Var(var) => {
                let env = context.env();
                match env.lookup(var) {
                    None => return Err(RuntimeError::UndefinedVariable(var.into())),
                    Some(v) => {
                        return Ok((*v).clone())
                    }
                }
            },
            Expr::Assign(var, ref lhs) => {
                let lhs = lhs.eval(context)?;
                let env = context.env_mut();
                if env.rebind(var, lhs.clone()) {
                    Ok(lhs)
                } else {
                    Err(RuntimeError::UndefinedVariable(var.into()))
                }
            },
        }
    }
}

macro_rules! numeric_binary_op (
    ($op:tt, $lhs:ident, $rhs:ident) => (
        match ($lhs, $rhs) {
            (Value::Number(nlhs), Value::Number(nrhs)) => {
                Ok(Value::Number(nlhs $op nrhs))
            },
            _ => {
                Err(RuntimeError::InvalidOperands("number".into()))
            },
        }
    );
);

macro_rules! comparison_op (
    ($op:tt, $lhs:ident, $rhs:ident) => (
        match ($lhs, $rhs) {
            (Value::Number(nlhs), Value::Number(nrhs)) => {
				if nlhs $op nrhs {
					Ok(Value::True)
				} else {
					Ok(Value::False)
				}
            },
            _ => Err(RuntimeError::InvalidOperands("number".into())),
        }
    );
);

impl<'t> Eval for Binary<'t> {
    fn eval(&self, context: &mut Context) -> Result<Value> {
        let lhs = self.lhs.eval(context)?;
        let rhs = self.rhs.eval(context)?;
        let op = &self.operator;

        match *op {
            BinaryOperator::Plus => match (lhs, rhs) {
                (Value::String(lhs), Value::String(rhs)) => {
                    let mut res = lhs.clone();
                    res.push_str(&rhs);
                    return Ok(Value::String(res));
                },
                (lhs, rhs) => numeric_binary_op!(+, lhs, rhs)
            },
            BinaryOperator::Minus => numeric_binary_op!(-, lhs, rhs),
            BinaryOperator::Star => numeric_binary_op!(*, lhs, rhs),
            BinaryOperator::Slash => {
                match (lhs, rhs) {
                    (Value::Number(_), Value::Number(denom)) if denom == 0.0 => {
                        Err(RuntimeError::DivideByZero)
                    },
                    (Value::Number(nlhs), Value::Number(nrhs)) => {
                        Ok(Value::Number(nlhs / nrhs))
                    },
                    _ => Err(RuntimeError::InvalidOperands("numbers".into()))
                }
            },
            BinaryOperator::GreaterThan => comparison_op!(>, lhs, rhs),
            BinaryOperator::GreaterThanEq => comparison_op!(>=, lhs, rhs),
            BinaryOperator::LessThan => comparison_op!(<, lhs, rhs),
            BinaryOperator::LessThanEq => comparison_op!(<=, lhs, rhs),
            _ => unimplemented!("==, !="),
        }
    }
}

impl<'t> Eval for Logical<'t> {
    fn eval(&self, context: &mut Context) -> Result<Value> {
        let lhs = self.lhs.eval(context)?;
        let lhsb = lhs.truthy();

        let shortcircuit = match self.operator {
            LogicalOperator::And => !lhsb,
            LogicalOperator::Or => lhsb,
        };
        if shortcircuit {
            return Ok(lhs);
        }
        self.rhs.eval(context)
    }
}

impl<'t> Eval for Unary<'t> {
    fn eval(&self, context: &mut Context) -> Result<Value> {
        let operand = self.unary.eval(context)?;
        match self.operator {
            UnaryOperator::Minus => {
                match operand {
                    Value::Number(n) => {
                        Ok(Value::Number(-1.0 * n))
                    },
                    _ => Err(RuntimeError::InvalidOperands("numbers".into()))
                }
            },
            UnaryOperator::Bang => {
                if operand.truthy() {
                    Ok(Value::False)
                } else {
                    Ok(Value::True)
                }
            },
        }
    }
}

impl Eval for Value {
    fn eval(&self, _: &mut Context) -> Result<Value> {
        Ok(self.clone())
    }
}

impl<'a, 't> Eval for &'a [Stmt<'t>]
{
    fn eval(&self, ctx: &mut Context) -> Result<Value> {
        {
            let stmts = self.into_iter();
            for stmt in stmts {
                stmt.eval(ctx)?;
            }
        }
        Ok(Value::Void)
    }
}
