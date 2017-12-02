use environment::Environment;
use parser::ast::{Expr,Stmt,Binary,Unary,UnaryOperator,BinaryOperator,Logical,LogicalOperator,Call};
use value::Value;
use value::LoxFunction;

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
    #[fail(display = "Can only call functions and classes (at line {})", line)]
    InvalidCallee {
        line: usize,
    },
    #[fail(display = "Expected {} argument(s) but got {}", expected, got)]
    BadArity{
        got: usize,
        expected: usize,
    },
    #[fail(display = "return")]
    Return,
}

pub type Result<T> = ::std::result::Result<T, RuntimeError>;

pub struct Interpreter {
    globals: Environment,

    // Stack of return values
    //
    // Ideally we could add the value to the RuntimeError::Return variant but that would require
    // Value to be Sync, which is quite complicated due to closures.
    retvals: Vec<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            globals: Environment::new(),
            retvals: Vec::new(),
        }
    }

    pub fn interpret<E: Eval>(&mut self, executable: E) -> Result<Value> {
        let mut globals = self.globals.clone();
        executable.eval(self, &mut globals)
    }

    fn push_return(&mut self, value: Value) {
        self.retvals.push(value);
    }

    fn pop_return(&mut self) -> Value {
        self.retvals.pop().expect("return stack to be nonempty")
    }
}

pub trait Eval {
    fn eval(&self, intepreter: &mut Interpreter, env: &mut Environment) -> Result<Value>;
}

impl Eval for Stmt {
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        match *self {
            Stmt::Expr(ref inner) => { inner.eval(interpreter, env)?; }
            Stmt::Print(ref inner) => {
                let evald = inner.eval(interpreter, env)?;
                println!("{}", evald);
            },
            Stmt::Var(ref var, ref expr) => {
                let val = expr.eval(interpreter, env)?;
                debug!("Set var '{}' to value {}", var, val);
                env.bind(var, val);
            },
            Stmt::Function(ref func_decl) => {
                let name = &(**func_decl).name;
                let func = Value::Function(LoxFunction::new(func_decl.clone(), env.clone()));
                env.bind(name, func);
            },
            Stmt::Block(ref stmts) => {
                let mut enclosing = env.extend();
                for stmt in stmts.iter() {
                    if let Err(err) = stmt.eval(interpreter, &mut enclosing) {
                        return Err(err);
                    }
                }
            },
            Stmt::If(ref cond, ref then_clause, ref else_clause) => {
                if cond.eval(interpreter, env)?.truthy() {
                    then_clause.eval(interpreter, env)?;
                } else if let &Some(ref else_clause) = else_clause {
                    else_clause.eval(interpreter, env)?;
                }
            },
			Stmt::While(ref cond, ref body) => {
				while cond.eval(interpreter, env)?.truthy() {
					match body.eval(interpreter, env) {
                        Err(RuntimeError::Break) => break,
                        val => val,
                    }?;
				}
			},
            Stmt::Break => return Err(RuntimeError::Break),
            Stmt::Return(ref expr) => {
                let retval = expr.eval(interpreter, env)?;
                interpreter.push_return(retval);
                return Err(RuntimeError::Return);
            },
        }
        Ok(Value::Void)
    }
}

impl Eval for Expr {
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        match *self {
            Expr::Grouping(ref inner) => inner.eval(interpreter, env),
            Expr::Logical(ref inner) => inner.eval(interpreter, env),
            Expr::Binary(ref inner) => inner.eval(interpreter, env),
            Expr::Unary(ref inner) => inner.eval(interpreter, env),
            Expr::Literal(ref inner) => inner.eval(interpreter, env),
            Expr::Var(ref var) => {
                match env.lookup(var) {
                    None => return Err(RuntimeError::UndefinedVariable(var.clone())),
                    Some(v) => {
                        return Ok((*v).clone())
                    }
                }
            },
            Expr::Assign(ref var, ref lhs) => {
                let lhs = lhs.eval(interpreter, env)?;
                if env.rebind(var, lhs.clone()) {
                    Ok(lhs)
                } else {
                    Err(RuntimeError::UndefinedVariable(var.clone()))
                }
            },
            Expr::Call(ref inner) => inner.eval(interpreter, env),
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

impl Eval for Call {
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        let callee = self.callee.eval(interpreter, env)?;
        let args = self.arguments
            .iter()
            .map(|arg| arg.eval(interpreter, env))
            .collect::<Result<Vec<_>>>()?;
        if let Value::Function(ref fun) = callee {
            let decl = &fun.declaration;
            if fun.arity() != args.len() {
                return Err(RuntimeError::BadArity {
                    got: args.len(),
                    expected: fun.arity(),
                });
            }
            // Create new environment
            let mut fun_env = fun.closure.extend();
            for (p, a) in decl.parameters.iter().zip(args) {
                fun_env.bind(p, a);
            }
            // Evaluate body
            for stmt in &decl.body {
                match stmt.eval(interpreter, &mut fun_env) {
                    Err(RuntimeError::Return) => {
                        let retval = interpreter.pop_return();
                        return Ok(retval);
                    },
                    val => val
                }?;
            }
            return Ok(Value::Nil)
        }
        Err(RuntimeError::InvalidCallee{line: self.position.line})
    }
}

impl Eval for Binary {
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        let lhs = self.lhs.eval(interpreter, env)?;
        let rhs = self.rhs.eval(interpreter, env)?;
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

impl Eval for Logical {
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        let lhs = self.lhs.eval(interpreter, env)?;
        let lhsb = lhs.truthy();

        let shortcircuit = match self.operator {
            LogicalOperator::And => !lhsb,
            LogicalOperator::Or => lhsb,
        };
        if shortcircuit {
            return Ok(lhs);
        }
        self.rhs.eval(interpreter, env)
    }
}

impl Eval for Unary {
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        let operand = self.unary.eval(interpreter, env)?;
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
    fn eval(&self, _: &mut Interpreter, _: &mut Environment) -> Result<Value> {
        Ok(self.clone())
    }
}

impl<'a> Eval for &'a [Stmt]
{
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        {
            let stmts = self.into_iter();
            for stmt in stmts {
                stmt.eval(interpreter, env)?;
            }
        }
        Ok(Value::Void)
    }
}
