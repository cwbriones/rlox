use environment::Environment;
use parser::ast::*;
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
    #[fail(display = "can only call functions and classes (at line {})", line)]
    InvalidCallee {
        line: usize,
    },
    #[fail(display = "expected {} argument(s) but got {}", expected, got)]
    BadArity{
        got: usize,
        expected: usize,
    },
    #[fail(display = "return")]
    Return,
    #[fail(display = "only instances have properties")]
    BadAccess,
    #[fail(display = "Superclass must be a class")]
    SuperNotAClass,
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
        let mut globals = Environment::new();
        globals.set_at("clock", Value::builtin_clock(), 0);

        Interpreter {
            globals,
            retvals: Vec::new(),
        }
    }

    pub fn interpret<E: Eval>(&mut self, executable: E) -> Result<Value> {
        let mut globals = self.globals.clone();
        executable.eval(self, &mut globals)
    }

    pub fn interpret_within<E: Eval>(&mut self, environment: &mut Environment, executable: E) -> Result<Value> {
        executable.eval(self, environment)
    }

    pub fn push_return(&mut self, value: Value) {
        self.retvals.push(value);
    }

    pub fn pop_return(&mut self) -> Value {
        self.retvals.pop().expect("return stack to be nonempty")
    }

    pub fn lookup(&self, env: &Environment, var: &Variable) -> Option<Value> {
        if let Some(depth) = var.depth() {
            return env.get_at(var.name(), depth);
        }
        self.globals.get_at(var.name(), 0)
    }

    pub fn assign(&mut self, env: &mut Environment, var: &Variable, val: Value) -> bool {
        if let Some(depth) = var.depth() {
            return env.set_at(var.name(), val, depth);
        }
        self.globals.set_at(var.name(), val, 0)
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
                debug!("Set var '{:?}' to value {}", var, val);
                interpreter.assign(env, var, val);
            },
            Stmt::Function(ref function) => {
                let name = function.var.name();
                let val = Value::new_function(name, function.declaration.clone(), env.clone());
                interpreter.assign(env, &function.var, val);
            },
            Stmt::Block(ref stmts) => {
                let mut enclosing = env.extend();
                for stmt in stmts.iter() {
                    stmt.eval(interpreter, &mut enclosing)?;
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
            Stmt::Class(ref class_decl) => {
                // TODO: Clean this up.
                let class = if let Some(ref sc_var) = class_decl.superclass {
                    let superclass = match interpreter.lookup(env, sc_var) {
                        Some(v) => v.clone(),
                        None => return Err(RuntimeError::UndefinedVariable(sc_var.name().into())),
                    };
                    if superclass.clone().into_class().is_none() {
                        return Err(RuntimeError::SuperNotAClass);
                    }
                    let mut env = env.extend();
                    env.set_at("super", superclass.clone(), 0);
                    Value::new_class(class_decl.var.name(), class_decl.methods.clone(), env, Some(superclass))
                } else {
                    Value::new_class(class_decl.var.name(), class_decl.methods.clone(), env.clone(), None)
                };
                interpreter.assign(env, &class_decl.var, class);
                return Ok(Value::Void);
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
                match interpreter.lookup(env, var) {
                    None => return Err(RuntimeError::UndefinedVariable(var.name().into())),
                    Some(v) => {
                        return Ok(v)
                    }
                }
            },
            Expr::Assign(ref var, ref lhs) => {
                let lhs = lhs.eval(interpreter, env)?;
                if interpreter.assign(env, var, lhs.clone()) {
                    Ok(lhs)
                } else {
                    Err(RuntimeError::UndefinedVariable(var.name().into()))
                }
            },
            Expr::Call(ref inner) => inner.eval(interpreter, env),
            Expr::Get(ref expr, ref property) => {
                if let Value::Instance(ref mut instance) = expr.eval(interpreter, env)? {
                    Ok(instance.get(property).unwrap_or(Value::Nil))
                } else {
                    Err(RuntimeError::BadAccess)
                }
            },
            Expr::Set(ref lhs, ref name, ref value) => {
                let mut instance = lhs.eval(interpreter, env)?;
                let value = value.eval(interpreter, env)?;
                if let Value::Instance(ref mut instance) = instance {
                    instance.set(name, value.clone());
                    Ok(value)
                } else {
                    Err(RuntimeError::BadAccess)
                }
            },
            Expr::This(ref this, _) => {
                // Any use of 'this' has already been validated
                let val = interpreter.lookup(env, this).expect("'this' should always be defined");
                Ok(val)
            },
            Expr::Super(ref supervar, _, ref method) => {
                // XXX: This is all kind of hacky.
                //
                // Any use of 'super' has already been validated
                let superclass = interpreter.lookup(env, supervar).expect("'super' should always be defined");
                // This should always be one scope above 'super'
                let this = env.get_at("this", supervar.depth().expect("super is resolved") - 1).expect("'this' should be defined");

                // Now we have to resolve the method bound with this
                let superclass = superclass.into_class().expect("'super' should always resolve to a class");
                match superclass.method(method) {
                    Some(method) => {
                        Ok(method.bind(this).into())
                    },
                    None => Err(RuntimeError::BadAccess),
                }
            },
            Expr::Function(ref declaration) => {
                Ok(Value::new_lambda(declaration.clone(), env.clone()))
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

impl Eval for Call {
    fn eval(&self, interpreter: &mut Interpreter, env: &mut Environment) -> Result<Value> {
        let callee = self.callee.eval(interpreter, env)?;
        let args = self.arguments
            .iter()
            .map(|arg| arg.eval(interpreter, env))
            .collect::<Result<Vec<_>>>()?;
        if let Value::Callable(ref callable) = callee {
            if callable.arity() != args.len() {
                return Err(RuntimeError::BadArity {
                    got: args.len(),
                    expected: callable.arity(),
                });
            }
            return callable.call(interpreter, args);
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
            BinaryOperator::Equal => Ok((lhs == rhs).into()),
            BinaryOperator::BangEq => Ok((lhs != rhs).into()),
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
                    _ => Err(RuntimeError::InvalidOperands("numbers".into())) }
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

impl Eval for Literal {
    fn eval(&self, _: &mut Interpreter, _: &mut Environment) -> Result<Value> {
        let val = match *self {
            Literal::Nil => Value::Nil,
            Literal::True => Value::True,
            Literal::False => Value::False,
            Literal::Number(n) => Value::Number(n),
            Literal::String(ref s) => Value::String(s.clone()),
        };
        Ok(val)
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
