use std::fmt::Debug;
use std::rc::Rc;
use std::cell::RefCell;

use value::Value;
use environment::Environment;
use parser::ast::FunctionDecl;

use eval::RuntimeError;
use eval::Interpreter;

#[derive(PartialEq, Clone)]
pub enum Callable {
    Function(LoxFunction),
    Clock,
}

impl Callable {
    pub fn new_function(declaration: Rc<RefCell<FunctionDecl>>, env: Environment) -> Self {
        Callable::Function(LoxFunction::new(declaration, env))
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        match *self {
            Callable::Function(ref fun) => fun.call(interpreter, arguments),
            Callable::Clock => clock(interpreter, arguments),
        }
    }

    pub fn arity(&self) -> usize {
        match *self {
            Callable::Function(ref fun) => fun.arity(),
            Callable::Clock => 0,
        }
    }
}

impl Debug for Callable {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Callable::Function(ref fun) => {
                let decl = fun.declaration.borrow();
                write!(f, "<fn {}>", decl.var.name())
            },
            Callable::Clock => {
                write!(f, "<builtin 'clock'>")
            }
        }
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    declaration: Rc<RefCell<FunctionDecl>>,
    closure: Environment,
}

impl LoxFunction {
    fn new(declaration: Rc<RefCell<FunctionDecl>>, closure: Environment) -> Self {
        LoxFunction {
            declaration,
            closure,
        }
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        let decl = &self.declaration.borrow();
        // Create new environment
        let mut env = self.closure.extend();
        for (p, a) in decl.parameters.iter().zip(arguments) {
            env.set(p, a);
        }
        // Evaluate body
        match interpreter.interpret_within(&mut env, decl.body.as_slice()) {
            Err(RuntimeError::Return) => {
                let retval = interpreter.pop_return();
                return Ok(retval);
            },
            val => val,
        }?;
        return Ok(Value::Nil)
    }

    fn arity(&self) -> usize {
        self.declaration.borrow().parameters.len()
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        // Since all environments are created fresh on evaluation,
        // they are equal if they are the same function.
        self.closure == other.closure
    }
}

fn clock(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, RuntimeError> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let epoch_time =
        SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("[FATAL] failed to get system time")
        .as_secs();
    Ok(Value::Number(epoch_time as f64))
}
