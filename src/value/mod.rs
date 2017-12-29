use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;
use std::cell::RefCell;

use parser::ast::{FunctionDecl, FunctionStmt};
use environment::Environment;
use eval::RuntimeError;
use eval::Interpreter;

use self::instance::LoxInstance;

pub use self::class::{LoxClass, LoxClassHandle};

mod class;
mod instance;

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Void,
    Callable(Callable),
    Instance(LoxInstance),
}

impl Value {
    pub fn new_function(name: &str, declaration: Rc<RefCell<FunctionDecl>>, env: Environment) -> Self {
        Value::Callable(Callable::new_function(name, declaration, env))
    }

    pub fn new_lambda(declaration: Rc<RefCell<FunctionDecl>>, env: Environment) -> Self {
        Value::Callable(Callable::new_lambda(declaration, env))
    }

    pub fn new_class(name: &str, methods: Vec<FunctionStmt>, env: Environment, superclass: Option<Value>) -> Self {
        let superclass = superclass.map(|value| {
            if let Value::Callable(Callable::Class(class)) = value {
                return class;
            }
            panic!("superclass should have been validated in interpreter")
        });
        Value::Callable(Callable::new_class(name, methods, env, superclass))
    }

    pub fn builtin_clock() -> Self {
        Value::Callable(Callable::Clock)
    }

    pub fn truthy(&self) -> bool {
        match *self {
            Value::Nil => false,
            Value::False => false,
            _ => true,
        }
    }

    pub fn into_class(self) -> Option<LoxClassHandle> {
        if let Value::Callable(Callable::Class(cls)) = self {
            return Some(cls);
        }
        None
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(ref s) => write!(f, "{}", s),
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Nil => write!(f, "nil"),
            Value::Void => Ok(()),
            Value::Callable(ref fun) => write!(f, "{:?}", fun),
            Value::Instance(ref inst) => write!(f, "{:?}", inst),
        }
    }
}

pub trait Access {
    fn get(&self, field: &str) -> Option<Value>;

    fn set(&mut self, field: &str, value: Value);
}

#[derive(PartialEq, Clone)]
pub enum Callable {
    Function(LoxFunction),
    Clock,
    Class(LoxClassHandle),
}

impl Callable {
    pub fn new_function(name: &str, declaration: Rc<RefCell<FunctionDecl>>, env: Environment) -> Self {
        Callable::Function(LoxFunction::new(name, declaration, env))
    }

    pub fn new_lambda(declaration: Rc<RefCell<FunctionDecl>>, env: Environment) -> Self {
        Callable::Function(LoxFunction::new_lambda(declaration, env))
    }

    pub fn new_class(name: &str, methods: Vec<FunctionStmt>, env: Environment, superclass: Option<LoxClassHandle>) -> Self {
        Callable::Class(LoxClassHandle::new(LoxClass::new(name, methods, env, superclass)))
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        match *self {
            Callable::Function(ref fun) => fun.call(interpreter, arguments),
            Callable::Clock => clock(interpreter, arguments),
            Callable::Class(ref cls) => cls.call(interpreter, arguments),
        }
    }

    pub fn arity(&self) -> usize {
        match *self {
            Callable::Function(ref fun) => fun.arity(),
            Callable::Clock => 0,
            Callable::Class(ref cls) => cls.arity(),
        }
    }
}

impl Debug for Callable {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Callable::Function(ref fun) => {
                if let Some(ref name) = fun.name {
                    write!(f, "<fn '{}'>", name)
                } else {
                    write!(f, "<fn>")
                }
            },
            Callable::Clock => {
                write!(f, "<builtin 'clock'>")
            },
            Callable::Class(ref cls) => {
                write!(f, "{}", cls.name())
            },
        }
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    name: Option<String>,
    declaration: Rc<RefCell<FunctionDecl>>,
    closure: Environment,
}

impl LoxFunction {
    fn new(name: &str, declaration: Rc<RefCell<FunctionDecl>>, closure: Environment) -> Self {
        let name = Some(name.to_owned());
        LoxFunction {
            name,
            declaration,
            closure,
        }
    }

    fn new_lambda(declaration: Rc<RefCell<FunctionDecl>>, closure: Environment) -> Self {
        LoxFunction {
            name: None,
            declaration,
            closure,
        }
    }

    pub fn bind(&self, this: Value) -> Self {
        let mut closure = self.closure.extend();
        closure.set_at("this", this, 0);
        LoxFunction {
            name: self.name.clone(),
            declaration: self.declaration.clone(),
            closure: closure,
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

impl From<f64> for Value {
    fn from(n: f64) -> Value {
        Value::Number(n)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Value {
        Value::String(s)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        if b {
            Value::True
        } else {
            Value::False
        }
    }
}

impl From<LoxFunction> for Value {
    fn from(f: LoxFunction) -> Value {
        Value::Callable(Callable::Function(f))
    }
}

impl From<LoxClassHandle> for Value {
    fn from(class: LoxClassHandle) -> Value {
        Value::Callable(Callable::Class(class))
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
