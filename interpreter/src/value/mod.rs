use std::fmt::Display;
use std::rc::Rc;
use std::cell::RefCell;

use parser::ast::{FunctionDecl, FunctionStmt};
use environment::Environment;

use self::callable::Callable;
use self::instance::LoxInstance;

mod callable;
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

    pub fn into_class(self) -> Option<callable::LoxClassHandle> {
        if let Value::Callable(Callable::Class(cls)) = self {
            return Some(cls);
        }
        None
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

impl Display for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            // We have to manually match 0 to correctly display -0.
            Value::Number(n) if n == 0.0 => write!(f, "{:.0?}", n),
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
