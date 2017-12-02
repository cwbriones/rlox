use std::fmt::Display;
use std::rc::Rc;
use parser::ast::FunctionDecl;

use environment::Environment;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Void,
    Function(LoxFunction),
}

#[derive(Clone)]
pub struct LoxFunction {
    pub declaration: Rc<FunctionDecl>,
    pub closure: Environment,
}

impl LoxFunction {
    pub fn new(declaration: Rc<FunctionDecl>, closure: Environment) -> Self {
        LoxFunction {
            declaration,
            closure,
        }
    }

    pub fn arity(&self) -> usize {
        self.declaration.parameters.len()
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        let declaration = &*self.declaration;
        declaration.eq(&*other.declaration)
    }
}

impl ::std::fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "<fn {}>", (*self.declaration).name)
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "<fn {}>", (*self.declaration).name)
    }
}

impl Eq for Value {}

impl Value {
    pub fn truthy(&self) -> bool {
        match *self {
            Value::Nil => false,
            Value::False => false,
            _ => true,
        }
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
            Value::Number(n) => write!(f, "{}", n),
            Value::String(ref s) => write!(f, "{}", s),
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Nil => write!(f, "nil"),
            Value::Void => Ok(()),
            Value::Function(ref fun) => write!(f, "{}", fun)
        }
    }
}
