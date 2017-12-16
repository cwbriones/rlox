use std::fmt::Debug;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use value::Value;
use value::LoxInstance;
use environment::Environment;
use parser::ast::FunctionDecl;

use eval::RuntimeError;
use eval::Interpreter;

#[derive(PartialEq, Clone)]
pub enum Callable {
    Function(LoxFunction),
    Clock,
    Class(LoxClassHandle),
}

impl Callable {
    pub fn new_function(declaration: Rc<RefCell<FunctionDecl>>, env: Environment) -> Self {
        Callable::Function(LoxFunction::new(declaration, env))
    }

    pub fn new_class(name: &str, methods: Vec<Rc<RefCell<FunctionDecl>>>, env: Environment, superclass: Option<LoxClassHandle>) -> Self {
        Callable::Class(LoxClassHandle {
            class: Rc::new(LoxClass::new(name, methods, env, superclass))
        })
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
                let decl = fun.declaration.borrow();
                write!(f, "<fn '{}'>", decl.var.name())
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

    pub fn bind(&self, this: Value) -> Self {
        let mut closure = self.closure.clone();
        closure.set_at("this", this, 0);
        LoxFunction {
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

fn clock(_: &mut Interpreter, _: Vec<Value>) -> Result<Value, RuntimeError> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let epoch_time =
        SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("[FATAL] failed to get system time")
        .as_secs();
    Ok(Value::Number(epoch_time as f64))
}

#[derive(Clone)]
pub struct LoxClassHandle {
    class: Rc<LoxClass>,
}

use std::ops::Deref;

impl Deref for LoxClassHandle {
    type Target = LoxClass;

    fn deref(&self) -> &LoxClass{
        &*self.class
    }
}

#[derive(Clone)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, LoxFunction>,
    superclass: Option<LoxClassHandle>,
}

type LoxFunctionRef = Rc<RefCell<FunctionDecl>>;

impl LoxClassHandle {
    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        let instance = Value::Instance(LoxInstance::new(self.class.clone()));
        if let Some(init) = self.init() {
            let bound = init.bind(instance.clone());
            bound.call(interpreter, arguments)?;
        }
        Ok(instance)
    }

    fn arity(&self) -> usize {
        self.init().map(|m| m.arity()).unwrap_or(0)
    }
}

impl LoxClass {
    pub fn new(name: &str, declarations: Vec<LoxFunctionRef>, env: Environment, superclass: Option<LoxClassHandle>) -> Self {
        let mut methods = HashMap::new();
        for decl in declarations {
            let name = decl.borrow().var.name().into();
            let method = LoxFunction::new(decl, env.clone());
            methods.insert(name, method);
        }
        LoxClass {
            name: name.to_owned(),
            methods: methods,
            superclass: superclass,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    fn init(&self) -> Option<LoxFunction> {
        self.methods.get("init").map(Clone::clone)
    }

    pub fn method(&self, name: &str) -> Option<LoxFunction> {
        self.methods
            .get(name)
            .map(Clone::clone)
            .or_else(|| {
                // FIXME: Do I need to clone?
                let superclass = self.superclass.clone();
                superclass.and_then(|sc| sc.method(name))
            })
    }
}

impl PartialEq for LoxClassHandle {
    fn eq(&self, other: &LoxClassHandle) -> bool {
        &*self.class as *const _ == &*other.class as *const _
    }
}

impl ::std::fmt::Debug for LoxClassHandle {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "<class '{}'>", self.name())
    }
}
