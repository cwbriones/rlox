use std::fmt::Debug;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use value::Value;
use value::LoxInstance;
use environment::Environment;
use parser::ast::FunctionDecl;
use parser::ast::FunctionStmt;

use eval::RuntimeError;
use eval::Interpreter;

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
                if let Some(ref name) = fun.name {
                    write!(f, "<fn {}>", name)
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
    is_method: bool,
}

impl LoxFunction {
    fn new(name: &str, declaration: Rc<RefCell<FunctionDecl>>, closure: Environment) -> Self {
        let name = Some(name.to_owned());
        LoxFunction {
            name,
            declaration,
            closure,
            is_method: false,
        }
    }

    fn new_lambda(declaration: Rc<RefCell<FunctionDecl>>, closure: Environment) -> Self {
        LoxFunction {
            name: None,
            declaration,
            closure,
            is_method: false,
        }
    }

    pub fn bind(&self, this: Value) -> Self {
        let mut closure = self.closure.extend();
        closure.set_at("this", this, 0);
        LoxFunction {
            name: self.name.clone(),
            declaration: self.declaration.clone(),
            closure: closure,
            is_method: true,
        }
    }

    fn is_initializer(&self) -> bool {
        self.is_method &&
            self.name.as_ref().map(|s| s == "init").unwrap_or(false)
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
        if self.is_initializer() {
            // This is always bound 1 level above.
            return Ok(env.get_at("this", 1).expect("this to be bound"));
        }
        Ok(Value::Nil)
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

impl Into<Value> for LoxFunction {
    fn into(self) -> Value {
        Value::Callable(Callable::Function(self))
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
    pub fn new(name: &str, method_stmts: Vec<FunctionStmt>, env: Environment, superclass: Option<LoxClassHandle>) -> Self {
        let mut methods = HashMap::new();
        for stmt in method_stmts {
            let mname = stmt.var.name();
            let fun_decl = stmt.declaration.clone();
            let method = LoxFunction::new(mname, fun_decl, env.clone());
            methods.insert(mname.to_owned(), method);
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
            .or_else(|| self.superclass.as_ref().and_then(|sc| sc.method(name)))
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
