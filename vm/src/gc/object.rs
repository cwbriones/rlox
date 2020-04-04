use super::value::Value;

use ::chunk::Chunk;

use broom::prelude::Trace;
use broom::prelude::Tracer;

use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
pub enum Object {
    String(String),
    LoxFunction(LoxFunction),
    LoxClosure(LoxClosure),
    NativeFunction(NativeFunction),
}

impl Trace<Self> for Object {
    fn trace(&self, tracer: &mut Tracer<Self>) {
        match self {
            Object::String(_) => {},
            Object::LoxFunction(f) => f.trace(tracer),
            Object::NativeFunction(_) => {},
            Object::LoxClosure(c) => c.trace(tracer),
        }
    }
}

#[derive(Debug)]
pub struct LoxFunctionBuilder {
    name: String,
    chunk: Chunk,
    arity: u8,
    upvalue_count: usize,
}

impl LoxFunctionBuilder {
    pub fn new(name: &str, arity: u8) -> Self {
        let name: String = name.into();
        let chunk = Chunk::new(name.clone());
        LoxFunctionBuilder { name, arity, chunk, upvalue_count: 0 }
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    pub fn set_upvalue_count(&mut self, count: usize) {
        self.upvalue_count = count;
    }

    pub fn build(self) -> LoxFunction {
        LoxFunction::new(self)
    }
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    name: String,
    chunk: Chunk,
    arity: u8,
    upvalue_count: usize,
}

impl LoxFunction {
    fn new(builder: LoxFunctionBuilder) -> Self {
        LoxFunction {
            name: builder.name,
            arity: builder.arity,
            chunk: builder.chunk,
            upvalue_count: builder.upvalue_count,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn upvalue_count(&self) -> usize {
        self.upvalue_count
    }
}

impl Trace<Object> for LoxFunction {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        self.chunk.trace(tracer);
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub function: fn(&[Value]) -> Value,
}

#[derive(Clone)]
pub struct LoxClosure {
    function: LoxFunction,
    upvalues: Vec<LoxUpValue>,
}

impl LoxClosure {
    pub fn new(function: LoxFunction, upvalues: Vec<LoxUpValue>) -> Self {
        LoxClosure {
            function,
            upvalues,
        }
    }

    pub fn arity(&self) -> u8 {
        self.function.arity
    }

    pub fn chunk(&self) -> &Chunk {
        self.function.chunk()
    }

    pub fn upvalue_count(&self) -> usize {
        self.upvalues.len()
    }

    pub fn get(&self, idx: usize) -> LoxUpValue {
        self.upvalues[idx].clone()
    }
}

impl Trace<Object> for LoxClosure {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        self.function.trace(tracer);
        self.upvalues.iter()
            .flat_map(|u| u.get())
            .for_each(|v| v.trace(tracer));
    }
}

#[derive(Clone)]
pub struct LoxUpValue {
    inner: Rc<RefCell<Result<Value, usize>>>,
}

impl LoxUpValue {
    pub fn new(local: usize) -> Self {
        LoxUpValue {
            inner: Rc::new(RefCell::new(Err(local))),
        }
    }

    pub fn close<F: FnOnce(usize) -> Value>(&mut self, f: F) {
        let mut inner = self.inner.borrow_mut();
        if let Err(e) = *inner {
            *inner = Ok(f(e))
        }
    }

    pub fn as_local(&self) -> Option<usize> {
        self.inner.borrow().err()
    }

    pub fn get(&self) -> Result<Value, usize> {
        self.inner.borrow().clone()
    }

    pub fn set(&mut self, value: Value) -> Result<(), usize> {
        let mut inner = self.inner.borrow_mut();
        (*inner)?;
        *inner = Ok(value);
        Ok(())
    }
}

impl Object {
    pub fn native_fn(name: &str, arity: u8, function: fn(&[Value]) -> Value) -> Self {
        Object::NativeFunction(
            NativeFunction {
                name: name.into(),
                arity,
                function,
            },
        )
    }

    pub fn as_function(&self) -> Option<&LoxFunction> {
        if let Object::LoxFunction(ref f) = *self {
            Some(f)
        } else {
            None
        }
    }

    pub fn is_closure(&self) -> bool {
        if let Object::LoxClosure(_) = *self {
            true
        } else {
            false
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "{:?}", s),
            Object::LoxFunction(ref fun) => write!(f, "<function: {:?}>", fun.name),
            Object::LoxClosure(ref cl) => write!(f, "<closure {:?}>", cl.function),
            Object::NativeFunction(ref na) => write!(f, "<native fn {:?}>", na.name),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "{}", s),
            Object::LoxFunction(ref fun) => write!(f, "<fn {}>", fun.name),
            Object::LoxClosure(ref cl) => write!(f, "<fn {}>", cl.function.name),
            Object::NativeFunction(ref na) => write!(f, "<native fn {}>", na.name),
        }
    }
}
