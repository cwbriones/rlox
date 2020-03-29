use super::value::Value;

use ::chunk::Chunk;

use broom::prelude::Trace;
use broom::prelude::Tracer;

use std::fmt::{Debug, Display};

#[derive(Clone)]
pub enum Object {
    String(String),
    LoxFunction(LoxFunction),
    LoxClosure(LoxClosure),
    LoxUpValue(LoxUpValue),
    NativeFunction(NativeFunction),
}

impl Trace<Self> for Object {
    fn trace(&self, tracer: &mut Tracer<Self>) {
        match self {
            Object::String(_) => {},
            Object::LoxFunction(f) => f.trace(tracer),
            Object::NativeFunction(_) => {},
            Object::LoxUpValue(_) => {},
            Object::LoxClosure(c) => c.trace(tracer),
        }
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
    pub fn new(name: &str, arity: u8) -> Self {
        let name: String = name.into();
        let chunk = Chunk::new(name.clone());
        LoxFunction { name, arity, chunk, upvalue_count: 0 }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    pub fn set_upvalue_count(&mut self, count: usize) {
        self.upvalue_count = count;
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
    pub arity: u8,
}

impl LoxClosure {
    pub fn new(function: LoxFunction, upvalues: Vec<LoxUpValue>) -> Self {
        let arity = function.arity;
        LoxClosure {
            function,
            upvalues,
            arity,
        }
    }

    pub fn function(&self) -> &LoxFunction {
        &self.function
    }

    pub fn upvalue_count(&self) -> usize {
        self.upvalues.len()
    }

    pub fn get(&self, idx: usize) -> LoxUpValue {
        self.upvalues[idx]
    }

    pub fn get_in(&self, idx: usize) -> Value {
        // FIXME(unsafe)
        // ugh...
        unsafe {
            self.upvalues[idx].get()
        }
    }

    pub fn set(&mut self, idx: usize, upvalue: LoxUpValue) {
        self.upvalues[idx] = upvalue;
    }

    pub fn set_in(&mut self, idx: usize, value: Value) {
        self.upvalues[idx].value = value;
    }
}

impl Trace<Object> for LoxClosure {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        self.function.trace(tracer);
        self.upvalues.trace(tracer);
    }
}

#[derive(Clone, Copy)]
pub struct LoxUpValue {
    value: Value,
    local: *mut Value,
    closed: bool,
}

impl Trace<Object> for LoxUpValue {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        if let Some(o) = self.value.as_object() {
            o.trace(tracer)
        }
        // We don't need to trace local since if it's valid then it's still rooted on the stack.
    }
}

impl LoxUpValue {
    pub fn new(local: *mut Value) -> Self {
        LoxUpValue {
            value: Value::nil(),
            local,
            closed: false,
        }
    }

    pub fn is(&self, other: *mut Value) -> bool {
        other == self.local
    }

    pub fn local(&self) -> *mut Value {
        self.local
    }

    pub unsafe fn close(&mut self) {
        self.closed = true;
        self.value = *self.local;
    }

    pub unsafe fn get(&self) -> Value {
        if self.closed {
            self.value
        } else {
            *self.local
        }
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

    pub fn string(string: String) -> Self {
        Object::String(string)
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
            Object::LoxUpValue(ref up) => write!(f, "<upvalue [{:?}]>", up.value),
            Object::NativeFunction(ref na) => write!(f, "<native fn {:?}>", na.name),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "{}", s),
            Object::LoxFunction(ref fun) => write!(f, "<fn '{}'>", fun.name),
            Object::LoxClosure(ref cl) => write!(f, "<closure '{:?}'>", cl.function),
            Object::LoxUpValue(ref up) => write!(f, "{}", up.value),
            Object::NativeFunction(ref na) => write!(f, "<native fn '{}'>", na.name),
        }
    }
}
