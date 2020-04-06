use super::value::Value;
use super::value::WithHeap;

use ::chunk::Chunk;

use broom::prelude::Trace;
use broom::prelude::Tracer;
use broom::prelude::Heap;
use broom::prelude::Handle;

use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Clone)]
pub enum Object {
    String(String),
    LoxFunction(LoxFunction),
    LoxClass(LoxClass),
    LoxClosure(LoxClosure),
    LoxInstance(LoxInstance),
    NativeFunction(NativeFunction),
    BoundMethod(BoundMethod),
}

/// Quickly implement a method that collapses the enum into an Option for
/// matching a single variant.
macro_rules! impl_as (
    ($name:ident, $typ:ident) => {
        pub fn $name(&self) -> Option<&$typ> {
            if let Object::$typ(ref o) = *self {
                Some(o)
            } else {
                None
            }
        }
    }
);

impl Object {
    pub fn native_fn(name: &str, arity: u8, function: fn(&Heap<Object>, &[Value]) -> Value) -> Self {
        Object::NativeFunction(
            NativeFunction {
                name: name.into(),
                arity,
                function,
            },
        )
    }

    impl_as!(as_string, String);
    impl_as!(as_function, LoxFunction);
    impl_as!(as_closure, LoxClosure);
    impl_as!(as_class, LoxClass);
    impl_as!(as_instance, LoxInstance);
}

impl Trace<Self> for Object {
    fn trace(&self, tracer: &mut Tracer<Self>) {
        match self {
            Object::String(_) => {},
            Object::LoxFunction(f) => f.trace(tracer),
            Object::NativeFunction(_) => {},
            Object::LoxClass(c) => c.trace(tracer),
            Object::LoxClosure(c) => c.trace(tracer),
            Object::LoxInstance(c) => c.trace(tracer),
            Object::BoundMethod(c) => c.trace(tracer),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "{:?}", s),
            Object::LoxFunction(ref fun) => write!(f, "<function: {:?}>", fun.name),
            Object::LoxClass(ref class) => write!(f, "<class {:?}>", class.name),
            Object::LoxClosure(ref cl) => write!(f, "<closure {:?}>", cl.function),
            Object::LoxInstance(ref inst) => write!(f, "<instance {}>", inst.classname()),
            Object::NativeFunction(ref na) => write!(f, "<native fn {:?}>", na.name),
            Object::BoundMethod(ref b) => write!(f, "<bound method {:?}>", b.closure.function),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "{}", s),
            Object::LoxFunction(ref fun) => write!(f, "<fn {}>", fun.name),
            Object::LoxClass(ref class) => write!(f, "{}", class.name),
            Object::LoxClosure(ref cl) => write!(f, "<fn {}>", cl.function.name),
            Object::LoxInstance(ref inst) => write!(f, "{} instance", inst.classname()),
            Object::NativeFunction(ref na) => write!(f, "<native fn {}>", na.name),
            Object::BoundMethod(ref b) => write!(f, "<fn {}>", b.closure.function.name),
        }
    }
}

impl<'h, 'a> Display for WithHeap<'h, &'a Object> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self.item {
            Object::String(ref s) => write!(f, "{}", s),
            Object::LoxFunction(ref fun) => write!(f, "<fn {}>", fun.name),
            Object::LoxClass(ref class) => write!(f, "{}", class.name),
            Object::LoxClosure(ref cl) => write!(f, "<fn {}>", cl.function.name),
            Object::LoxInstance(ref inst) => write!(f, "{} instance", inst.classname()),
            Object::NativeFunction(ref na) => write!(f, "<native fn {}>", na.name),
            Object::BoundMethod(ref b) => write!(f, "<fn {}>", b.closure.function.name),
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

    pub fn name(&self) -> &str {
        &self.name
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
    pub function: fn(&Heap<Object>, &[Value]) -> Value,
}

#[derive(Debug, Clone)]
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

    pub fn name(&self) -> &str {
        self.function.name()
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, Handle<Object>>,
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Handle<Object>>) -> Self {
        LoxClass { name, methods }
    }

    pub fn method(&self, name: &str) -> Option<Handle<Object>> {
        self.methods.get(name).cloned()
    }
}

impl Trace<Object> for LoxClass {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        self.methods.values().for_each(|v| v.trace(tracer));
    }
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    class: Handle<Object>,
    fields: HashMap<String, Value>,
}

impl LoxInstance {
    pub fn new(class: Handle<Object>) -> Self {
        LoxInstance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn class(&self) -> Handle<Object> {
        self.class
    }

    pub fn get_property(&self, name: &str) -> Option<Value> {
        self.fields.get(name).cloned()
    }

    pub fn set_property(&mut self, name: &str, value: Value) {
        self.fields.insert(name.into(), value);
    }

    pub fn classname(&self) -> &str {
        unsafe {
            let class = self.class
                .get_unchecked()
                .as_class()
                .expect("should be a class");
            &class.name
        }
    }
}

impl Trace<Object> for LoxInstance {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        self.class.trace(tracer);
        self.fields.values().for_each(|v| v.trace(tracer));
    }
}

#[derive(Debug, Clone)]
pub struct BoundMethod {
    pub receiver: Handle<Object>,
    pub closure: LoxClosure,
}

impl BoundMethod {
    pub fn new(receiver: Handle<Object>, closure: LoxClosure) -> Self {
        BoundMethod { receiver, closure }
    }
}

impl Trace<Object> for BoundMethod {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        self.receiver.trace(tracer);
        self.closure.trace(tracer);
    }
}
