use super::arena::ArenaPtr;
use super::value::Value;

use ::chunk::Chunk;

use std::fmt::{Debug, Display};
use std::ops::Deref;
use std::ops::DerefMut;

pub enum Object {
    String(String),
    LoxFunction(LoxFunction),
    LoxClosure(LoxClosure),
    LoxUpValue(LoxUpValue),
}

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

pub struct LoxClosure {
    function: ObjectHandle,
    upvalues: Vec<ObjectHandle>,
}

impl LoxClosure {
    pub fn new(function: ObjectHandle, upvalues: Vec<ObjectHandle>) -> Self {
        LoxClosure {
            function,
            upvalues,
        }
    }

    pub fn function(&self) -> ObjectHandle {
        self.function
    }

    pub fn upvalue_count(&self) -> usize {
        self.upvalues.len()
    }

    pub fn get(&self, idx: usize) -> ObjectHandle {
        self.upvalues[idx]
    }

    pub fn get_in(&self, idx: usize) -> Value {
        // ugh...
        if let Object::LoxUpValue(ref up) = *self.upvalues[idx] {
            return unsafe { up.get() };
        }
        panic!("closure should only contain upvalues")
    }

    pub fn set(&mut self, idx: usize, upvalue: ObjectHandle) {
        self.upvalues[idx] = upvalue;
    }

    pub fn set_in(&mut self, idx: usize, value: Value) {
        if let Object::LoxUpValue(ref mut up) = *self.upvalues[idx] {
            up.value = value;
            return;
        }
        panic!("closure should only contain upvalues")
    }
}

pub struct LoxUpValue {
    value: Value,
    local: *mut Value,
    closed: bool,
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
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "{}", s),
            Object::LoxFunction(ref fun) => write!(f, "<function {:?}>", fun.name),
            Object::LoxClosure(ref cl) => write!(f, "<closure {:?}>", cl.function),
            Object::LoxUpValue(ref up) => write!(f, "{}", up.value),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct ObjectHandle {
    ptr: ArenaPtr<Object>
}

impl ObjectHandle {
    pub fn new(ptr: ArenaPtr<Object>) -> Self {
        ObjectHandle {ptr}
    }

    pub fn into_value(self) -> Value {
        Value::object(self)
    }

    pub fn into_raw(self) -> u64 {
        self.ptr.into_raw()
    }

    pub fn mark(&mut self) {
        self.ptr.mark();
    }

    pub fn trace(&mut self) {
        if self.ptr.is_marked() {
            return;
        }
        match **self {
            Object::LoxClosure(ref mut cl) => {
                cl.function.trace();
                cl.upvalues.iter_mut().for_each(|o| o.trace());
            }
            Object::LoxUpValue(ref mut up) => {
                up.value.as_object().map(|mut o| o.trace());
            }
            Object::LoxFunction(ref mut f) => {
                f.chunk.constants()
                    .filter_map(|v| v.as_object())
                    .for_each(|ref mut o| o.trace());
            }
            _ => {},
        }
    }
}

impl Deref for ObjectHandle {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        self.ptr.deref()
    }
}

impl DerefMut for ObjectHandle {
    fn deref_mut(&mut self) -> &mut Object {
        self.ptr.deref_mut()
    }
}

impl Display for ObjectHandle {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.deref())
    }
}

impl Debug for ObjectHandle {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", self.deref())
    }
}
