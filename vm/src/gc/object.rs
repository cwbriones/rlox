use super::arena::ArenaPtr;
use super::value::Value;

use std::fmt::{Debug, Display};

use std::ops::Deref;

pub enum Object {
    String(String)
}

impl Object {
    pub fn string(string: String) -> Self {
        Object::String(string)
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "<String: {:?}>", s)
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Object::String(ref s) => write!(f, "{}", s)
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

    pub fn is_marked(&self) -> bool {
        self.ptr.is_marked()
    }
}

impl Deref for ObjectHandle {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        self.ptr.deref()
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
