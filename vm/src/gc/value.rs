use super::object::Object;

use std::fmt::{Debug, Display};

use broom::{
    Heap,
    Handle,
    tag::{Tag, TaggedHandle},
    prelude::{Trace, Tracer},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value {
    handle: TaggedHandle<Object>,
}

#[derive(Debug)]
pub enum Variant {
    Float(f64),
    True,
    False,
    Nil,
    Obj(Handle<Object>)
}

const TAG_TRUE: u8 = 0x01;
const TAG_FALSE: u8 = 0x02;
const TAG_NIL: u8 = 0x03;

impl Value {
    pub unsafe fn from_raw(raw: u64) -> Self {
        Value {
            handle: TaggedHandle::from_raw(raw),
        }
    }

    pub fn float(float: f64) -> Self {
        Value {
            handle: TaggedHandle::from_float(float),
        }
    }

    pub fn truelit() -> Self {
        Value {
            handle: TaggedHandle::from_tag(TAG_TRUE),
        }
    }

    pub fn falselit() -> Self {
        Value {
            handle: TaggedHandle::from_tag(TAG_FALSE),
        }
    }

    pub fn nil() -> Self {
        Value {
            handle: TaggedHandle::from_tag(TAG_NIL),
        }
    }

    pub fn object(handle: Handle<Object>) -> Self {
        Value {
            handle: TaggedHandle::from_handle(handle)
        }
    }

    pub fn as_float(&self) -> f64 {
        if let Variant::Float(f) = self.decode() {
            return f;
        }
        panic!("value not a float");
    }

    pub fn truthy(&self) -> bool {
        match self.decode() {
            Variant::False | Variant::Nil => false,
            _ => true,
        }
    }

    pub fn falsey(&self) -> bool {
        !self.truthy()
    }

    pub fn decode(&self) -> Variant {
        match self.handle.clone().decode() {
            Tag::Float(float) => {
                Variant::Float(float)
            },
            Tag::Handle(handle) => {
                Variant::Obj(handle)
            },
            Tag::Tag(t) if t == TAG_TRUE => Variant::True,
            Tag::Tag(t) if t == TAG_FALSE => Variant::False,
            Tag::Tag(t) if t == TAG_NIL => Variant::Nil,
            Tag::Tag(t) => panic!("Unknown tag {}", t)
        }
    }

    pub fn deref<'a>(&self, heap: &'a Heap<Object>) -> Option<&'a Object> {
        self.as_object().and_then(|h| heap.get(h))
    }

    pub fn deref_mut<'a>(&self, heap: &'a mut Heap<Object>) -> Option<&'a mut Object> {
        self.as_object().and_then(move |h| heap.get_mut(h))
    }

    pub fn as_object(&self) -> Option<Handle<Object>> {
        if let Tag::Handle(h) = self.handle.clone().decode() {
            Some(h)
        } else {
            None
        }
    }
}

impl Trace<Object> for Value {
    fn trace(&self, tracer: &mut Tracer<Object>) {
        if let Variant::Obj(obj) = self.decode() {
            obj.trace(tracer);
        }
    }
}

impl From<Handle<Object>> for Value {
    fn from(handle: Handle<Object>) -> Self {
        Value::object(handle)
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self.decode() {
            Variant::Nil => write!(f, "nil"),
            Variant::False => write!(f, "false"),
            Variant::True => write!(f, "true"),
            Variant::Float(n) => write!(f, "{:?}", n),
            Variant::Obj(o) => write!(f, "{:?}", o),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self.decode() {
            Variant::Nil => write!(f, "nil"),
            Variant::False => write!(f, "false"),
            Variant::True => write!(f, "true"),
            Variant::Float(n) => write!(f, "{}", n),
            // FIXME: Display shouldn't be unsafe but there's no way to check the
            // pointer without dereferencing.
            Variant::Obj(o) => unsafe {
                write!(f, "{}", o.get_unchecked())
            }
        }
    }
}

impl Into<Value> for f64 {
   fn into(self) -> Value {
       Value::float(self)
   }
}

impl Into<Value> for bool {
    fn into(self) -> Value {
        if self {
            Value::truelit()
        } else {
            Value::falselit()
        }
    }
}
