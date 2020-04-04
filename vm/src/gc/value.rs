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

#[derive(Debug, Clone)]
pub enum Variant<Obj> {
    Float(f64),
    True,
    False,
    Nil,
    Obj(Obj)
}

impl Variant<Handle<Object>> {
    pub fn cloned<'a>(&self, heap: &'a Heap<Object>) -> Variant<Object> {
        match self {
            Variant::Obj(o) => Variant::Obj(heap.get(o).expect("runtime reference to be valid").clone()),
            Variant::Float(f) => Variant::Float(*f),
            Variant::True => Variant::True,
            Variant::False => Variant::False,
            Variant::Nil => Variant::Nil,
        }
    }

    pub fn deref<'a>(&self, heap: &'a Heap<Object>) -> Variant<&'a Object> {
        match self {
            Variant::Obj(o) => Variant::Obj(heap.get(o).expect("runtime reference to be valid")),
            Variant::Float(f) => Variant::Float(*f),
            Variant::True => Variant::True,
            Variant::False => Variant::False,
            Variant::Nil => Variant::Nil,
        }
    }

    pub fn deref_mut<'a>(&self, heap: &'a mut Heap<Object>) -> Variant<&'a mut Object> {
        match self {
            Variant::Obj(o) => Variant::Obj(heap.get_mut(o).expect("runtime reference to be valid")),
            Variant::Float(f) => Variant::Float(*f),
            Variant::True => Variant::True,
            Variant::False => Variant::False,
            Variant::Nil => Variant::Nil,
        }
    }

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

    pub fn to_raw(self) -> u64 {
        self.handle.to_raw()
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

    pub fn decode(&self) -> Variant<Handle<Object>> {
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

impl Display for Variant<&Object> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            Variant::Nil => write!(f, "nil"),
            Variant::False => write!(f, "false"),
            Variant::True => write!(f, "true"),
            Variant::Float(n) => write!(f, "{}", n),
            Variant::Obj(o) => write!(f, "{}", o),
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
