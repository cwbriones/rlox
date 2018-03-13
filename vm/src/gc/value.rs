use super::object::ObjectHandle;
use super::arena::ArenaPtr;

use std::mem::transmute;
use std::fmt::{Debug, Display};

#[derive(Copy, Clone, PartialEq)]
pub struct Value {
    raw: u64,
}

#[derive(Debug)]
pub enum Variant {
    Float(f64),
    True,
    False,
    Nil,
    Obj(ObjectHandle)
}

const TAG_TRUE: u64 = 0x01;
const TAG_FALSE: u64 = 0x02;
const TAG_NIL: u64 = 0x03;

const QNAN: u64 = 0x7ffc000000000000;
const SIGN: u64 = 1 << 63;
const TRUE: u64 = QNAN | TAG_TRUE;
const FALSE: u64 = QNAN | TAG_FALSE;
const NIL: u64 = QNAN | TAG_NIL;

impl Value {
    pub unsafe fn from_raw(raw: u64) -> Self {
        Value { raw }
    }

    pub fn into_raw(self) -> u64 {
        self.raw
    }

    pub fn float(f: f64) -> Self {
        Value {
            raw: unsafe { transmute(f) }
        }
    }

    pub fn truelit() -> Self {
        Value {
            raw: TRUE,
        }
    }

    pub fn falselit() -> Self {
        Value {
            raw: FALSE,
        }
    }

    pub fn nil() -> Self {
        Value {
            raw: NIL,
        }
    }

    pub fn object(handle: ObjectHandle) -> Self {
        Value { raw: handle.into_raw() | QNAN | SIGN }
    }

    pub fn as_float(&self) -> f64 {
        if let Variant::Float(f) = self.decode() {
            return f;
        }
        panic!("value not a float");
    }

    pub fn truthy(&self) -> bool {
        self.raw != FALSE && self.raw != NIL
    }

    pub fn falsey(&self) -> bool {
        self.raw == FALSE || self.raw == NIL
    }

    pub fn decode(&self) -> Variant {
        let u = self.raw;
        if u & QNAN != QNAN {
            return Variant::Float(unsafe{ transmute(u) });
        }
        // sign bit indicates pointer
        if (u & (QNAN | SIGN)) == (QNAN | SIGN) {
            let ptr = u & (!(QNAN | SIGN)); // only keep lower 51 bits
            unsafe {
                let arena_ptr = ArenaPtr::from_raw(ptr);
                let handle = ObjectHandle::new(arena_ptr);
                return Variant::Obj(handle);
            }
        }
        match u & 7 {
            TAG_TRUE => Variant::True,
            TAG_FALSE => Variant::False,
            TAG_NIL => Variant::Nil,
            tag => panic!("unknown singleton {}", tag),
        }
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
