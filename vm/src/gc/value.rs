use std::mem::transmute;
use std::fmt::{Debug, Display};

#[derive(Copy, Clone, PartialEq)]
pub struct Value {
    raw: u64,
}

#[derive(Debug)]
enum Variant {
    Float(f64),
    True,
    False,
    Nil,
    Ptr(ObjectRef),
}

const TAG_TRUE: u64 = 0x01;
const TAG_FALSE: u64 = 0x02;
const TAG_NIL: u64 = 0x03;

const QNAN: u64 = 0x7ffc000000000000;
const SIGN: u64 = 1 << 63;
const TRUE: u64 = QNAN | TAG_TRUE;
const FALSE: u64 = QNAN | TAG_FALSE;
const NIL: u64 = QNAN | TAG_NIL;
const MAN: u64 = (1 << 52) - 1;

#[derive(Debug)]
pub struct ObjectRef {
    ptr: usize,
}

fn decode(u: u64) -> Variant {
    if u & QNAN != QNAN {
        return Variant::Float(unsafe{ transmute(u) });
    }
    // sign bit indicates pointer
    if (u & SIGN) == 1 {
        let man = u & MAN; // only keep lower 51 bits
        let ptr = man as usize;
        return Variant::Ptr(ObjectRef { ptr })
    }
    match u & 7 {
        TAG_TRUE => Variant::True,
        TAG_FALSE => Variant::False,
        TAG_NIL => Variant::Nil,
        tag => panic!("unknown singleton {}", tag),
    }
}

impl Value {
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

    pub fn as_float(&self) -> f64 {
        if let Variant::Float(f) = decode(self.raw) {
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
}

impl Debug for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", decode(self.raw))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match decode(self.raw) {
            Variant::Nil => write!(f, "nil"),
            Variant::False => write!(f, "false"),
            Variant::True => write!(f, "true"),
            Variant::Float(n) => write!(f, "{}", n),
            Variant::Ptr(_) => write!(f, "<Object>"),
        }
    }
}
