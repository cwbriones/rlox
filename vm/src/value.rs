// An IEEE 754 double-precision float is a 64-bit value with bits laid out like:
//
// 1 Sign bit
// | 11 Exponent bits
// | |          52 Mantissa (i.e. fraction) bits
// | |          |
// S[Exponent-][Mantissa------------------------------------------]
//
// The details of how these are used to represent numbers aren't really
// relevant here as long we don't interfere with them. The important bit is NaN.
//
// An IEEE double can represent a few magical values like NaN ("not a number"),
// Infinity, and -Infinity. A NaN is any value where all exponent bits are set:
//
//  v--NaN bits
// -11111111111----------------------------------------------------
//
// Here, "-" means "doesn't matter". Any bit sequence that matches the above is
// a NaN. With all of those "-", it obvious there are a *lot* of different
// bit patterns that all mean the same thing. NaN tagging takes advantage of
// this. We'll use those available bit patterns to represent things other than
// numbers without giving up any valid numeric values.
//
// NaN values come in two flavors: "signalling" and "quiet". The former are
// intended to halt execution, while the latter just flow through arithmetic
// operations silently. We want the latter. Quiet NaNs are indicated by setting
// the highest mantissa bit:
//
//             v--Highest mantissa bit
// -[NaN      ]1---------------------------------------------------
//
// If all of the NaN bits are set, it's not a number. Otherwise, it is.
// That leaves all of the remaining bits as available for us to play with. We
// stuff a few different kinds of things here: special singleton values like
// "true", "false", and "null", and pointers to objects allocated on the heap.
// We'll use the sign bit to distinguish singleton values from pointers. If
// it's set, it's a pointer.
//
// v--Pointer or singleton?
// S[NaN      ]1---------------------------------------------------
//
// For singleton values, we just enumerate the different values. We'll use the
// low bits of the mantissa for that, and only need a few:
//
//                                                 3 Type bits--v
// 0[NaN      ]1------------------------------------------------[T]
//
// For pointers, we are left with 51 bits of mantissa to store an address.
// That's more than enough room for a 32-bit address. Even 64-bit machines
// only actually use 48 bits for addresses, so we've got plenty. We just stuff
// the address right into the mantissa.
//
// Ta-da, double precision numbers, pointers, and a bunch of singleton values,
// all stuffed into a single 64-bit sequence. Even better, we don't have to
// do any masking or work to extract number values: they are unmodified. This
// means math on numbers is fast.
use std::mem::transmute;
use std::fmt::{Debug, Display};

#[derive(Copy, Clone)]
pub struct Value {
    raw: u64,
}

#[derive(Debug)]
enum Variant {
    Float(f64),
    True,
    False,
    Nil,
    Ptr(u64),
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

fn decode(u: u64) -> Variant {
    if u & QNAN != QNAN {
        return Variant::Float(unsafe{ transmute(u) });
    }
    // sign bit indicates pointer
    if (u & SIGN) == 1 {
        let man = u & MAN; // only keep lower 51 bits
        return Variant::Ptr(man as u64)
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
            Variant::Ptr(p) => write!(f, "{:0x}", p),
        }
    }
}
