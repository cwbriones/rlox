use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Void,
}

impl Eq for Value {
}

impl Into<bool> for Value {
    fn into(self: Self) -> bool {
        match self {
            Value::Nil => false,
            Value::False => false,
            _ => true,
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Value {
        if b {
            Value::True
        } else {
            Value::False
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(ref s) => write!(f, "{:?}", s),
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Nil => write!(f, "nil"),
            Value::Void => Ok(()),
        }
    }
}
