use std::collections::HashMap;

use chunk::Chunk;
use gc::Gc;
use gc::object::Object;
use gc::value::Value;
use gc::value::Variant;

pub struct VM {
    stack: Vec<Value>,
    ip: usize,
    chunk: Chunk,
    gc: Gc,
    globals: HashMap<String, Value>,
    locals: [Value; 256],
}

impl VM {
    pub fn new(chunk: Chunk, gc: Gc) -> Self {
        VM {
            stack: Vec::new(),
            ip: 0,
            chunk,
            gc,
            globals: HashMap::new(),
            locals: [Value::nil(); 256],
        }
    }

    pub fn run(mut self) {
        let l = self.chunk.len();
        while self.ip < l {
            let inst = self.read_byte();
            decode_op!(inst, self);
        }
    }

    fn ret(&mut self) {
        panic!("Cannot return from top-level.");
    }

    fn constant(&mut self, idx: u8) {
        let val = *self.chunk.get_constant(idx).unwrap();
        self.push(val);
    }

    fn print(&mut self) {
        let val = self.pop();
        println!("{}", val);
    }

    fn add(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let c = a.as_float() + b.as_float();
        self.push(Value::float(c));
    }

    fn sub(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let c = a.as_float() - b.as_float();
        self.push(Value::float(c));
    }

    fn mul(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let c = a.as_float() * b.as_float();
        self.push(Value::float(c));
    }

    fn div(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let c = a.as_float() / b.as_float();
        self.push(Value::float(c));
    }

    fn neg(&mut self) {
        let a = self.pop().as_float();
        self.push(Value::float(-a));
    }

    fn not(&mut self) {
        let a = self.pop();
        if a.truthy() {
            self.push(Value::falselit());
        } else {
            self.push(Value::truelit());
        }
    }

    fn eq(&mut self) {
        let b = self.pop();
        let a = self.pop();
        if a == b {
            self.push(Value::truelit());
        } else {
            self.push(Value::falselit());
        }
    }

    fn gt(&mut self) {
        let b = self.pop().as_float();
        let a = self.pop().as_float();
        if a > b {
            self.push(Value::truelit());
        } else {
            self.push(Value::falselit());
        }
    }

    fn lt(&mut self) {
        let b = self.pop().as_float();
        let a = self.pop().as_float();
        if a < b {
            self.push(Value::truelit());
        } else {
            self.push(Value::falselit());
        }
    }

    fn jmp(&mut self) {
        self.ip = self.read_u16() as usize;
    }

    fn jze(&mut self) {
        let ip = self.read_u16();
        if self.peek().falsey() {
            self.ip = ip as usize;
        }
    }

    fn get_global(&mut self) {
        let idx = self.read_byte();
        let val = *self.chunk.get_constant(idx).unwrap();

        if let Variant::Obj(h) = val.decode() {
            match *h {
                Object::String(ref s) => {
                    let val = *self.globals.get(s).expect("undefined global");
                    self.push(val);
                    return;
                },
            }
        }
        panic!("GET_GLOBAL constant was not a string");
    }

    fn set_global(&mut self) {
        let idx = self.read_byte();
        let val = *self.chunk.get_constant(idx).unwrap();

        if let Variant::Obj(h) = val.decode() {
            match *h {
                Object::String(ref s) => {
                    let lhs = self.pop();
                    self.globals.insert(s.clone(), lhs);
                    self.push(lhs);
                    return;
                },
            }
        }
        panic!("SET_GLOBAL constant was not a string");
    }

    fn define_global(&mut self) {
        let idx = self.read_byte();
        let val = *self.chunk.get_constant(idx).unwrap();

        if let Variant::Obj(h) = val.decode() {
            match *h {
                Object::String(ref s) => {
                    let lhs = self.pop();
                    self.globals.insert(s.clone(), lhs);
                    return;
                },
            }
        }
        panic!("DEF_GLOBAL constant was not a string");
    }

    fn get_local(&mut self) {
        let idx = self.read_byte();
        let val = self.locals[idx as usize];
        self.push(val);
    }

    fn set_local(&mut self) {
        let idx = self.read_byte();
        // We peek because we would just push it back after
        // the assignment occurs.
        let val = self.peek();
        self.locals[idx as usize] = val;
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.get(self.ip - 1)
    }

    fn read_u16(&mut self) -> u16 {
        self.ip += 2;
        let lo = self.chunk.get(self.ip - 2) as u16;
        let hi = self.chunk.get(self.ip - 1) as u16;
        lo + (hi << 8)
    }

    // fn reset_stack(&mut self) {
    // }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn peek(&mut self) -> Value {
        self.stack.last()
            .expect("stack to be nonempty")
            .clone()
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        for constant in self.chunk.constants() {
            if let Variant::Obj(ref o) = constant.decode() {
                // Unroot all non-primitive constants.
                unsafe { self.gc.unroot(*o); }
            }
        }
    }
}
