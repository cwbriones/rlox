use value::Value;
use chunk::Chunk;

pub struct VM {
    stack: Vec<Value>,
    ip: usize,
    chunk: Chunk,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        VM {
            stack: Vec::new(),
            ip: 0,
            chunk,
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

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        self.chunk.get(self.ip - 1)
    }

    // fn read_u16(&mut self) -> u16 {
    //     self.ip += 2;
    //     (self.chunk.get(self.ip - 1) as u16) << 8 + self.chunk.get(self.ip - 2) as u16
    // }
    //
    // fn reset_stack(&mut self) {
    // }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
}
