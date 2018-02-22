use chunk::{Chunk, Op};
use value::Value;
use std::env;

#[macro_use]
mod chunk;

mod value;
mod debug;

fn main() {
    // print 1 + (2 * 3)
    let mut chunk = Chunk::new("test chunk".into());

    let idx = chunk.add_constant(Value::float(2.0));
    chunk.write(Op::Constant(idx), 1);

    let idx = chunk.add_constant(Value::float(3.0));
    chunk.write(Op::Constant(idx), 1);

    chunk.write(Op::Multiply, 1);

    let idx = chunk.add_constant(Value::float(1.0));
    chunk.write(Op::Constant(idx), 1);

    chunk.write(Op::Add, 1);
    chunk.write(Op::Print, 1);

    if env::var("DEBUG").is_ok() {
        let disassembler = debug::Disassembler::new(&chunk);
        disassembler.disassemble();
    }
 
    VM::new(chunk).run();
}

struct VM { 
    stack: Vec<Value>,
    ip: usize,
    chunk: Chunk,
}

impl VM {
    fn new(chunk: Chunk) -> Self {
        VM {
            stack: Vec::new(),
            ip: 0,
            chunk,
        }
    }

    fn run(mut self) {
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
        let a = self.pop();
        let b = self.pop();
        let c = a.as_float() + b.as_float();
        self.push(Value::float(c));
    }

    fn mul(&mut self) {
        let a = self.pop();
        let b = self.pop();
        let c = a.as_float() * b.as_float();
        self.push(Value::float(c));
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
