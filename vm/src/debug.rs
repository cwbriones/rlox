use chunk::Chunk;
use gc::object::Object;
use gc::value::Value;

use broom::Heap;

pub struct Disassembler<'c> {
    offset: usize,
    line: usize,
    chunk: &'c Chunk,
    heap: &'c Heap<Object>,
}

impl<'c> Disassembler<'c> {
    pub fn new(chunk: &'c Chunk, heap: &'c Heap<Object>) -> Self {
        Disassembler {
            offset: 0,
            line: 0,
            chunk,
            heap,
        }
    }

    pub fn disassemble(mut self) {
        let bytes = self.chunk.as_ref();
        eprintln!("== {} ==", self.chunk.name());
        while self.offset < bytes.len() {
            self.disassemble_instruction();
        }
    }

    fn disassemble_instruction(&mut self) {
        print!("{:04} ", self.offset);
        let line = self.chunk.line(self.offset);
        if self.line == line {
            print!("   | ");
        } else {
            self.line = line;
            print!("{:4} ", line);
        }
        let inst = self.read_byte();
        decode_op!(inst, self);
    }

    fn constant(&mut self, idx: u8) {
        let val = self.chunk.get_constant(idx).expect("invalid constant segment index");
        eprintln!("OP_CONSTANT\t{}\t{:?}", idx, val);
    }

    fn ret(&self) { eprintln!("OP_RETURN"); }
    fn print(&self) { eprintln!("OP_PRINT"); }
    fn add(&self) { eprintln!("OP_ADD"); }
    fn sub(&self) { eprintln!("OP_SUB"); }
    fn mul(&self) { eprintln!("OP_MUL"); }
    fn div(&self) { eprintln!("OP_DIV"); }
    fn neg(&self) { eprintln!("OP_NEG"); }
    fn not(&self) { eprintln!("OP_NOT"); }
    fn eq(&self) { eprintln!("OP_EQ"); }
    fn gt(&self) { eprintln!("OP_GT"); }
    fn lt(&self) { eprintln!("OP_LT"); }
    fn pop(&self) { eprintln!("OP_POP"); }

    fn jmp(&mut self) {
        let offset = self.offset - 1;
        let ip = self.read_u16();
        eprintln!("OP_JUMP\t{} -> {}", offset, ip);
    }

    fn jze(&mut self) {
        let offset = self.offset - 1;
        let ip = self.read_u16();
        eprintln!("OP_JUMP_IF_FALSE\t{} -> {}", offset, ip);
    }

    fn op_loop(&mut self) {
        let sub = self.read_u16() as usize;
        eprintln!("OP_LOOP\t{} -> {}", self.offset, self.offset - sub);
    }

    fn get_global(&mut self) {
        let val = self.read_constant();
        eprintln!("OP_GET_GLOBAL\t{}", val.with_heap(self.heap));
    }

    fn set_global(&mut self) {
        let val = self.read_constant();
        eprintln!("OP_SET_GLOBAL\t{}", val.with_heap(self.heap));
    }

    fn define_global(&mut self) {
        let val = self.read_constant();
        eprintln!("OP_DEFINE_GLOBAL\t{}", val.with_heap(self.heap));
    }

    fn get_local(&mut self) {
        let val = self.read_byte();
        eprintln!("OP_GET_LOCAL\t{}", val);
    }

    fn set_local(&mut self) {
        let val = self.read_byte();
        eprintln!("OP_SET_LOCAL\t{}", val);
    }

    fn immediate(&mut self) {
        self.offset += 8;
        let b1 = self.chunk.get(self.offset - 8) as u64;
        let b2 = self.chunk.get(self.offset - 7) as u64;
        let b3 = self.chunk.get(self.offset - 6) as u64;
        let b4 = self.chunk.get(self.offset - 5) as u64;
        let b5 = self.chunk.get(self.offset - 4) as u64;
        let b6 = self.chunk.get(self.offset - 3) as u64;
        let b7 = self.chunk.get(self.offset - 2) as u64;
        let b8 = self.chunk.get(self.offset - 1) as u64;
        let raw = b1 +
            (b2 << 8) +
            (b3 << 16) +
            (b4 << 24) +
            (b5 << 32) +
            (b6 << 40) +
            (b7 << 48) +
            (b8 << 56);
        let val = unsafe { Value::from_raw(raw) };
        eprintln!("OP_FLOAT\t{}", val.with_heap(self.heap));
    }

    fn imm_nil(&self) {
        eprintln!("OP_NIL");
    }

    fn imm_true(&self) {
        eprintln!("OP_TRUE");
    }

    fn imm_false(&self) {
        eprintln!("OP_FALSE");
    }

    fn call(&self, arity: u8) {
        eprintln!("OP_CALL_{}", arity);
    }

    fn invoke(&mut self, arity: u8) {
        let idx = self.read_byte();
        let val = self.chunk.get_constant(idx).expect("invalid constant segment index");
        eprintln!("OP_INVOKE_{} {}", arity, val.with_heap(&self.heap));
    }

    fn close_upvalue(&self) {
        eprintln!("OP_CLOSE_UPVALUE");
    }

    fn get_upvalue(&mut self) {
        let index = self.read_byte();
        eprintln!("OP_GET_UPVALUE\t{}", index);
    }

    fn set_upvalue(&mut self) {
        let index = self.read_byte();
        eprintln!("OP_SET_UPVALE\t{}", index);
    }

    fn closure(&mut self) {
        let val = self.read_constant();
        let count = val
            .as_object()
            .and_then(|o| self.heap.get(o))
            .and_then(|o| o.as_function())
            .expect("closure argument to be a function")
            .upvalue_count();
        print!("OP_CLOSURE\t{} ", val.with_heap(self.heap));
        for _ in 0..count {
            let is_local = self.read_byte() > 0;
            let index = self.read_byte();
            if is_local {
                print!("L{}", index);
            } else {
                print!("U{}", index);
            }
        }
        eprintln!();
    }

    fn class(&mut self, idx: u8) {
        let val = self.chunk.get_constant(idx).expect("invalid constant segment index");
        let methods = self.read_byte();
        eprintln!("OP_CLASS\t{}\t{}\t({} method(s))", idx, val.with_heap(&self.heap), methods);
    }

    fn get_property(&mut self) {
        let idx = self.read_byte();
        let val = self.chunk.get_constant(idx).expect("invalid constant segment index");
        eprintln!("GET_PROPERTY\t{}\t{}", idx, val.with_heap(&self.heap));
    }

    fn set_property(&mut self) {
        let idx = self.read_byte();
        let val = self.chunk.get_constant(idx).expect("invalid constant segment index");
        eprintln!("SET_PROPERTY\t{}\t{}", idx, val.with_heap(&self.heap));
    }

    fn read_byte(&mut self) -> u8 {
        self.offset += 1;
        self.chunk.as_ref()[self.offset - 1]
    }

    fn read_u16(&mut self) -> u16 {
        self.offset += 2;
        let lo = self.chunk.get(self.offset - 2) as u16;
        let hi = self.chunk.get(self.offset - 1) as u16;
        lo + (hi << 8)
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        *self.chunk.get_constant(idx).expect("invalid constant segment index")
    }
}
