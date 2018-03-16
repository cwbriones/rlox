use chunk::Chunk;
use gc::value::Value;

pub struct Disassembler<'c> {
    offset: usize,
    line: usize,
    chunk: &'c Chunk,
}

impl<'c> Disassembler<'c> {
    pub fn new(chunk: &'c Chunk) -> Self {
        Disassembler {
            offset: 0,
            line: 0,
            chunk,
        }
    }

    pub fn disassemble(mut self) {
        let bytes = self.chunk.as_ref();
        println!("== {} ==", self.chunk.name());
        for (i, c) in self.chunk.constants().enumerate() {
            println!("{:3} {}", i, c);
        }
        println!("=========");
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
        println!("OP_CONSTANT\t{}\t{:?}", idx, val);
    }

    fn ret(&self) { println!("OP_RETURN"); }
    fn print(&self) { println!("OP_PRINT"); }
    fn add(&self) { println!("OP_ADD"); }
    fn sub(&self) { println!("OP_SUB"); }
    fn mul(&self) { println!("OP_MUL"); }
    fn div(&self) { println!("OP_DIV"); }
    fn neg(&self) { println!("OP_DIV"); }
    fn not(&self) { println!("OP_DIV"); }
    fn eq(&self) { println!("OP_EQ"); }
    fn gt(&self) { println!("OP_GT"); }
    fn lt(&self) { println!("OP_LT"); }
    fn pop(&self) { println!("OP_POP"); }

    fn jmp(&mut self) {
        let offset = self.offset - 1;
        let ip = self.read_u16();
        println!("OP_JMP\t{} -> {}", offset, ip);
    }

    fn jze(&mut self) {
        let offset = self.offset - 1;
        let ip = self.read_u16();
        println!("OP_JZE\t{} -> {}", offset, ip);
    }

    fn op_loop(&mut self) {
        let sub = self.read_u16() as usize;
        println!("OP_LOOP\t{} -> {}", self.offset, self.offset - sub);
    }

    fn get_global(&mut self) {
        let val = self.read_constant();
        println!("OP_GET_GLOBAL\t{:?}", val);
    }

    fn set_global(&mut self) {
        let val = self.read_constant();
        println!("OP_SET_GLOBAL\t{:?}", val);
    }

    fn define_global(&mut self) {
        let val = self.read_constant();
        println!("OP_DEFINE_GLOBAL\t{:?}", val);
    }

    fn get_local(&mut self) {
        let val = self.read_byte();
        println!("OP_GET_LOCAL\t{}", val);
    }

    fn set_local(&mut self) {
        let val = self.read_byte();
        println!("OP_SET_LOCAL\t{}", val);
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
        println!("OP_FLOAT\t{}", val);
    }

    fn imm_nil(&self) {
        println!("OP_NIL");
    }

    fn imm_true(&self) {
        println!("OP_TRUE");
    }

    fn imm_false(&self) {
        println!("OP_FALSE");
    }

    fn call(&self, arity: u8) {
        println!("OP_CALL_{}", arity);
    }

    fn close_upvalue(&self) {
        println!("OP_CLOSE_UPVALUE");
    }

    fn get_upvalue(&mut self) {
        let index = self.read_byte();
        println!("OP_GET_UPVALUE\t{}", index);
    }

    fn set_upvalue(&mut self) {
        let index = self.read_byte();
        println!("OP_SET_UPVALE\t{}", index);
    }

    fn closure(&mut self) {
        let value = self.read_constant();
        let count = value.as_object()
            .and_then(|o| o.as_function().map(|f| f.upvalue_count()))
            .expect("closure argument to be function");
        print!("OP_CLOSURE\t{} ", value);
        for _ in 0..count {
            let is_local = self.read_byte() > 0;
            let index = self.read_byte();
            if is_local {
                print!("L{}", index);
            } else {
                print!("U{}", index);
            }
        }
        println!();
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
