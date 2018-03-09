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
        println!("CONSTANTS");
        for (i, c) in self.chunk.constants().enumerate() {
            println!("{} {}", i, c);
        }
        println!("=========");
        println!("IP   LINE");
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
            print!("{:04} ", line);
        }
        let inst = self.read_byte();
        decode_op!(inst, self);
    }

    fn constant(&mut self, idx: u8) {
        let val = self.chunk.get_constant(idx).expect("invalid constant segment index");
        println!("OP_CONSTANT {:?}", val);
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
        let ip = self.read_u16();
        println!("OP_JMP {}", ip);
    }

    fn jze(&mut self) {
        let ip = self.read_u16();
        println!("OP_JZE {}", ip);
    }

    fn get_global(&mut self) {
        let val = self.read_constant();
        println!("GET_GLOBAL {:?}", val);
    }

    fn set_global(&mut self) {
        let val = self.read_constant();
        println!("SET_GLOBAL {:?}", val);
    }

    fn define_global(&self) {
        println!("DEFINE_GLOBAL");
    }

    fn get_local(&mut self) {
        let val = self.read_byte();
        println!("GET_LOCAL({})", val);
    }

    fn set_local(&mut self) {
        let val = self.read_byte();
        println!("SET_LOCAL({})", val);
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
