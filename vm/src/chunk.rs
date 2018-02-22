use value::Value;

pub struct Chunk {
    code: Vec<u8>,
    name: String,
    constants: Vec<Value>,
    lines: Vec<LineInfo>,
}

#[derive(Debug, Copy, Clone)]
struct LineInfo {
    pub start: usize,
    pub line: usize,
}

impl Chunk {
    pub fn new(name: String) -> Self {
        Chunk {
            code: Vec::new(),
            name,
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, op: Op, line: usize) {
        self.add_line(line);
        op.write(&mut self.code);
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.code.push(byte);
    }

    pub fn write_byte_at(&mut self, idx: usize, byte: u8) {
        self.code[idx] = byte;
    }

    fn add_line(&mut self, line: usize) {
        match self.lines.last().cloned() {
            Some(last) if last.line == line => return,
            _ => (),
        }
        self.lines.push(LineInfo {
            start: self.code.len(),
            line: line,
        });
    }

    pub fn line(&self, offset: usize) -> usize {
        let idx =
            self.lines
                .binary_search_by_key(&offset, |line_info| line_info.start)
                .map_err(|idx| idx - 1) // on failure we want the earlier line
                .unwrap_or_else(|idx| idx);
        self.lines[idx].line
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn add_constant(&mut self, constant: Value) -> u8 {
        if self.constants.len() == 256 {
            panic!("A chunk cannot have more than 256 constants");
        }
        self.constants.push(constant);
        self.constants.len() as u8 - 1
    }

    pub fn get(&self, ip: usize) -> u8 {
        self.code[ip]
    }

    pub fn get_constant(&self, idx: u8) -> Option<&Value> {
        self.constants.get(idx as usize)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }
}

impl AsRef<[u8]> for Chunk {
    fn as_ref(&self) -> &[u8] {
        &self.code[..]
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Op {
    Return,
    Constant(u8),
    // Nil,
    // True,
    // False,
    // Pop,
    // GetLocal,
    // SetLocal,
    // GetGlobal,
    // DefineGlobal,
    // SetGlobal,
    // GetUpValue,
    // SetUpValue,
    // GetProperty,
    // SetProperty,
    // GetSuper,
    Equal,
    GreaterThan,
    LessThan,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    // Loop,
    // Call_0,
    // Call_1,
    // Call_2,
    // Call_3,
    // Call_4,
    // Call_5,
    // Call_6,
    // Call_7,
    // Call_8,
    // Invoke_0,
    // Invoke_1,
    // Invoke_2,
    // Invoke_3,
    // Invoke_4,
    // Invoke_5,
    // Invoke_6,
    // Invoke_7,
    // Invoke_8,
    // Super_0,
    // Super_1,
    // Super_2,
    // Super_3,
    // Super_4,
    // Super_5,
    // Super_6,
    // Super_7,
    // Super_8,
    // Closure,
    // CloseUpValue,
    // Return,
    // Class,
    // SubClass,
    // Method,
}

impl Op {
    fn write(&self, buf: &mut Vec<u8>) {
        match *self {
            Op::Return => { buf.push(0x00); }
            Op::Constant(idx) => { buf.push(0x01); buf.push(idx); }
            Op::Print => { buf.push(0x02); }
            Op::Add => { buf.push(0x03); }
            Op::Subtract => { buf.push(0x04); }
            Op::Multiply => { buf.push(0x05); }
            Op::Divide => { buf.push(0x06); }
            Op::Not => { buf.push(0x07); }
            Op::Negate => { buf.push(0x08); }
            Op::Equal => { buf.push(0x09); }
            Op::GreaterThan => { buf.push(0x0a); }
            Op::LessThan => { buf.push(0x0b); }
            Op::Jump => { buf.push(0x0c); }
            Op::JumpIfFalse => { buf.push(0x0d); }
        }
    }
}

macro_rules! decode_op {
    ($op:expr, $this:ident) => {
        match $op {
            0x00 => { $this.ret(); }
            0x01 => { let idx = $this.read_byte(); $this.constant(idx); }
            0x02 => { $this.print(); }
            0x03 => { $this.add(); }
            0x04 => { $this.sub(); }
            0x05 => { $this.mul(); }
            0x06 => { $this.div() }
            0x07 => { $this.not() }
            0x08 => { $this.neg() }
            0x09 => { $this.eq() }
            0x0a => { $this.gt() }
            0x0b => { $this.lt() }
            0x0c => { $this.jmp() },
            0x0d => { $this.jze() },
            _ => { 
                panic!("Unknown op {}", $op);
            }
        }
    }
}
