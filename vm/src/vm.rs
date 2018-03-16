use std::collections::HashMap;

use chunk::Chunk;
use compile::Compiler;
use gc::Gc;
use gc::object::Object;
use gc::object::LoxClosure;
use gc::object::ObjectHandle;
use gc::object::LoxUpValue;
use gc::value::Value;
use gc::value::Variant;
use parser::ast::Stmt;

const STACK_SIZE: usize = 4096;

pub struct VM {
    // FIXME: Local variables are not currently rooted properly, we will need
    // to scan the stack to address this at this point.
    gc: Gc,
    globals: HashMap<String, Value>,
    open_upvalues: Vec<(ObjectHandle, usize)>,

    stack: Vec<Value>,
    frames: Vec<CallFrame>,
}

pub struct CallFrame {
    closure: ObjectHandle,
    ip: usize,
    stack_start: usize,
}

impl CallFrame {
    pub fn new(closure: ObjectHandle, stack_start: usize) -> Self {
        if !closure.is_closure() {
            panic!("Callframe must be constructed from a closure");
        }
        CallFrame {
            closure,
            ip: 0,
            stack_start,
        }
    }

    pub fn read_byte(&mut self) -> u8 {
        let ip = self.ip;
        self.ip += 1;
        self.with_chunk(|c| c.read_byte(ip))
    }

    pub fn read_u16(&mut self) -> u16 {
        let ip = self.ip;
        self.ip += 2;
        self.with_chunk(|c| c.read_u16(ip))
    }

    pub fn read_u64(&mut self) -> u64 {
        let ip = self.ip;
        self.ip += 8;
        self.with_chunk(|c| c.read_u64(ip))
    }

    pub fn read_constant_at(&mut self, idx: u8) -> Value {
        self.with_chunk(|c| c.get_constant(idx).unwrap().clone())
    }

    pub fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.with_chunk(|c| c.get_constant(idx).unwrap().clone())
    }

    fn with_closure<F, T>(&mut self, fun: F) -> T
        where
            F: FnOnce(&mut LoxClosure) -> T
    {
        if let Object::LoxClosure(ref mut cl) = *self.closure {
            return fun(cl);
        }
        panic!("should always refer to a closure");
    }

    pub fn with_chunk<F, T>(&self, fun: F) -> T
        where
            F: FnOnce(&Chunk) -> T
    {
        if let Object::LoxClosure(ref closure) = *self.closure {
            if let Object::LoxFunction(ref f) = *closure.function() {
                return fun(f.chunk());
            }
        }
        panic!("should always refer to a closure");
    }
}

macro_rules! binary_op {
    ($self:ident, $op:tt) => {
        let b = $self.pop();
        let a = $self.pop();
        if let (Variant::Float(a), Variant::Float(b)) = (a.decode(), b.decode()) {
            let c = a $op b;
            $self.push(c.into());
            return;
        }
        $self.runtime_error(RuntimeError::ArgumentNotANumber);
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum RuntimeError {
    DivideByZero,
    ArgumentNotANumber,
    ArgumentNotAString,
    BadCall,
}

impl RuntimeError {
    pub fn cause(&self) -> &'static str {
        match *self {
            RuntimeError::DivideByZero => "divide by zero",
            RuntimeError::ArgumentNotANumber => "argument is not a number",
            RuntimeError::ArgumentNotAString => "argument is not a string",
            RuntimeError::BadCall => "value is not callable",
        }
    }
}

impl VM {
    pub fn new(gc: Gc) -> Self {
        VM {
            stack: Vec::with_capacity(STACK_SIZE),
            gc,
            globals: HashMap::new(),
            frames: Vec::with_capacity(256),
            open_upvalues: Vec::with_capacity(16),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) {
        let function = {
            let compiler = Compiler::new(&mut self.gc);
            compiler.compile(stmts)
        };
        let closure = LoxClosure::new(function, Vec::new());
        let value = self.allocate(Object::LoxClosure(closure)).into_value();
        self.push(value);
        self.call(0);
        self.run();
    }

    fn run(&mut self) {
        while !self.frames.is_empty() {
            let inst = self.read_byte();
            decode_op!(inst, self);
        }
    }

    fn constant(&mut self, idx: u8) {
        let val = self.frame_mut().read_constant_at(idx);
        self.push(val);
    }

    fn print(&mut self) {
        let val = self.pop();
        println!("{}", val);
    }

    fn add(&mut self) {
        let b = self.pop();
        let a = self.pop();
        match (a.decode(), b.decode()) {
            (Variant::Float(a), Variant::Float(b)) => {
                self.push((a + b).into());
            }
            (Variant::Obj(a), Variant::Obj(b)) => {
                if let (&Object::String(ref a), &Object::String(ref b)) = (&*a, &*b) {
                    let c = a.clone() + b;
                    // FIXME: Gc Rooting.
                    let val = self.gc.allocate_string(c, || { [].iter().cloned() }).into_value();
                    self.push(val);
                    return;
                }
                self.runtime_error(RuntimeError::ArgumentNotAString);
            }
            _ => self.runtime_error(RuntimeError::ArgumentNotANumber)
        }
    }

    fn sub(&mut self) {
        binary_op!(self, -);
    }

    fn mul(&mut self) {
        binary_op!(self, *);
    }

    fn div(&mut self) {
        let b = self.pop();
        let a = self.pop();
        if let (Variant::Float(a), Variant::Float(b)) = (a.decode(), b.decode()) {
            if b == 0.0 {
                self.runtime_error(RuntimeError::DivideByZero);
            }
            self.push((a/b).into());
            return;
        }
        self.runtime_error(RuntimeError::ArgumentNotANumber);
    }

    fn neg(&mut self) {
        if let Variant::Float(a) = self.pop().decode() {
            self.push((-a).into());
        }
        self.runtime_error(RuntimeError::ArgumentNotANumber);
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
        let a = self.pop();
        let b = self.pop();
        self.push((a == b).into());
    }

    fn gt(&mut self) {
        binary_op!(self, >);
    }

    fn lt(&mut self) {
        binary_op!(self, <);
    }

    fn jmp(&mut self) {
        self.frame_mut().ip = self.read_u16() as usize;
    }

    fn jze(&mut self) {
        let ip = self.read_u16();
        if self.peek().falsey() {
            self.frame_mut().ip = ip as usize;
        }
    }

    fn op_loop(&mut self) {
        self.frame_mut().ip -= self.read_u16() as usize;
    }

    fn get_global(&mut self) {
        let val = self.frame_mut().read_constant();

        if let Variant::Obj(h) = val.decode() {
            if let Object::String(ref s) = *h {
                let val = *self.globals.get(s).expect("undefined global");
                self.push(val);
                return;
            }
        }
        panic!("GET_GLOBAL constant was not a string");
    }

    fn set_global(&mut self) {
        let val = self.frame_mut().read_constant();

        if let Variant::Obj(h) = val.decode() {
            if let Object::String(ref s) = *h {
                let lhs = self.peek();
                self.globals.insert(s.clone(), lhs);
                return;
            }
        }
        panic!("SET_GLOBAL constant was not a string");
    }

    fn get_local(&mut self) {
        let start = self.frame().stack_start;
        let idx = self.read_byte() as usize;
        let val = self.stack[start + idx];
        self.push(val);
    }

    fn set_local(&mut self) {
        // We peek because we would just push it back after
        // the assignment occurs.
        let val = self.peek();
        let start = self.frame().stack_start;
        let idx = self.read_byte() as usize;
        self.stack[start + idx] = val;
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().expect("frames to be nonempty")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("frames to be nonempty")
    }

    fn immediate(&mut self) {
        let raw = self.frame_mut().read_u64();
        let val = unsafe { Value::from_raw(raw) };
        self.push(val);
    }

    fn imm_nil(&mut self) {
        self.push(Value::nil());
    }

    fn imm_true(&mut self) {
        self.push(Value::truelit());
    }

    fn imm_false(&mut self) {
        self.push(Value::falselit());
    }

    fn call(&mut self, arity: u8) {
        let last = self.stack.len();
        let stack_start = last - (arity + 1) as usize;
        let callee = self.stack[stack_start];

        // ensure callee is a closure
        match callee.decode() {
            Variant::Obj(o) if o.is_closure() => {
                let frame = CallFrame::new(o, stack_start);
                self.frames.push(frame);
            }
            _ => self.runtime_error(RuntimeError::BadCall)
        }
    }

    fn ret(&mut self) {
        self.close_upvalues();
        if let Some(frame) = self.frames.pop() {
            let val = self.pop(); // return value
            // Remove all arguments off the stack
            self.stack.truncate(frame.stack_start);
            self.push(val);
            return;
        }
        panic!("Cannot return from top-level.");
    }

    fn close_upvalue(&mut self) {
        let last = self.stack.len() - 1;
        self.capture_upvalue(last);
        self.pop();
    }

    fn get_upvalue(&mut self) {
        let idx = self.frame_mut().read_byte();
        let val = self.frame_mut().with_closure(|cl| cl.get_in(idx as usize));
        self.push(val);
    }

    fn set_upvalue(&mut self) {
        let val = self.peek();
        let idx = self.frame_mut().read_byte();
        self.frame_mut().with_closure(|cl| cl.set_in(idx as usize, val));
    }

    fn closure(&mut self) {
        let value = self.frame_mut().read_constant();
        if let Variant::Obj(o) = value.decode() {
            let f = o.as_function().expect("closure argument to be function");
            let count = f.upvalue_count();
            let mut upvalues = Vec::new();
            for i in 0..count {
                let is_local = self.read_byte() > 0;
                let idx = self.read_byte() as usize;
                if is_local {
                    let upvalue = self.capture_upvalue(idx);
                    upvalues.push(upvalue);
                } else {
                    let upvalue = self.frame_mut().with_closure(|c| c.get(idx));
                    upvalues.push(upvalue);
                }
            }
            let mut closure = LoxClosure::new(o.clone(), upvalues);
            let val = self.allocate(Object::LoxClosure(closure)).into_value();
            self.push(val);
            return;
        }
        panic!("closure argument should be a function");
    }

    fn capture_upvalue(&mut self, idx: usize) -> ObjectHandle {
        let start = self.frame().stack_start;
        let local = &self.stack[start + idx] as *const _ as *mut _;
        let matching = self.open_upvalues.iter().rev().find(|&&(o, _)| {
            if let Object::LoxUpValue(ref o) = *o {
                o.is(local)
            } else {
                false
            }
        })
        .cloned()
        .map(|t| t.0);

        matching.unwrap_or_else(|| {
            let upvalue = LoxUpValue::new(local);
            let val = self.allocate(Object::LoxUpValue(upvalue));
            self.open_upvalues.push((val, self.frames.len()));
            val
        })
    }

    fn close_upvalues(&mut self) {
        let depth = self.frames.len();
        let mut open_upvalues = Vec::with_capacity(self.open_upvalues.len());
        for &mut (ref mut value, d) in &mut self.open_upvalues {
            if d == depth {
                if let Object::LoxUpValue(ref mut up) = **value {
                    unsafe { up.close(); }
                }
            } else {
                open_upvalues.push((value.clone(), d));
            }
        }
        self.open_upvalues = open_upvalues;
    }

    fn read_byte(&mut self) -> u8 {
        self.frame_mut().read_byte()
    }

    fn read_u16(&mut self) -> u16 {
        self.frame_mut().read_u16()
    }

    fn push(&mut self, value: Value) {
        // It is important the stack never allocate so that open upvalues don't hold
        // dangling local references.
        if self.stack.len() == STACK_SIZE {
            panic!("stack overflow.");
        }
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack to be nonempty")
    }

    fn peek(&mut self) -> Value {
        self.stack.last()
            .expect("stack to be nonempty")
            .clone()
    }

    fn runtime_error(&self, err: RuntimeError) {
        eprintln!("[ERROR]: {}", err.cause());
        for frame in self.frames.iter().rev() {
            let ip = frame.ip;
            frame.with_chunk(|chunk| {
                let name = chunk.name();
                let line = chunk.line(ip);
                eprintln!("         at [line {}] in {}", line, name);
            });
        }
        ::std::process::exit(1);
    }

    /// 
    /// GC wrapper that handles rooting.
    ///
    fn allocate(&mut self, object: Object) -> ObjectHandle {
        // Root everything on the stack as well as all closures in the current
        // set of callframes.
        let frame_iter = self.frames.iter().map(|f| f.closure.into_value());
        let upvalue_iter = self.open_upvalues.iter().map(|&(o, _)| o.into_value());
        let roots = self.stack.iter().cloned().chain(frame_iter).chain(upvalue_iter);

        self.gc.allocate(object, || roots)
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        // TODO: Unroot all non-primitive constants.
    }
}
