use std::collections::HashMap;

use broom::Heap;
use broom::Handle;

use chunk::Chunk;
use compile::Compiler;

use gc::object::Object;
use gc::object::LoxClass;
use gc::object::LoxClosure;
use gc::object::LoxUpValue;
use gc::object::LoxInstance;
use gc::value::Value;
use gc::value::Variant;
use parser::ast::Stmt;
use native;

const STACK_SIZE: usize = 4096;

pub struct VM {
    // FIXME: Local variables are not currently rooted properly, we will need
    // to scan the stack to address this at this point.
    heap: Heap<Object>,
    globals: HashMap<String, Value>,
    open_upvalues: Vec<LoxUpValue>,

    stack: Vec<Value>,
    frames: Vec<CallFrame>,
}

pub struct CallFrame {
    closure: LoxClosure,
    ip: usize,
    stack_start: usize,
}

impl CallFrame {
    pub fn new(closure: LoxClosure, stack_start: usize) -> Self {
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
        self.with_chunk(|c| *c.get_constant(idx).expect("invalid constant index"))
    }

    pub fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.read_constant_at(idx)
    }

    pub fn with_chunk<F, T>(&self, fun: F) -> T
        where
            F: FnOnce(&Chunk) -> T
    {
        fun(self.closure.chunk())
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
        $self.runtime_error(RuntimeError::BadArgument("Operands must be numbers"));
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    DivideByZero,
    BadArgument(&'static str),
    BadCall,
    ArityMismatch(u8, u8),
    UndefinedVariable(String),
    UndefinedProperty(String),
}

impl ::std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            RuntimeError::DivideByZero => write!(f, "divide by zero"),
            RuntimeError::BadArgument(msg) => write!(f, "{}", msg),
            RuntimeError::BadCall => write!(f, "Can only call functions and classes"),
            RuntimeError::ArityMismatch(expected, got) => write!(f, "Expected {} arguments but got {}", expected, got),
            RuntimeError::UndefinedVariable(ref var) => write!(f, "Undefined variable '{}'", var),
            RuntimeError::UndefinedProperty(ref prop) => write!(f, "Undefined property '{}'", prop),
        }
    }
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: Vec::with_capacity(STACK_SIZE),
            heap: Heap::default(),
            globals: HashMap::new(),
            frames: Vec::with_capacity(256),
            open_upvalues: Vec::with_capacity(16),
        }
    }

    fn define_natives(&mut self) {
        let clock = self.allocate(Object::native_fn("clock", 0, native::clock));
        self.globals.insert("clock".into(), clock.into());

        let print = self.allocate(Object::native_fn("printf", 1, native::native_print));
        self.globals.insert("printf".into(), print.into());
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) {
        self.define_natives();
        let function = {
            let compiler = Compiler::new(&mut self.heap);
            compiler.compile(stmts)
        };
        let closure = LoxClosure::new(function, Vec::new());
        let value = self.allocate(Object::LoxClosure(closure)).into();
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
        println!("{}", val.decode().deref(&self.heap));
    }

    fn add(&mut self) {
        let b = self.pop();
        let a = self.pop();
        match (a.decode(), b.decode()) {
            (Variant::Float(a), Variant::Float(b)) => { return self.push((a + b).into()); }
            (Variant::Obj(a), Variant::Obj(b)) => {
                if let (Some(&Object::String(ref a)), Some(&Object::String(ref b))) = (self.heap.get(a), self.heap.get(b)) {
                    let c = a.clone() + b;
                    // FIXME: Gc Rooting.
                    let handle = self.heap.insert(Object::String(c)).into_handle();
                    self.push(Value::object(handle));
                    return;
                }
            }
            _ => {}
        }
         self.runtime_error(RuntimeError::BadArgument("Operands must be two numbers or two strings"));
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
        self.runtime_error(RuntimeError::BadArgument("Operands must be numbers"))
    }

    fn neg(&mut self) {
        if let Variant::Float(a) = self.pop().decode() {
            self.push((-a).into());
        }
        self.runtime_error(RuntimeError::BadArgument("Operand must be a number"));
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
            if let Some(&Object::String(ref s)) = self.heap.get(h) {
                if let Some(val) = self.globals.get(s).cloned() {
                    self.push(val);
                    return;
                } else {
                    self.runtime_error(RuntimeError::UndefinedVariable(s.clone()));
                }
            }
        }
        panic!("GET_GLOBAL constant was not a string");
    }

    fn define_global(&mut self) {
        let val = self.frame_mut().read_constant();
        let obj = val.decode().deref(&self.heap);
        if let Variant::Obj(Object::String(s)) = obj {
            let lhs = self.stack.pop().unwrap();
            self.globals.insert(s.clone(), lhs);
            return;
        }
        panic!("DEFINE_GLOBAL constant was not a string");
    }

    fn set_global(&mut self) {
        let val = self.frame_mut().read_constant();
        let obj = val.decode().deref(&self.heap);
        if let Variant::Obj(Object::String(s)) = obj {
            let lhs = self.stack.last().cloned().unwrap();
            self.globals.insert(s.clone(), lhs);
            return;
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
        let frame_start = last - (arity + 1) as usize;
        let callee = self.stack[frame_start].decode();

        // ensure callee is a closure
        if let Variant::Obj(handle) = callee {
            match self.heap.get(handle) {
                Some(Object::LoxClosure(ref closure)) => {
                    if closure.arity() != arity {
                        self.runtime_error(RuntimeError::ArityMismatch(closure.arity(), arity));
                    }
                    let frame = CallFrame::new(closure.clone(), frame_start);
                    self.frames.push(frame);
                    return;
                },
                Some(Object::NativeFunction(ref native)) => {
                    if native.arity != arity {
                        self.runtime_error(RuntimeError::ArityMismatch(native.arity, arity));
                    }
                    let val = {
                        (native.function)(&self.heap, &self.stack[frame_start..])
                    };
                    self.stack.pop(); // function
                    self.stack.push(val);
                    return;
                },
                Some(Object::LoxClass(_)) => {
                    let val = self.allocate(Object::LoxInstance(LoxInstance::new(handle))).into();
                    self.stack.push(val);
                    return;
                },
                _ => {},
            }
        }
        self.runtime_error(RuntimeError::BadCall)
    }

    fn ret(&mut self) {
        if let Some(frame) = self.frames.pop() {
            let retval = self.pop(); // return value
            if frame.stack_start < self.stack.len() {
                self.close_upvalues(frame.stack_start);
            }
            // Remove all arguments off the stack
            self.stack.truncate(frame.stack_start);
            self.push(retval);
            return;
        }
        panic!("Cannot return from top-level.");
    }

    fn close_upvalue(&mut self) {
        let end = self.stack.len() - 1;
        self.close_upvalues(end);
        self.pop();
    }

    fn get_upvalue(&mut self) {
        let idx = self.frame_mut().read_byte();
        let val = self.current_closure()
            .get(idx as usize)
            .get()
            .unwrap_or_else(|i| self.stack[i]);
        self.push(val);
    }

    fn set_upvalue(&mut self) {
        let val = self.peek();
        let idx = self.frame_mut().read_byte();
        let closure = self.current_closure();
        let res = closure.get(idx as usize).set(val);
        if let Err(i) = res {
            self.stack[i] = val;
        }
    }

    fn closure(&mut self) {
        let value = self.frame_mut().read_constant();
        let obj = value.decode().deref(&self.heap);
        let function = if let Variant::Obj(Object::LoxFunction(f)) = obj {
            f.clone()
        } else {
            panic!("closure argument should be a function");
        };
        let mut upvalues = Vec::new();
        for _ in 0..function.upvalue_count() {
            let is_local = self.read_byte() > 0;
            let idx = self.read_byte() as usize;
            let upvalue = if is_local {
                // This upvalue is local and so this closure may need to capture it
                // to ensure that the reference will continue to be valid.
                self.capture_upvalue(idx)
            } else {
                // This value has been previously captured across some enclosing scope.
                self.current_closure().get(idx)
            };
            upvalues.push(upvalue);
        }
        let closure = LoxClosure::new(function, upvalues);
        let val = self.allocate(Object::LoxClosure(closure)).into();
        self.push(val);
    }

    fn class(&mut self, idx: u8) {
        let obj = self.frame_mut().read_constant_at(idx);
        let name = if let Variant::Obj(Object::String(name)) = obj.decode().deref(&self.heap) {
            name.clone()
        } else {
            panic!("class constant was not a string");
        };
        let val = self.allocate(Object::LoxClass(LoxClass::new(name))).into();
        self.push(val);
    }

    fn get_property(&mut self) {
        let idx = self.read_byte();
        let name_val = self.frame_mut().read_constant_at(idx);
        let instance_val = self.pop();

        let name = name_val.decode().deref(&self.heap);
        let instance = instance_val.decode().deref(&self.heap);

        if let Variant::Obj(Object::String(name)) = name {
            if let Variant::Obj(Object::LoxInstance(inst)) = instance {
                if let Some(prop) = inst.get_property(&name) {
                    self.push(prop);
                } else {
                    self.runtime_error(RuntimeError::UndefinedProperty(name.clone()));
                }
                return
            }
            panic!("expression was not an instance")
        }
        panic!("property name was not a string")
    }

    fn set_property(&mut self) {
        let idx = self.read_byte();
        let name_val = self.frame_mut().read_constant_at(idx);

        // Current stack looks like:
        //
        // [ ... / ... / <instance> / <value> ]
        let val = self.pop();
        let instance_val = self.pop();

        let name = name_val.decode().cloned(&self.heap);
        let instance = instance_val.decode().deref_mut(&mut self.heap);

        if let Variant::Obj(Object::String(name)) = name {
            if let Variant::Obj(Object::LoxInstance(inst)) = instance {
                inst.set_property(&name, val);
                self.push(val);
                return
            }
            panic!("expression was not an instance")
        }
        panic!("property name was not a string")
    }

    fn current_closure(&mut self) -> &mut LoxClosure {
        &mut self.frame_mut().closure
    }

    fn capture_upvalue(&mut self, idx: usize) -> LoxUpValue {
        let offset = self.frame().stack_start + idx;
        self.open_upvalues.iter().rev().find(|&up| {
            up.as_local().map(|i| i == offset).unwrap_or(false)
        })
        .cloned()
        .unwrap_or_else(|| {
            let up = LoxUpValue::new(offset);
            self.open_upvalues.push(up.clone());
            up
        })
    }

    fn close_upvalues(&mut self, stack_end: usize) {
        // close all upvalues that are about to be invalidated and remove them from
        // the list of open upvalues.
        let mut open_upvalues = Vec::new();
        ::std::mem::swap(&mut self.open_upvalues, &mut open_upvalues);
        for mut up in open_upvalues {
            // will this blow up?
            if up.get().map_err(|i| i >= stack_end).is_err() {
                up.close(|i| self.stack[i]);
                self.open_upvalues.push(up);
            }
        }
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
        *self.stack.last().expect("stack to be nonempty")
    }

    fn runtime_error(&self, err: RuntimeError) {
        eprintln!("[error]: {}.", err);
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
    fn allocate(&mut self, object: Object) -> Handle<Object> {
        self.heap.insert(object).into_handle()
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        // TODO: Unroot all non-primitive constants.
    }
}
