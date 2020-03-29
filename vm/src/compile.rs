use chunk::{Chunk, Op};

use broom::Heap;

use gc::value::Value;
use gc::object::{Object, LoxFunction};

use parser::ast::*;

pub struct Compiler<'g> {
    heap: &'g mut Heap<Object>,
    states: Vec<CompileState>,
}

#[derive(Debug)]
struct Local {
    pub name: String,
    pub depth: usize,
    pub captured: bool,
}

#[derive(Debug, Clone)]
struct UpValue {
    pub index: u8,
    pub is_local: bool,
}

struct CompileState {
    line: usize,
    locals: Vec<Local>,
    upvalues: Vec<UpValue>,
    function: LoxFunction,
    scope_depth: usize,
    breaks: Vec<usize>,
}

impl CompileState {
    fn new(function: LoxFunction, scope_depth: usize) -> Self {
        // Reserve the first local
        let locals = vec![Local { name: "".into(), depth: 0, captured: false}];
        let upvalues = Vec::new();
        CompileState {
            line: 1,
            locals,
            upvalues,
            function,
            scope_depth,
            breaks: Vec::new(),
        }
    }

    fn capture_local(&mut self, var: &str) -> Option<u8> {
        for (i, local) in self.locals.iter_mut().enumerate().rev() {
            if local.name == var {
                local.captured = true;
                return Some(i as u8);
            }
        }
        None
    }

    fn resolve_local(&mut self, var: &str, depth: usize) -> u8 {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == var {
                return i as u8;
            }
        }

        // No local was found, assign a new one.
        let depth = self.scope_depth - depth;
        if self.locals.len() == ::std::u8::MAX as usize {
            panic!("TOO MANY LOCAL VARIABLES");
        }
        self.locals.push(Local {
            name: var.into(),
            depth,
            captured: false,
        });

        debug!("RESOLVE LOCAL: {} @ {}", var, depth);
        debug!("CURRENT      : {:?}", self.locals);

        (self.locals.len() - 1) as u8
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> u8 {
        for (i, upval) in self.upvalues.iter().enumerate() {
            if upval.index == index && upval.is_local == is_local {
                return i as u8;
            }
        }
        if self.upvalues.len() == ::std::u8::MAX as usize {
            panic!("TOO MANY UPVALUES");
        }
        self.upvalues.push(UpValue {
            index,
            is_local,
        });
        (self.upvalues.len() - 1) as u8
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let last = self.scope_depth;
        self.scope_depth -= 1;
        let mut ops = Vec::new();
        self.locals.retain(|local| {
            if local.depth < last {
                return true;
            }
            if local.captured {
                ops.push(Op::CloseUpValue);
            } else {
                ops.push(Op::Pop);
            }
            false
        });
        ops.into_iter().rev().for_each(|op| self.emit(op));
    }

    // TODO: Unify this with Compiler
    fn emit(&mut self, op: Op) {
        self.function.chunk_mut().write(op, self.line);
    }

    fn add_break(&mut self, jmp: usize) {
        self.breaks.push(jmp);
    }

    // This shouldn't need to return the whole Vec.
    fn breaks(&mut self) -> Vec<usize> {
        let bs = self.breaks.clone();
        self.breaks.clear();
        bs
    }
}

impl<'g> Compiler<'g> {
    pub fn new(heap: &'g mut Heap<Object>) -> Self {
        Compiler {
            heap,
            states: Vec::new(),
        }
    }

    pub fn compile(mut self, stmts: &[Stmt]) -> LoxFunction {
        self.start_function("<script>", 0, 0);
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
        self.end_function()
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        let line = stmt.position().map(|pos| pos.line);
        if let Some(line) = line {
            self.state_mut().line = line;
        }
        match *stmt {
            Stmt::Print(ref expr) => {
                self.compile_expr(expr);
                self.emit(Op::Print);
            },
            Stmt::Expr(ref expr) => {
                self.compile_expr(expr);
                self.emit(Op::Pop); // expression is unused
            },
            Stmt::Block(ref stmts) => {
                self.state_mut().begin_scope();
                for s in stmts {
                    self.compile_stmt(s);
                }
                self.state_mut().end_scope();
            },
            Stmt::If(ref cond, ref then_clause, ref else_clause) => {
                self.compile_expr(cond);
                // Jump to the else clause if false
                let else_jmp = self.emit_jze();
                self.emit(Op::Pop); // condition
                self.compile_stmt(&*then_clause);
                let end_jmp = self.emit_jmp();
                // Jump to just past the else clause from the then clause
                self.patch_jmp(else_jmp);
                self.emit(Op::Pop); // condition
                if let &Some(ref else_clause) = else_clause {
                    self.compile_stmt(&*else_clause);
                }
                self.patch_jmp(end_jmp);
            },
            Stmt::While(ref cond, ref body) => {
                let ip = self.ip(); // remember loop start
                self.compile_expr(cond);
                let end_jmp = self.emit_jze();
                self.emit(Op::Pop); // condition
                self.compile_stmt(body);
                self.emit_loop(ip);
                self.patch_jmp(end_jmp);
                self.emit(Op::Pop); // condition

                // Patch all breaks to end at `end_jmp`
                for b in self.state_mut().breaks() {
                    self.patch_jmp(b);
                }
            },
            Stmt::Function(ref f) => self.function_decl(f),
            Stmt::Return(ref expr) => {
                if let &Some(ref expr) = expr {
                    self.compile_expr(expr);
                } else {
                    self.emit(Op::Nil);
                }
                self.emit(Op::Return);
            }
            // Stmt::Class(_) => {
            // },
            Stmt::Break => {
                let jmp = self.emit_jmp();
                self.state_mut().add_break(jmp);
            }
            Stmt::Var(ref var, ref init) => {
                self.compile_expr(init);
                self.var_define(var);
            }
            ref s => unimplemented!("{:?}", s),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr.node {
            ExprKind::Binary(ref binary) => {
                match binary.operator {
                    BinaryOperator::GreaterThanEq | BinaryOperator::LessThanEq => {
                        // Swap to logically equivalent arguments
                        self.compile_expr(&*binary.rhs);
                        self.compile_expr(&*binary.lhs);
                    }
                    _ => {
                        self.compile_expr(&*binary.lhs);
                        self.compile_expr(&*binary.rhs);
                    }
                }

                match binary.operator {
                    BinaryOperator::Plus => self.emit(Op::Add),
                    BinaryOperator::Minus => self.emit(Op::Subtract),
                    BinaryOperator::Star => self.emit(Op::Multiply),
                    BinaryOperator::Slash => self.emit(Op::Divide),
                    BinaryOperator::Equal => self.emit(Op::Equal),
                    BinaryOperator::GreaterThan => self.emit(Op::GreaterThan),
                    BinaryOperator::LessThan => self.emit(Op::LessThan),
                    BinaryOperator::GreaterThanEq => self.emit(Op::LessThan),
                    BinaryOperator::LessThanEq => self.emit(Op::GreaterThan),
                    BinaryOperator::BangEq => {
                        self.emit(Op::Equal);
                        self.emit(Op::Not);
                    }
                }
            }
            ExprKind::Grouping(ref group) => self.compile_expr(group),
            ExprKind::Literal(ref lit) => self.emit_constant(lit),
            ExprKind::Unary(ref unary) => {
                self.compile_expr(&*unary.unary);
                match unary.operator {
                    UnaryOperator::Minus => self.emit(Op::Negate),
                    UnaryOperator::Bang  => self.emit(Op::Not),
                } }, ExprKind::Logical(ref logical) => { match logical.operator {
                    LogicalOperator::And => self.and(&*logical.lhs, &*logical.rhs),
                    LogicalOperator::Or => self.or(&*logical.lhs, &*logical.rhs),
                }
            },
            ExprKind::Var(ref var) => self.var_get(var),
            ExprKind::Assign(ref var, ref rhs) => {
                self.compile_expr(rhs);
                if var.is_upvalue() {
                    let idx = self.resolve_upvalue(var.name());
                    self.emit(Op::SetUpValue);
                    self.emit_byte(idx);
                    return;
                }
                match var.scope() {
                    Scope::Global => self.set_global(var.name()),
                    Scope::Local(d) => {
                        let idx = self.state_mut().resolve_local(var.name(), d);
                        self.emit(Op::SetLocal);
                        self.emit_byte(idx);
                    },
                }
            }
            ExprKind::Call(ref call) => {
                self.compile_expr(&call.callee);
                let arity = call.arguments.len();
                for arg in call.arguments.iter() {
                    self.compile_expr(arg);
                }
                if arity > 8 {
                    panic!("Too many arguments.");
                }
                let op = Op::Call(arity as u8);
                self.emit(op);
            },
            ref e => unimplemented!("{:?}", e),
        }
    }

    fn var_get(&mut self, var: &Variable) {
        if var.is_upvalue() {
            let idx = self.resolve_upvalue(var.name());
            self.emit(Op::GetUpValue);
            self.emit_byte(idx);
            return;
        }
        match var.scope() {
            Scope::Global => {
                self.emit(Op::GetGlobal);
                let idx = {
                    let chunk = self.states.last_mut()
                        .unwrap()
                        .function
                        .chunk_mut();
                    chunk.string_constant(self.heap, var.name())
                };
                self.emit_byte(idx);
            },
            Scope::Local(d) => {
                let idx = self.state_mut().resolve_local(var.name(), d);
                self.emit(Op::GetLocal);
                self.emit_byte(idx);
            },
        }
    }

    fn var_define(&mut self, var: &Variable) {
        match var.scope() {
            Scope::Global => {
                self.emit(Op::DefineGlobal);
                let idx = {
                    let chunk = self.states.last_mut()
                        .unwrap()
                        .function
                        .chunk_mut();
                    chunk.string_constant(self.heap, var.name())
                };
                self.emit_byte(idx);
            },
            Scope::Local(d) => {
                // Declarations do not need to call have a SET_LOCAL instruction.
                self.state_mut().resolve_local(var.name(), d);
            },
        }
    }

    fn and(&mut self, lhs: &Expr, rhs: &Expr) {
        self.compile_expr(lhs);
        let short_circuit_jmp = self.emit_jze();
        self.emit(Op::Pop); // left operand
        self.compile_expr(rhs);
        self.patch_jmp(short_circuit_jmp);
    }

    fn or(&mut self, lhs: &Expr, rhs: &Expr) {
        self.compile_expr(lhs);
        let else_jmp = self.emit_jze(); // if false, jump to rhs
        let end_jmp = self.emit_jmp(); // if true, jump to end
        self.patch_jmp(else_jmp);
        self.emit(Op::Pop);
        self.compile_expr(rhs); // left operand
        self.patch_jmp(end_jmp);
    }

    fn set_global(&mut self, name: &str) {
        self.emit(Op::SetGlobal);
        let idx = {
            let chunk = self.states.last_mut()
                .unwrap()
                .function
                .chunk_mut();
            chunk.string_constant(self.heap, name)
        };
        self.emit_byte(idx);
    }

    fn function_decl(&mut self, f: &FunctionStmt) {
        let name = f.var.name();
        let decl = f.declaration.borrow();

        let parameters = &decl.parameters;
        let body = &decl.body;
        let arity = parameters.len() as u8;

        self.start_function(name, arity, 1);

        // Now that we've pushed to states we are in a new scope.

        for p in parameters {
            self.state_mut().resolve_local(p.name(), 0);
        }
        for stmt in body {
            self.compile_stmt(stmt);
        }

        self.state_mut().end_scope();
        // XXX: There may be a better way to do this, but state will be lost
        // once `end_function` is called.
        let upvalues = self.state_mut().upvalues.clone();

        let function = self.end_function();
        let handle = self.heap.insert(Object::LoxFunction(function)).into_handle();
        let value = Value::object(handle);
        let idx = self.chunk_mut().add_constant(value);
        self.emit(Op::Closure);
        self.emit_byte(idx);
        for upvalue in upvalues {
            self.emit_byte(if upvalue.is_local {
                1
            } else {
                0
            });
            self.emit_byte(upvalue.index);
        }
        self.var_define(&f.var);
    }

    fn resolve_upvalue(&mut self, name: &str) -> u8 {
        //
        // First we walk backwards to find the scope that contains this captured value as a local.
        //
        // Once found, we need to mark it as captured so that it is closed over when
        // it goes out of scope.
        let end = self.states.len() - 1;
        let (scope, mut index) =
            self.states[..end].iter_mut()
                .enumerate()
                .rev()
                .filter_map(|(i, enclosing)| {
                    enclosing.capture_local(name).map(|local| (i, local))
                })
                .next()
                .expect("upvalue marked during resolution but could not be found");

        // Add the upvalue to the immediately following scope and update the index
        index = self.states[scope + 1].add_upvalue(index, true);
        if scope >= self.states.len() - 2 {
            // If we are only one scope up from the current function, we are done.
            return index;
        }
        // Walk forwards and propagate the upvalue up to our current function
        for enclosing in &mut self.states[scope + 2..] {
            index = enclosing.add_upvalue(index, false);
        }
        index
    }

    fn start_function(&mut self, name: &str, arity: u8, scope: usize) {
        let next_function = LoxFunction::new(name, arity);
        let state = CompileState::new(next_function, scope);
        self.states.push(state);
    }

    fn end_function(&mut self) -> LoxFunction {
        self.emit(Op::Nil);
        self.emit(Op::Return);
        let mut state = self.states.pop().expect("states to be nonempty");
        #[cfg(debug_assertions)]
        {
            self.dissassemble(state.function.chunk());
        }
        state.function.set_upvalue_count(state.upvalues.len());
        // TODO: This should be removed instead of copied so that it cannot be used again.
        state.function
    }

    #[cfg(debug_assertions)]
    fn dissassemble(&self, chunk: &Chunk) {
        use debug::Disassembler;

        let dis = Disassembler::new(chunk);
        dis.disassemble();
    }

    fn state_mut(&mut self) -> &mut CompileState {
        self.states.last_mut().expect("states to be nonempty")
    }

    fn chunk(&mut self) -> &Chunk {
        self.states.last()
            .expect("states to be nonempty")
            .function
            .chunk()
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        self.states.last_mut()
            .expect("states to be nonempty")
            .function
            .chunk_mut()
    }

    fn line(&mut self) -> usize {
        self.states.last_mut()
            .expect("states to be nonempty")
            .line
    }

    fn emit_constant(&mut self, lit: &Literal) {
        match *lit {
            Literal::Nil => self.emit(Op::Nil),
            Literal::True => self.emit(Op::True),
            Literal::False => self.emit(Op::False),
            Literal::Number(n) => self.emit_number_literal(n),
            Literal::String(ref s) => {
                let idx = {
                    let chunk = self.states.last_mut().unwrap().function.chunk_mut();
                    chunk.string_constant(self.heap, s)
                };
                self.emit(Op::Constant(idx));
            }
        }
    }

    #[cfg(feature = "op-immediate")]
    fn emit_number_literal(&mut self, n: f64) {
        self.emit(Op::Immediate);
        let val = Value::float(n).to_raw();
        let chunk = self.chunk_mut();
        chunk.write_u64(val);
    }

    #[cfg(not(feature = "op-immediate"))]
    fn emit_number_literal(&mut self, n: f64) {
        let idx = {
            let chunk = self.states.last_mut().unwrap().function.chunk_mut();
            chunk.add_constant(Value::float(n))
        };
        self.emit(Op::Constant(idx));
    }

    // FIXME: The high-level global ops should have this in their repr, or
    // we should make emit_constant handle the case without OP_CONSTANT
    fn emit_byte(&mut self, byte: u8) {
        self.chunk_mut().write_byte(byte);
    }

    fn emit_jze(&mut self) -> usize {
        let line = self.line();
        let chunk = self.chunk_mut();
        chunk.write(Op::JumpIfFalse, line);
        chunk.write_byte(0xff);
        chunk.write_byte(0xff);
        chunk.len() - 2
    }

    fn emit_jmp(&mut self) -> usize {
        let line = self.line();
        let chunk = self.chunk_mut();
        chunk.write(Op::Jump, line);
        chunk.write_byte(0xff);
        chunk.write_byte(0xff);
        chunk.len() - 2
    }

    fn emit_loop(&mut self, ip: usize) {
        let line = self.line();
        let chunk = self.chunk_mut();
        let sub = chunk.len() - ip + 3; // 3 bytes for the instruction itself

        let lo = (sub & 0xff) as u8;
        let hi = ((sub >> 8) & 0xff) as u8;
        chunk.write(Op::Loop, line);
        chunk.write_byte(lo);
        chunk.write_byte(hi);
    }

    // FIXME: Does not need to be mut
    fn ip(&mut self) -> usize {
        self.chunk_mut().len()
    }

    fn patch_jmp(&mut self, idx: usize) {
        let jmp = self.ip();
        let lo = (jmp & 0xff) as u8;
        let hi = ((jmp >> 8) & 0xff) as u8;
        self.chunk_mut().write_byte_at(idx, lo);
        self.chunk_mut().write_byte_at(idx + 1, hi);
    }

    fn emit(&mut self, op: Op) {
        let line = self.line();
        self.chunk_mut().write(op, line);
    }
}
