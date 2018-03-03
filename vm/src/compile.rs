use chunk::{Chunk, Op};
use value::Value;

use parser;
use parser::ast::*;

pub struct Compiler {
    chunk: Chunk,
    line: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            chunk: Chunk::new("<unnamed chunk>".into()),
            line: 1,
        }
    }

    pub fn compile(mut self, name: &str, stmts: &[Stmt]) -> Chunk {
        // Singleton values are always the first 3 constants in a chunk.
        self.chunk.add_constant(Value::nil());
        self.chunk.add_constant(Value::truelit());
        self.chunk.add_constant(Value::falselit());
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
        self.chunk
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
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
                // TODO: Proper scope.
                for s in stmts {
                    self.compile_stmt(s);
                }
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
                self.emit_jmp_to(ip);
                self.patch_jmp(end_jmp);
            },
            // Stmt::Function(_) => {
            // },
            Stmt::Return(ref expr) => {
                self.compile_expr(expr);
                self.emit(Op::Return);
            }
            // Stmt::Class(_) => {
            // },
            // Stmt::Break => {
            // },
            // Stmt::Var(_, _) => {
            // }
            _ => unimplemented!(),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match *expr {
            Expr::Binary(ref binary) => {
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
            Expr::Grouping(ref group) => self.compile_expr(group),
            Expr::Literal(ref lit) => self.emit_constant(lit),
            Expr::Unary(ref unary) => {
                self.compile_expr(&*unary.unary);
                match unary.operator {
                    UnaryOperator::Minus => self.emit(Op::Negate),
                    UnaryOperator::Bang  => self.emit(Op::Not),
                }
            },
            Expr::Logical(ref logical) => {
                match logical.operator {
                    LogicalOperator::And => self.and(&*logical.lhs, &*logical.rhs),
                    LogicalOperator::Or => self.or(&*logical.lhs, &*logical.rhs),
                }
            },
            _ => unimplemented!(),
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

    fn emit_constant(&mut self, lit: &Literal) {
        match *lit {
            Literal::Nil => self.emit(Op::Constant(0)),
            Literal::True => self.emit(Op::Constant(1)),
            Literal::False => self.emit(Op::Constant(2)),
            Literal::Number(n) => {
                let value = Value::float(n);
                let idx = self.chunk.add_constant(value);
                self.emit(Op::Constant(idx));
            }
            Literal::String(_) => {
                unimplemented!("Compiler::emit_constant string");
            }
        }
    }

    fn emit_jze(&mut self) -> usize {
        self.chunk.write(Op::JumpIfFalse, self.line);
        self.chunk.write_byte(0xff);
        self.chunk.write_byte(0xff);
        self.chunk.len() - 2
    }

    fn emit_jmp(&mut self) -> usize {
        self.chunk.write(Op::Jump, self.line);
        self.chunk.write_byte(0xff);
        self.chunk.write_byte(0xff);
        self.chunk.len() - 2
    }

    fn emit_jmp_to(&mut self, ip: usize) -> usize {
        let lo = (ip & 0xff) as u8;
        let hi = ((ip >> 8) & 0xff) as u8;
        self.chunk.write(Op::Jump, self.line);
        self.chunk.write_byte(lo);
        self.chunk.write_byte(hi);
        self.chunk.len() - 2
    }

    fn ip(&self) -> usize {
        self.chunk.len()
    }

    fn patch_jmp(&mut self, idx: usize) {
        let jmp = self.ip();
        let lo = (jmp & 0xff) as u8;
        let hi = ((jmp >> 8) & 0xff) as u8;
        self.chunk.write_byte_at(idx, lo);
        self.chunk.write_byte_at(idx + 1, hi);
    }

    fn emit(&mut self, op: Op) {
        self.chunk.write(op, self.line);
    }
}
