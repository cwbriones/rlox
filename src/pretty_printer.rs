use environment::Variable;
use parser::ast::{FunctionStmt, Stmt, Expr, Literal};

pub struct PrettyPrinter {
    inner: String,
    indent_size: usize,
    indent: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        PrettyPrinter {
            inner: String::new(),
            indent_size: 4,
            indent: 0,
        }
    }

    pub fn pretty_print(mut self, stmts: &[Stmt]) -> String {
        let mut iter = stmts.iter();
        if let Some(stmt) = iter.next() {
            // no newline
            self.print(stmt);
        }
        for stmt in iter {
            // newline
            self.newline().print(stmt);
        }
        self.inner
    }

    fn print<'a, T>(&mut self, t: T) -> &mut Self
        where T: PrintInto,
    {
        t.print_into(self);
        self
    }

    fn block<P>(&mut self, stmts: &[P]) -> &mut Self
        where P: PrintInto
    {
        self.print('{');
        self.indent += self.indent_size;
        for stmt in stmts {
            self.newline().print(stmt);
        }
        self.indent -= self.indent_size;
        self.newline().print('}');
        self
    }

    fn parameters<P>(&mut self, params: &[P]) -> &mut Self
        where P: PrintInto
    {
        self.print('(');
        if !params.is_empty() {
            let mut iter = params.iter();
            {
                let take = iter.by_ref().take(params.len() - 1);
                for param in take {
                    self.print(param).print(", ");
                }
            }
            if let Some(param) = iter.next() {
                self.print(param);
            }
        }
        self.print(')');
        self
    }

    fn newline(&mut self) -> &mut Self {
        self.print('\n');
        for _ in ::std::iter::repeat(' ').take(self.indent) {
            self.print(' ');
        }
        self
    }
}

trait PrintInto {
    fn print_into(&self, pp: &mut PrettyPrinter);
}

impl<P> PrintInto for Box<P> where P: PrintInto {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        self.as_ref().print_into(pp);
    }
}

impl<'a, P> PrintInto for &'a P where P: PrintInto {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        (*self).print_into(pp);
    }
}

impl<'a> PrintInto for &'a str {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        pp.inner.push_str(self);
    }
}

impl PrintInto for String {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        pp.inner.push_str(self);
    }
}

impl PrintInto for char {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        pp.inner.push(*self);
    }
}

impl PrintInto for Variable {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        pp.print(self.name());
    }
}

impl<'a> PrintInto for Expr {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        match *self {
            Expr::Binary(ref bin) => {
                let op = bin.operator;
                pp.print(&bin.lhs).print(' ').print(op.to_str())
                  .print(' ').print(&bin.rhs);
            },
            Expr::Logical(ref logical) => {
                let op = logical.operator;
                pp.print(&logical.lhs).print(' ').print(op.to_str())
                    .print(' ')
                    .print(&logical.rhs);
            },
            Expr::Grouping(ref group) => {
                pp.print('(').print(group).print(')');
            },
            Expr::Literal(ref lit) => {
                match *lit {
                    Literal::Number(n) => { pp.print(&n.to_string()); },
                    Literal::String(ref s) => {
                        pp.print('"').print(s).print('"');
                    }
                    Literal::True => { pp.print("true"); },
                    Literal::False => { pp.print("false"); },
                    Literal::Nil => { pp.print("nil"); },
                }
            },
            Expr::Unary(ref unary) => {
                let op = unary.operator;
                pp.print(op.to_str()).print(&unary.unary);
            },
            Expr::Var(ref var) => {
                pp.print(var.name());
            },
            Expr::Assign(ref var, ref expr) => {
                pp.print(var.name()).print(" = ").print(&expr);
            },
            Expr::Call(ref call) => {
                pp.print(&call.callee).parameters(&call.arguments);
            },
            Expr::Get(ref lhs, ref property) => {
                pp.print(lhs).print('.').print(property);
            },
            Expr::Set(ref expr, ref name, ref value) => {
                pp.print(expr)
                    .print('.')
                    .print(name)
                    .print(" = ")
                    .print(value);
            },
            Expr::This(_, _) => { pp.print("this"); },
            Expr::Super(_, _, ref method) => { pp.print("super").print('.').print(method); },
            Expr::Function(ref function) => {
                let decl = function.borrow();
                pp.print("fun").parameters(&decl.parameters).print(' ').block(&decl.body);
            },
        }
    }
}

impl PrintInto for FunctionStmt {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        let decl = self.declaration.borrow();
        pp.print(self.var.name())
            .parameters(&decl.parameters)
            .print(' ')
            .block(&decl.body);
    }
}

impl PrintInto for Stmt {
    fn print_into(&self, pp: &mut PrettyPrinter) {
        match *self {
            Stmt::Expr(ref expr) => {
                pp.print(&*expr).print(';');
            },
            Stmt::Print(ref expr) => {
                pp.print("print ").print(&expr).print(';');
            },
            Stmt::Var(ref var, ref expr) => {
                pp.print("var ").print(var.name()).print(" = ").print(&expr).print(';');
            },
            Stmt::Block(ref stmts) => {
                pp.block(stmts);
            },
            Stmt::Break => {
                pp.print("break;");
            },
            Stmt::If(ref cond, ref then_clause, ref else_clause) => {
                pp.print("if (").print(cond).print(')');

                let then_block = is_block(then_clause);
                if then_block {
                    pp.print(' ').print(then_clause);
                } else {
                    pp.indent += pp.indent_size;
                    pp.newline().print(&then_clause);
                    pp.indent -= pp.indent_size;
                }
                if let &Some(ref else_clause) = else_clause {
                    if then_block {
                        pp.print(' ');
                    } else {
                        pp.newline();
                    }
                    if is_block(else_clause) {
                        pp.print("else ").print(&else_clause);
                    } else {
                        pp.indent += pp.indent_size;
                        pp.print("else").newline().print(&else_clause);
                        pp.indent -= pp.indent_size;
                    }
                }
            },
            Stmt::While(ref cond, ref body) => {
                pp.print("while (").print(cond).print(") ").newline().print(&body);
            },
            Stmt::Function(ref function) => {
                pp.print("fun ").print(function);
            },
            Stmt::Return(ref expr) => {
                pp.print("return ").print(expr).print(';');
            },
            Stmt::Class(ref cls) => {
                pp.print("class ").print(&cls.var).print(' ');
                if let Some(ref superclass) = cls.superclass {
                    pp.print("< ").print(superclass).print(' ');
                }
                pp.block(&cls.methods);
            },
        }
    }
}

fn is_block(stmt: &Stmt) -> bool {
    match *stmt {
        Stmt::Block(_) => true,
        _ => false,
    }
}

fn as_block(stmt: &Stmt) -> Option<&[Stmt]> {
    match *stmt {
        Stmt::Block(ref block) => Some(block),
        _ => None,
    }
}
