use parser::ast::{Stmt, Expr};
use value::Value;

pub struct PrettyPrinter {
    inner: String,
    indent_size: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        PrettyPrinter {
            inner: String::new(),
            indent_size: 4,
        }
    }

    pub fn pretty_print<'t>(mut self, stmts: &[Stmt<'t>]) -> String {
        let mut iter = stmts.iter();
        if let Some(stmt) = iter.next() {
            self.push_stmt(stmt, 0, false);
        }
        for stmt in iter {
            self.push_stmt(stmt, 0, true);
        }
        self.inner
    }

    fn push<'a, S>(&mut self, s: S) -> &mut Self where S: AsRef<str> {
        self.inner.push_str(s.as_ref());
        self
    }

    fn push_char(&mut self, c: char) -> &mut Self {
        self.inner.push(c);
        self
    }

    fn push_stmt<'t>(&mut self, stmt: &Stmt<'t>, indent: usize, newline: bool) {
        let indent_size = self.indent_size;
        if newline {
            self.newline(indent);
        }
        match *stmt {
            Stmt::Expr(ref expr) => {
                self.push_expr(expr).push_char(';');
            },
            Stmt::Print(ref expr) => {
                self.push("print ").push_expr(expr).push_char(';');
            },
            Stmt::Var(var, ref expr) => {
                self.push("var ").push(var).push(" = ").push_expr(expr).push_char(';');
            },
            Stmt::Block(ref stmts) => {
                self.push_char('{');
                for stmt in stmts {
                    self.push_stmt(stmt, indent + indent_size, true);
                }
                self.newline(indent).push_char('}');
            },
            Stmt::Break => {
                self.push("break;");
            },
            Stmt::If(ref cond, ref then_clause, ref else_clause) => {
                self.push("if (").push_expr(cond).push_char(')');

                let then_block = is_block(then_clause);
                if then_block {
                    self.push_char(' ').push_stmt(then_clause, indent, false);
                } else {
                    self.push_stmt(then_clause, indent + indent_size, true);
                }
                if let &Some(ref else_clause) = else_clause {
                    if then_block {
                        self.push_char(' ');
                    } else {
                        self.newline(indent);
                    }
                    if is_block(else_clause) {
                        self.push("else ").push_stmt(else_clause, indent, false);
                    } else {
                        self.push("else").push_stmt(else_clause, indent + indent_size, true);
                    }
                }
            },
            Stmt::While(ref cond, ref body) => {
                self.push("while (").push_expr(cond).push(") ").push_stmt(body, indent, true);
            },
        }
    }

    fn newline(&mut self, indent: usize) -> &mut Self {
        self.push_char('\n');
        for _ in ::std::iter::repeat(' ').take(indent) {
            self.push_char(' ');
        }
        self
    }

    fn push_expr<'t>(&mut self, expr: &Expr<'t>) -> &mut Self {
        match *expr {
            Expr::Binary(ref bin) => {
                let op = bin.operator;
                self.push_expr(&*bin.lhs).push_char(' ').push(op.to_str())
                    .push_char(' ').push_expr(&*bin.rhs);
            },
            Expr::Logical(ref logical) => {
                let op = logical.operator;
                self.push_expr(&*logical.lhs).push_char(' ').push(op.to_str())
                    .push_char(' ').push_expr(&*logical.rhs);
            },
            Expr::Grouping(ref group) => {
                self.push_char('(').push_expr(group).push_char(')');
            },
            Expr::Literal(ref lit) => {
                match *lit {
                    Value::Number(n) => { self.push(n.to_string()); },
                    Value::String(ref s) => {
                        self.push_char('"').push(s).push_char('"');
                    }
                    Value::True => { self.push("true"); },
                    Value::False => { self.push("false"); },
                    Value::Nil => { self.push("nil"); },
                    Value::Void => { panic!("Obtained a literal of type 'Void'"); },
                }
            },
            Expr::Unary(ref unary) => {
                let op = unary.operator;
                self.push(op.to_str()).push_expr(&*unary.unary);
            },
            Expr::Var(ref var) => {
                self.push(var);
            },
            Expr::Assign(var, ref expr) => {
                self.push(var).push(" = ").push_expr(expr);
            }
        }
        self
    }
}

fn is_block<'t>(stmt: &Stmt<'t>) -> bool {
    match *stmt {
        Stmt::Block(_) => true,
        _ => false,
    }
}
