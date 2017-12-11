use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use parser::ast::*;
use environment::Variable;

#[derive(Debug, Fail, PartialEq)]
pub enum ResolveError {
    #[fail(display =  "'return' outside function")]
    ReturnOutsideFunction,
    #[fail(display =  "'break' outside loop")]
    BreakOutsideLoop,
    #[fail(display = "Cannot read local variable in its own initializer")]
    InitializerSelfReference,
    #[fail(display = "Variable with this name already declared in this scope")]
    AlreadyDeclared,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    function: Option<Rc<RefCell<FunctionDecl>>>,
    loop_depth: usize,
}

type Result = ::std::result::Result<(), ResolveError>;

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![HashMap::new()],
            function: None,
            loop_depth: 0,
        }
    }

    pub fn resolve(&mut self, stmts: &mut [Stmt]) -> Result {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn resolve_stmt(&mut self, stmt: &mut Stmt) -> Result {
        match *stmt {
            Stmt::Expr(ref mut expr) => {
                self.resolve_expr(expr)?;
            },
            Stmt::Print(ref mut expr) => {
                self.resolve_expr(expr)?;
            },
            Stmt::Var(ref mut var, ref mut expr) => {
                self.declare(var.name())?;
                self.resolve_expr(expr)?;
                self.define(var.name());
                self.resolve_local(var);
            },
            Stmt::Function(ref mut decl) => {
                self.resolve_function(decl)?;
            },
            Stmt::Block(ref mut stmts) => {
                self.begin_scope();
                self.resolve(stmts)?;
                self.end_scope();
            },
            Stmt::If(ref mut cond, ref mut then_clause, ref mut else_clause) => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(then_clause)?;
                if let Some(ref mut else_clause) = *else_clause {
                    self.resolve_stmt(else_clause)?;
                }
            },
			Stmt::While(ref mut cond, ref mut body) => {
                self.resolve_expr(cond)?;
                self.loop_depth += 1;
                self.resolve_stmt(body)?;
                self.loop_depth -= 1;
            },
            Stmt::Break => {
                if self.loop_depth == 0 {
                    return Err(ResolveError::BreakOutsideLoop);
                }
            },
            Stmt::Return(ref mut expr) => {
                if self.function.is_none() {
                    return Err(ResolveError::ReturnOutsideFunction);
                }
                self.resolve_expr(expr)?;
            },
            Stmt::Class(ref cls, ref mut methods) => {
                self.declare(cls.name())?;
                self.define(cls.name());

                self.begin_scope();
                self.declare("this")?;
                self.define("this");
                for method in methods {
                    self.resolve_function(method)?;
                }
                self.end_scope();
            },
        }
        Ok(())
    }

    pub fn resolve_function(&mut self, decl: &mut Rc<RefCell<FunctionDecl>>) -> Result {
        let enclosing_function = self.function.take();
        self.function = Some(decl.clone());

        let mut decl = decl.borrow_mut();
        // Define the function itself
        self.declare(decl.var.name())?;
        self.define(decl.var.name());

        self.begin_scope();
        for param in &decl.parameters {
            self.declare(param.name())?;
            self.define(param.name());
        }
        self.resolve(&mut decl.body)?;
        self.end_scope();

        self.function = enclosing_function;
        Ok(())
    }

    pub fn resolve_expr(&mut self, expr: &mut Expr) -> Result {
        match *expr {
            Expr::Grouping(ref mut inner) => {
                self.resolve_expr(inner)?;
            },
            Expr::Logical(ref mut inner) => {
                self.resolve_expr(&mut inner.lhs)?;
                self.resolve_expr(&mut inner.rhs)?;
            },
            Expr::Binary(ref mut inner) => {
                self.resolve_expr(&mut inner.lhs)?;
                self.resolve_expr(&mut inner.rhs)?;
            },
            Expr::Unary(ref mut inner) => {
                self.resolve_expr(&mut inner.unary)?;
            },
            Expr::Literal(_) => {},
            Expr::Var(ref mut var) => {
                if let Some(false) = self.check_var(var.name()) {
                    return Err(ResolveError::InitializerSelfReference);
                }
                self.resolve_local(var);
            },
            Expr::Assign(ref mut var, ref mut value) => {
                self.resolve_expr(value)?;
                self.resolve_local(var);
            },
            Expr::Call(ref mut call) => {
                self.resolve_expr(&mut call.callee)?;
                for arg in &mut call.arguments {
                    self.resolve_expr(arg)?;
                }
            },
            Expr::Get(ref mut lhs, _) => {
                self.resolve_expr(lhs)?;
            },
            Expr::Set(ref mut expr, _, ref mut value) => {
                self.resolve_expr(expr)?;
                self.resolve_expr(value)?;
            },
            Expr::This(ref mut var, _) => {
                self.resolve_local(var);
            },
        }
        Ok(())
    }

    fn resolve_local(&mut self, var: &mut Variable) {
        debug!("scopes: {:?}", self.scopes);
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(var.name()) {
                debug!("var '{}' resolved to a depth {}", var.name(), depth);
                var.resolve(depth);
                return;
            }
        }
        debug!("var '{}' assumed global", var.name());
    }

    fn check_var(&self, var: &str) -> Option<bool> {
        self.scopes.last()
            .and_then(|scope| scope.get(var))
            .map(|b| *b)
    }

    fn declare(&mut self, var: &str) -> Result {
        use std::collections::hash_map::Entry;

        let scope = self.scopes.last_mut().expect("scope stack to be nonempty");
        match scope.entry(var.into()) {
            Entry::Occupied(_) => Err(ResolveError::AlreadyDeclared),
            Entry::Vacant(entry) => {
                entry.insert(false);
                Ok(())
            }
        }
    }

    fn define(&mut self, var: &str) {
        let scope = self.scopes.last_mut().expect("scope stack to be nonempty");
        if let Some(val) = scope.get_mut(var) {
            *val = true;
        }
    }

    fn begin_scope(&mut self) {
        debug!("NEW SCOPE");
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        debug!("END SCOPE");
        self.scopes.pop().expect("scopes stack to be nonempty");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Parser;

    #[test]
    fn break_outside_loop() {
        let prog = "break;";
        let err  = parse_and_resolve(prog).unwrap_err();
        assert_eq!(err, ResolveError::BreakOutsideLoop);
    }

    fn parse_and_resolve(prog: &str) -> Result {
        let mut stmts = Parser::new(prog).parse().unwrap();
        Resolver::new().resolve(&mut stmts)
    }
}
