use std::collections::HashMap;

use ast::*;
use errors::ResolveError;

pub struct Resolver {
    scopes: Scopes,
    class: Option<ClassType>,
    loop_depth: usize,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum FunctionType {
    Initializer,
    Method,
    Function,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum ClassType {
    Class,
    Subclass,
}

type Result = ::std::result::Result<(), ResolveError>;

struct Scopes {
    scopes: Vec<HashMap<String, bool>>,
    functions: Vec<(FunctionType, usize)>,
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            scopes: vec![HashMap::new()],
            functions: Vec::new(),
        }
    }

    fn resolve_local(&mut self, var: &mut Variable) {
        let scope_len = self.scopes.len();
        // We skip the first scope to treat it as global.
        let scopes_iter = self.scopes.iter().skip(1).rev();
        // How many scopes are within the current function?
        // This is used to track if a var is being closed over.
        let function_size = self.scopes.len() - self.function_start() - 1;

        for (depth, scope) in scopes_iter.enumerate() {
            if scope.contains_key(var.name()) {
                var.resolve_local(depth, function_size);
                debug!("[scope={}] var '{}' resolved to depth={} function_size={}", scope_len, var.name(), depth + 1, function_size);
                return;
            }
        }
        debug!("[scope={}] var '{}' assumed global", scope_len, var.name());
    }

    fn init(&mut self, var: &str) -> Result {
        self.declare(var)?;
        self.define(var);
        Ok(())
    }

    fn declare(&mut self, var: &str) -> Result {
        use std::collections::hash_map::Entry;

        let scope_len = self.scopes.len();
        let is_local = scope_len > 1;
        let scope = self.scopes.last_mut().expect("scope stack to be nonempty");

        debug!("[scope={}] declaring {} variable {}", scope_len, if is_local { "local" } else { "global" }, var);

        match scope.entry(var.into()) {
            Entry::Occupied(_) if is_local => {
                Err(ResolveError::AlreadyDeclared)
            }
            // Global Scope, okay to redeclare.
            Entry::Occupied(_) => Ok(()),
            Entry::Vacant(entry) => {
                entry.insert(false);
                Ok(())
            }
        }
    }

    fn define(&mut self, var: &str) {
        let scope_len = self.scopes.len();
        debug!("[scope={}] defining variable {}", scope_len, var);
        let scope = self.scopes.last_mut().expect("scope stack to be nonempty");
        if let Some(val) = scope.get_mut(var) {
            *val = true;
        }
    }

    fn check_var(&self, var: &str) -> Option<bool> {
        self.scopes.last()
            .and_then(|scope| scope.get(var))
            .map(|b| *b)
    }

    fn begin(&mut self) {
        debug!("entering scope {}", self.scopes.len() + 1);
        self.scopes.push(HashMap::new());
    }

    fn end(&mut self) {
        debug!("exiting scope {}", self.scopes.len());
        self.scopes.pop().expect("scopes stack to be nonempty");
    }

    pub fn function(&self) -> Option<FunctionType> {
        self.functions.last().map(|f| f.0)
    }

    fn function_start(&self) -> usize {
        self.functions.last().map(|&(_, d)| d).unwrap_or(0)
    }

    fn begin_function(&mut self, function: FunctionType) {
        self.begin();
        let scope = self.scopes.len() - 1;
        debug!("[scope={}] entering function", scope + 1);
        self.functions.push((function, scope));
    }

    fn end_function(&mut self) {
        self.end();
        self.functions.pop().expect("end_function called at top-level");
        debug!("[scope={}] exiting function", self.scopes.len());
    }
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: Scopes::new(),
            class: None,
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
                self.scopes.declare(var.name())?;
                self.resolve_expr(expr)?;
                self.scopes.define(var.name());
                self.scopes.resolve_local(var);
            },
            Stmt::Function(ref mut function) => {
                // Define the function itself
                self.scopes.init(function.var.name())?;
                self.scopes.resolve_local(&mut function.var);
                let mut declaration = function.declaration.borrow_mut();
                self.resolve_function(&mut *declaration, FunctionType::Function)?;
            },
            Stmt::Block(ref mut stmts) => {
                self.scopes.begin();
                self.resolve(stmts)?;
                self.scopes.end();
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
                match (self.scopes.function(), expr) {
                    (Some(FunctionType::Initializer), &mut Some(_)) =>
                        return Err(ResolveError::ReturnFromInitializer),
                    (Some(_), &mut Some(ref mut expr)) => self.resolve_expr(expr)?,
                    (Some(_), _) => {},
                    (None, _) => return Err(ResolveError::ReturnOutsideFunction),
                }
            },
            Stmt::Class(ref mut class_decl) =>{
                self.scopes.init(class_decl.var.name())?;
                self.scopes.resolve_local(&mut class_decl.var);
                let enclosing_class = self.class.take();
                if let Some(ref mut superclass) = class_decl.superclass {
                    self.class = Some(ClassType::Subclass);
                    self.scopes.resolve_local(superclass);
                    self.scopes.begin(); // begin 'super' scope
                    self.scopes.init("super")?;
                } else {
                    self.class = Some(ClassType::Class);
                }
                self.scopes.begin(); // begin 'this' scope
                self.scopes.init("this")?;
                for method in &class_decl.methods {
                    let name = method.var.name();
                    let mut declaration = method.declaration.borrow_mut();
                    self.scopes.init(name)?;

                    if name == "init" {
                        self.resolve_function(&mut *declaration, FunctionType::Initializer)?;
                    } else {
                        self.resolve_function(&mut *declaration, FunctionType::Method)?;
                    }
                }
                self.scopes.end(); // end 'this' scope
                if class_decl.superclass.is_some() {
                    self.scopes.end(); // end 'super' scope
                }
                self.class = enclosing_class;
            },
        }
        Ok(())
    }

    pub fn resolve_expr(&mut self, expr: &mut Expr) -> Result {
        match expr.node {
            ExprKind::Grouping(ref mut inner) => {
                self.resolve_expr(inner)?;
            },
            ExprKind::Logical(ref mut inner) => {
                self.resolve_expr(&mut inner.lhs)?;
                self.resolve_expr(&mut inner.rhs)?;
            },
            ExprKind::Binary(ref mut inner) => {
                self.resolve_expr(&mut inner.lhs)?;
                self.resolve_expr(&mut inner.rhs)?;
            },
            ExprKind::Unary(ref mut inner) => {
                self.resolve_expr(&mut inner.unary)?;
            },
            ExprKind::Literal(_) => {},
            ExprKind::Var(ref mut var) => {
                if let Some(false) = self.scopes.check_var(var.name()) {
                    return Err(ResolveError::InitializerSelfReference);
                }
                self.scopes.resolve_local(var);
            },
            ExprKind::Assign(ref mut var, ref mut value) => {
                self.resolve_expr(value)?;
                self.scopes.resolve_local(var);
            },
            ExprKind::Call(ref mut call) => {
                self.resolve_expr(&mut call.callee)?;
                for arg in &mut call.arguments {
                    self.resolve_expr(arg)?;
                }
            },
            ExprKind::Get(ref mut lhs, _) => {
                self.resolve_expr(lhs)?;
            },
            ExprKind::Set(ref mut expr, _, ref mut value) => {
                self.resolve_expr(expr)?;
                self.resolve_expr(value)?;
            },
            ExprKind::This(ref mut var, _) => {
                if self.class.is_none() {
                    return Err(ResolveError::ThisOutsideClass);
                }
                self.scopes.resolve_local(var);
            },
            ExprKind::Super(ref mut var, _, _) => {
                match self.class {
                    None => Err(ResolveError::SuperOutsideClass),
                    Some(ClassType::Class) => Err(ResolveError::SuperInBaseClass),
                    _ => Ok(()),
                }?;
                self.scopes.resolve_local(var);
            },
            ExprKind::Function(ref mut function) => {
                let mut declaration = function.borrow_mut();
                self.resolve_function(&mut *declaration, FunctionType::Function)?;
            },
        }
        Ok(())
    }

    fn resolve_function(&mut self, declaration: &mut FunctionDecl, function_type: FunctionType) -> Result {
        self.scopes.begin_function(function_type);
        for param in &declaration.parameters {
            self.scopes.init(param.name())?;
        }
        self.resolve(&mut declaration.body)?;
        self.scopes.end_function();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::parse;

    #[test]
    fn break_outside_loop() {
        let prog = "break;";
        let err  = parse_and_resolve(prog).unwrap_err();
        assert_eq!(err, ResolveError::BreakOutsideLoop);
    }

    #[test]
    fn this_outside_class() {
        let prog = "this;";
        let err  = parse_and_resolve(prog).unwrap_err();
        assert_eq!(err, ResolveError::ThisOutsideClass);

        let prog = "fun foo() { this; }";
        let err  = parse_and_resolve(prog).unwrap_err();
        assert_eq!(err, ResolveError::ThisOutsideClass);
    }

    #[test]
    fn return_from_init() {
        let prog = "class Foo { init() { return; } }";
        parse_and_resolve(prog).unwrap();
    }

    #[test]
    fn return_value_from_init() {
        let prog = "class Foo { init() { return 1; } }";
        let err  = parse_and_resolve(prog).unwrap_err();
        assert_eq!(err, ResolveError::ReturnFromInitializer);
    }

    #[test]
    fn super_outside_class() {
        let prog = "super.init();";
        let err  = parse_and_resolve(prog).unwrap_err();
        assert_eq!(err, ResolveError::SuperOutsideClass);
    }

    #[test]
    fn super_from_base() {
        let prog = "class Foo { init() { super.init(); } }";
        let err  = parse_and_resolve(prog).unwrap_err();
        assert_eq!(err, ResolveError::SuperInBaseClass);
    }

    fn parse_and_resolve(prog: &str) -> Result {
        let mut stmts = parse(prog).unwrap();
        Resolver::new().resolve(&mut stmts)
    }
}
