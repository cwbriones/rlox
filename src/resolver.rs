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
    #[fail(display = "Cannot use 'this' outside of a class")]
    ThisOutsideClass,
    #[fail(display = "Cannot return a value from an initializer")]
    ReturnFromInitializer,
    #[fail(display = "Cannot use 'super' outside of a class")]
    SuperOutsideClass,
    #[fail(display = "Cannot use 'super' in a class with no superclass")]
    SuperInBaseClass,
}

pub struct Resolver {
    scopes: Scopes,
    function: Option<FunctionType>,
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
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            scopes: vec![HashMap::new()],
        }
    }

    fn resolve_local(&mut self, var: &mut Variable) {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(var.name()) {
                debug!("var '{}' resolved to a depth {}", var.name(), depth);
                var.resolve(depth);
                return;
            }
        }
        debug!("var '{}' assumed global", var.name());
    }

    fn init(&mut self, var: &str) -> Result {
        self.declare(var)?;
        self.define(var);
        Ok(())
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

    fn check_var(&self, var: &str) -> Option<bool> {
        self.scopes.last()
            .and_then(|scope| scope.get(var))
            .map(|b| *b)
    }

    fn begin(&mut self) {
        debug!("NEW SCOPE");
        self.scopes.push(HashMap::new());
    }

    fn end(&mut self) {
        debug!("END SCOPE");
        self.scopes.pop().expect("scopes stack to be nonempty");
    }
}


impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: Scopes::new(),
            function: None,
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
            Stmt::Function(ref mut decl) => {
                self.resolve_function(decl, FunctionType::Function)?;
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
                match self.function {
                    Some(FunctionType::Initializer) => return Err(ResolveError::ReturnFromInitializer),
                    Some(_) => self.resolve_expr(expr)?,
                    None => return Err(ResolveError::ReturnOutsideFunction),
                }
            },
            Stmt::Class(ref cls, ref mut methods, ref mut superclass) => {
                self.scopes.init(cls.name())?;
                let enclosing_class = self.class.take();
                if let &mut Some(ref mut superclass) = superclass {
                    self.class = Some(ClassType::Subclass);
                    self.scopes.resolve_local(superclass);
                    self.scopes.begin(); // begin 'super' scope
                    self.scopes.init("super")?;
                } else {
                    self.class = Some(ClassType::Class);
                }
                self.scopes.begin(); // begin 'this' scope
                self.scopes.init("this")?;
                for method in methods {
                    let is_init = method.borrow().var.name() == "init";
                    if is_init {
                        self.resolve_function(method, FunctionType::Initializer)?;
                    } else {
                        self.resolve_function(method, FunctionType::Method)?;
                    }
                }
                self.scopes.end(); // end 'this' scope
                if superclass.is_some() {
                    self.scopes.end(); // end 'super' scope
                }
                self.class = enclosing_class;
            },
        }
        Ok(())
    }

    fn resolve_function(&mut self, decl: &mut Rc<RefCell<FunctionDecl>>, function_type: FunctionType) -> Result {
        let enclosing_function = self.function.take();
        self.function = Some(function_type);

        let mut decl = decl.borrow_mut();
        // Define the function itself
        self.scopes.init(decl.var.name())?;

        self.scopes.begin();
        for param in &decl.parameters {
            self.scopes.init(param.name())?;
        }
        self.resolve(&mut decl.body)?;
        self.scopes.end();

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
                if let Some(false) = self.scopes.check_var(var.name()) {
                    return Err(ResolveError::InitializerSelfReference);
                }
                self.scopes.resolve_local(var);
            },
            Expr::Assign(ref mut var, ref mut value) => {
                self.resolve_expr(value)?;
                self.scopes.resolve_local(var);
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
                if self.class.is_none() {
                    return Err(ResolveError::ThisOutsideClass);
                }
                self.scopes.resolve_local(var);
            },
            Expr::Super(ref mut var, _, _) => {
                match self.class {
                    None => Err(ResolveError::SuperOutsideClass),
                    Some(ClassType::Class) => Err(ResolveError::SuperInBaseClass),
                    _ => Ok(()),
                }?;
                self.scopes.resolve_local(var);
            },
        }
        Ok(())
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
        let mut stmts = Parser::new(prog).parse().unwrap();
        Resolver::new().resolve(&mut stmts)
    }
}
