use super::Position;
use std::rc::Rc;
use std::cell::RefCell;

#[cfg(test)]
pub mod dsl {
    use super::*;

    pub fn binary(operator: BinaryOperator, lhs: Expr, rhs: Expr) -> ExprKind {
        ExprKind::binary(operator, lhs, rhs)
    }

    // TODO: Write test for parsing logical operator
    // pub fn logical(operator: LogicalOperator, lhs: Expr, rhs: Expr) -> Expr {
    //     Expr::logical(operator, lhs, rhs)
    // }

    pub fn unary(operator: UnaryOperator, unary: Expr) -> ExprKind {
        ExprKind::unary(operator, unary)
    }

    pub fn number(n: f64) -> ExprKind {
        ExprKind::Literal(Literal::Number(n))
    }

    pub fn nil() -> ExprKind {
        ExprKind::Literal(Literal::Nil)
    }

    pub fn truelit() -> ExprKind {
        ExprKind::Literal(Literal::True)
    }

    // pub fn falselit() -> Expr {
    //     Expr::Literal(Value::False)
    // }

    pub fn string<S: Into<String>>(s: S) -> ExprKind {
        ExprKind::Literal(Literal::String(s.into()))
    }

    pub fn grouping(expr: Expr) -> ExprKind {
        ExprKind::Grouping(Box::new(expr))
    }

    pub fn var(name: &str) -> ExprKind {
        ExprKind::Var(Variable::new_global(name))
    }

    pub fn assign(name: &str, assignment: Expr) -> ExprKind {
        ExprKind::Assign(Variable::new_global(name), Box::new(assignment))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Variable {
    name: String,
    depth: Option<usize>,
    function_depth: usize,
}

#[derive(Debug)]
pub enum Scope {
    Global,
    Local(usize),
}

impl Variable {
    pub fn new_global(name: &str) -> Self {
        Variable {
            name: name.to_owned(),
            depth: None,
            function_depth: 0,
        }
    }

    pub fn new_local(name: &str) -> Self {
        Variable {
            name: name.to_owned(),
            depth: Some(0),
            function_depth: 0,
        }
    }

    pub fn resolve_local(&mut self, depth: usize, function_depth: usize) {
        self.depth = Some(depth);
        self.function_depth = function_depth;
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn scope(&self) -> Scope {
        self.depth
            .map(Scope::Local)
            .unwrap_or(Scope::Global)
    }

    pub fn is_upvalue(&self) -> bool {
        // If we are reaching out further than our local scope,
        // then it is an upvalue.
        self.depth
            .map(|d| d > self.function_depth)
            .unwrap_or(false)
    }

    // FIXME: Can we re-structure this resolution so that we do
    // not need this, and instead can use a variant of Scope?
    pub fn upvalue_depth(&self) -> Option<usize> {
        self.depth
            .and_then(|d| {
                if d > self.function_depth {
                    Some(d - self.function_depth)
                } else {
                    None
                }
            })
    }
}

#[derive(PartialEq, Debug)]
pub enum Stmt {
    // Needs line
    Expr(Expr),
    // Needs line
    Print(Expr),
    // Needs line
    Var(Variable, Expr),
    // Does not need line
    Block(Vec<Stmt>),
    // Does not need line
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    // Does not need line
    While(Expr, Box<Stmt>),
    // Needs line
    Break,
    // Needs line, internally.
    Function(FunctionStmt),
    // Needs line
    Return(Option<Expr>),
    Class(Class),
}

#[derive(PartialEq, Debug)]
pub struct Class {
    pub var: Variable,
    pub methods: Vec<FunctionStmt>,
    pub class_methods: Vec<FunctionStmt>,
    pub superclass: Option<Variable>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionStmt {
    pub var: Variable,
    pub declaration: Rc<RefCell<FunctionDecl>>,
}

impl FunctionStmt {
    pub fn new(name: &str, declaration: FunctionDecl) -> Self {
        FunctionStmt {
            var: Variable::new_global(name),
            declaration: Rc::new(RefCell::new(declaration)),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct FunctionDecl {
    // FIXME: this field should probably be an enum variant instead.
    pub method: bool,
    pub parameters: Vec<Variable>,
    pub body: Vec<Stmt>,
}

impl FunctionDecl {
    pub(super) fn new(method:bool, parameters: Vec<Variable>, body: Vec<Stmt>) -> Self {
        FunctionDecl {
            method,
            parameters,
            body,
        }
    }
}

impl Stmt {
    pub(super) fn var(name: &str, initializer: Expr) -> Self {
        Stmt::Var(
            Variable::new_global(name),
            initializer
        )
    }

    pub(super) fn if_stmt(cond: Expr, then_clause: Stmt) -> Self {
        Stmt::If(cond, Box::new(then_clause), None)
    }

    pub(super) fn if_else_stmt(cond: Expr, then_clause: Stmt, else_clause: Stmt) -> Self {
        Stmt::If(cond, Box::new(then_clause), Some(Box::new(else_clause)))
    }

    pub(super) fn class(name: &str, methods: Vec<FunctionStmt>, class_methods: Vec<FunctionStmt>, superclass: Option<Variable>) -> Stmt {
        let var = Variable::new_global(name);
        Stmt::Class(Class {
            var,
            methods,
            class_methods,
            superclass,
        })
    }

    pub fn position(&self) -> Option<&Position> {
        match *self {
            Stmt::Expr(ref expr) => Some(&expr.pos),
            Stmt::Print(ref expr) => Some(&expr.pos),
            Stmt::If(ref expr, _, _) => Some(&expr.pos),
            Stmt::While(ref expr, _) => Some(&expr.pos),
            Stmt::Return(ref retval) => {
                retval.as_ref().map(|e| &e.pos)
            }
            // FIXME: Maybe reconsider which position to return
            // for those below.
            Stmt::Var(_, ref expr) => Some(&expr.pos),
            Stmt::Block(_) => None,
            Stmt::Break => None,
            Stmt::Class(_) => None,
            Stmt::Function(_) => None,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Expr {
    pub pos: Position,
    pub node: ExprKind,
}

impl Expr {
    pub fn dummy(node: ExprKind) -> Expr {
        let pos = Position {
            start: 0,
            end: 0,
            line: 0,
        };
        Expr {
            pos,
            node,
        }
    }

    pub fn line(&self) -> usize {
        self.pos.line
    }
}

#[derive(PartialEq, Debug)]
pub enum ExprKind {
    Logical(Logical),
    Binary(Binary),
    Call(Call),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(Unary),
    Var(Variable),
    Assign(Variable, Box<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    This(Variable, Position),
    Super(Variable, Position, String),
    Function(Rc<RefCell<FunctionDecl>>),
}

impl ExprKind {
    pub fn call(callee: Expr, position: Position, arguments: Vec<Expr>) -> ExprKind {
        ExprKind::Call(Call{
            callee: Box::new(callee),
            position: position,
            arguments: arguments,
        })
    }

    pub(super) fn binary(operator: BinaryOperator, lhs: Expr, rhs: Expr) -> Self {
        ExprKind::Binary(Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn logical(operator: LogicalOperator, lhs: Expr, rhs: Expr) -> Self {
        ExprKind::Logical(Logical {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn unary(operator: UnaryOperator, unary: Expr) -> Self {
        ExprKind::Unary(Unary {
            operator: operator,
            unary: Box::new(unary),
        })
    }

    pub(super) fn get(expr: Expr, name: &str) -> Self {
        ExprKind::Get(Box::new(expr), name.to_owned())
    }

    pub(super) fn set(expr: Box<Expr>, name: String, value: Expr) -> Self {
        // FIXME: This is strangely coupled
        ExprKind::Set(expr, name, Box::new(value))
    }

    pub(super) fn function(declaration: FunctionDecl) -> Self {
        ExprKind::Function(Rc::new(RefCell::new(declaration)))
    }
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(PartialEq, Debug)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: BinaryOperator,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum BinaryOperator {
    Equal,
    BangEq,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
    Minus,
    Plus,
    Slash,
    Star,
}

impl BinaryOperator {
    pub fn to_str(&self) -> &str {
        match *self {
            BinaryOperator::Equal => "=",
            BinaryOperator::BangEq => "!=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEq => ">=",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEq => "<=",
            BinaryOperator::Minus => "-",
            BinaryOperator::Plus => "+",
            BinaryOperator::Slash => "/",
            BinaryOperator::Star => "*",
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Call {
    pub callee: Box<Expr>,
    pub position: Position,
    pub arguments: Vec<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Logical {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: LogicalOperator
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}

impl LogicalOperator {
    pub fn to_str(&self) -> &str {
        match *self {
            LogicalOperator::And => "and",
            LogicalOperator::Or  => "or",
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Unary {
    pub operator: UnaryOperator,
    pub unary: Box<Expr>,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl UnaryOperator {
    pub fn to_str(&self) -> &str {
        match *self {
            UnaryOperator::Minus => "-",
            UnaryOperator::Bang  => "!",
        }
    }
}
