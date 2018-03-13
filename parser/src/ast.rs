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
}

pub enum Scope {
    Global,
    Local(usize)
}

impl Variable {
    pub fn new_global(name: &str) -> Self {
        Variable {
            name: name.to_owned(),
            depth: None,
        }
    }

    pub fn new_local(name: &str) -> Self {
        Variable {
            name: name.to_owned(),
            depth: Some(0),
        }
    }

    pub fn resolve_local(&mut self, depth: usize) {
        self.depth = Some(depth);
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn scope(&self) -> Scope {
        self.depth
            .map(Scope::Local)
            .unwrap_or(Scope::Global)
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
            var: Variable::new_local(name),
            declaration: Rc::new(RefCell::new(declaration)),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct FunctionDecl {
    pub parameters: Vec<Variable>,
    pub body: Vec<Stmt>,
}

impl FunctionDecl {
    pub(super) fn new(parameters: Vec<Variable>, body: Vec<Stmt>) -> Self {
        FunctionDecl {
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
        let var = Variable::new_local(name);
        Stmt::Class(Class {
            var,
            methods,
            class_methods,
            superclass,
        })
    }

    pub fn position(&self) -> Option<&Position> {
        None
        // match *self {
        //     Stmt::Expr(ref expr) => {
        //         None
        //     }
            // Print(Expr),
            // Var(Variable, Expr),
            // Block(Vec<Stmt>),
            // If(Expr, Box<Stmt>, Option<Box<Stmt>>),
            // While(Expr, Box<Stmt>),
            // Break,
            // Function(FunctionStmt),
            // Return(Expr),
            // Class(Class),
        //     _ => None,
        // }
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
