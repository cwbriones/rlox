use super::Position;
use std::rc::Rc;
use std::cell::RefCell;

use environment::Variable;

#[cfg(test)]
pub mod dsl {
    use super::*;

    pub fn binary(operator: BinaryOperator, lhs: Expr, rhs: Expr) -> Expr {
        Expr::binary(operator, lhs, rhs)
    }

    // TODO: Write test for parsing logical operator
    // pub fn logical(operator: LogicalOperator, lhs: Expr, rhs: Expr) -> Expr {
    //     Expr::logical(operator, lhs, rhs)
    // }

    pub fn unary(operator: UnaryOperator, unary: Expr) -> Expr {
        Expr::unary(operator, unary)
    }

    pub fn number(n: f64) -> Expr {
        Expr::Literal(Literal::Number(n))
    }

    pub fn nil() -> Expr {
        Expr::Literal(Literal::Nil)
    }

    pub fn truelit() -> Expr {
        Expr::Literal(Literal::True)
    }

    // pub fn falselit() -> Expr {
    //     Expr::Literal(Value::False)
    // }

    pub fn string<S: Into<String>>(s: S) -> Expr {
        Expr::Literal(Literal::String(s.into()))
    }

    pub fn grouping(expr: Expr) -> Expr {
        Expr::Grouping(Box::new(expr))
    }

    pub fn var(name: &str) -> Expr {
        Expr::Var(Variable::new_global(name))
    }

    pub fn assign(name: &str, assignment: Expr) -> Expr {
        Expr::Assign(Variable::new_global(name), Box::new(assignment))
    }
}

#[derive(PartialEq, Debug)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Variable, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Break,
    Function(Rc<RefCell<FunctionDecl>>),
    Return(Expr),
    Class(Variable, Vec<Rc<RefCell<FunctionDecl>>>, Option<Variable>),
}

#[derive(PartialEq, Debug)]
pub struct FunctionDecl {
    pub var: Variable,
    pub parameters: Vec<Variable>,
    pub body: Vec<Stmt>,
}

impl FunctionDecl {
    pub(super) fn new(name: &str, parameters: Vec<Variable>, body: Vec<Stmt>) -> Self {
        let var = Variable::new_local(name.into());
        FunctionDecl {
            var,
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

    pub(super) fn class(name: &str, methods: Vec<FunctionDecl>, superclass: Option<Variable>) -> Stmt {
        let methods = methods.into_iter()
            .map(|d| Rc::new(RefCell::new(d)))
            .collect::<Vec<_>>();
        Stmt::Class(Variable::new_local(name), methods, superclass)
    }

    pub(super) fn function_from_decl(declaration: FunctionDecl) -> Stmt {
        Stmt::Function(Rc::new(RefCell::new(declaration)))
    }
}

#[derive(PartialEq, Debug)]
pub enum Expr {
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
}

impl Expr {
    pub fn call(callee: Expr, position: Position, arguments: Vec<Expr>) -> Expr {
        Expr::Call(Call{
            callee: Box::new(callee),
            position: position,
            arguments: arguments,
        })
    }

    pub(super) fn binary(operator: BinaryOperator, lhs: Expr, rhs: Expr) -> Self {
        Expr::Binary(Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn logical(operator: LogicalOperator, lhs: Expr, rhs: Expr) -> Self {
        Expr::Logical(Logical {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        })
    }

    pub(super) fn unary(operator: UnaryOperator, unary: Expr) -> Self {
        Expr::Unary(Unary {
            operator: operator,
            unary: Box::new(unary),
        })
    }

    pub(super) fn get(expr: Expr, name: &str) -> Self {
        Expr::Get(Box::new(expr), name.to_owned())
    }

    pub(super) fn set(expr: Box<Expr>, name: String, value: Expr) -> Self {
        // FIXME: This is strangely coupled
        Expr::Set(expr, name, Box::new(value))
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
