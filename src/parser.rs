/// Parsing lox expressions from a series of Tokens.
///
/// # The Lox Grammar
///
/// The currently supported grammar that we use, taken straight from
/// Crafting Interpreters.
///
/// ```
/// expression → equality
/// equality   → comparison ( ( "!=" | "==" ) comparison )*
/// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
/// term       → factor ( ( "-" | "+" ) factor )*
/// factor     → unary ( ( "/" | "*" ) unary )*
/// unary      → ( "!" | "-" ) unary
///            | primary
/// primary    → NUMBER | STRING | "false" | "true" | "nil"
///            | "(" expression ")"
/// ```

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;

use errors::*;

use scanner::Token;
use scanner::TokenType;
use scanner::Keyword;

pub struct Parser<'t> {
    tokens: &'t [Token<'t>],
    current: usize,
}

struct PrettyPrinter {
    buf: String,
}

impl PrettyPrinter {
    fn new() -> Self {
        PrettyPrinter {
            buf: String::new(),
        }
    }

    fn print<'t>(mut self, expr: &Expr<'t>) -> String {
        self.print_inner(expr, 0);
        self.buf
    }

    fn print_inner<'t>(&mut self, expr: &Expr<'t>, indent: usize) {
        match *expr {
            Expr::Binary(ref bin) => {
                let op = bin.operator.value;
                write!(&mut self.buf, "{:indent$}Binary({:?})\n", "", op, indent=indent);
                self.print_inner(&*bin.lhs, indent + 2);
                self.print_inner(&*bin.rhs, indent + 2);
            },
            Expr::Grouping(ref group) => {
                self.print_inner(group, indent + 2);
            },
            Expr::Literal(ref lit) => {
                write!(&mut self.buf, "{:indent$}{:?}\n", "", lit, indent=indent);
            },
            Expr::Unary(ref unary) => {
                let op = unary.operator.value;
                write!(&mut self.buf, "{:indent$}Unary({:?})\n", "", op, indent=indent);
                self.print_inner(&*unary.unary, indent + 2);
            },
        }
    }
}

impl<'t> Debug for Expr<'t> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        let pp = PrettyPrinter::new();
        let s = pp.print(self);
        write!(f, "{}", s)
    }
}

pub enum Stmt<'t> {
    Expression(Expr<'t>),
    Print(Expr<'t>),
}

pub enum Expr<'t> {
    Binary(Binary<'t>),
    Grouping(Box<Expr<'t>>),
    Literal(Literal),
    Unary(Unary<'t>),
}

pub struct Binary<'t> {
    pub lhs: Box<Expr<'t>>,
    pub rhs: Box<Expr<'t>>,
    pub operator: &'t Token<'t>,
}

impl<'t> Binary<'t> {
    fn new(lhs: Expr<'t>, rhs: Expr<'t>, operator: &'t Token<'t>) -> Self {
        Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator,
        }
    }
}

pub struct Unary<'t> {
    pub operator: &'t Token<'t>,
    pub unary: Box<Expr<'t>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Void,
}

impl Eq for Literal {
}

impl Literal {
    pub fn into_bool(self) -> bool {
        match self {
            Literal::Nil => false,
            Literal::False => false,
            _ => true,
        }
    }
}

impl Into<Literal> for bool {
    fn into(self) -> Literal {
        if self {
            Literal::True
        } else {
            Literal::False
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(ref s) => write!(f, "{:?}", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
            Literal::Void => Ok(()),
        }
    }
}

// Encapsulates rules with the following form:
// name   → inner ( ( opA | obB | ... ) inner )*
macro_rules! binary_expr_impl (
    ($name:ident, $inner:ident, $($pattern:pat)|*) => (
        fn $name(&mut self) -> Result<Expr<'t>> {
            let mut expr = self.$inner()?;
            while let Some(&Token{ref ty, ..}) = self.peek() {
                match *ty {
                    $($pattern)|* => { self.advance(); },
                    _ => break,
                }
                let operator = self.previous().unwrap();
                let rhs = self.$inner()?;
                let binary = Binary::new(expr, rhs, operator);

                expr = Expr::Binary(binary);
            }
            Ok(expr)
        }
    );
);

impl<'t> Parser<'t> {
    pub fn new(tokens: &'t [Token<'t>]) -> Self {
        Parser {
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt<'t>>> {
        let mut statements = Vec::new();
        while let Ok(stmt) = self.statement() {
            // TODO: This should return an error if the statement parsing
            // fails with something other than end of input
            statements.push(stmt);
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt<'t>> {
        match *self.peek_type()? {
            TokenType::Keyword(Keyword::Print) => {
                self.advance();
                self.print_statement()
            },
            _ => self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt<'t>> {
        let value = self.expression()?;
        match *self.peek_type()? {
            TokenType::Semicolon => {
                self.advance();
            },
            _ => { return Err("Expected ';' after value.".into()); }
        }
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt<'t>> {
        let expr = self.expression()?;
        match *self.peek_type()? {
            TokenType::Semicolon => {
                self.advance();
                Ok(Stmt::Expression(expr))
            },
            _ => { Err("Expected ';' after value.".into()) }
        }
    }

    // expression → equality
    pub fn expression(&mut self) -> Result<Expr<'t>> {
        self.equality()
    }

    // equality   → comparison ( ( "!=" | "==" ) comparison )*
    binary_expr_impl!(equality, comparison, TokenType::BangEq | TokenType::EqualEq);
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
    binary_expr_impl!(comparison, term,
        TokenType::GreaterThan
        | TokenType::GreaterThanEq
        | TokenType::LessThan
        | TokenType::LessThanEq);
    // term       → factor ( ( "-" | "+" ) factor )*
    binary_expr_impl!(term, factor, TokenType::Plus | TokenType::Minus);
    // factor     → unary ( ( "/" | "*" ) unary )*
    binary_expr_impl!(factor, unary, TokenType::Slash | TokenType::Star);

    // unary      → ( "!" | "-" ) unary
    //            | primary
    fn unary(&mut self) -> Result<Expr<'t>> {
        match self.peek_type()? {
            &TokenType::Bang | &TokenType::Minus => {
                let op = self.advance();
                let unary = self.unary()?;

                Ok(Expr::Unary(Unary {
                    operator: &op,
                    unary: Box::new(unary),
                }))
            }
            _ => self.primary()
        }
    }

    // primary    → NUMBER | STRING | "false" | "true" | "nil"
    //            | "(" expression ")"
    fn primary(&mut self) -> Result<Expr<'t>> {
        let peek_type = self.peek_type()?;
        match peek_type {
            &TokenType::Keyword(Keyword::Nil) => {
                self.advance();
                Ok(Expr::Literal(Literal::Nil))
            },
            &TokenType::Keyword(Keyword::True) => {
                self.advance();
                Ok(Expr::Literal(Literal::True))
            },
            &TokenType::Keyword(Keyword::False) => {
                self.advance();
                Ok(Expr::Literal(Literal::False))
            },
            &TokenType::String(s) => {
                self.advance();
                Ok(Expr::Literal(Literal::String(s.into())))
            },
            &TokenType::Number(n) => {
                self.advance();
                Ok(Expr::Literal(Literal::Number(n)))
            },
            &TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                match self.peek_type()? {
                    &TokenType::RightParen => {
                        self.advance();
                        Ok(Expr::Grouping(Box::new(expr)))
                    }
                    _ => Err("Expect ')' after expression".into()),
                }
            },
            _ => Err("Expected a literal or parenthesized expression".into())
        }
    }

    fn advance(&mut self) -> &'t Token<'t> {
        self.current += 1;
        self.previous().unwrap()
    }

    fn peek(&self) -> Option<&'t Token<'t>> {
        self.tokens.get(self.current)
    }

    fn peek_type(&self) -> Result<&'t TokenType<'t>> {
        let token_type = self.tokens.get(self.current).map(|token| {
            &token.ty
        });
        match token_type {
            Some(ty) => Ok(ty),
            None => Err(ErrorKind::UnexpectedEOF.into()),
        }
    }

    fn previous(&self) -> Option<&'t Token<'t>> {
        self.tokens.get(self.current - 1)
    }
}
