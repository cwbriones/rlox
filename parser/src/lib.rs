/// Parsing lox expressions from a series of Tokens.
///
/// # The Lox Grammar
///
/// The currently supported grammar that we use, taken straight from
/// Crafting Interpreters.
///
///
/// expression → equality
/// assignment → identifier "=" assignment
///            | logic_or ;
/// logic_or   → logic_and ( "or" logic_and )* ;
/// logic_and  → equality ( "and" equality )* ;
/// equality   → comparison ( ( "!=" | "==" ) comparison )*
/// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
/// term       → factor ( ( "-" | "+" ) factor )*
/// factor     → unary ( ( "/" | "*" ) unary )*
/// unary      → ( "!" | "-" ) unary
///            | primary
/// primary    → NUMBER | STRING | "false" | "true" | "nil"
///            | "(" expression ")"
///

#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use std::iter::Peekable;

use self::errors::*;
use self::scanner::Scanner;
use self::ast::{Expr, ExprKind, Stmt, Literal, FunctionDecl, FunctionStmt, Variable};
use self::scanner::Token;
use self::scanner::TokenType;

pub use self::scanner::Keyword;
pub use self::scanner::Position;

pub mod ast;
pub mod errors;
mod ext;
mod scanner;
mod resolver;
mod peek;

type Result<T> = ::std::result::Result<T, SyntaxError>;

pub fn parse(input: &str) -> ::std::result::Result<Vec<Stmt>, Vec<SyntaxError>> {
    let mut parser = Parser::new(&input);
    parser.parse()
}

pub fn parse_stmt(input: &str) -> ::std::result::Result<Stmt, SyntaxError> {
    let mut parser = Parser::new(&input);
    parser.parse_statement()
}

pub fn parse_expr(input: &str) -> ::std::result::Result<Expr, SyntaxError> {
    let mut parser = Parser::new(&input);
    parser.expression()
}

pub fn resolve(stmts: &mut [Stmt]) -> ::std::result::Result<(), Vec<ResolveError>> {
    let resolver = resolver::Resolver::new();
    resolver.resolve(stmts)
}

const MAX_NUM_PARAMETERS: usize = 8;

struct Parser<'t> {
    scanner: Peekable<Scanner<'t>>,
}

// Encapsulates rules with the following form:
// name   → inner ( ( opA | obB | ... ) inner )*
macro_rules! __binary_rule (
    ($name:ident, $inner:ident, $convert:ident, $cons:expr, $($pattern:pat)|*) => (
        fn $name(&mut self) -> Result<Expr> {
            let mut expr = self.$inner()?;
            while let Ok(ty) = self.peek_type() {
                let tok = match ty {
                    $($pattern)|* => { self.advance()? },
                    _ => break,
                };
                let rhs = self.$inner()?;
                let operator = tok.ty.$convert().expect("attempted invalid conversion into operator type");
                expr = Expr {
                    pos: tok.position,
                    node: $cons(operator, expr, rhs),
                };
            }
            Ok(expr)
        }
    );
);

macro_rules! logical_impl (
    ($name:ident, $inner:ident, $($pattern:pat)|*) => (
        __binary_rule!($name, $inner, into_logical, ExprKind::logical, $($pattern)|*);
    );
);

macro_rules! binary_impl (
    ($name:ident, $inner:ident, $($pattern:pat)|*) => (
        __binary_rule!($name, $inner, into_binary, ExprKind::binary, $($pattern)|*);
    );
);

impl<'t> Iterator for Parser<'t> {
    type Item = Result<Stmt>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.has_next() {
            Some(self.parse_statement())
        } else {
            None
        }
    }
}

impl<'t> Parser<'t> {
    pub fn new(program: &'t str) -> Self {
        let scanner = Scanner::new(program);
        Parser {
            scanner: scanner.peekable(),
        }
    }

    pub fn parse(&mut self) -> ::std::result::Result<Vec<Stmt>, Vec<SyntaxError>> {
        use ext::Partition;

        let (stmts, errs): (Vec<Stmt>, Vec<SyntaxError>) = Partition::partition(self);
        if !errs.is_empty() {
            return Err(errs);
        }
        Ok(stmts)
    }

    // program → declaration* eof ;
    pub fn parse_statement(&mut self) -> Result<Stmt> {
        match self.declaration() {
            Ok(stmt) => Ok(stmt),
            Err(err) => {
                self.synchronize();
                Err(err)
            }
        }
    }

    // declaration → varDecl
    //             | statement ;
    fn declaration(&mut self) -> Result<Stmt> {
        match self.peek_type()? {
            TokenType::Keyword(Keyword::Var) => {
                self.advance()?;
                self.var_decl()
            },
            TokenType::Keyword(Keyword::Class) => {
                self.advance()?;
                self.class_decl()
            },
            TokenType::Keyword(Keyword::Fun) => {
                let keyword = self.advance()?;
                if let TokenType::Identifier = self.peek_type()? {
                    let ident = self.advance()?;
                    let decl = self.function_declaration()?;
                    Ok(Stmt::Function(FunctionStmt::new(ident.value, decl)))
                } else {
                    // TODO: Unify the parsing. If we could scan two tokens
                    // ahead we could fallback to a expression statement
                    // and this would already be handled.
                    let decl = self.function_declaration()?;
                    self.expect(TokenType::Semicolon).after("lambda expression")?;
                    let pos = keyword.position;
                    let node = ExprKind::function(decl);
                    Ok(Stmt::Expr(Expr{ pos, node }))
                }
            },
            _ => self.statement(),
        }
    }

    fn class_decl(&mut self) -> Result<Stmt> {
        let ident =
            self.expect(TokenType::Identifier).after("keyword 'class'")?;
        let superclass = if let TokenType::LessThan = self.peek_type()? {
            self.advance()?;
            let superclass_ident = self.expect(TokenType::Identifier)
                .alias_as("superclass name")
                .check()?;
            Some(Variable::new_local(superclass_ident.value))
        } else {
            None
        };
        self.expect(TokenType::LeftBrace).after("class name")?;
        let mut methods = Vec::new();
        let mut class_methods = Vec::new();
        loop {
            if let TokenType::RightBrace = self.peek_type()? {
                break;
            }
            if let TokenType::Keyword(Keyword::Class) = self.peek_type()? {
                self.advance()?;
                class_methods.push(self.function_statement()?);
            } else {
                methods.push(self.function_statement()?);
            }
        }
        self.expect(TokenType::RightBrace).after("method declarations")?;
        Ok(Stmt::class(ident.value, methods, class_methods, superclass))
    }

    fn function_statement(&mut self) -> Result<FunctionStmt> {
        let ident =
            self.expect(TokenType::Identifier).after("function name")?;
        let decl = self.function_declaration()?;
        Ok(FunctionStmt::new(ident.value, decl))
    }

    fn function_declaration(&mut self) -> Result<FunctionDecl> {
        // FIXME: These errors are weird.
        self.expect(TokenType::LeftParen).after("function name")?;
        let mut parameters = Vec::new();
        match self.peek_type()? {
            TokenType::RightParen => {},
            _ => {
                loop {
                    let param =
                        self.expect(TokenType::Identifier).after("parameters")?;
                    parameters.push(Variable::new_global(param.value.into()));
                    if parameters.len() > MAX_NUM_PARAMETERS {
                        // FIXME: This shouldn't stop parsing the function
                        return Err(SyntaxError::TooManyParameters);
                    }
                    if let TokenType::Comma = self.peek_type()? {
                        self.advance()?;
                    } else {
                        break;
                    }
                }
            },
        }
        self.expect(TokenType::RightParen).after("parameters")?;
        self.expect(TokenType::LeftBrace).before("function body")?;
        let block = self.block()?;
        Ok(FunctionDecl::new(parameters, block))
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_decl(&mut self) -> Result<Stmt> {
        let ident =
            self.expect(TokenType::Identifier).after("keyword 'var'")?;
        let mut initializer = Expr {
            pos: ident.position,
            node: ExprKind::Literal(Literal::Nil)
        };
        if let TokenType::Equal = self.peek_type()? {
            self.advance()?;
            initializer = self.expression()?;
        }
        self.expect(TokenType::Semicolon).after("variable declaration")?;
        Ok(Stmt::var(ident.value, initializer))
    }

    // statement  → exprStmt
    //            | ifStmt
    //            | printStmt
    //            | block ;
    fn statement(&mut self) -> Result<Stmt> {
        match self.peek_type()? {
            TokenType::Keyword(Keyword::While) => {
                self.advance()?;
                self.while_statement()
            },
            TokenType::Keyword(Keyword::Print) => {
                self.advance()?;
                self.print_statement()
            },
            TokenType::Keyword(Keyword::Return) => {
                self.advance()?;
                self.return_statement()
            },
            TokenType::Keyword(Keyword::If) => {
                self.advance()?;
                self.if_statement()
            },
            TokenType::Keyword(Keyword::For) => {
                self.advance()?;
                self.for_statement()
            },
            TokenType::Keyword(Keyword::Break) => {
                self.advance()?;
                self.expect(TokenType::Semicolon).after("break")?;
                Ok(Stmt::Break)
            },
            TokenType::LeftBrace => {
                self.advance()?;
                self.block().map(Stmt::Block)
            },
            _ => self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.expect(TokenType::Semicolon).after("expression")?;
        Ok(Stmt::Print(value))
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let expr = if let TokenType::Semicolon = self.peek_type()? {
            None
        } else {
            Some(self.expression()?)
        };
        self.expect(TokenType::Semicolon).after("return")?;
        Ok(Stmt::Return(expr))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.expect(TokenType::LeftParen).after("if")?;
        let cond = self.expression()?;
        self.expect(TokenType::RightParen).after("if condition")?;
        let then_clause = self.declaration()?;
        if let TokenType::Keyword(Keyword::Else) = self.peek_type()? {
            self.advance()?;
            let else_clause = self.declaration()?;
            Ok(Stmt::if_else_stmt(cond, then_clause, else_clause))
        } else {
            Ok(Stmt::if_stmt(cond, then_clause))
        }
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.expect(TokenType::LeftParen).after("while")?;
        let cond = self.expression()?;
        self.expect(TokenType::RightParen).after("while condition")?;
        let body = self.statement()
            .map_err(|_| SyntaxError::Expect("expression"))?;
        Ok(Stmt::While(cond, Box::new(body)))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.expect(TokenType::LeftParen).after("for")?;
        let init = match self.peek_type()? {
            TokenType::Semicolon => {
                self.advance()?;
                None
            },
            TokenType::Keyword(Keyword::Var) => {
                self.advance()?;
                Some(self.var_decl()?)
            },
            _ => Some(self.expression_statement()?),
        };

        let condition = match self.peek_type()? {
            TokenType::Semicolon => None,
            _ => Some(self.expression()?),
        };
        let condition_pos =
            self.expect(TokenType::Semicolon)
                .after("for conditon")?
                .position;

        let increment = match self.peek_type()? {
            TokenType::RightParen => None,
            _ => Some(self.expression()?),
        };
        self.expect(TokenType::RightParen).after("for clause")?;

        let mut body = self.statement()?;

        // Desugar into while loop
        body = match increment {
            Some(increment) => {
                Stmt::Block(vec![body, Stmt::Expr(increment)])
            },
            None => body,
        };
        let condition_expr = condition.unwrap_or_else(|| {
            Expr {
                pos: condition_pos,
                node: ExprKind::Literal(Literal::True),
            }
        });

        let while_loop = Stmt::While(condition_expr, Box::new(body));
        match init {
            Some(init) => Ok(Stmt::Block(vec![init, while_loop])),
            None => Ok(while_loop),
        }
    }

    // block  → '{' declaration * '}'
    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut block = Vec::new();
        loop {
            if let TokenType::RightBrace = self.peek_type()? {
                self.advance()?;
                return Ok(block);
            }
            let stmt = self.declaration()?;
            block.push(stmt);
        }
    }

    fn expect<'a>(&'a mut self, expected: TokenType<'t>) -> Expect<'a, 't> {
        Expect::new(self, expected)
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon).after("expression")?;
        Ok(Stmt::Expr(expr))
    }

    // expression → equality
    pub fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.logical_or()?;
        if let TokenType::Equal = self.peek_type()? {
            self.advance()?;
            let value = self.assignment()?;
            if let ExprKind::Var(var) = expr.node {
                let node = ExprKind::Assign(var, Box::new(value));
                return Ok(Expr { node, pos: expr.pos });
            } else if let ExprKind::Get(expr, name) = expr.node {
                let pos = expr.pos;
                let node = ExprKind::set(expr, name, value);
                return Ok(Expr { node, pos });
            }
            return Err(SyntaxError::InvalidAssignment);
        }
        Ok(expr)
    }

    logical_impl!(logical_or, logical_and, TokenType::Keyword(Keyword::Or));
    logical_impl!(logical_and, equality, TokenType::Keyword(Keyword::And));

    // equality   → comparison ( ( "!=" | "==" ) comparison )*
    binary_impl!(equality, comparison, TokenType::BangEq | TokenType::EqualEq);
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
    binary_impl!(comparison, term,
        TokenType::GreaterThan
        | TokenType::GreaterThanEq
        | TokenType::LessThan
        | TokenType::LessThanEq);
    // term       → factor ( ( "-" | "+" ) factor )*
    binary_impl!(term, factor, TokenType::Plus | TokenType::Minus);
    // factor     → unary ( ( "/" | "*" ) unary )*
    binary_impl!(factor, unary, TokenType::Slash | TokenType::Star);

    // unary      → ( "!" | "-" ) unary
    //            | primary
    fn unary(&mut self) -> Result<Expr> {
        match self.peek_type()? {
            TokenType::Bang | TokenType::Minus => {
                let tok = self.advance()?;
                let operator = tok.ty.into_unary().unwrap();
                let unary = self.unary()?;
                let node = ExprKind::unary(operator, unary);
                Ok(Expr { node, pos: tok.position })
            }
            _ => self.call()
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        loop {
            match self.peek_type()? {
                TokenType::LeftParen => {
                    self.advance()?;
                    expr = self.finish_call(expr)?;
                },
                TokenType::Dot => {
                    let pos = self.advance()?.position;
                    let name = self.expect(TokenType::Identifier)
                        .alias_as("property name")
                        .after("'.'")?;
                    let node = ExprKind::get(expr, name.value);
                    expr = Expr { node, pos };
                },
                _ => break,
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = Vec::new();
        match self.peek_type()? {
            TokenType::RightParen => {},
            _ => {
                loop {
                    // FIXME: This shouldn't stop parsing the call
                    arguments.push(self.expression()?);
                    if arguments.len() > MAX_NUM_PARAMETERS {
                        return Err(SyntaxError::TooManyArguments);
                    }
                    match self.peek_type()? {
                        TokenType::Comma => {
                            self.advance()?;
                        },
                        _ => break,
                    }
                }
            },
        }

        let paren = self.expect(TokenType::RightParen).after("arguments")?;
        Ok(Expr {
            pos: callee.pos,
            node: ExprKind::call(callee, paren.position, arguments)
        })
    }

    // primary    → NUMBER | STRING | "false" | "true" | "nil"
    //            | "(" expression ")"
    //            | IDENTIFIER
    fn primary(&mut self) -> Result<Expr> {
        let peek_type = self.peek_type()?;
        match peek_type {
            TokenType::Keyword(Keyword::Super) => {
                let keyword = self.advance()?;
                self.expect(TokenType::Dot).after("'super'")?;
                let ident =
                    self.expect(TokenType::Identifier)
                        .alias_as("superclass method name")
                        .check()?;
                let var = Variable::new_local("super");
                let node = ExprKind::Super(var, keyword.position, ident.value.to_owned());
                let pos = keyword.position;
                Ok(Expr { node, pos })
            },
            TokenType::Keyword(Keyword::This) => {
                let var = Variable::new_local("this");
                let previous = self.advance()?;
                let node = ExprKind::This(var, previous.position);
                Ok(Expr {
                    node,
                    pos: previous.position,
                })
            },
            TokenType::Keyword(Keyword::Nil) => {
                let pos = self.advance()?.position;
                let node = ExprKind::Literal(Literal::Nil);
                Ok(Expr { node, pos })
            },
            TokenType::Keyword(Keyword::True) => {
                let pos = self.advance()?.position;
                let node = ExprKind::Literal(Literal::True);
                Ok(Expr { node, pos })
            },
            TokenType::Keyword(Keyword::False) => {
                let pos = self.advance()?.position;
                let node = ExprKind::Literal(Literal::False);
                Ok(Expr { node, pos })
            },
            TokenType::String(s) => {
                let pos = self.advance()?.position;
                let node = ExprKind::Literal(Literal::String(s.into()));
                Ok(Expr { node, pos })
            },
            TokenType::Number(n) => {
                let pos = self.advance()?.position;
                let node = ExprKind::Literal(Literal::Number(n));
                Ok(Expr { node, pos })
            },
            TokenType::LeftParen => {
                let pos = self.advance()?.position;
                let expr = self.expression()?;
                self.expect(TokenType::RightParen).after("expression")?;
                let node = ExprKind::Grouping(Box::new(expr));
                Ok(Expr { node, pos })
            },
            TokenType::Identifier => {
                let token = self.advance()?;
                let var = Variable::new_global(token.value.into());
                let node = ExprKind::Var(var);
                Ok(Expr { pos: token.position, node })
            },
            TokenType::Keyword(Keyword::Fun) => {
                let token = self.advance()?;
                let declaration = self.function_declaration()?;
                let node = ExprKind::function(declaration);
                Ok(Expr {
                    node,
                    pos: token.position
                })
            },
            _ => Err(SyntaxError::Expect("expression"))
        }
    }

    /// Discards tokens until a statement or expression boundary.
    fn synchronize(&mut self) {
        loop {
            match self.peek_type() {
                Ok(ty) => {
                    match ty {
                        TokenType::Semicolon => {
                            self.advance().unwrap();
                            return;
                        },
                        TokenType::Keyword(Keyword::Class)
                        | TokenType::Keyword(Keyword::Fun)
                        | TokenType::Keyword(Keyword::Var)
                        | TokenType::Keyword(Keyword::For)
                        | TokenType::Keyword(Keyword::If)
                        | TokenType::Keyword(Keyword::While)
                        | TokenType::Keyword(Keyword::Return)
                        | TokenType::Keyword(Keyword::Print)
                        | TokenType::EOF => return,
                        _ => { self.advance().unwrap(); }
                    }
                },
                // We want to skip scanning errors as well.
                //
                // This will never loop with Err(UnexpectedEOF) because the happy
                // Ok(EOF) case will be picked up first.
                Err(_) => { self.advance().unwrap_err(); }
            }
        }
    }

    fn advance(&mut self) -> Result<Token<'t>> {
        // FIXME: Don't unwrap
        self.scanner.next().unwrap_or_else(|| Err(SyntaxError::UnexpectedEOF))
    }

    fn has_next(&mut self) -> bool {
        match self.peek_type() {
            Ok(ty) => ty != TokenType::EOF,
            Err(_) => true,
        }
    }

    fn peek_type(&mut self) -> Result<TokenType<'t>> {
        match self.scanner.peek() {
            Some(&Ok(tok)) => Ok(tok.ty),
            Some(&Err(ref err)) => Err(err.clone()),
            None => Ok(TokenType::EOF),
        }
    }
}

struct Expect<'a, 't> {
    parser: &'a mut Parser<'t>,
    expected: TokenType<'t>,
    alias: Option<&'static str>,
}

impl<'a, 't> Expect<'a, 't> {
    fn new(parser: &'a mut Parser<'t>, expected: TokenType<'t>) -> Self {
        Self {
            parser,
            expected,
            alias: None,
        }
    }
}

macro_rules! impl_expected {
    ($sel:ident) => {
        {
            if $sel.parser.peek_type()? == $sel.expected {
                return $sel.parser.advance();
            }
            $sel.alias
                .map(|s| s)
                .unwrap_or_else(|| $sel.expected.name())
        }
    }
}

impl<'a, 't> Expect<'a, 't> {
    fn alias_as(mut self, name: &'static str) -> Self {
        self.alias = Some(name);
        self
    }

    fn after(self, message: &'static str) -> Result<Token<'t>> {
        let expected = impl_expected!(self);
        Err(SyntaxError::ExpectAfter(expected, message))
    }

    fn before(self, message: &'static str) -> Result<Token<'t>> {
        let expected = impl_expected!(self);
        Err(SyntaxError::ExpectBefore(expected, message))
    }

    fn check(self) -> Result<Token<'t>> {
        let expected = impl_expected!(self);
        Err(SyntaxError::Expect(expected))
    }
}

#[cfg(test)]
mod tests {
    // use super::Parser;
    // use super::ast::{UnaryOperator,BinaryOperator,Stmt};
    // use super::ast::dsl::*;
    // use super::errors::SyntaxError;
    //
    // #[test]
    // fn decimal_point_at_eof() {
    //     let prog = "123.";
    //
    //     let mut parser = Parser::new(prog);
    //     let errs = parser.parse().unwrap_err();
    // }
    //
    // #[test]
    // fn expression() {
    //     let expressions = [
    //         "1",
    //         "1 + -1 * (1 + 1)"
    //     ];
    //     for expr in &expressions {
    //         Parser::new(expr).expression().unwrap();
    //     }
    // }
    //
    // #[test]
    // fn expression_statement() {
    //     let prog = r#"
    //     1;
    //     "foobar";
    //     print (1 + 2) * -3;
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::Expr(number(1.0)),
    //         Stmt::Expr(string("foobar")),
    //         Stmt::Print(binary(
    //             BinaryOperator::Star,
    //             grouping(binary(
    //                 BinaryOperator::Plus,
    //                 number(1.0),
    //                 number(2.0),
    //             )),
    //             unary(UnaryOperator::Minus, number(3.0)),
    //         )),
    //     ], statements);
    // }
    //
    // #[test]
    // fn declaration() {
    //     let prog = r#"
    //     var a;
    //     var b = 1 + 1;
    //     a = 1;
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::var("a", nil()),
    //         Stmt::var("b", binary(BinaryOperator::Plus, number(1.0), number(1.0))),
    //         Stmt::Expr(assign("a", number(1.0)))
    //     ], statements);
    // }
    //
    // #[test]
    // fn block() {
    //     let prog = r#"
    //     {
    //         var a = 1;
    //         print a;
    //     }
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::Block(vec![
    //             Stmt::var("a", number(1.0)),
    //             Stmt::Print(var("a")),
    //         ])
    //     ], statements);
    // }
    //
    // #[test]
    // fn synchronize() {
    //     let prog = r#"
    //     var a;
    //     var b = 1 + 1;
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::var("a", nil()),
    //         Stmt::var("b", binary(BinaryOperator::Plus, number(1.0), number(1.0)))
    //     ], statements);
    // }
    //
    // #[test]
    // fn parse_with_comments() {
    //     let prog = r#"
    //     // Some insightful remark
    //     1 + 1;
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::Expr(binary(
    //             BinaryOperator::Plus,
    //             number(1.0),
    //             number(1.0),
    //         )),
    //     ], statements);
    // }
    //
    // #[test]
    // fn parse_if_statement() {
    //     let prog = r#"
    //     if (true)
    //         print "this";
    //     else
    //         print "that";
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::if_else_stmt(
    //             truelit(),
    //             Stmt::Print(string("this")),
    //             Stmt::Print(string("that"))
    //         )
    //     ], statements);
    // }
    //
    // #[test]
    // fn parse_while_loop() {
    //     let prog = r#"
    //     while (a > 1) {
    //         print "body";
    //     }
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::While(
    //             binary(BinaryOperator::GreaterThan, var("a"), number(1.0)),
    //             Box::new(Stmt::Block(vec![
    //                 Stmt::Print(string("body"))
    //             ]))
    //         )
    //     ], statements);
    // }
    //
    // #[test]
    // fn break_statement() {
    //     let prog = r#"
    //     while (true) {
    //         break;
    //     }
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     let statements = parser.parse().unwrap();
    //     assert_eq!(vec![
    //         Stmt::While(
    //             truelit(),
    //             Box::new(Stmt::Block(vec![Stmt::Break]))
    //         )
    //     ], statements);
    // }
    //
    // #[test]
    // fn call() {
    //     let prog = r#"
    //     call();
    //     call(nil, 1, "two");
    //     call(nested_one(nested_two(3)));
    //     call(1)(2)("three");
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     parser.parse().unwrap();
    // }
    //
    // #[test]
    // fn function() {
    //     let prog = r#"
    //         fun hello() {
    //             print "hello";
    //         }
    //
    //         fun increment(a) {
    //             a + 1;
    //         }
    //
    //         fun increment(a) {
    //             return 1;
    //         }
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     parser.parse().unwrap();
    // }
    //
    // #[test]
    // fn property_get() {
    //     let prog = r#"
    //         a.field;
    //         chained.field.access;
    //         a.method.call();
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     parser.parse().unwrap();
    // }
    //
    // #[test]
    // fn property_set() {
    //     let prog = r#"
    //         a.field = 1;
    //         a.chain.of.access = "works";
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     parser.parse().unwrap();
    // }
    //
    // #[test]
    // fn class() {
    //     let prog = r#"
    //         class Cake {
    //             bake() {
    //                 print "Baking...";
    //             }
    //
    //             class classMethod() {
    //                 print "Static...";
    //             }
    //         }
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     parser.parse().unwrap();
    // }
    //
    // #[test]
    // fn superclass() {
    //     let prog = r#"
    //         class A < B {
    //             init() {}
    //         }
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     parser.parse().unwrap();
    // }
    //
    // #[test]
    // fn lambda() {
    //     let prog = r#"
    //         var p = fun () { print x; };
    //         fun () {};
    //     "#;
    //
    //     let mut parser = Parser::new(prog);
    //     parser.parse().unwrap();
    // }
}
