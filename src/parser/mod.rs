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

use std::iter::Peekable;

use errors::*;
use errors::Result;
use value::Value;
use self::scanner::Scanner;
use self::ast::{Expr, Stmt};

use self::scanner::Token;
use self::scanner::TokenType;
pub use self::scanner::Keyword;

pub mod ast;
mod scanner;

pub struct Parser<'t> {
    scanner: Peekable<Scanner<'t>>,
    current: usize,
}

// Encapsulates rules with the following form:
// name   → inner ( ( opA | obB | ... ) inner )*
macro_rules! binary_expr_impl (
    ($name:ident, $inner:ident, $($pattern:pat)|*) => (
        fn $name(&mut self) -> Result<Expr<'t>> {
            let mut expr = self.$inner()?;
            while let Ok(Token{ref ty, ..}) = self.peek() {
                let tok = match *ty {
                    $($pattern)|* => { self.advance() },
                    _ => break,
                };
                let rhs = self.$inner()?;
                let operator = tok.ty.into_binary().unwrap();
                expr = Expr::binary(operator, expr, rhs);
            }
            Ok(expr)
        }
    );
);

impl<'t> Parser<'t> {
    pub fn new(program: &'t str) -> Self {
        let scanner = Scanner::new(program);
        Parser {
            scanner: scanner.peekable(),
            current: 0,
        }
    }

    // program → declaration* eof ;
    pub fn parse(&mut self) -> Result<Vec<Stmt<'t>>> {
        let mut statements = Vec::new();
        while self.has_next() {
            // TODO: This should return the errors encountered line by line
            // in one pass
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(_)   => self.synchronize(),
            }
        }
        Ok(statements)
    }

    // declaration → varDecl
    //             | statement ;
    fn declaration(&mut self) -> Result<Stmt<'t>> {
        if let TokenType::Keyword(Keyword::Var) = self.peek_type()? {
            self.advance();
            return self.var_decl();
        }
        self.statement()
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_decl(&mut self) -> Result<Stmt<'t>> {
        let ident = self.expect(TokenType::Identifier, "Expected identifier after keyword 'var'")?;
        let mut initializer = Expr::Literal(Value::Nil);
        if let TokenType::Equal = self.peek_type()? {
            self.advance();
            initializer = self.expression()?;
        }
        self.expect(TokenType::Semicolon, "Expected semicolon after variable declaration")?;
        Ok(Stmt::Var(ident.value, initializer))
    }

    // statement  → exprStmt
    //            | ifStmt
    //            | printStmt
    //            | block ;
    fn statement(&mut self) -> Result<Stmt<'t>> {
        match self.peek_type()? {
            TokenType::Keyword(Keyword::While) => {
                self.advance();
                self.while_statement()
            },
            TokenType::Keyword(Keyword::Print) => {
                self.advance();
                self.print_statement()
            },
            TokenType::Keyword(Keyword::If) => {
                self.advance();
                self.if_statement()
            },
            TokenType::Keyword(Keyword::For) => {
                self.advance();
                self.for_statement()
            },
            TokenType::LeftBrace => {
                self.advance();
                self.block()
            },
            _ => self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt<'t>> {
        let value = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after value")?;
        Ok(Stmt::Print(value))
    }

    fn if_statement(&mut self) -> Result<Stmt<'t>> {
        self.expect(TokenType::LeftParen, "Expected '(' after if")?;
        let cond = self.expression()?;
        self.expect(TokenType::RightParen, "Expected ')' after if condition")?;
        let then_clause = self.declaration()?;
        if let TokenType::Keyword(Keyword::Else) = self.peek_type()? {
            self.advance();
            let else_clause = self.declaration()?;
            Ok(Stmt::if_else_stmt(cond, then_clause, else_clause))
        } else {
            Ok(Stmt::if_stmt(cond, then_clause))
        }
    }

    fn while_statement(&mut self) -> Result<Stmt<'t>> {
        self.expect(TokenType::LeftParen, "Expected '(' after while")?;
        let cond = self.expression()?;
        self.expect(TokenType::RightParen, "Expected ')' after while condition")?;
        let body = self.statement()?;
		Ok(Stmt::While(cond, Box::new(body)))
    }

    fn for_statement(&mut self) -> Result<Stmt<'t>> {
        self.expect(TokenType::LeftParen, "Expected '(' after for")?;
        let init = match self.peek_type()? {
            TokenType::Semicolon => {
                self.advance();
                None
            },
            TokenType::Keyword(Keyword::Var) => {
                self.advance();
                Some(self.var_decl()?)
            },
            _ => Some(self.expression_statement()?),
        };

        let condition = match self.peek_type()? {
            TokenType::Semicolon => Expr::Literal(Value::True),
            _ => self.expression()?,
        };
        self.expect(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = match self.peek_type()? {
            TokenType::RightParen => None,
            _ => Some(self.expression()?),
        };
        self.expect(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        // Desugar into while loop
        body = match increment {
            Some(increment) => {
                Stmt::Block(vec![body, Stmt::Expr(increment)])
            },
            None => body,
        };

        let while_loop = Stmt::While(condition, Box::new(body));
        match init {
            Some(init) => Ok(Stmt::Block(vec![init, while_loop])),
            None => Ok(while_loop),
        }
    }

    // block  → '{' declaration * '}'
    fn block(&mut self) -> Result<Stmt<'t>> {
        let mut block = Vec::new();
        loop {
            if let TokenType::RightBrace = self.peek_type()? {
                self.advance();
                return Ok(Stmt::Block(block));
            }
            let stmt = self.declaration()?;
            block.push(stmt);
        }
    }

    fn expect(&mut self, token_type: TokenType, msg: &str) -> Result<Token<'t>> {
        if self.peek_type()? == token_type {
            let tok = self.advance();
            Ok(tok)
        } else {
            Err(msg.into())
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt<'t>> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon, "Expected ';' after value")?;
        Ok(Stmt::Expr(expr))
    }

    // expression → equality
    pub fn expression(&mut self) -> Result<Expr<'t>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'t>> {
        let expr = self.equality()?;
        if let TokenType::Equal = self.peek_type()? {
            self.advance();
            let value = self.assignment()?;
            if let Expr::Var(name) = expr {
                return Ok(Expr::Assign(name, Box::new(value)));
            }
            return Err("Invalid assignment target".into());
        }
        Ok(expr)
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
            TokenType::Bang | TokenType::Minus => {
                let tok = self.advance();
                let operator = tok.ty.into_unary().unwrap();
                let unary = self.unary()?;

                Ok(Expr::unary(operator, unary))
            }
            _ => self.primary()
        }
    }

    // primary    → NUMBER | STRING | "false" | "true" | "nil"
    //            | "(" expression ")"
    //            | IDENTIFIER
    fn primary(&mut self) -> Result<Expr<'t>> {
        let peek_type = self.peek_type()?;
        match peek_type {
            TokenType::Keyword(Keyword::Nil) => {
                self.advance();
                Ok(Expr::Literal(Value::Nil))
            },
            TokenType::Keyword(Keyword::True) => {
                self.advance();
                Ok(Expr::Literal(Value::True))
            },
            TokenType::Keyword(Keyword::False) => {
                self.advance();
                Ok(Expr::Literal(Value::False))
            },
            TokenType::String(s) => {
                self.advance();
                Ok(Expr::Literal(Value::String(s.into())))
            },
            TokenType::Number(n) => {
                self.advance();
                Ok(Expr::Literal(Value::Number(n)))
            },
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                match self.peek_type()? {
                    TokenType::RightParen => {
                        self.advance();
                        Ok(Expr::Grouping(Box::new(expr)))
                    }
                    _ => Err("Expect ')' after expression".into()),
                }
            },
            TokenType::Identifier => {
                let token = self.advance();
                Ok(Expr::Var(token.value))
            },
            _ => Err("Expected a literal or parenthesized expression".into())
        }
    }

    /// Discards tokens until a statement or expression boundary.
    fn synchronize(&mut self) {
        while self.peek_type().is_ok() {
            let token = self.advance();
            // We are already at an explicit statement boundary
            if token.ty == TokenType::Semicolon {
                return;
            }
            // If the next token is a keyword we consider this an implicit
            // statement boundary
            let peek_type = self.peek_type().unwrap();
            match peek_type {
                TokenType::Keyword(Keyword::Class)
                | TokenType::Keyword(Keyword::Fun)
                | TokenType::Keyword(Keyword::Var)
                | TokenType::Keyword(Keyword::For)
                | TokenType::Keyword(Keyword::If)
                | TokenType::Keyword(Keyword::While)
                | TokenType::Keyword(Keyword::Return)
                | TokenType::Keyword(Keyword::Print) => return,
                _ => {},
            }
        }
    }

    fn advance(&mut self) -> Token<'t> {
        // FIXME: Don't unwrap
        let token = self.scanner.next().unwrap().unwrap();
        self.current += 1;
        token
    }

    fn has_next(&mut self) -> bool {
        self.scanner.peek().is_some()
    }

    fn peek(&mut self) -> Result<Token<'t>> {
        match self.scanner.peek() {
            Some(&Ok(tok)) => Ok(tok),
            // FIXME: Pass up the error
            Some(&Err(_)) => Err("There was a scanning error".into()),
            None => Err(ErrorKind::UnexpectedEOF.into()),
        }
    }

    fn peek_type(&mut self) -> Result<TokenType<'t>> {
        self.peek().map(|t| t.ty)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use super::ast::{UnaryOperator,BinaryOperator,Stmt,Expr};
    use super::ast::dsl::*;

    #[test]
    fn expression() {
        let prog = r#"
        1;
        "foobar";
        print (1 + 2) * -3;
        "#;

        let mut parser = Parser::new(prog);
        let statements = parser.parse().unwrap();
        assert_eq!(vec![
            Stmt::Expr(number(1.0)),
            Stmt::Expr(string("foobar")),
            Stmt::Print(binary(
                BinaryOperator::Star,
                grouping(binary(
                    BinaryOperator::Plus,
                    number(1.0),
                    number(2.0),
                )),
                unary(UnaryOperator::Minus, number(3.0)),
            )),
        ], statements);
    }

    #[test]
    fn declaration() {
        let prog = r#"
        var a;
        var b = 1 + 1;
        a = 1;
        "#;

        let mut parser = Parser::new(prog);
        let statements = parser.parse().unwrap();
        assert_eq!(vec![
            Stmt::Var("a", nil()),
            Stmt::Var("b", binary(BinaryOperator::Plus, number(1.0), number(1.0))),
            Stmt::Expr(Expr::Assign("a", Box::new(number(1.0))))
        ], statements);
    }

    #[test]
    fn block() {
        let prog = r#"
        {
            var a = 1;
            print a;
        }
        "#;

        let mut parser = Parser::new(prog);
        let statements = parser.parse().unwrap();
        assert_eq!(vec![
            Stmt::Block(vec![
                Stmt::Var("a", number(1.0)),
                Stmt::Print(Expr::Var("a")),
            ])
        ], statements);
    }

    #[test]
    fn synchronize() {
        let prog = r#"
        var a;
        var b = 1 + 1;
        "#;

        let mut parser = Parser::new(prog);
        let statements = parser.parse().unwrap();
        assert_eq!(vec![
            Stmt::Var("a", nil()),
            Stmt::Var("b", binary(BinaryOperator::Plus, number(1.0), number(1.0)))
        ], statements);
    }

    #[test]
    fn parse_with_comments() {
        let prog = r#"
        // Some insightful remark
        1 + 1;
        "#;

        let mut parser = Parser::new(prog);
        let statements = parser.parse().unwrap();
        assert_eq!(vec![
            Stmt::Expr(binary(
                BinaryOperator::Plus,
                number(1.0),
                number(1.0),
            )),
        ], statements);
    }

    #[test]
    fn parse_if_statement() {
        let prog = r#"
        if (true)
            print "this";
        else
            print "that";
        "#;

        let mut parser = Parser::new(prog);
        let statements = parser.parse().unwrap();
        assert_eq!(vec![
            Stmt::if_else_stmt(
                truelit(),
                Stmt::Print(string("this")),
                Stmt::Print(string("that"))
            )
        ], statements);
    }

    #[test]
    fn parse_while_loop() {
        let prog = r#"
        while (a > 1) {
            print "body";
        }
        "#;

        let mut parser = Parser::new(prog);
        let statements = parser.parse().unwrap();
        assert_eq!(vec![
            Stmt::While(
                binary(BinaryOperator::GreaterThan, var("a"), number(1.0)),
                Box::new(Stmt::Block(vec![
                    Stmt::Print(string("body"))
                ]))
            )
        ], statements);
    }
}
