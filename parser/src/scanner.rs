use std::str;
use std::str::CharIndices;

use super::ast::{BinaryOperator, UnaryOperator, LogicalOperator};
use super::peek::Peek2;
use super::errors::*;

type Result<T> = ::std::result::Result<T, SyntaxError>;

#[derive(Debug, Clone, Copy)]
pub(super) struct Token<'a> {
    pub ty: TokenType<'a>,
    pub value: &'a str,
    pub position: Position,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Position {
    pub start: usize,
    pub end: usize,
    pub line: usize,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub(super) enum TokenType<'s> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Bang,
    BangEq,
    Equal,
    EqualEq,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Slash,
    Comment,
    String(&'s str),
    Number(f64),
    Identifier,
    Keyword(Keyword),
    EOF,
}

use std::fmt::{self, Display};

impl<'s> Display for TokenType<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::String(ref s) => {
                write!(f, "{:?}", s)?;
                return Ok(());
            },
            TokenType::Number(ref n) => {
                write!(f, "number '{}'", n)?;
                return Ok(());
            },
            TokenType::Keyword(kw) => {
                write!(f, "keyword '{}'", kw)?;
                return Ok(());
            }
            _ => write!(f, "{}", self.name()),
        }
    }
}

impl<'s> TokenType<'s> {
    pub(super) fn into_unary(&self) -> Option<UnaryOperator> {
        match *self {
            TokenType::Bang => Some(UnaryOperator::Bang),
            TokenType::Minus => Some(UnaryOperator::Minus),
            _ => None,
        }
    }

    pub(super) fn into_binary(&self) -> Option<BinaryOperator> {
        match *self {
            TokenType::EqualEq => Some(BinaryOperator::Equal),
            TokenType::BangEq => Some(BinaryOperator::BangEq),
            TokenType::GreaterThan => Some(BinaryOperator::GreaterThan),
            TokenType::GreaterThanEq => Some(BinaryOperator::GreaterThanEq),
            TokenType::LessThan => Some(BinaryOperator::LessThan),
            TokenType::LessThanEq => Some(BinaryOperator::LessThanEq),
            TokenType::Minus => Some(BinaryOperator::Minus),
            TokenType::Plus => Some(BinaryOperator::Plus),
            TokenType::Slash => Some(BinaryOperator::Slash),
            TokenType::Star => Some(BinaryOperator::Star),
            _ => None
        }
    }

    pub(super) fn into_logical(&self) -> Option<LogicalOperator> {
        match *self {
            TokenType::Keyword(Keyword::And) => Some(LogicalOperator::And),
            TokenType::Keyword(Keyword::Or) => Some(LogicalOperator::Or),
            _ => None
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            TokenType::LeftParen => "'('",
            TokenType::RightParen => "')'",
            TokenType::LeftBrace => "'{'",
            TokenType::RightBrace => "'}'",
            TokenType::Comma => "','",
            TokenType::Dot => "'.'",
            TokenType::Minus => "'-'",
            TokenType::Plus => "'+'",
            TokenType::Semicolon => "';'",
            TokenType::Star => "'*'",
            TokenType::Bang => "'!'",
            TokenType::BangEq => "'!='",
            TokenType::Equal => "'='",
            TokenType::EqualEq => "'=='",
            TokenType::LessThan => "'<'",
            TokenType::LessThanEq => "'<='",
            TokenType::GreaterThan => "'>'",
            TokenType::GreaterThanEq => "'>='",
            TokenType::Slash => "'/'",
            TokenType::Comment => "<comment>",
            TokenType::String(_) => "string",
            TokenType::Number(_) => "number",
            TokenType::Identifier => "identifier",
            TokenType::Keyword(kw) => kw.name(),
            TokenType::EOF => "EOF",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Keyword {
    Class,
    Var,
    And,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    While,
    Break,
}

impl Keyword {
    fn name(&self) -> &'static str {
        match *self {
            Keyword::Class  => "class",
            Keyword::Var    => "var",
            Keyword::And    => "and",
            Keyword::Else   => "else",
            Keyword::False  => "false",
            Keyword::For    => "for",
            Keyword::Fun    => "fun",
            Keyword::If     => "if",
            Keyword::Nil    => "nil",
            Keyword::Or     => "or",
            Keyword::Print  => "print",
            Keyword::Return => "return",
            Keyword::Super  => "super",
            Keyword::This   => "this",
            Keyword::True   => "true",
            Keyword::While  => "while",
            Keyword::Break  => "break",
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl str::FromStr for Keyword {
    type Err = ();

    fn from_str(word: &str) -> ::std::result::Result<Self, Self::Err> {
        match word {
            "class"  => Ok(Keyword::Class),
            "var"    => Ok(Keyword::Var),
            "and"    => Ok(Keyword::And),
            "else"   => Ok(Keyword::Else),
            "false"  => Ok(Keyword::False),
            "for"    => Ok(Keyword::For),
            "fun"    => Ok(Keyword::Fun),
            "if"     => Ok(Keyword::If),
            "nil"    => Ok(Keyword::Nil),
            "or"     => Ok(Keyword::Or),
            "print"  => Ok(Keyword::Print),
            "return" => Ok(Keyword::Return),
            "super"  => Ok(Keyword::Super),
            "this"   => Ok(Keyword::This),
            "true"   => Ok(Keyword::True),
            "while"  => Ok(Keyword::While),
            "break"  => Ok(Keyword::Break),
            _ => Err(()),
        }
    }
}

pub(super) struct Scanner<'a> {
    source: &'a str,
    iter: Peek2<CharIndices<'a>>,
    current: usize,
    line: usize,
    at_eof: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source,
            iter: Peek2::new(source.char_indices()),
            current: 0,
            line: 1,
            at_eof: false,
        }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.iter.next().map(|(current, c)| {
            self.current = current;
            if c == '\n' {
                self.line += 1;
            }
            (current, c)
        })
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|&(_, c)| c)
    }

    fn peek_next(&mut self) -> Option<char> {
        self.iter.peek_next().map(|&(_, c)| c)
    }

    fn token_contents(&mut self, start: usize) -> &'a str {
        let end = self.iter.peek().map(|&(i, _)| i).unwrap_or(self.source.len());
        &self.source[start..end].trim_end()
    }

    fn eatwhitespace(&mut self) {
        self.advance_while(|&c| {
            c == '\n' || c == '\t' || c == ' '
        });
    }

    pub fn eof(&mut self) -> Token<'a> {
        self.yield_token(self.source.len(), TokenType::EOF)
    }

    fn scan_token(&mut self) -> Option<Result<Token<'a>>> {
        self.eatwhitespace();

        let c = self.advance();
        if c.is_none() {
            if !self.at_eof {
                self.at_eof = true;
                return Some(Ok(self.eof()));
            }
            return None
        }
        let (start, c) = c.unwrap();

        let ty = match c {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            '/' => {
                if let Some('/') = self.peek() {
                    // This is a comment.
                    self.advance_while(|&c| c != '\n');
                    self.advance();
                    TokenType::Comment
                } else {
                    TokenType::Slash
                }
            },
            '!' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenType::BangEq
                } else {
                    TokenType::Bang
                }
            },
            '>' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenType::GreaterThanEq
                } else {
                    TokenType::GreaterThan
                }
            },
            '<' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenType::LessThanEq
                } else {
                    TokenType::LessThan
                }
            },
            '=' => {
                if let Some('=') = self.peek() {
                    self.advance();
                    TokenType::EqualEq
                } else {
                    TokenType::Equal
                }
            },
            '"' => {
                match self.string(start) {
                    Ok(ty) => ty,
                    Err(err) => return Some(Err(err)),
                }
            },
            _ if c.is_digit(10) => {
                match self.number(start) {
                    Ok(ty) => ty,
                    Err(err) => return Some(Err(err)),
                }
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                self.identifier(start)
            },
            c => return Some(Err(SyntaxError::UnexpectedChar(c))),
        };

        let token = self.yield_token(start, ty);
        Some(Ok(token))
    }

    fn yield_token(&mut self, start: usize, ty: TokenType<'a>) -> Token<'a> {
        let token_contents = self.token_contents(start);
        let token_len = token_contents.len();
        let position = Position {
            start: start,
            end: start + token_len,
            line: self.line,
        };
        Token {
            ty: ty,
            value: token_contents,
            position: position,
        }
    }

    fn advance_while<F>(&mut self, f: F) -> usize where for<'r> F: Fn(&'r char,) -> bool {
        let mut count = 0;
        while let Some(c) = self.peek() {
            if f(&c) {
                self.advance();
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    fn identifier(&mut self, start: usize) -> TokenType<'a> {
        self.advance_while(|&c| {
            ('a' <= c && c <= 'z') ||
            ('A' <= c && c <= 'Z') ||
            ('0' <= c && c <= '9') ||
            c == '_'
        });
        let word = self.token_contents(start);
        str::parse::<Keyword>(word)
            .map(TokenType::Keyword)
            .unwrap_or(TokenType::Identifier)
    }

    fn number(&mut self, start: usize) -> Result<TokenType<'a>> {
        self.advance_while(|c| c.is_digit(10));
        if let Some('.') = self.peek() {
            let cont = self.peek_next().map(|c| c.is_digit(10)).unwrap_or(false);
            if cont {
                self.advance();
                let count = self.advance_while(|&c| c.is_digit(10));
                if count == 0 {
                    return Err(SyntaxError::UnexpectedChar('.'));
                }
            }
        };
        let num = self.token_contents(start).parse::<f64>().unwrap();
        Ok(TokenType::Number(num))
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn string(&mut self, start: usize) -> Result<TokenType<'a>> {
        self.advance_while(|&c| c != '"');
        if self.is_at_end() {
            return Err(SyntaxError::UnterminatedString);
        }
        // consume the "
        self.advance();
        let token_contents = self.token_contents(start);
        Ok(TokenType::String(token_contents.trim_matches('"')))
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(item) = self.scan_token() {
            match item {
                Ok(t) if t.ty != TokenType::Comment => return Some(Ok(t)),
                Err(e) => return Some(Err(e)),
                _ => {},
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_types() {
        let prog = r#"
        var a = 1;
        // This is a comment
        "Some string";
        {
            a.method_call(1, 2);
        }
        // Doesn't need to be syntactically correct
        + - * /
        ! != ==
        > >= < <=
        "#;
        let tokens = Scanner::new(prog).collect::<Result<Vec<_>>>().unwrap();
        let types = tokens.iter().map(|t| t.ty).collect::<Vec<_>>();
        assert_eq!(&[
            TokenType::Keyword(Keyword::Var),
            TokenType::Identifier,
            TokenType::Equal,
            TokenType::Number(1.0),
            TokenType::Semicolon,
            TokenType::String("Some string"),
            TokenType::Semicolon,
            TokenType::LeftBrace,
            TokenType::Identifier,
            TokenType::Dot,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::Number(1.0),
            TokenType::Comma,
            TokenType::Number(2.0),
            TokenType::RightParen,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Bang,
            TokenType::BangEq,
            TokenType::EqualEq,
            TokenType::GreaterThan,
            TokenType::GreaterThanEq,
            TokenType::LessThan,
            TokenType::LessThanEq,
            TokenType::EOF,
        ], &types[..]);
    }

    #[test]
    fn test_numbers() {
        let prog = "1 2.0 3.14159";
        let tokens = Scanner::new(prog).collect::<Result<Vec<_>>>().unwrap();
        let types = tokens.iter().map(|t| t.ty).collect::<Vec<_>>();
        assert_eq!(&[
            TokenType::Number(1.0),
            TokenType::Number(2.0),
            TokenType::Number(3.14159),
            TokenType::EOF,
        ], &types[..]);

        // Leading dot is not an error, but won't be parsed as a number
        let prog = ".1";
        let tokens = Scanner::new(prog).collect::<Result<Vec<_>>>().unwrap();
        let types = tokens.iter().map(|t| t.ty).collect::<Vec<_>>();
        assert_eq!(&[
            TokenType::Dot,
            TokenType::Number(1.0),
            TokenType::EOF,
        ], &types[..]);

        // Trailing dot is not an error, but won't be parsed as a number
        let tokens = Scanner::new("123.").collect::<Result<Vec<_>>>().unwrap();
        let types = tokens.iter().map(|t| t.ty).collect::<Vec<_>>();
        assert_eq!(&[
            TokenType::Number(123.0),
            TokenType::Dot,
            TokenType::EOF,
        ], &types[..]);
    }

    #[test]
    fn test_string() {
        let token = Scanner::new("\"Hello, World\"").next().unwrap().unwrap();
        assert_eq!(token.ty, TokenType::String("Hello, World"));
    }

    #[test]
    fn unclosed_string() {
        let err = Scanner::new("\"Hello, World!").next().unwrap().unwrap_err();
        match err {
            SyntaxError::UnterminatedString => (),
            _ => panic!("Expected SyntaxError::UnterminatedString"),
        }
    }
}
