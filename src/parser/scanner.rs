use std::str::CharIndices;
use std::iter::Peekable;
use super::ast::{BinaryOperator, UnaryOperator};

use errors::*;

use std::str;

#[derive(Debug, Clone, Copy)]
pub(super) struct Token<'a> {
    pub ty: TokenType<'a>,
    pub value: &'a str,
    pub position: Position,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub(super) struct Position {
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
            TokenType::Equal => Some(BinaryOperator::Equal),
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
            _ => Err(()),
        }
    }
}

pub(super) struct Scanner<'a> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source,
            iter: source.char_indices().peekable(),
            current: 0,
            line: 1,
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

    fn token_contents(&mut self, start: usize) -> &'a str {
        let end = self.iter.peek().map(|&(i, _)| i).unwrap_or(self.source.len());
        &self.source[start..end].trim_right()
    }

    fn eatwhitespace(&mut self) {
        self.advance_while(|&c| {
            c == '\n' || c == '\t' || c == ' '
        });
    }

    fn scan_token(&mut self) -> Option<Result<Token<'a>>> {
        self.eatwhitespace();

        let c = self.advance();
        if c.is_none() {
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
            '0'...'9' => {
                match self.number(start) {
                    Ok(ty) => ty,
                    Err(err) => return Some(Err(err)),
                }
            },
            'a'...'z' | 'A'...'Z' | '_' => {
                self.identifier(start)
            },
            c => return Some(Err(ErrorKind::UnexpectedChar(c).into())),
        };

        let token_contents = self.token_contents(start);
        let token_len = token_contents.len();

        let position = Position {
            start: start,
            end: start + token_len,
            line: self.line,
        };
        let token = Token {
            ty: ty,
            value: token_contents,
            position: position,
        };
        Some(Ok(token))
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
        if let Ok(keyword) = str::parse::<Keyword>(word) {
            TokenType::Keyword(keyword)
        } else {
            TokenType::Identifier
        }
    }

    fn number(&mut self, start: usize) -> Result<TokenType<'a>> {
        self.advance_while(|&c| '0' <= c && c <= '9');
        if let Some('.') = self.peek() {
            // Look for a fractional part
            // Consume the '.'
            self.advance();
            let count = self.advance_while(|&c| '0' <= c && c <= '9');
            if count == 0 {
                return Err(ErrorKind::UnexpectedChar('.').into());
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
            return Err(ErrorKind::UnterminatedString.into());
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
        ], &types[..]);
    }

    // #[test]
    // fn test_token_info() {
    //     unimplemented!()
    // }

    #[test]
    fn test_numbers() {
        let prog = "1 2.0 3.14159";
        let tokens = Scanner::new(prog).collect::<Result<Vec<_>>>().unwrap();
        let types = tokens.iter().map(|t| &t.ty).collect::<Vec<_>>();
        assert_eq!(&[
            &TokenType::Number(1.0),
            &TokenType::Number(2.0),
            &TokenType::Number(3.14159),
        ], &types[..]);

        // Leading dot is not an error, but won't be parsed as a number
        let prog = ".1";
        let tokens = Scanner::new(prog).collect::<Result<Vec<_>>>().unwrap();
        let types = tokens.iter().map(|t| t.ty).collect::<Vec<_>>();
        assert_eq!(&[
            TokenType::Dot,
            TokenType::Number(1.0),
        ], &types[..]);

        // We explicitly disallow a decimal point without a fractional part
        let err = Scanner::new("1.").next().unwrap().unwrap_err();
        match *err.kind() {
            ErrorKind::UnexpectedChar('.') => (),
            _ => panic!("Expected ErrorKind::UnexpectedChar"),
        }
    }

    #[test]
    fn test_string() {
        let token = Scanner::new("\"Hello, World\"").next().unwrap().unwrap();
        assert_eq!(token.ty, TokenType::String("Hello, World"));
    }

    fn test_escaped_string() {
        let prog = r#"
        "\"Hello, World!\"\n"
        "#;
        let token = Scanner::new(prog).next().unwrap().unwrap();
        assert_eq!(token.ty, TokenType::String("\"Hello, World!\"\n"));
    }

    #[test]
    fn unclosed_string() {
        let err = Scanner::new("\"Hello, World!").next().unwrap().unwrap_err();
        match *err.kind() {
            ErrorKind::UnterminatedString => (),
            _ => panic!("Expected ErrorKind::UnterminatedString"),
        }
    }
}
