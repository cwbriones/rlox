use std::str::CharIndices;

use errors::*;
use multipeek::MultiPeek;
use multipeek::multipeek;

#[derive(PartialEq, Debug)]
pub struct Token<'a> {
    pub ty: TokenType<'a>,
    pub value: &'a str,
    pub start: usize,
    pub end: usize,
    pub line: usize,
}

#[derive(PartialEq, Debug)]
pub enum TokenType<'s> {
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

#[derive(PartialEq, Eq, Debug)]
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

impl Keyword {
    pub fn from_str(word: &str) -> Option<Keyword> {
        match word {
            "class"  => Some(Keyword::Class),
            "var"    => Some(Keyword::Var),
            "and"    => Some(Keyword::And),
            "else"   => Some(Keyword::Else),
            "false"  => Some(Keyword::False),
            "for"    => Some(Keyword::For),
            "fun"    => Some(Keyword::Fun),
            "if"     => Some(Keyword::If),
            "nil"    => Some(Keyword::Nil),
            "or"     => Some(Keyword::Or),
            "print"  => Some(Keyword::Print),
            "return" => Some(Keyword::Return),
            "super"  => Some(Keyword::Super),
            "this"   => Some(Keyword::This),
            "true"   => Some(Keyword::True),
            "while"  => Some(Keyword::While),
            _ => None,
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    iter: MultiPeek<CharIndices<'a>>,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source,
            iter: multipeek(source.char_indices()),
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

    fn peekn(&mut self, n: usize) -> Option<char> {
        self.iter.peekn(n).map(|&(_, c)| c)
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
            }
            '0'...'9' => {
                self.number(start)
            },
            'a'...'z' | 'A'...'Z' | '_' => {
                self.identifier(start)
            },
            c => return Some(Err(ErrorKind::UnexpectedChar(c).into())),
        };

        let token_contents = self.token_contents(start);
        let token_len = token_contents.len();

        let token = Token {
            ty: ty,
            start: start,
            end : start + token_len,
            value: token_contents,
            line: self.line,
        };
        Some(Ok(token))
    }

    fn advance_while<F>(&mut self, f: F) where for<'r> F: Fn(&'r char,) -> bool {
        while let Some(c) = self.peek() {
            if f(&c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn identifier(&mut self, start: usize) -> TokenType<'a> {
        self.advance_while(|&c| {
            ('a' <= c && c <= 'z') ||
            ('A' <= c && c <= 'Z') ||
            ('0' <= c && c <= '9') ||
            c == '_'
        });
        let word = self.token_contents(start);
        if let Some(keyword) = Keyword::from_str(word) {
            TokenType::Keyword(keyword)
        } else {
            TokenType::Identifier
        }
    }

    fn number(&mut self, start: usize) -> TokenType<'a> {
        self.advance_while(|&c| '0' <= c && c <= '9');

        // Look for a fractional part
        let first = self.peek();
        let second = self.peekn(1);

        let num = if let (Some('.'), Some('0'...'9')) = (first, second) {
            // Consume the '.'
            self.advance();
            self.advance_while(|&c| '0' <= c && c <= '9');
            let token_contents = self.token_contents(start);
            token_contents.parse::<f64>().unwrap()
        } else {
            let token_contents = self.token_contents(start);
            token_contents.parse::<u64>().unwrap() as f64
        };
        TokenType::Number(num)
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn string(&mut self, start: usize) -> Result<TokenType<'a>> {
        self.advance_while(|&c| c != '"');
        // consume the "
        self.advance();
        if self.is_at_end() {
            return Err(ErrorKind::UnterminatedString.into());
        }
        let token_contents = self.token_contents(start);
        Ok(TokenType::String(token_contents.trim_matches('"')))
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}
