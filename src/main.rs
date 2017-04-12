use std::env;
use std::error::Error;
use std::io::prelude::*;
use std::fs::File;
use std::str::CharIndices;

use multipeek::MultiPeek;
use multipeek::multipeek;

mod multipeek;

fn main() {
    let mut args = env::args();
    let _ = args.next();
    if let Some(ref sourcefile) = args.next() {
        if let Err(err) = run_file(sourcefile) {
            println!("Error: {}", err);
        }
    } else {
        println!("Usage: rlox [script]");
    }
}

fn run_file(filename: &str) -> Result<(), Box<Error>> {
    let mut file = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut scanner = Scanner::new(&contents);

    scanner.scan()?;
    Ok(())
}

#[derive(Debug)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub value: &'a str,
	pub start: usize,
	pub end: usize,
}

#[derive(Debug)]
pub enum TokenType {
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
	String(String),
	Number(f64),
	Identifier,
	Keyword(Keyword),
}

#[derive(Debug)]
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
			"class" => Some(Keyword::Class),
			"var" => Some(Keyword::Var),
			"and" => Some(Keyword::And),
			"else" => Some(Keyword::Else),
			"false" => Some(Keyword::False),
			"for" => Some(Keyword::For),
			"fun" => Some(Keyword::Fun),
			"if" => Some(Keyword::If),
			"nil" => Some(Keyword::Nil),
			"or" => Some(Keyword::Or),
			"print" => Some(Keyword::Print),
			"return" => Some(Keyword::Return),
			"super" => Some(Keyword::Super),
			"this" => Some(Keyword::This),
			"true" => Some(Keyword::True),
			"while" => Some(Keyword::While),
			_ => None,
		}
	}
}

struct Scanner<'a> {
    source: &'a str,
    iter: MultiPeek<CharIndices<'a>>,
    current: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source,
            iter: multipeek(source.char_indices()),
            current: 0,
        }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.iter.next().map(|(current, c)| {
            self.current = current;
            (current, c)
        })
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|&(_, c)| c)
    }

    fn peekn(&mut self, n: usize) -> Option<char> {
        self.iter.peekn(n).map(|&(_, c)| c)
    }

    fn token_end(&mut self) -> usize {
        self.iter.peek().map(|&(i, _)| i).unwrap_or(self.source.len() - 1)
    }

	fn eatwhitespace(&mut self) {
		self.advance_while(|&c| {
			c == '\n' || c == '\t' || c == ' '
		});
	}

    fn token(&mut self) -> Result<Option<Token<'a>>, Box<Error>> {
		self.eatwhitespace();

		let c = self.advance();
		if c.is_none() {
			return Ok(None)
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
			'/' => TokenType::Slash,
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
				self.string(start)
			}
			'0'...'9' => {
				self.number(start)
			},
			'a'...'z' | 'A'...'Z' | '_' => {
				self.identifier(start)
			},
			_ => panic!("Unexpected character.")
		};

		let end = self.token_end();

        let token = Token {
			ty: ty,
			start: start,
			end : end,
			value: &self.source[start..end].trim_right()
		};
        Ok(Some(token))
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

    fn identifier(&mut self, start: usize) -> TokenType {
		self.advance_while(|&c| {
			('a' <= c && c <= 'z') ||
			('A' <= c && c <= 'Z') ||
			('0' <= c && c <= '9') ||
			c == '_'
		});
		let end = self.token_end();
        let word = &self.source[start..end];
		if let Some(keyword) = Keyword::from_str(word) {
			TokenType::Keyword(keyword)
		} else {
			TokenType::Identifier
		}
    }

    fn number(&mut self, start: usize) -> TokenType {
		self.advance_while(|&c| '0' <= c && c <= '9');

		// Look for a fractional part
		let first = self.peek();
		let second = self.peekn(1);

		if let (Some('.'), Some('0'...'9')) = (first, second) {
			// Consume the '.'
			self.advance();
			self.advance_while(|&c| '0' <= c && c <= '9');
		}
		let end = self.token_end();
        let s = &self.source[start..end];
		let num = s.parse::<f64>().unwrap();
		TokenType::Number(num)
    }

	fn is_at_end(&mut self) -> bool {
		self.peek().is_none()
	}

	fn string(&mut self, start: usize) -> TokenType {
		self.advance_while(|&c| c != '"');
		// consume the "
		self.advance();
		if self.is_at_end() {
			panic!("Unterminated String");
		}
		let end = self.token_end();
		let val = (&self.source[start+1..end-1]).to_string();
		TokenType::String(val)
	}

    pub fn scan(&mut self) -> Result<Vec<Token>, Box<Error>> {
        let mut tokens = Vec::new();

        while let Ok(Some(token)) = self.token() {
            println!("{:?}", token);
            tokens.push(token);
        }

        Ok(tokens)
    }
}
