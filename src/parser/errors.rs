#[derive(Debug, Fail, Clone)]
pub enum SyntaxError {
    #[fail(display = "unexpected end of input")]
    UnexpectedEOF,
    #[fail(display = "unexpected character '{}'", _0)]
    UnexpectedChar(char),
    #[fail(display = "unterminated string, expected '\"'")]
    UnterminatedString,
    #[fail(display = "expected {} after {}", _0, _1)]
    Missing(String, &'static str),
    #[fail(display = "expected a literal or parenthesized expression")]
    PrimaryFailure,
    #[fail(display = "invalid assignment target")]
    InvalidAssignment,
}

pub type Result<T> = ::std::result::Result<T, SyntaxError>;
