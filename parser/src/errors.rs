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
    #[fail(display = "Expect expression.")]
    PrimaryFailure,
    #[fail(display = "Invalid assignment target.")]
    InvalidAssignment,
    #[fail(display = "Cannot have more than 8 arguments.")]
    TooManyArguments,
    #[fail(display = "Cannot have more than 8 parameters.")]
    TooManyParameters,
}

#[derive(Debug, Fail, PartialEq)]
pub enum ResolveError {
    #[fail(display =  "'return' outside function.")]
    ReturnOutsideFunction,
    #[fail(display =  "'break' outside loop.")]
    BreakOutsideLoop,
    #[fail(display = "Cannot read local variable in its own initializer.")]
    InitializerSelfReference,
    #[fail(display = "Variable with this name already declared in this scope.")]
    AlreadyDeclared,
    #[fail(display = "Cannot use 'this' outside of a class.")]
    ThisOutsideClass,
    #[fail(display = "Cannot return a value from an initializer.")]
    ReturnFromInitializer,
    #[fail(display = "Cannot use 'super' outside of a class.")]
    SuperOutsideClass,
    #[fail(display = "Cannot use 'super' in a class with no superclass.")]
    SuperInBaseClass,
    #[fail(display = "Multiple errors found in block.")]
    BlockErrors(Vec<ResolveError>),
}

impl From<Vec<ResolveError>> for ResolveError {
    fn from(errs: Vec<ResolveError>) -> Self {
        ResolveError::BlockErrors(errs)
    }
}
