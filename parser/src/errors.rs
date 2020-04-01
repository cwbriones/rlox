#[derive(Debug, Fail, Clone)]
pub enum SyntaxError {
    #[fail(display = "unexpected end of input")]
    UnexpectedEOF,
    #[fail(display = "unexpected character '{}'", _0)]
    UnexpectedChar(char),
    #[fail(display = "Unterminated string.")]
    UnterminatedString,
    #[fail(display = "Expect {} after {}.", _0, _1)]
    ExpectAfter(&'static str, &'static str),
    #[fail(display = "Expect {} before {}.", _0, _1)]
    ExpectBefore(&'static str, &'static str),
    #[fail(display = "Expect {}.", _0)]
    Expect(&'static str),
    #[fail(display = "Invalid assignment target.")]
    InvalidAssignment,
    #[fail(display = "Cannot have more than 8 arguments.")]
    TooManyArguments,
    #[fail(display = "Cannot have more than 8 parameters.")]
    TooManyParameters,
}

#[derive(Debug, Fail, PartialEq)]
pub enum ResolveError {
    #[fail(display =  "Cannot return from top-level code.")]
    ReturnOutsideFunction,
    #[fail(display =  "Cannot break outside of a loop.")]
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
