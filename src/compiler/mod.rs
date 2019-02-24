use sapphire_parser::lex::{Item, Lexer};
use sapphire_parser::parse::{parse, ParseError};
use std::fmt;

/// An owned version of [`ParseError`].
///
/// Display-formatting this is equivalent to using [`ParseError::fmt_with_src`].
#[derive(Debug)]
pub struct OwnedParseError {
    // these two fields are for all intents and purposes immutable
    input: Box<String>,
    _tokens: Box<[Item<'static>]>,
    error: ParseError<'static>,
}

impl OwnedParseError {
    pub fn source(&self) -> &str {
        &self.input
    }

    pub fn error(&self) -> &ParseError<'static> {
        &self.error
    }
}

impl fmt::Display for OwnedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.error.fmt_with_src(&self.input))
    }
}

/// Compiles Ruby source code.
pub fn compile(input: String) -> Result<(), OwnedParseError> {
    let input = Box::new(input);
    // this and tokens_ref are safe because the referenced values will never be moved out
    // of their boxes
    // TODO: use Pin when that becomes stable maybe
    let input_ref = unsafe { &*(&*input as *const String) };

    let tokens = Lexer::new(&input_ref)
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let tokens_ref = unsafe { &*(&*tokens as *const [Item]) };

    let ast = match parse(&tokens_ref) {
        Ok(ast) => ast,
        Err(err) => {
            return Err(OwnedParseError {
                input,
                _tokens: tokens,
                error: err,
            });
        }
    };

    unimplemented!("compile {:?}", ast)
}
