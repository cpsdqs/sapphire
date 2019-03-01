use sapphire_parser::lex::{Item, Lexer};
use sapphire_parser::parse::{parse, ParseError};
use std::fmt;
use std::pin::Pin;

mod ir;

/// An owned version of [`ParseError`].
///
/// Display-formatting this is equivalent to using [`ParseError::fmt_with_src`] (without ANSI
/// colors).
#[derive(Debug)]
pub struct OwnedParseError {
    // these two fields are for all intents and purposes immutable
    input: Pin<Box<String>>,
    _tokens: Pin<Box<[Item<'static>]>>,
    error: ParseError<'static>,
}

impl OwnedParseError {
    pub fn source(&self) -> &str {
        &self.input
    }

    pub fn error(&self) -> &ParseError<'static> {
        &self.error
    }

    pub fn fmt_ansi(&self) -> String {
        self.error.fmt_with_src(&self.input, true)
    }
}

impl fmt::Display for OwnedParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.error.fmt_with_src(&self.input, false))
    }
}

/// Compiles Ruby source code.
pub fn compile(input: String) -> Result<(), OwnedParseError> {
    let input = Pin::new(Box::new(input));
    let input_ref = unsafe { &*(&*input as *const String) };

    let tokens = Pin::new(Lexer::new(&input_ref)
        .collect::<Vec<_>>()
        .into_boxed_slice());
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

    let mut symbols = crate::symbol::Symbols::new();
    let proc = ir::Proc::new(&ast, &mut symbols);
    unimplemented!(
        "further compililation of:\n{}\n---\n{}\n",
        input,
        proc.unwrap().fmt_with_symbols(&symbols)
    )
}

#[test]
fn fhjdskahfjksdla() {
    match compile(
        "
def horse
    for i in cat
        p i + 1
    end
end
"
        .into(),
    ) {
        Ok(_) => (),
        Err(e) => panic!("\n{}", e.fmt_ansi()),
    }
}
