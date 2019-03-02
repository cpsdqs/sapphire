use crate::proc::Proc;
use crate::symbol::Symbols;
use sapphire_parser::lex::{Item, Lexer};
use sapphire_parser::parse::{parse, ParseError};
use std::fmt;
use std::pin::Pin;

mod byte;
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

/// A compile error.
#[derive(Debug)]
pub enum CompileError {
    Parse(OwnedParseError),
    IR(ir::IRError),
}

impl CompileError {
    fn fmt_ansi(&self) -> String {
        match self {
            CompileError::Parse(err) => err.fmt_ansi(),
            CompileError::IR(err) => format!("{}", err),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::Parse(err) => write!(f, "{}", err),
            CompileError::IR(err) => write!(f, "{}", err),
        }
    }
}

/// Compiles Ruby source code.
pub fn compile(name: &str, input: String, symbols: &mut Symbols) -> Result<Proc, CompileError> {
    let input = Pin::new(Box::new(input));
    let input_ref = unsafe { &*(&*input as *const String) };

    let tokens = Pin::new(
        Lexer::new(&input_ref)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    );
    let tokens_ref = unsafe { &*(&*tokens as *const [Item]) };

    let ast = match parse(&tokens_ref) {
        Ok(ast) => ast,
        Err(err) => {
            return Err(CompileError::Parse(OwnedParseError {
                input,
                _tokens: tokens,
                error: err,
            }));
        }
    };

    let name = symbols.symbol(name);
    let proc = ir::IRProc::new(name, &ast, symbols).map_err(CompileError::IR)?;
    Ok(proc.into())
}

#[test]
fn fdshajkfhdsajk() {
    let mut symbols = Symbols::new();
    let s = "puts \"hello world\"";
    match compile("main", String::from(s), &mut symbols) {
        Ok(res) => panic!("\n{}\n-> {:?}, {:?}", s, res, symbols),
        Err(err) => panic!("\n{}", err.fmt_ansi()),
    }
}
