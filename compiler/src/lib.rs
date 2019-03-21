//! Bytecode compiler.

pub extern crate sapphire_parser as parser;

use sapphire_parser::lex::{Item, Lexer};
use sapphire_parser::parse::{parse, ParseError};
use std::fmt;
use std::pin::Pin;

mod byte;
mod ir;
mod proc;

pub use proc::*;

/// An owned version of [`ParseError`].
///
/// Display-formatting this is equivalent to using [`ParseError::fmt_with_src`] (without ANSI
/// colors).
#[derive(Debug)]
pub struct OwnedParseError {
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

    /// Display-formatting with ANSI colors.
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
    pub fn fmt_ansi(&self) -> String {
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

/// Compiles Ruby source code into an intermediate representation.
pub fn compile_ir<T: SymbolTable>(
    name: &str,
    input: String,
    symbols: &mut T,
) -> Result<ir::IRProc<T>, CompileError> {
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
    Ok(ir::IRProc::new(name, &ast, symbols).map_err(CompileError::IR)?)
}

/// Compiles Ruby source code.
pub fn compile<T: SymbolTable, U: PartialEq>(
    name: &str,
    input: String,
    symbols: &mut T,
) -> Result<Proc<T, U>, CompileError> {
    Ok(compile_ir(name, input, symbols)?.into())
}

/// A symbol table that can convert identifiers to symbols.
pub trait SymbolTable {
    type Symbol: core::fmt::Debug + Copy + PartialEq;
    fn symbol(&mut self, name: &str) -> Self::Symbol;
    fn symbol_name(&self, symbol: Self::Symbol) -> Option<&str>;
}
