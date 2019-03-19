//! The parser.
//!
//! > this parser is probably really inefficient
//! > but [lalrpop](https://crates.io/crates/lalrpop) did not halt when compiling this grammar and
//! > just kept consuming memory until it received sigkill from the system, and
//! > [pest](https://crates.io/crates/pest) had complaints about all the left-recursion.
//! > So here’s a nom-style parser.

use crate::ast::StatementList;
use crate::lex::Item;
pub use error::{Expected, ParseError};
use grammar::{program, Cursor, ParseState};
use std::mem;

mod error;
mod grammar;

/// Parses a ruby program.
pub fn parse<'input>(tokens: &'input [Item]) -> Result<StatementList, ParseError<'input>> {
    let state = ParseState::new();
    program(Cursor::new(tokens, &state))
        .map(|(_, r)| r)
        .map_err(|err| unsafe {
            // Rust complains about a reference to the local variable `state` (above) being returned
            // here, but because ParseError contains a Cursor<'input, ()>, there is actually no
            // reference to state being returned.
            // But apparently Rust won’t acknowledge that, so here’s a quick transmute to fix it:
            mem::transmute::<ParseError<'_>, ParseError<'input>>(err)
        })
}
