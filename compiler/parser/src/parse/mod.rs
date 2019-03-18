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

#[test]
fn parser() {
    use crate::ast::*;
    use crate::lex::*;

    macro_rules! lex_parse {
        ($input:expr) => {{
            match parse(&Lexer::new($input).collect::<Vec<_>>()) {
                Ok(parsed) => parsed,
                Err(err) => panic!(
                    "parse error:\n{}\n--\n{}",
                    $input,
                    err.fmt_with_src($input, true)
                ),
            }
        }};
    }

    assert_eq!(
        lex_parse!("::Kernel.p 1, ('a ' + 'cat') => 2, a: 'a', &horse"),
        vec![Statement::Expr(Expression::Call {
            member: Some(Box::new(Expression::RootConst(Ident::Const(
                "Kernel".into()
            )))),
            name: Ident::Local("p".into()),
            args: Arguments {
                items: vec![Argument::Expr(Expression::Literal(Literal::Number {
                    positive: true,
                    value: NumericValue::Decimal("1".into()),
                }))],
                hash: vec![
                    (
                        Expression::BinOp(
                            Box::new(Expression::Literal(Literal::String("a ".into()))),
                            BinaryOp::Add,
                            Box::new(Expression::Literal(Literal::String("cat".into())))
                        ),
                        Expression::Literal(Literal::Number {
                            positive: true,
                            value: NumericValue::Decimal("2".into()),
                        })
                    ),
                    (
                        Expression::Literal(Literal::Symbol("a".into())),
                        Expression::Literal(Literal::String("a".into())),
                    )
                ],
                block: Some(Box::new(Expression::Variable(Ident::Local("horse".into())))),
            },
        })]
    );

    assert_eq!(
        lex_parse!("def a b,\nc\nend"),
        vec![Statement::Expr(Expression::Method {
            name: Ident::Local("a".into()),
            params: vec![
                Parameter::Mandatory(Ident::Local("b".into())),
                Parameter::Mandatory(Ident::Local("c".into())),
            ],
            body: BodyStatement {
                body: Vec::new(),
                rescue: Vec::new(),
                else_: None,
                ensure: None,
            },
        })]
    );

    /* assert_eq!(
        lex_parse!("puts 'a' if true if false while true ? self : nil"),
        vec![Statement::WhileMod(
            Box::new(Statement::IfMod(
                Box::new(Statement::IfMod(
                    Box::new(Statement::Expr(Expression::Call {
                        member: None,
                        name: Ident::Local("puts".into()),
                        args: Arguments {
                            items: vec![Argument::Expr(Expression::Literal(Literal::String(
                                "a".into()
                            )))],
                            hash: Vec::new(),
                            block: None,
                        }
                    })),
                    Expression::True
                )),
                Expression::False
            )),
            Expression::Ternary {
                cond: Box::new(Expression::True),
                then: Box::new(Expression::SelfExpr),
                else_: Box::new(Expression::Nil),
            }
        )]
    ); */

    assert_eq!(
        lex_parse!("a ? b : c ? d : e"),
        vec![Statement::Expr(Expression::Ternary {
            cond: Box::new(Expression::Variable(Ident::Local("a".into()))),
            then: Box::new(Expression::Variable(Ident::Local("b".into()))),
            else_: Box::new(Expression::Ternary {
                cond: Box::new(Expression::Variable(Ident::Local("c".into()))),
                then: Box::new(Expression::Variable(Ident::Local("d".into()))),
                else_: Box::new(Expression::Variable(Ident::Local("e".into()))),
            })
        })]
    );

    assert_eq!(
        lex_parse!("a b c"),
        vec![Statement::Expr(Expression::Call {
            member: None,
            name: Ident::Local("a".into()),
            args: Arguments {
                items: vec![Argument::Expr(Expression::Call {
                    member: None,
                    name: Ident::Local("b".into()),
                    args: Arguments {
                        items: vec![Argument::Expr(Expression::Variable(Ident::Local(
                            "c".into()
                        )))],
                        hash: Vec::new(),
                        block: None,
                    }
                })],
                hash: Vec::new(),
                block: None,
            }
        })]
    );
}
