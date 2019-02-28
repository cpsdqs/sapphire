use super::error::{Expected, ParseError};
use super::parse;
use crate::ast::*;
use crate::lex::{Item, Token};
use nom::call;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::iter;

type IResult<'input, T> = Result<(Cursor<'input>, T), ParseError<'input>>;

/// A cursor in a list of tokens.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cursor<'input, State = &'input RefCell<ParseState<'input>>> {
    buffer: &'input [Item<'input>],
    pos: usize,
    state: State,
}

impl<'input, T> Cursor<'input, T> {
    pub(crate) fn get(&self) -> Option<&Item<'input>> {
        self.buffer.get(self.pos)
    }

    fn at_end(&self) -> bool {
        self.pos >= self.buffer.len() - 1
    }

    pub(crate) fn buffer(&self) -> &[Item<'input>] {
        &self.buffer
    }

    pub(crate) fn pos(&self) -> usize {
        self.pos
    }
}

impl<'input> Cursor<'input> {
    /// Creates a new cursor with the given token list.
    pub(crate) fn new(
        buffer: &'input [Item<'input>],
        state: &'input RefCell<ParseState<'input>>,
    ) -> Cursor<'input> {
        Cursor {
            buffer,
            pos: 0,
            state: state,
        }
    }

    fn cached_err(&self, symbol: &'static str) -> Option<ParseError<'input>> {
        self.state
            .borrow()
            .cached_err(self.pos, symbol)
            .map(|err| err.clone())
    }

    fn cache_err(&self, symbol: &'static str, err: ParseError<'input>) {
        self.state.borrow_mut().cache_err(self.pos, symbol, err);
    }

    fn without_state(self) -> Cursor<'input, ()> {
        Cursor {
            buffer: self.buffer,
            pos: self.pos,
            state: (),
        }
    }
}

impl<'input> Iterator for Cursor<'input> {
    type Item = &'input Token<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        self.pos += 1;
        match self.buffer.get(self.pos - 1) {
            Some(Ok((_, token, _))) => Some(token),
            _ => None,
        }
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub struct ParseState<'input> {
    cache: HashMap<(usize, &'static str), ParseError<'input>>,
}

impl<'input> ParseState<'input> {
    pub fn new() -> RefCell<ParseState<'input>> {
        RefCell::new(ParseState {
            cache: HashMap::new(),
        })
    }

    fn cached_err(&self, pos: usize, key: &'static str) -> Option<&ParseError<'input>> {
        self.cache.get(&(pos, key))
    }

    fn cache_err(&mut self, pos: usize, key: &'static str, err: ParseError<'input>) {
        self.cache.insert((pos, key), err);
    }
}
macro_rules! def_token_type {
    ($($($p:pat)|+ => $i:ident => $f:expr,)+) => {
        /// Token types.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[allow(non_camel_case_types)]
        pub enum TokenType { $($i,)+ }

        impl<'input> From<Token<'input>> for TokenType {
            fn from(token: Token<'input>) -> TokenType {
                use Token::*;
                match token {
                    $($(| $p)+ => TokenType::$i,)+
                }
            }
        }

        impl<'input> From<&Token<'input>> for TokenType {
            fn from(token: &Token<'input>) -> TokenType {
                use Token::*;
                match token {
                    $($(| $p)+ => TokenType::$i,)+
                }
            }
        }

        impl fmt::Display for TokenType {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $(TokenType::$i => write!(f, $f),)+
                }
            }
        }
    }
}

def_token_type! {
    K__LINE__ => K__LINE__ => "__LINE__",
    K__ENCODING__ => K__ENCODING__ => "__ENCODING__",
    K__FILE__ => K__FILE__ => "__FILE__",
    KBEGIN => KBEGIN => "BEGIN",
    KEND => KEND => "END",
    Kalias => Kalias => "alias",
    Kand => Kand => "and",
    Kbegin => Kbegin => "begin",
    Kbreak => Kbreak => "break",
    Kcase => Kcase => "case",
    Kclass => Kclass => "class",
    Kdef => Kdef => "def",
    Kdefined => Kdefined => "defined?",
    Kdo => Kdo => "do",
    Kelse => Kelse => "else",
    Kelsif => Kelsif => "elsif",
    Kend => Kend => "end",
    Kensure => Kensure => "ensure",
    Kfor => Kfor => "for",
    Kfalse => Kfalse => "false",
    Kif => Kif => "if",
    Kin => Kin => "in",
    Kmodule => Kmodule => "module",
    Knext => Knext => "next",
    Knil => Knil => "nil",
    Knot => Knot => "not",
    Kor => Kor => "or",
    Kredo => Kredo => "redo",
    Krescue => Krescue => "rescue",
    Kretry => Kretry => "retry",
    Kreturn => Kreturn => "return",
    Kself => Kself => "self",
    Ksuper => Ksuper => "super",
    Kthen => Kthen => "then",
    Ktrue => Ktrue => "true",
    Kundef => Kundef => "undef",
    Kunless => Kunless => "unless",
    Kuntil => Kuntil => "until",
    Kwhen => Kwhen => "when",
    Kwhile => Kwhile => "while",
    Kyield => Kyield => "yield",
    ILocal(..) => ILocal => "local variable identifier",
    IGlobal(..) => IGlobal => "global variable identifier",
    IClass(..) => IClass => "class variable identifier",
    IInstance(..) => IInstance => "instance variable identifier",
    IConstant(..) => IConstant => "constant identifier",
    IMethodOnly(..) => IMethodOnly => "method-only identifier",
    IAssignmentLikeMethod(..) => IAssignmentLikeMethod => "assignment-like method identifier",
    PLBracket => PLBracket => "[",
    PRBracket => PRBracket => "]",
    PLParen => PLParen => "(",
    PRParen => PRParen => ")",
    PLBrace => PLBrace => "{{",
    PRBrace => PRBrace => "}}",
    PDblColon => PDblColon => "::",
    PComma => PComma => ",",
    PSemicolon => PSemicolon => ";",
    P3Dots => P3Dots => "...",
    P2Dots => P2Dots => "..",
    PQuestion => PQuestion => "?",
    PColon => PColon => ":",
    PFatArrow => PFatArrow => "=>",
    PArrow => PArrow => "->",
    PDot => PDot => ".",
    ONeq => ONeq => "!=",
    ONMatch => ONMatch => "!~",
    ONot => ONot => "!",
    OAnd => OAnd => "&&",
    OOr => OOr => "||",
    OAssign => OAssign => "=",
    OAssignAnd => OAssignAnd => "&&=",
    OAssignOr => OAssignOr => "||=",
    OAssignBitAnd => OAssignBitAnd => "&=",
    OAssignBitOr => OAssignBitOr => "|=",
    OAssignBitXor => OAssignBitXor => "^=",
    OAssignShl => OAssignShl => "<<=",
    OAssignShr => OAssignShr => ">>=",
    OAssignAdd => OAssignAdd => "+=",
    OAssignSub => OAssignSub => "-=",
    OAssignMul => OAssignMul => "*=",
    OAssignDiv => OAssignDiv => "/=",
    OAssignRem => OAssignRem => "%=",
    OAssignPow => OAssignPow => "**=",
    OBitXor => OBitXor => "^",
    OBitAnd => OBitAnd => "&",
    OBitOr => OBitOr => "|",
    OCmp => OCmp => "<=>",
    OCaseEq => OCaseEq => "===",
    OEq => OEq => "==",
    OMatch => OMatch => "=~",
    OGeq => OGeq => ">=",
    OShr => OShr => ">>",
    OGt => OGt => ">",
    OLeq => OLeq => "<=",
    OShl => OShl => "<<",
    OLt => OLt => "<",
    OUPlus => OUPlus => "unary +",
    OAdd => OAdd => "+",
    OUMinus => OUMinus => "unary -",
    OSub => OSub => "-",
    OPow => OPow => "**",
    OMul => OMul => "*",
    ODiv => ODiv => "/",
    ORem => ORem => "%",
    OBitInv => OBitInv => "~",
    OAssignIndex => OAssignIndex => "[]=",
    OIndex => OIndex => "[]",
    LNum(..) => LNum => "numeric literal",
    LSingleStr(..) | LDoubleStr(..) | LQNeStr(..) | LQStr(..) => LStr => "string literal",
    LHereDoc(..) => LHereDoc => "heredoc",
    LExecCmd(..) => LExecCmd => "external command execution",
    LQNeArray(..) | LQArray(..) => LArray => "array literal",
    LRegExp(..) | LQRegExp(..) => LRegExp => "regular expression literal",
    LSymbol(..) => LSymbol => "symbol literal",
    Whitespace => Whitespace => "whitespace",
    Newlines => Newlines => "line break",
}

macro_rules! token {
    ($i:expr, $token_type:ident) => {{
        let mut i = $i.clone();
        let item = i.next();
        match item.map(TokenType::from) {
            Some(TokenType::$token_type) => Ok((i, item.unwrap())),
            _ => Err(ParseError::Unexpected(
                $i.without_state(),
                Expected::Token(TokenType::$token_type),
            )),
        }
    }};
    ($i:expr, $($tokens:ident,)+) => {{
        let mut i = $i.clone();
        let item = i.next();
        match item.map(TokenType::from) {
            $(Some(TokenType::$tokens) => Ok((i, item.unwrap())),)+
            _ => Err(ParseError::Unexpected(
                $i.without_state(),
                Expected::OneOfToken(&[$(TokenType::$tokens),+]),
            )),
        }
    }};
}

macro_rules! opt {
    ($i:expr, $submac:ident!($($args:tt)*)) => {{
        let i = $i.clone();
        match $submac!(i, $($args)*) {
            Ok((i, o)) => Ok((i, Some(o))),
            Err(_) => Ok(($i, None)),
        }
    }};
    ($i:expr, $f:expr) => (opt!($i, call!($f)));
}

macro_rules! many0 {
    ($i:expr, $submac:ident!($($args:tt)*)) => {{
        let mut res = Vec::new();
        let mut i = $i.clone();
        loop {
            let j = i.clone();
            match $submac!(j, $($args)*) {
                Ok((j, o)) => {
                    res.push(o);
                    if j.pos == i.pos {
                        panic!(
                            "many0!({}!({})) failed to advance",
                            stringify!($submac),
                            stringify!($($args)*)
                        );
                    }
                    i = j;
                }
                Err(_) => break Ok((i, res)),
            }
        }
    }};
    ($i:expr, $f:expr) => (many0!($i, call!($f)));
}

macro_rules! many1 {
    ($i:expr, $submac:ident!($($args:tt)*), err: $err:ident($($eargs:tt)*)) => {{
        match many0!($i, $submac!($($args)*)) {
            Ok((i, res)) => if res.is_empty() {
                Err(ParseError::$err($i.without_state(), $($eargs)*))
            } else {
                Ok((i, res))
            },
            Err(err) => Err(err),
        }
    }};
    ($i:expr, $f:expr, err: $err:ident($($eargs:tt)*)) => {
        many1!($i, call!($f), err: $err($($eargs)*))
    };
}

macro_rules! alt {
    ($i:expr, $submac:ident!($($args:tt)*), $($rest:tt)*) => {{
        let i = $i.clone();
        match $submac!(i, $($args)*) {
            Ok((i, res)) => Ok((i, res)),
            Err(e) => match alt!($i, $($rest)*) {
                Ok(r) => Ok(r),
                Err(f) => Err(f.alt_merge(e.reduce())),
            },
        }
    }};
    ($i:expr, $f:path, $($rest:tt)*) => {
        alt!($i, call!($f), $($rest)*)
    };
    ($i:expr, err: $err:ident) => {{
        Err(ParseError::Alt(
            Box::new(ParseError::Unexpected($i.without_state(), Expected::$err)),
            Vec::new(),
        ))
    }};
}

macro_rules! not {
    ($i:expr, $submac:ident!($($args:tt)*); err: $err:ident($($eargs:tt)*)) => {{
        let i = $i.clone();
        if let Err(..) = $submac!($i, $($args)*) {
            Ok((i, ()))
        } else {
            Err(ParseError::$err($i.without_state(), $($eargs)*))
        }
    }};
    ($i:expr, $f:expr; err: $err:ident($($eargs:tt)*)) => {{
        not!($i, call!($f); err: $err($($eargs)*))
    }};
}

macro_rules! do_parse {
    ($i:expr, $bind:ident: $submac:ident!($($args:tt)*) >> $($rest:tt)*) => {{
        match $submac!($i, $($args)*) {
            Ok((i, $bind)) => do_parse!(i, $($rest)*),
            Err(e) => Err(e),
        }
    }};
    ($i:expr, $bind:ident: $f:ident >> $($rest:tt)*) => {
        do_parse!($i, $bind: call!($f) >> $($rest)*)
    };
    ($i:expr, $submac:ident!($($args:tt)*) >> $($rest:tt)*) => {
        do_parse!($i, __: $submac!($($args)*) >> $($rest)*)
    };
    ($i:expr, $f:path >> $($rest:tt)*) => {
        do_parse!($i, __: call!($f) >> $($rest)*)
    };
    ($i:expr, [$j:ident => $handler:block]) => {{
        let $j = $i;
        $handler
    }};
    ($i:expr, $handler:tt) => {{
        Ok(($i, $handler))
    }};
}

macro_rules! tail_rule {
    ($i:expr, $accum:ident -> $name:ident >> $($rest:tt)*) => {{
        let $name = $accum.clone();
        do_parse!($i, $($rest)*)
    }};
    ($i:expr, $($rest:tt)*) => {
        do_parse!($i, $($rest)*)
    };
}

macro_rules! tail_rules {
    (
        $i:expr,
        $accum:ident = $submac:ident!($($args:tt)*);
        $(tail_rule!($($targs:tt)*),)+;
        err: $err:ident
    ) => {{
        match $submac!($i, $($args)*) {
            Ok((mut i, mut $accum)) => {
                loop {
                    let j = i.clone();
                    match alt!(j, $(tail_rule!($($targs)*),)+ err: $err) {
                        Ok((j, r)) => {
                            i = j;
                            $accum = r;
                        }
                        Err(_) => {
                            break Ok((i, $accum));
                        }
                    }
                }
            }
            Err(e) => Err(e),
        }
    }}
}

macro_rules! call_macro {
    ($i:expr, $submac:ident!($($args:tt)*)) => {{
        $submac!($i, $($args)*)
    }}
}

// 10.1 Program
/// Parses a program.
pub fn program(mut i: Cursor) -> IResult<StatementList> {
    if let Ok((j, _)) = ws(i) {
        i = j;
    }

    if let Ok((j, _)) = separator_list(i) {
        i = j;
    }

    if let Ok((j, _)) = ws(i) {
        i = j;
    }

    let mut statements = Vec::new();
    loop {
        let (j, statement) = statement(i)?;
        i = j;
        statements.push(statement);

        if i.at_end() {
            break;
        }

        if let Ok((j, _)) = ws(i) {
            i = j;
        }

        let (j, _) = separator_list(i)?;
        i = j;

        if i.at_end() {
            break;
        }

        if let Ok((j, _)) = ws(i) {
            i = j;
        }
    }

    Ok((i, statements))
}

// TODO: Ruby 2.0 double splat
// TODO: Ruby 2.0 %i %I symbol lists
// TODO: Ruby 2.1 r/i number suffixes
// TODO: Ruby 2.3 <<~ indented heredoc
// TODO: Ruby 2.3 &. safe navigation operator
// TODO: Ruby 2.7 .: method references

sapphire_parser_gen::parser! {
    // Matches optional single-line whitespace.
    ws: () = { many0!(token!(Whitespace)) => () }

    // Matches optional whitespace including newlines.
    wss: () = { many0!(token!(Whitespace, Newlines,)) => () }

    keyword: (&Token) = {
        t: token!(
            K__LINE__,
            K__ENCODING__,
            K__FILE__,
            KBEGIN,
            KEND,
            Kalias,
            Kand,
            Kbegin,
            Kbreak,
            Kcase,
            Kclass,
            Kdef,
            Kdefined,
            Kdo,
            Kelse,
            Kelsif,
            Kend,
            Kensure,
            Kfor,
            Kfalse,
            Kif,
            Kin,
            Kmodule,
            Knext,
            Knil,
            Knot,
            Kor,
            Kredo,
            Krescue,
            Kretry,
            Kreturn,
            Kself,
            Ksuper,
            Kthen,
            Ktrue,
            Kundef,
            Kunless,
            Kuntil,
            Kwhen,
            Kwhile,
            Kyield,
        ) => (t)
    }

    operator_method_name: (&Token) = {
        t: token!(
            OBitXor,
            OBitAnd,
            OBitOr,
            OCmp,
            OCaseEq,
            OEq,
            OMatch,
            OGeq,
            OShr,
            OGt,
            OLeq,
            OShl,
            OLt,
            OUPlus,
            OUMinus,
            OAdd,
            OSub,
            OMul,
            ODiv,
            ORem,
            OPow,
            OBitInv,
            OAssignIndex,
            OIndex,
        ) => (t)
    }

    assignment_operator: (&Token) = {
        t: token!(
            OAssignAnd,
            OAssignOr,
            OAssignBitAnd,
            OAssignBitOr,
            OAssignBitXor,
            OAssignShl,
            OAssignShr,
            OAssignAdd,
            OAssignSub,
            OAssignMul,
            OAssignDiv,
            OAssignRem,
            OAssignPow,
        ) => (t)
    }

    literal: Expression = {
        t: token!(LNum, LStr, LHereDoc, LExecCmd, LArray, LRegExp, LSymbol,)
            => [ i => {
            let literal = match t {
                Token::LNum(num) => Ok(Literal::Number {
                    positive: num.sign == '+',
                    value: num.contents.clone().into(),
                }),
                Token::LSingleStr(s) | Token::LQNeStr(s) | Token::LHereDoc(s) => {
                    Ok(Literal::String(s.to_string()))
                },
                Token::LDoubleStr(q) | Token::LQStr(q) | Token::LExecCmd(q) => {
                    use crate::lex::Quoted;

                    let mut items = Vec::new();

                    for item in q {
                        match item {
                            Quoted::Fragment(s) => items.push(QuotedFragment::String(s.to_string())),
                            Quoted::Ident(t) => items.push(QuotedFragment::Ident(t.clone().into())),
                            Quoted::Interpolated(tokens) => {
                                items.push(QuotedFragment::Interpolated(parse(&tokens)?));
                            }
                        }
                    }

                    Ok(Literal::QuotedString(items))
                },
                Token::LQNeArray(_) => unimplemented!(),
                Token::LQArray(_) | Token::LRegExp(..) | Token::LQRegExp(..) => unimplemented!(),
                Token::LSymbol(s) => Ok(Literal::Symbol(s.to_string())),
                _ => unreachable!(), // see token! call above
            };
            literal.map(|l| (i, Expression::Literal(l)))
        }],
    }

    // 10.2 Compound Statement
    compound_statement: StatementList = {
        // statement_list? separator_list?
        s: opt!(statement_list) ws opt!(separator_list) => {
            s.unwrap_or_else(|| Vec::with_capacity(0))
        }
    }

    statement_list: StatementList = {
        // statement (separator_list statement)*
        a: statement b: many0!(do_parse!(ws >> separator_list >> ws >> s: statement >> (s))) => {
            iter::once(a).chain(b.into_iter()).collect()
        }
    }

    separator_list: () = {
        many1!(separator, err: Unexpected(Expected::Separator)) => ()
    }
    separator: (&Token) = { t: token!(PSemicolon, Newlines,) => (t) }

    // 11 Expressions
    expression: Expression = { keyword_logical_expression }

    // 11.2 Logical expressions
    // 11.2.2 Keyword logical expressions
    keyword_logical_expression: Expression = {
        keyword_not_expression,
        keyword_and_expression,
        keyword_or_expression,
    }

    // 11.2.3 Logical NOT expressions
    keyword_not_expression: Expression = {
        token!(ONot) wss e: method_invocation_without_parentheses => (Expression::Not(Box::new(e))),
        token!(Knot) wss e: keyword_not_expression => (Expression::Not(Box::new(e))),
        method_invocation_without_parentheses,
        operator_expression,
    }
    /* never used, apparently?
    operator_not_expression: Expression = {
        token!(ONot) wss e: method_invocation_without_parentheses => (Expression::Not(Box::new(e))),
        token!(ONot) wss e: unary_expression => (Expression::Not(Box::new(e))),
    }*/

    // 11.2.4 Logical AND expressions
    keyword_and_expression: Expression = {
        l: expression ws token!(Kand) wss r: keyword_not_expression => {
            Expression::And(Box::new(l), Box::new(r))
        }
    }
    operator_and_expression: Expression = {
        l: operator_and_expression ws token!(OAnd) wss r: equality_expression => {
            Expression::And(Box::new(l), Box::new(r))
        },
        equality_expression,
    }

    // 11.2.5 Logical OR expressions
    keyword_or_expression: Expression = {
        l: expression ws token!(Kor) wss r: keyword_not_expression => {
            Expression::Or(Box::new(l), Box::new(r))
        }
    }
    operator_or_expression: Expression = {
        l: operator_or_expression ws token!(OOr) wss r: operator_and_expression => {
            Expression::Or(Box::new(l), Box::new(r))
        },
        operator_and_expression,
    }

    // 11.3 Method invocation expressions
    primary_method_invocation: Expression = {
        super_with_optional_argument,
        indexing_method_invocation,
        t: token!(IMethodOnly) => (Expression::Call {
            member: None,
            name: t.clone().into(),
            args: Arguments::default(),
        }),
        i: method_identifier wss b: block => (Expression::Call {
            member: None,
            name: i,
            args: Arguments {
                items: Vec::with_capacity(0),
                hash: Vec::with_capacity(0),
                block: Some(Box::new(Expression::Block(b))),
            }
        }),
        i: method_identifier a: argument_with_parentheses wss b: opt!(block) => {
            let mut args = a;
            if let Some(b) = b {
                args.block = Some(Box::new(Expression::Block(b)));
            }

            Expression::Call {
                member: None,
                name: i,
                args,
            }
        },
        e: primary_expression ws token!(PDot) wss i: opt!(method_name)
            a: opt!(argument_with_parentheses) wss b: opt!(block) => {
            let mut args = a.unwrap_or_else(|| Arguments::default());
            if let Some(b) = b {
                args.block = Some(Box::new(Expression::Block(b)));
            }

            // Ruby 1.9: .(·) proc call syntax
            let i = i.unwrap_or(Ident::Local("call".into()));

            Expression::Call {
                member: Some(Box::new(e)),
                name: i,
                args,
            }
        },
        e: primary_expression ws token!(PDblColon) wss i: method_name a: argument_with_parentheses
            wss b: opt!(block) => {
            let mut args = a;
            if let Some(b) = b {
                args.block = Some(Box::new(Expression::Block(b)));
            }
            Expression::Call {
                member: Some(Box::new(e)),
                name: i,
                args,
            }
        },
        e: primary_expression ws token!(PDblColon) wss i: method_name_except_constant wss
            b: opt!(block) => (Expression::Call {
            member: Some(Box::new(e)),
            name: i,
            args: Arguments {
                items: Vec::with_capacity(0),
                hash: Vec::with_capacity(0),
                block: b.map(|b| Box::new(Expression::Block(b))),
            },
        }),
    }
    method_identifier: Ident = {
        t: token!(ILocal, IConstant, IMethodOnly,) => (t.clone().into())
    }
    method_name: Ident = {
        i: method_identifier => (i.into()),
        i: operator_method_name => (i.clone().into()),
        i: keyword => (i.clone().into()),
    }
    indexing_method_invocation: Expression = {
        e: primary_expression token!(PLBracket) wss a: opt!(indexing_argument_list) wss
            token!(PRBracket) => {
            Expression::Index(Box::new(e), a.unwrap_or_default())
        }
    }
    method_name_except_constant: Ident = {
        not!(token!(IConstant); err: Unexpected(Expected::NonConstantIdentifier)) i: method_name =>
            (i)
    }
    method_invocation_without_parentheses: Expression = {
        // TODO: check if the following rule works
        e: chained_command_with_do_block wss token!(PDot, PDblColon,) wss i: method_name ws
            a: argument_without_parentheses => (Expression::Call {
            member: Some(Box::new(e)),
            name: i,
            args: a,
        }),
        chained_command_with_do_block,
        command,
        return_with_argument,
        break_with_argument,
        next_with_argument,
    }
    command: Expression = {
        i: method_identifier ws a: argument_without_parentheses => (Expression::Call {
            member: None,
            name: i,
            args: a,
        }),
        /* This rule will never work because primary_expression -> primary_method_invocation
           will consume the (. | ::) method_name before this rule can
        e: primary_expression ws token!(PDot, PDblColon,) i: method_name ws
            a: argument_without_parentheses => (Expression::Call {
            member: Some(Box::new(e)),
            name: i,
            args: a,
        }),
           so here’s an alternative implementation that simply exploits that
        */
        e: primary_expression ws a: argument_without_parentheses => [ i => {
            let call = match e {
                Expression::Call {
                    member,
                    name,
                    args,
                } => if args.is_empty() {
                    Some((member, name))
                } else {
                    None
                },
                _ => None,
            };
            if let Some((member, name)) = call {
                Ok((i, Expression::Call {
                    member,
                    name,
                    args: a,
                }))
            } else {
                Err(ParseError::Unexpected(i.without_state(), Expected::Command))
            }
        }],
        super_with_argument,
        yield_with_argument,
    }
    chained_command_with_do_block: Expression = {
        c: command_with_do_block i: opt!(do_parse!(
            wss >> i: many1!(chained_method_invocation, err: Unexpected(Expected::Command)) >> (i)
        )) => {
            let mut expr = c;
            if let Some(i) = i {
                for (name, args) in i {
                    expr = Expression::Call {
                        member: Some(Box::new(expr)),
                        name,
                        args,
                    };
                }
            }
            expr
        },
    }
    chained_method_invocation: (Ident, Arguments) = {
        token!(PDot, PDblColon,) i: method_name a: argument_with_parentheses => ((i, a)),
        token!(PDot, PDblColon,) i: method_name => ((i, Arguments::default())),
    }
    command_with_do_block: Expression = {
        super_with_argument_and_do_block,
        // frankly I’m not sure why argument_without_parentheses is marked as required in the
        // ISO standard but it shouldn’t be; otherwise `method do; end` wouldn’t be valid syntax
        i: method_identifier ws a: opt!(argument_without_parentheses) wss b: do_block => {
            let mut args = a.unwrap_or_else(|| Arguments::default());
            args.block = Some(Box::new(Expression::Block(b)));
            Expression::Call {
                member: None,
                name: i,
                args,
            }
        },
        /* This rule also won’t work (see above in command)
        e: primary_expression ws token!(PDot, PDblColon,) i: method_name ws
            a: argument_without_parentheses wss b: do_block => {
            let mut args = a;
            args.block = Some(Box::new(Expression::Block(b)));
            Expression::Call {
                member: Some(Box::new(e)),
                name: i,
                args,
            }
        }, */
        e: primary_expression ws a: opt!(argument_without_parentheses) wss b: do_block => [ i => {
            let call = match e {
                Expression::Call {
                    member,
                    name,
                    args,
                } => if args.is_empty() {
                    Some((member, name))
                } else {
                    None
                },
                _ => None,
            };
            if let Some((member, name)) = call {
                let mut args = a.unwrap_or_else(|| Arguments::default());
                args.block = Some(Box::new(Expression::Block(b)));
                Ok((i, Expression::Call {
                    member,
                    name,
                    args,
                }))
            } else {
                Err(ParseError::Unexpected(i.without_state(), Expected::Command))
            }
        }],
    }

    // 11.3.2 Method arguments
    // Ruby 1.9: laxer arguments
    // Ruby 2.0: keyword arguments
    indexing_argument_list: Arguments = {
        p: positional_argument_list opt!(do_parse!(ws >> token!(PComma) >> ())) => (Arguments {
            items: p,
            hash: Vec::with_capacity(0),
            block: None,
        }),
        a: association_list opt!(do_parse!(ws >> token!(PComma) >> ())) => (Arguments {
            items: Vec::with_capacity(0),
            hash: a,
            block: None,
        })
    }
    positional_argument: Argument = {
        s: splatting_argument => (Argument::Splat(s)),
        not!(association; err: Unexpected(Expected::PositionalArgument)) a: alt!(
            command,
            operator_expression,
            err: PositionalArgument
        ) => (Argument::Expr(a)),
    }
    positional_argument_list: (Vec<Argument>) = {
        a: positional_argument
            b: many0!(do_parse!(ws >> token!(PComma) >> wss >> a: positional_argument >> (a))) => {
            iter::once(a).chain(b.into_iter()).collect()
        }
    }
    splatting_argument: Expression = {
        token!(OMul) wss e: operator_expression => (e)
    }
    operator_expression_list: (Vec<Expression>) = {
        a: operator_expression b: many0!(do_parse!(
                ws >> token!(PComma) >> wss >> e: operator_expression >> (e)
            ))
            => (iter::once(a).chain(b.into_iter()).collect())
    }
    argument_with_parentheses: Arguments = {
        token!(PLParen)
            a: alt!(
                do_parse!(wss >> a: argument_list >> wss >> (a)),
                do_parse!(
                    wss >>
                    m: positional_argument_list >>
                    ws >>
                    token!(PComma) >>
                    wss >>
                    c: chained_command_with_do_block >>
                    wss >>
                    token!(PRParen) >> (Arguments {
                        items: m.into_iter().chain(iter::once(Argument::Expr(c))).collect(),
                        hash: Vec::with_capacity(0),
                        block: None,
                    })
                ),
                do_parse!(wss >> c: chained_command_with_do_block >> wss >> (Arguments {
                    items: vec![Argument::Expr(c)],
                    hash: Vec::with_capacity(0),
                    block: None,
                })),
                do_parse!(wss >> (Arguments::default())),
                err: Arguments
            )
            token!(PRParen)
            => (a)
    }
    // no WW before this one
    argument_without_parentheses: Arguments = {
        not!(
            do_parse!(ws >> token!(PLBrace) >> ());
            err: Unexpected(Expected::Arguments)
        ) a: argument_list => (a)
    }
    // Ruby 1.9: laxer arguments
    argument_list: Arguments = {
        p: positional_argument_list
            h: opt!(do_parse!(wss >> token!(PComma) >> wss >> a: association_list >> (a)))
            b: opt!(do_parse!(ws >> token!(PComma) >> wss >> b: block_argument >> (b))) => {
            Arguments {
                items: p,
                hash: h.unwrap_or_else(|| Vec::with_capacity(0)),
                block: b.map(Box::new),
            }
        },
        h: association_list
            b: opt!(do_parse!(ws >> token!(PComma) >> wss >> b: block_argument >> (b))) => {
            Arguments {
                items: Vec::with_capacity(0),
                hash: h,
                block: b.map(Box::new),
            }
        },
        b: block_argument => {
            Arguments {
                items: Vec::with_capacity(0),
                hash: Vec::with_capacity(0),
                block: Some(Box::new(b)),
            }
        },
    }
    block_argument: Expression = {
        token!(OBitAnd) wss e: operator_expression => (e)
    }

    // 11.3.3 Blocks
    block: Block = {
        brace_block,
        do_block,
        lambda,
    }
    brace_block: Block = {
        token!(PLBrace) wss p: opt!(block_parameter) wss b: block_body wss token!(PRBrace)
            => (Block {
                params: p.unwrap_or_else(|| Vec::with_capacity(0)),
                body: b,
                lambda: false,
            })
    }
    do_block: Block = {
        token!(Kdo) wss p: opt!(block_parameter) wss b: block_body wss token!(Kend) => (Block {
                params: p.unwrap_or_else(|| Vec::with_capacity(0)),
                body: b,
                lambda: false,
            })
    }
    block_parameter: Parameters = {
        token!(OBitOr) wss c: opt!(block_parameter_list) wss token!(OBitOr)
            => (c.unwrap_or_else(|| Vec::with_capacity(0)))
    }
    // Ruby 1.9: laxer block parameters
    block_parameter_list: Parameters = { parameter_list }
    block_body: StatementList = { compound_statement }

    // Ruby 1.9: lambdas
    lambda: Block = {
        token!(PArrow) p: opt!(do_parse!(wss >> p: lambda_parameter >> (p))) ws token!(PLBrace)
            wss b: block_body wss token!(PRBrace) => (Block {
                params: p.unwrap_or_else(|| Vec::with_capacity(0)),
                body: b,
                lambda: true,
            }),
        token!(PArrow) p: opt!(do_parse!(wss >> p: lambda_parameter >> (p))) ws token!(Kdo)
            wss b: block_body wss token!(Kend) => (Block {
                params: p.unwrap_or_else(|| Vec::with_capacity(0)),
                body: b,
                lambda: true,
            }),
    }
    lambda_parameter: Parameters = {
        token!(PLParen) wss p: opt!(block_parameter_list) wss token!(PRParen) => {
            p.unwrap_or_else(|| Vec::with_capacity(0))
        },
        block_parameter_list,
    }

    // 11.3.4 The super expression
    super_with_optional_argument: Expression = {
        token!(Ksuper) a: opt!(argument_with_parentheses) wss b: opt!(block) => {
            let mut args = a.unwrap_or_else(|| Arguments::default());
            if let Some(b) = b {
                args.block = Some(Box::new(Expression::Block(b)));
            }
            Expression::Super(args)
        }
    }
    super_with_argument: Expression = {
        token!(Ksuper) ws a: argument_without_parentheses => (Expression::Super(a))
    }
    super_with_argument_and_do_block: Expression = {
        token!(Ksuper) ws a: argument_without_parentheses wss b: opt!(do_block) => {
            let mut args = a;
            if let Some(b) = b {
                args.block = Some(Box::new(Expression::Block(b)));
            }
            Expression::Super(args)
        }
    }

    // 11.3.5 The yield expression
    yield_with_optional_argument: Expression = {
        yield_with_parentheses_and_argument,
        yield_with_parentheses_without_argument,
        token!(Kyield) => (Expression::Yield(None)),
    }
    yield_with_parentheses_and_argument: Expression = {
        token!(Kyield) token!(PLParen) wss a: argument_list wss token!(PRParen) => {
            Expression::Yield(Some(a))
        }
    }
    yield_with_parentheses_without_argument: Expression = {
        token!(Kyield) token!(PLParen) wss token!(PRParen) => (Expression::Yield(None))
    }
    yield_with_argument: Expression = {
        token!(Kyield) ws a: argument_without_parentheses => (Expression::Yield(Some(a)))
    }

    // 11.4 Operator expressions
    operator_expression: Expression = {
        assignment_expression,
        defined_without_parentheses,
        conditional_operator_expression,
    }

    // 11.4.2 Assignments
    assignment_expression: Expression = {
        single_assignment_expression,
        abbreviated_assignment_expression,
        assignment_with_rescue_modifier,
    }
    assignment_statement: Statement = {
        single_assignment_statement,
        abbreviated_assignment_statement,
        multiple_assignment_statement,
    }

    // 11.4.2.2 Single assignments
    single_assignment_expression: Expression = {
        single_variable_assignment_expression,
        scoped_constant_assignment_expression,
        single_indexing_assignment_expression,
        single_method_assignment_expression,
    }
    single_assignment_statement: Statement = {
        single_variable_assignment_statement,
        scoped_constant_assignment_statement,
        single_indexing_assignment_statement,
        single_method_assignment_statement,
    }

    // 11.4.2.2.2 Single variable assignments
    single_variable_assignment_expression: Expression = {
        i: variable_i ws token!(OAssign) wss v: operator_expression => {
            Expression::AssignVar(i, Box::new(v))
        }
    }
    single_variable_assignment_statement: Statement = {
        i: variable_i ws token!(OAssign) wss v: method_invocation_without_parentheses => {
            Statement::AssignVar(i, v)
        }
    }

    // 11.4.2.2.3 Scoped constant assignments
    scoped_constant_assignment_expression: Expression = {
        token!(PDblColon) wss i: token!(IConstant) ws token!(OAssign) wss v: operator_expression =>
            (Expression::AssignConst {
                member: None,
                name: i.clone().into(),
                value: Box::new(v),
            }),
        e: primary_expression token!(PDblColon) wss i: token!(IConstant) ws token!(OAssign) wss
            v: operator_expression =>
            (Expression::AssignConst {
                member: Some(Box::new(e)),
                name: i.clone().into(),
                value: Box::new(v),
            }),
    }
    scoped_constant_assignment_statement: Statement = {
        token!(PDblColon) wss i: token!(IConstant) ws token!(OAssign) wss
            v: method_invocation_without_parentheses =>
            (Statement::AssignConst {
                member: None,
                name: i.clone().into(),
                value: v,
            }),
        e: primary_expression token!(PDblColon) wss i: token!(IConstant) ws token!(OAssign) wss
            v: method_invocation_without_parentheses =>
            (Statement::AssignConst {
                member: Some(e),
                name: i.clone().into(),
                value: v,
            }),
    }

    // 11.4.2.2.4 Single indexing assignments
    single_indexing_assignment_expression: Expression = {
        e: primary_expression token!(PLBracket) wss i: opt!(indexing_argument_list) wss
            token!(PRBracket) ws token!(OAssign) wss v: operator_expression => {
            Expression::AssignIndex(Box::new(e), i.unwrap_or_default(), Box::new(v))
        }
    }
    single_indexing_assignment_statement: Statement = {
        e: primary_expression token!(PLBracket) wss i: opt!(indexing_argument_list) wss
            token!(PRBracket) ws token!(OAssign) wss v: method_invocation_without_parentheses
            => (Statement::AssignIndex(e, i.unwrap_or_default(), v))
    }

    // 11.4.2.2.5 Single method assignments
    single_method_assignment_expression: Expression = {
        e: primary_expression ws token!(PDot, PDblColon,) wss i: token!(ILocal) ws token!(OAssign)
            wss v: operator_expression =>
            (Expression::AssignMethod(Box::new(e), i.clone().into(), Box::new(v))),
        e: primary_expression ws token!(PDot) wss i: token!(IConstant) ws token!(OAssign) wss
            v: operator_expression =>
            (Expression::AssignMethod(Box::new(e), i.clone().into(), Box::new(v))),
    }
    single_method_assignment_statement: Statement = {
        e: primary_expression ws token!(PDot, PDblColon,) wss i: token!(ILocal) ws token!(OAssign)
            wss v: method_invocation_without_parentheses =>
            (Statement::AssignMethod(e, i.clone().into(), v)),
        e: primary_expression ws token!(PDot) wss i: token!(IConstant) ws token!(OAssign) wss
            v: method_invocation_without_parentheses =>
            (Statement::AssignMethod(e, i.clone().into(), v)),
    }

    // 11.4.2.3 Abbreviated assignments
    abbreviated_assignment_expression: Expression = {
        abbreviated_variable_assignment_expression,
        abbreviated_indexing_assignment_expression,
        abbreviated_method_assignment_expression,
    }
    abbreviated_assignment_statement: Statement = {
        abbreviated_variable_assignment_statement,
        abbreviated_indexing_assignment_statement,
        abbreviated_method_assignment_statement,
    }

    // 11.4.2.3.2 Abbreviated variable assignments
    abbreviated_variable_assignment_expression: Expression = {
        i: variable_i ws o: assignment_operator wss v: operator_expression => (Expression::AssignOp {
            member: None,
            name: i,
            op: o.clone().into(),
            value: Box::new(v),
        }),
    }
    abbreviated_variable_assignment_statement: Statement = {
        i: variable_i ws o: assignment_operator wss v: method_invocation_without_parentheses => {
            Statement::AssignOp {
                member: None,
                name: i,
                op: o.clone().into(),
                value: v,
            }
        }
    }

    // 11.4.2.3.3 Abbreviated indexing assignments
    abbreviated_indexing_assignment_expression: Expression = {
        e: primary_expression token!(PLBracket) wss i: opt!(indexing_argument_list) wss
            token!(PRBracket) ws o: assignment_operator wss v: operator_expression => {
            Expression::AssignIndexOp {
                expr: Box::new(e),
                index: i.unwrap_or_default(),
                op: o.clone().into(),
                value: Box::new(v),
            }
        }
    }
    abbreviated_indexing_assignment_statement: Statement = {
        e: primary_expression token!(PLBracket) wss i: opt!(indexing_argument_list) wss
            token!(PRBracket) ws o: assignment_operator wss v: method_invocation_without_parentheses
            => (Statement::AssignIndexOp {
                expr: e,
                index: i.unwrap_or_default(),
                op: o.clone().into(),
                value: v,
            })
    }

    // 11.4.2.3.4 Abbreviated method assignments
    abbreviated_method_assignment_expression: Expression = {
        e: primary_expression ws token!(PDot, PDblColon,) wss i: token!(ILocal) ws
            o: assignment_operator wss v: operator_expression =>
            (Expression::AssignOp {
                member: Some(Box::new(e)),
                name: i.clone().into(),
                op: o.clone().into(),
                value: Box::new(v),
            }),
        e: primary_expression ws token!(PDot) wss i: token!(IConstant) ws o: assignment_operator
            wss v: operator_expression =>
            (Expression::AssignOp {
                member: Some(Box::new(e)),
                name: i.clone().into(),
                op: o.clone().into(),
                value: Box::new(v),
            }),
    }
    abbreviated_method_assignment_statement: Statement = {
        e: primary_expression ws token!(PDot, PDblColon,) wss i: token!(ILocal) ws
            o: assignment_operator wss v: method_invocation_without_parentheses =>
            (Statement::AssignOp {
                member: Some(e),
                name: i.clone().into(),
                op: o.clone().into(),
                value: v,
            }),
        e: primary_expression ws token!(PDot) wss i: token!(IConstant) ws
            o: assignment_operator wss v: method_invocation_without_parentheses =>
            (Statement::AssignOp {
                member: Some(e),
                name: i.clone().into(),
                op: o.clone().into(),
                value: v,
            }),
    }

    // 11.4.2.4 Multiple assignments
    multiple_assignment_statement: Statement = {
        many_to_one_assignment_statement,
        one_to_packing_assignment_statement,
        many_to_many_assignment_statement,
    }
    many_to_one_assignment_statement: Statement = {
        l: left_hand_side ws token!(OAssign) wss r: multiple_right_hand_side => {
            Statement::MultiAssign(vec![MultiLHSItem::LHS(l)], r)
        }
    }
    one_to_packing_assignment_statement: Statement = {
        l: packing_left_hand_side ws token!(OAssign) wss r: method_invocation_without_parentheses =>
            (Statement::MultiAssign(vec![l], MultiRightHandSide {
                items: vec![r],
                splat: None,
            })),
        l: packing_left_hand_side ws token!(OAssign) wss r: operator_expression =>
            (Statement::MultiAssign(vec![l], MultiRightHandSide {
                items: vec![r],
                splat: None,
            })),
    }
    many_to_many_assignment_statement: Statement = {
        l: multiple_left_hand_side ws token!(OAssign) wss r: multiple_right_hand_side =>
            (Statement::MultiAssign(l, r)),
        l: multiple_left_hand_side_not_packing ws token!(OAssign) wss
            r: method_invocation_without_parentheses =>
            (Statement::MultiAssign(l, MultiRightHandSide {
                items: vec![r],
                splat: None,
            })),
        l: multiple_left_hand_side_not_packing ws token!(OAssign) wss r: operator_expression =>
            (Statement::MultiAssign(l, MultiRightHandSide {
                items: vec![r],
                splat: None,
            })),
    }
    left_hand_side: LeftHandSide = {
        v: variable_i => (LeftHandSide::Var(v)),
        e: primary_expression token!(PLBracket) wss i: opt!(indexing_argument_list) wss
            token!(PRBracket) =>
            (LeftHandSide::Index(e, i)),
        token!(PDblColon) wss i: token!(IConstant) => (LeftHandSide::RootConst(i.clone().into())),
        e: primary_expression ws token!(PDot, PDblColon,) wss i: token!(ILocal, IConstant,) =>
            (LeftHandSide::Member(e, i.clone().into())),
    }
    multiple_left_hand_side: MultiLeftHandSide = {
        a: many1!(do_parse!(
            i: multiple_left_hand_side_item >> ws >> token!(PComma) >> wss >> (i)
        ), err: Unexpected(Expected::MultipleLeftHandSide))
        b: opt!(alt!(
            multiple_left_hand_side_item, packing_left_hand_side,
            err: Expression // error won’t be shown anyway because of opt!
        )) => {
            a.into_iter().chain(iter::once(b).filter(|x| x.is_some()).map(|x| x.unwrap())).collect()
        },
        i: packing_left_hand_side => (vec![i]),
        i: grouped_left_hand_side => (vec![i]),
    }
    multiple_left_hand_side_not_packing: MultiLeftHandSide = {
        not!(
            packing_left_hand_side; err: Unexpected(Expected::MultipleLeftHandSideNotPacking)
        ) m: multiple_left_hand_side => (m)
    }
    packing_left_hand_side: MultiLHSItem = {
        token!(OMul) wss l: opt!(left_hand_side) => (MultiLHSItem::Packing(l))
    }
    grouped_left_hand_side: MultiLHSItem = {
        token!(PLParen) wss g: multiple_left_hand_side wss token!(PRParen) =>
            (MultiLHSItem::Group(g))
    }
    multiple_left_hand_side_item: MultiLHSItem = {
        l: left_hand_side => (MultiLHSItem::LHS(l)),
        grouped_left_hand_side,
    }
    multiple_right_hand_side: MultiRightHandSide = {
        a: operator_expression_list
            s: opt!(do_parse!(ws >> token!(PComma) >> wss >> s: splatting_right_hand_side >> (s)))
            => (MultiRightHandSide {
                items: a,
                splat: s,
            }),
        s: splatting_right_hand_side => (MultiRightHandSide {
            items: Vec::with_capacity(0),
            splat: Some(s),
        }),
    }
    splatting_right_hand_side: Expression = { splatting_argument }

    // 11.4.2.5 Assignments with rescue modifiers
    assignment_with_rescue_modifier: Expression = {
        l: left_hand_side ws token!(OAssign) wss o: operator_expression ws token!(Krescue) wss
            r: operator_expression => {
            Expression::AssignRescue(Box::new(l), Box::new(o), Box::new(r))
        }
    }

    // 11.4.3 Unary operator expressions
    unary_minus_expression: Expression = {
        token!(OSub) wss e: power_expression => (Expression::UMinus(Box::new(e))),
        power_expression,
    }
    unary_expression: Expression = {
        token!(OBitInv) e: unary_expression => (Expression::BitInv(Box::new(e))),
        token!(OAdd) e: unary_expression => (Expression::UPlus(Box::new(e))),
        token!(ONot) e: unary_expression => (Expression::Not(Box::new(e))),
        primary_expression,
    }

    // 11.4.3.2 The defined? expression
    defined_with_parentheses: Expression = {
        token!(Kdefined) wss token!(PLParen) wss e: expression wss token!(PRParen) =>
            (Expression::Defined(Box::new(e)))
    }
    defined_without_parentheses: Expression = {
        token!(Kdefined) wss e: operator_expression => (Expression::Defined(Box::new(e)))
    }

    // 11.4.4 Binary operator expressions
    equality_expression: Expression = {
        a: relational_expression b: opt!(do_parse!(
            ws >> o: token!(OCmp, OCaseEq, OEq, ONeq, OMatch, ONMatch,)
                >> wss
                >> b: relational_expression
                >> ((o, b))
        )) => (match b {
            Some((o, b)) => bin_op_expr(a, o.clone(), b),
            None => a,
        })
    }
    relational_expression: Expression = {
        a: relational_expression ws o: token!(OGeq, OGt, OLeq, OLt,) wss b: bitwise_or_expression =>
            (bin_op_expr(a, o.clone(), b)),
        bitwise_or_expression,
    }
    bitwise_or_expression: Expression = {
        a: bitwise_or_expression ws o: token!(OBitOr, OBitXor,) wss b: bitwise_and_expression =>
            (bin_op_expr(a, o.clone(), b)),
        bitwise_and_expression,
    }
    bitwise_and_expression: Expression = {
        a: bitwise_and_expression ws o: token!(OBitAnd) wss b: bitwise_shift_expression =>
            (bin_op_expr(a, o.clone(), b)),
        bitwise_shift_expression,
    }
    bitwise_shift_expression: Expression = {
        a: bitwise_shift_expression ws o: token!(OShl, OShr,) wss b: additive_expression =>
            (bin_op_expr(a, o.clone(), b)),
        additive_expression,
    }
    additive_expression: Expression = {
        a: additive_expression ws o: token!(OAdd, OSub,) wss b: multiplicative_expression =>
            (bin_op_expr(a, o.clone(), b)),
        multiplicative_expression,
    }
    multiplicative_expression: Expression = {
        a: multiplicative_expression ws o: token!(OMul, ODiv, ORem,) wss b: unary_minus_expression
            => (bin_op_expr(a, o.clone(), b)),
        unary_minus_expression,
    }
    power_expression: Expression = {
        a: power_expression ws o: token!(OPow) wss b: unary_expression =>
            (bin_op_expr(a, o.clone(), b)),
        unary_expression,
    }

    // 11.5 Primary expressions
    primary_expression: Expression = {
        class_definition,
        singleton_class_definition,
        module_definition,
        method_definition,
        singleton_method_definition,
        yield_with_optional_argument,
        if_expression,
        unless_expression,
        case_expression,
        while_expression,
        until_expression,
        for_expression,
        return_without_argument,
        break_without_argument,
        next_without_argument,
        redo_expression,
        retry_expression,
        begin_expression,
        grouping_expression,
        variable_reference,
        scoped_constant_reference,
        array_constructor,
        hash_constructor,
        literal,
        defined_with_parentheses,
        primary_method_invocation,
    }

    // 11.5.2 Control structures
    // 11.5.2.2 Conditional expressions
    // 11.5.2.2.2 The if expression
    if_expression: Expression = {
        token!(Kif) wss c: expression ws t: then_clause wss p: many0!(elsif_clause) wss
            e: opt!(else_clause) wss token!(Kend) => (Expression::If {
                cond: Box::new(c),
                then: t,
                elsif: p,
                else_: e,
            }),
    }
    then_clause: StatementList = {
        alt!(
            do_parse!(opt!(separator) >> wss >> token!(Kthen) >> ()),
            do_parse!(separator >> ()),
            err: ThenClause
        ) s: compound_statement => (s)
    }
    else_clause: StatementList = {
        token!(Kelse) wss s: compound_statement => (s)
    }
    elsif_clause: (Expression, StatementList) = {
        token!(Kelsif) wss e: expression ws b: then_clause => ((e, b))
    }

    // 11.5.2.2.3 The unless expression
    unless_expression: Expression = {
        token!(Kunless) wss c: expression ws t: then_clause wss e: opt!(else_clause) wss
            token!(Kend) => (Expression::Unless {
                cond: Box::new(c),
                then: t,
                else_: e,
            }),
    }

    // 11.5.2.2.4 The case expression
    case_expression: Expression = {
        case_expression_with_expression,
        case_expression_without_expression,
    }
    case_expression_with_expression: Expression = {
        token!(Kcase) wss c: expression ws opt!(separator_list) wss
            w: many1!(when_clause, err: Unexpected(Expected::WhenClause)) wss e: opt!(else_clause)
            wss token!(Kend) => (Expression::Case {
                expr: Some(Box::new(c)),
                cases: w,
                else_: e,
            }),
    }
    case_expression_without_expression: Expression = {
        token!(Kcase) ws opt!(separator_list) wss
            w: many1!(when_clause, err: Unexpected(Expected::WhenClause)) wss
            e: opt!(else_clause) wss token!(Kend) => (Expression::Case {
                expr: None,
                cases: w,
                else_: e,
            }),
    }
    when_clause: (WhenArgument, StatementList) = {
        token!(Kwhen) wss a: when_argument ws b: then_clause => ((a, b))
    }
    when_argument: WhenArgument = {
        a: operator_expression_list
            b: opt!(do_parse!(ws >> token!(PComma) >> wss >> s: splatting_argument >> (s))) =>
            (WhenArgument { args: a, array: b }),
        a: splatting_argument => (WhenArgument {
            args: Vec::with_capacity(0),
            array: Some(a),
        }),
    }

    // 11.5.2.2.5 Conditional operator expression
    conditional_operator_expression: Expression = {
        c: range_constructor ws token!(PQuestion) wss a: operator_expression ws token!(PColon)
            wss b: operator_expression =>
            (Expression::Ternary {
                cond: Box::new(c),
                then: Box::new(a),
                else_: Box::new(b),
            }),
        range_constructor,
    }

    // 11.5.2.3 Iteration expressions
    // 11.5.2.3.2 The while expression
    while_expression: Expression = {
        token!(Kwhile) wss c: expression ws b: do_clause wss token!(Kend) =>
            (Expression::While(Box::new(c), b))
    }
    do_clause: StatementList = {
        alt!(
            do_parse!(separator >> ()), do_parse!(token!(Kdo) >> ()),
            err: DoClause
        ) c: compound_statement => (c)
    }

    // 11.5.2.3.3 The until expression
    until_expression: Expression = {
        token!(Kuntil) wss c: expression ws b: do_clause wss token!(Kend) =>
            (Expression::Until(Box::new(c), b))
    }

    // 11.5.2.3.4 The for expression
    for_expression: Expression = {
        token!(Kfor) wss v: for_variable ws token!(Kin) wss e: expression ws b: do_clause wss
            token!(Kend) =>
            (Expression::For(v, Box::new(e), b))
    }
    for_variable: MultiLeftHandSide = {
        l: left_hand_side => (vec![MultiLHSItem::LHS(l)]),
        multiple_left_hand_side,
    }

    // 11.5.2.4 Jump expressions
    // 11.5.2.4.2 The return expression
    return_without_argument: Expression = {
        token!(Kreturn) => (Expression::Return(None))
    }
    return_with_argument: Expression = {
        token!(Kreturn) ws a: jump_argument => (Expression::Return(Some(a)))
    }
    jump_argument: Arguments = { argument_list }

    // 11.5.2.4.3 The break expression
    break_without_argument: Expression = {
        token!(Kbreak) => (Expression::Break(None))
    }
    break_with_argument: Expression = {
        token!(Kbreak) ws a: jump_argument => (Expression::Break(Some(a)))
    }

    // 11.5.2.4.4 The next expression
    next_without_argument: Expression = {
        token!(Knext) => (Expression::Next(None))
    }
    next_with_argument: Expression = {
        token!(Knext) ws a: jump_argument => (Expression::Next(Some(a)))
    }

    // 11.5.2.4.5 The redo expression
    redo_expression: Expression = {
        token!(Kredo) => (Expression::Redo)
    }

    // 11.5.2.4.6 The retry expression
    retry_expression: Expression = {
        token!(Kretry) => (Expression::Retry)
    }

    // 11.5.2.5 The begin expression
    begin_expression: Expression = {
        token!(Kbegin) wss b: body_statement wss token!(Kend) => (Expression::Begin(b))
    }
    body_statement: BodyStatement = {
        b: compound_statement wss r: many0!(rescue_clause) wss e: opt!(else_clause) wss
            f: opt!(ensure_clause) => [ i => {
            // Ruby 2.6: else without rescue is a syntax error
            if e.is_some() && r.is_empty() {
                Err(ParseError::Unexpected(i.without_state(), Expected::RescueClause))
            } else {
                Ok((i, BodyStatement {
                    body: b,
                    rescue: r,
                    else_: e,
                    ensure: f,
                }))
            }
        }]
    }
    rescue_clause: Rescue = {
        token!(Krescue) ws e: opt!(exception_class_list) wss v: opt!(exception_variable_assignment)
            ws t: then_clause => (Rescue {
            classes: e,
            variable: v,
            body: t,
        })
    }
    exception_class_list: MultiRightHandSide = {
        multiple_right_hand_side,
        e: operator_expression => (MultiRightHandSide { items: vec![e], splat: None }),
    }
    exception_variable_assignment: LeftHandSide = {
        token!(PFatArrow) wss l: left_hand_side => (l)
    }
    ensure_clause: StatementList = {
        token!(Kensure) wss c: compound_statement => (c)
    }

    // 11.5.3 Grouping statement
    grouping_expression: Expression = {
        token!(PLParen) wss c: alt!(
            expression,
            do_parse!(s: compound_statement >> (Expression::Statements(s))),
            err: Expression
        ) wss token!(PRParen) => (c)
    }

    // 11.5.4 Variable references
    variable_reference: Expression = {
        variable,
        pseudo_variable,
    }
    variable: Expression = {
        t: token!(ILocal, IConstant, IGlobal, IClass, IInstance,)
            => (Expression::Variable(t.clone().into()))
    }
    variable_i: Ident = {
        t: token!(ILocal, IConstant, IGlobal, IClass, IInstance,) => (t.clone().into())
    }
    scoped_constant_reference: Expression = {
        token!(PDblColon) wss i: token!(IConstant) => (Expression::RootConst(i.clone().into())),
        e: primary_expression token!(PDblColon) wss i: token!(IConstant) =>
            (Expression::SubConst(Box::new(e), i.clone().into())),
    }

    // 11.5.4.8 Pseudo variables
    pseudo_variable: Expression = {
        nil_expression,
        true_expression,
        false_expression,
        self_expression,
    }

    // 11.5.4.8.2 The nil expression
    nil_expression: Expression = {
        token!(Knil) => (Expression::Nil)
    }

    // 11.5.4.8.3 The true expression and the false expression
    true_expression: Expression = {
        token!(Ktrue) => (Expression::True)
    }
    false_expression: Expression = {
        token!(Kfalse) => (Expression::False)
    }

    // 11.5.4.8.4 The self expression
    self_expression: Expression = {
        token!(Kself) => (Expression::SelfExpr)
    }

    // 11.5.5.1 Array constructor
    array_constructor: Expression = {
        token!(PLBracket) wss i: opt!(indexing_argument_list) wss token!(PRBracket) =>
            (Expression::ArrayConstructor(i))
    }

    // 11.5.5.2 Hash constructor
    hash_constructor: Expression = {
        token!(PLBrace) wss a: opt!(do_parse!(
            a: association_list >> ws >> opt!(token!(PComma)) >> (a)
        )) wss token!(PRBrace) =>
            (Expression::HashConstructor(a.unwrap_or_else(|| Vec::with_capacity(0))))
    }
    association_list: (Vec<(Expression, Expression)>) = {
        a: association b: many0!(do_parse!(
            ws >> token!(PComma) >> wss >> a: association >> (a)
        )) => (iter::once(a).chain(b.into_iter()).collect())
    }
    association: (Expression, Expression) = {
        // Ruby 1.9: literal hash syntax
        k: token!(ILocal) ws token!(PColon) wss v: association_value => {
            (Expression::Variable(k.clone().into()), v)
        },
        k: association_key ws token!(PFatArrow) wss v: association_value => ((k, v)),
    }
    association_key: Expression = { operator_expression }
    association_value: Expression = { operator_expression }

    // 11.5.5.3 Range constructor
    range_constructor: Expression = {
        a: operator_or_expression ws o: token!(P3Dots, P2Dots,) wss b: operator_or_expression =>
            (Expression::Range {
                start: Some(Box::new(a)),
                end: Some(Box::new(b)),
                inclusive: o == &Token::P2Dots,
            }),
        // Ruby 2.6: endless ranges
        o: token!(P2Dots, P3Dots,) wss b: operator_or_expression => (Expression::Range {
            start: None,
            end: Some(Box::new(b)),
            inclusive: o == &Token::P2Dots,
        }),
        a: operator_or_expression ws o: token!(P2Dots, P3Dots,) => (Expression::Range {
            start: Some(Box::new(a)),
            end: None,
            inclusive: o == &Token::P2Dots,
        }),
        // --
        operator_or_expression,
    }

    // 12 Statements
    statement: Statement = {
        alias_statement,
        assignment_statement,
        undef_statement,
        if_modifier_statement,
        unless_modifier_statement,
        while_modifier_statement,
        until_modifier_statement,
        rescue_modifier_statement,
        expression_statement,
    }

    // 12.2 The expression statement
    expression_statement: Statement = {
        e: expression => (Statement::Expr(e))
    }

    // 12.3 The if modifier statement
    if_modifier_statement: Statement = {
        s: statement ws token!(Kif) wss e: expression => (Statement::IfMod(Box::new(s), e))
    }

    // 12.4 The unless modifier statement
    unless_modifier_statement: Statement = {
        s: statement ws token!(Kunless) wss e: expression => (Statement::UnlessMod(Box::new(s), e))
    }

    // 12.5 The while modifier statement
    while_modifier_statement: Statement = {
        s: statement ws token!(Kwhile) wss e: expression => (Statement::WhileMod(Box::new(s), e))
    }

    // 12.6 The until modifier statement
    until_modifier_statement: Statement = {
        s: statement ws token!(Kuntil) wss e: expression => (Statement::UntilMod(Box::new(s), e))
    }

    // 12.7 The rescue modifier statement
    rescue_modifier_statement: Statement = {
        s: main_statement_of_rescue_modifier_statement ws token!(Krescue)
            f: fallback_statement_of_rescue_modifier_statement =>
        (Statement::RescueMod(Box::new(s), Box::new(f)))
    }
    main_statement_of_rescue_modifier_statement: Statement = { statement }
    fallback_statement_of_rescue_modifier_statement: Statement = {
        not!(
            statement_not_allowed_in_fallback_statement;
            err: Unexpected(Expected::RescueFallbackStatement)
        ) s: statement => (s)
    }
    statement_not_allowed_in_fallback_statement: () = {
        keyword_and_expression => (),
        keyword_or_expression => (),
        if_modifier_statement => (),
        unless_modifier_statement => (),
        while_modifier_statement => (),
        until_modifier_statement => (),
        rescue_modifier_statement => (),
    }

    // 13.1.2 Module definition
    module_definition: Expression = {
        token!(Kmodule) wss p: module_path wss b: module_body wss token!(Kend) =>
            (Expression::Module { path: p, body: b })
    }
    module_path: DefPath = {
        top_module_path,
        module_name,
        nested_module_path,
    }
    module_name: DefPath = {
        i: token!(IConstant) => (DefPath::Current(i.clone().into()))
    }
    top_module_path: DefPath = {
        token!(PDblColon) wss i: token!(IConstant) => (DefPath::Root(i.clone().into()))
    }
    nested_module_path: DefPath = {
        e: primary_expression ws token!(PDblColon) i: token!(IConstant) =>
            (DefPath::Member(Box::new(e), i.clone().into()))
    }
    module_body: BodyStatement = { body_statement }

    // 13.2.2 Class definition
    class_definition: Expression = {
        token!(Kclass) wss p: class_path ws
            s: opt!(do_parse!(token!(OLt) >> wss >> s: superclass >> (s))) ws separator wss
            b: class_body wss token!(Kend) => (Expression::Class {
            path: p,
            superclass: s.map(|s| Box::new(s)),
            body: b,
        })
    }
    class_path: DefPath = {
        top_class_path,
        class_name,
        nested_class_path,
    }
    class_name: DefPath = {
        i: token!(IConstant) => (DefPath::Current(i.clone().into()))
    }
    top_class_path: DefPath = {
        token!(PDblColon) wss i: token!(IConstant) => (DefPath::Root(i.clone().into()))
    }
    nested_class_path: DefPath = {
        e: primary_expression ws token!(PDblColon) i: token!(IConstant) =>
            (DefPath::Member(Box::new(e), i.clone().into()))
    }
    superclass: Expression = { expression }
    class_body: BodyStatement = { body_statement }

    // 13.3.1 Method definition
    method_definition: Expression = {
        token!(Kdef) wss n: defined_method_name ws p: method_parameter_part wss b: method_body
            wss token!(Kend) => (Expression::Method {
            name: n,
            params: p,
            body: b,
        })
    }
    defined_method_name: Ident = {
        method_name,
        i: token!(IMethodOnly) => (i.clone().into()),
    }
    method_body: BodyStatement = { body_statement }

    // 13.3.2 Method parameters
    method_parameter_part: Parameters = {
        token!(PLParen) wss p: opt!(parameter_list) wss token!(PRParen) => (p.unwrap_or_default()),
        // FIXME: this causes a stack overflow somehow
        p: opt!(parameter_list) ws separator => (p.unwrap_or_default()),
    }
    // Ruby 1.9: laxer parameters
    parameter_list: Parameters = {
        a: parameter b: many0!(do_parse!(ws >> token!(PComma) >> wss >> p: parameter >> (p))) => {
            iter::once(a).chain(b.into_iter()).collect()
        }
    }
    parameter: Parameter = {
        i: token!(ILocal) ws a: opt!(do_parse!(
            ws >> a: alt!(
                do_parse!(token!(OAssign) >> wss >> e: operator_expression >> ((true, Some(e)))),
                // Ruby 2.0 keyword arguments/Ruby 2.1 required keyword arguments
                do_parse!(token!(PColon) >> wss >> e: opt!(operator_expression) >> ((false, e))),
                err: Parameter
            ) >> (a)
        )) => {
            match a {
                Some((true, Some(e))) => Parameter::Optional(i.clone().into(), e),
                Some((true, None)) => unreachable!(),
                Some((false, e)) => Parameter::Keyword(i.clone().into(), e),
                None => Parameter::Mandatory(i.clone().into()),
            }
        },
        token!(OBitAnd) wss i: token!(ILocal) => {
            Parameter::Block(i.clone().into())
        },
        token!(OMul) i: opt!(do_parse!(wss >> i: token!(ILocal) >> (i))) => {
            Parameter::Splat(i.map(|t| t.clone().into()))
        },
    }

    // 13.3.6 The alias statement
    alias_statement: Statement = {
        token!(Kalias) wss n: new_name wss a: aliased_name => (Statement::Alias(n, a))
    }
    new_name: IdentRef = { method_name_or_symbol }
    aliased_name: IdentRef = { method_name_or_symbol }

    // 13.3.7 The undef statement
    undef_statement: Statement = {
        token!(Kundef) wss l: undef_list => (Statement::Undef(l))
    }
    undef_list: (Vec<IdentRef>) = {
        a: method_name_or_symbol b: many0!(do_parse!(
            wss >> token!(PComma) >> wss >> n: method_name_or_symbol >> (n)
        )) => (iter::once(a).chain(b.into_iter()).collect())
    }
    method_name_or_symbol: IdentRef = {
        i: defined_method_name => (IdentRef::Ident(i)),
        i: token!(LSymbol) => (match i {
            Token::LSymbol(s) => IdentRef::Symbol(s.to_string()),
            _ => unreachable!() // token!(LSymbol) above
        }),
    }

    // 13.4.2 Singleton class definition
    singleton_class_definition: Expression = {
        token!(Kclass) wss token!(OShl) wss e: expression ws separator wss b: singleton_class_body
            wss token!(Kend) => (Expression::SingletonClass {
            expr: Box::new(e),
            body: b,
        })
    }
    singleton_class_body: BodyStatement = { body_statement }

    // 13.4.3 Singleton method definition
    singleton_method_definition: Expression = {
        token!(Kdef) wss s: singleton token!(PDot, PDblColon,) wss i: defined_method_name ws
            p: method_parameter_part wss b: method_body wss token!(Kend) => {
            Expression::SingletonMethod {
                expr: Box::new(s),
                name: i,
                params: p,
                body: b,
            }
        }
    }
    singleton: Expression = {
        token!(PLParen) wss e: expression wss token!(PRParen) => (e),
        variable_reference,
    }
}
