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
    OUPlus => OUPlus => "+@",
    OAdd => OAdd => "+",
    OUMinus => OUMinus => "-@",
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
    ($i:expr, $submac:ident!($($args:tt)*); err: $err:ident) => {{
        let i = $i.clone();
        if let Err(..) = $submac!($i, $($args)*) {
            Ok((i, ()))
        } else {
            Err(ParseError::Unexpected($i.without_state(), Expected::$err))
        }
    }};
    ($i:expr, $f:expr; err: $err:ident) => {{
        not!($i, call!($f); err: $err)
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

const MAX_PREC_LEVEL: usize = 10;
fn prec_level(op: BinaryOp) -> usize {
    match op {
        BinaryOp::KeywordOr => 10,
        BinaryOp::KeywordAnd => 9,
        BinaryOp::Eq
        | BinaryOp::Neq
        | BinaryOp::Match
        | BinaryOp::NMatch
        | BinaryOp::Cmp
        | BinaryOp::CaseEq => 8,
        BinaryOp::Geq | BinaryOp::Gt | BinaryOp::Leq | BinaryOp::Lt => 7,
        BinaryOp::Or | BinaryOp::BitOr | BinaryOp::BitXor => 6,
        BinaryOp::And | BinaryOp::BitAnd => 5,
        BinaryOp::Shl | BinaryOp::Shr => 4,
        BinaryOp::Add | BinaryOp::Sub => 3,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => 2,
        BinaryOp::Pow => 1,
    }
}

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
        ) => t
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
        ) => t
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
        ) => t
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
        s: opt!(statement_list) opt!(separator_list) => {
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

    statement: Statement = {
        e: expression => (Statement::Expr(e)),
    }

    expression: Expression = {
        token!(PLParen) wss e: compound_statement wss token!(PRParen) => {
            Expression::Statements(e)
        },
        token!(Knot, ONot,) wss e: expression => (Expression::Not(Box::new(e))),
        token!(OBitInv) wss e: expression => (Expression::BitInv(Box::new(e))),
        token!(OAdd) wss e: expression => (Expression::UPlus(Box::new(e))),
        token!(OSub) wss e: expression => (Expression::UMinus(Box::new(e))),
        e: expression ws token!(PQuestion) wss a: expression wss
            token!(PColon) wss b: expression => {
            Expression::Ternary {
                cond: Box::new(e),
                then: Box::new(a),
                else_: Box::new(b),
            }
        },
        e: expression ws t: token!(P2Dots, P3Dots,) wss b: opt!(expression) => {
            Expression::Range {
                start: Some(Box::new(e)),
                end: b.map(Box::new),
                inclusive: t == &Token::P2Dots,
            }
        },
        t: token!(P2Dots, P3Dots,) wss e: expression => {
            Expression::Range {
                start: None,
                end: Some(Box::new(e)),
                inclusive: t == &Token::P2Dots,
            }
        },
        a: expression ws t: token!(
                Kor, Kand, OEq, ONeq, OMatch, ONMatch, OCmp, OCaseEq, OGeq, OGt, OLeq, OLt, OOr,
                OBitOr, OBitXor, OAnd, OBitAnd, OShl, OShr, OAdd, OSub, OMul, ODiv, ORem, OPow,
        ) wss b: expression => {
            #[derive(Debug)]
            enum Item {
                Expr(Expression),
                Op(BinaryOp),
            }
            fn flatten_expr(expr: Expression) -> Vec<Item> {
                match expr {
                    Expression::BinOp(a, op, b) => {
                        flatten_expr(*a).into_iter()
                            .chain(iter::once(Item::Op(op)))
                            .chain(flatten_expr(*b).into_iter())
                            .collect()
                    }
                    e => vec![Item::Expr(e)],
                }
            }

            let mut items: Vec<_> = flatten_expr(b);
            items.insert(0, Item::Expr(a));
            items.insert(1, Item::Op(t.clone().into()));

            for level in 0..=MAX_PREC_LEVEL {
                let mut i = 0;
                while i < items.len() {
                    match items[i] {
                        Item::Op(op) if prec_level(op) == level => {
                            let prev = items.remove(i - 1);
                            i -= 1;
                            let next = items.remove(i + 1);

                            if let (Item::Expr(prev), Item::Expr(next)) = (prev, next) {
                                items[i] = Item::Expr(Expression::BinOp(
                                    Box::new(prev),
                                    op,
                                    Box::new(next),
                                ));
                                i += 1;
                            } else {
                                panic!("binary operation does not have expressions on either side");
                            }
                        }
                        _ => i += 1,
                    }
                }
            }

            assert_eq!(items.len(), 1, "binary operation was not reduced to one expression");
            match items.remove(0) {
                Item::Expr(e) => e,
                _ => panic!("binary operation was reduced to an operator somehow")
            }
        },
        e: expression ws t: token!(PDot, PDblColon,) wss i: method_name
            a: opt!(do_parse!(ws >> a: arguments_without_parens >> (a)))
            b: opt!(do_parse!(wss >> b: block >> (b))) => {
            let mut args = a.unwrap_or_default();
            if let Some(b) = b {
                args.block = Some(Box::new(Expression::Block(b)));
            }
            if args.is_empty() && t == &Token::PDblColon && i.is_const() {
                Expression::SubConst(Box::new(e), i)
            } else {
                Expression::Call {
                    member: Some(Box::new(e)),
                    name: i,
                    args,
                }
            }
        },
        method_definition,
        class_definition,
        module_definition,
        singleton_class_definition,
        if_expression,
        while_expression,
        for_expression,
        e: expression ws token!(PLBracket) wss a: opt!(indexing_argument_list) wss
            token!(PRBracket) => {
            Expression::Index(Box::new(e), a.unwrap_or_default())
        },
        i: variable ws token!(OAssign) wss e: expression => {
            Expression::AssignVar(i, Box::new(e))
        },
        not!(alt!(keyword, operator_method_name, err: Expression); err: Expression)
            i: method_name
            not!(alt!(
                // prevent this rule overriding things like `a + b` and making `+ b` a
                // unary expression
                do_parse!(
                    token!(Whitespace) >> token!(OAdd, OSub,) >> token!(Whitespace, Newlines,) >> ()
                ),
                do_parse!(token!(OAdd, OSub,) >> ()),
                // also prevent this rule overriding `a::B` (but not `a ::B`)
                do_parse!(token!(PDblColon) >> ()),
                err: Expression
            ); err: Expression)
            a: opt!(do_parse!(ws >> a: arguments_without_parens >> (a)))
            b: opt!(do_parse!(wss >> b: block >> (b))) => {
            let mut args = a.unwrap_or_default();
            if let Some(b) = b {
                args.block = Some(Box::new(Expression::Block(b)));
            }
            match (args.is_empty(), &i) {
                (true, Ident::Local(_)) | (true, Ident::Const(_)) => {
                    Expression::Variable(i)
                },
                _ => Expression::Call {
                    member: None,
                    name: i,
                    args,
                }
            }
        },
        not!(keyword; err: Expression) i: variable => (Expression::Variable(i)),
        token!(Kself) => (Expression::SelfExpr),
        token!(Ktrue) => (Expression::True),
        token!(Kfalse) => (Expression::False),
        token!(Knil) => (Expression::Nil),
        token!(PDblColon) wss i: token!(IConstant) ws token!(OAssign) wss e: expression => {
            Expression::AssignConst {
                member: None,
                name: i.clone().into(),
                value: Box::new(e),
            }
        },
        token!(PDblColon) wss i: token!(IConstant) => (Expression::RootConst(i.clone().into())),
        literal,
    }

    variable: Ident = {
        t: token!(ILocal, IConstant, IGlobal, IClass, IInstance,) => (t.clone().into()),
    }

    method_name: Ident = {
        i: token!(ILocal, IConstant, IMethodOnly, IAssignmentLikeMethod,) => (i.clone().into()),
        i: operator_method_name => (i.clone().into()),
        token!(PLBracket) token!(PRBracket) o: opt!(token!(OAssign)) => {
            if o.is_some() {
                Ident::Keyword("[]=")
            } else {
                Ident::Keyword("[]")
            }
        },
        i: keyword => (i.clone().into()),
    }

    arguments_without_parens: Arguments = {
        token!(PLParen) wss a: opt!(argument_list) wss token!(PLParen) => (a.unwrap_or_default()),
        argument_list,
    }
    argument_list: Arguments = {
        p: positional_argument_list
            h: opt!(do_parse!(ws >> token!(PComma) >> wss >> h: association_list >> (h)))
            b: opt!(do_parse!(ws >> token!(PComma) >> wss >> b: block_argument >> (b))) => {
            Arguments {
                items: p,
                hash: h.unwrap_or_default(),
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
    indexing_argument_list: Arguments = {
        p: positional_argument_list
            h: opt!(do_parse!(ws >> token!(PComma) >> wss >> h: association_list >> (h)))
            opt!(do_parse!(ws >> token!(PComma) >> ())) => {
            Arguments {
                items: p,
                hash: h.unwrap_or_else(|| Vec::with_capacity(0)),
                block: None,
            }
        },
        h: association_list opt!(do_parse!(ws >> token!(PComma) >> ())) => {
            Arguments {
                items: Vec::with_capacity(0),
                hash: h,
                block: None,
            }
        },
    }
    block_argument: Expression = { token!(OBitAnd) wss e: expression => e }
    positional_argument_list: (Vec<Argument>) = {
        a: positional_argument
            b: many0!(do_parse!(ws >> token!(PComma) >> wss >> a: positional_argument >> (a))) => {
            iter::once(a).chain(b.into_iter()).collect()
        }
    }
    positional_argument: Argument = {
        token!(OMul) wss s: expression => (Argument::Splat(s)),
        not!(association; err: PositionalArgument) a: expression => (Argument::Expr(a)),
    }
    association_list: (Vec<(Expression, Expression)>) = {
        a: association
            b: many0!(do_parse!(ws >> token!(PComma) >> wss >> a: association >> (a))) => {
            iter::once(a).chain(b.into_iter()).collect()
        }
    }
    association: (Expression, Expression) = {
        k: token!(ILocal) ws token!(PColon) wss v: expression => {
            (Expression::Literal(Literal::Symbol(match k {
                Token::ILocal(name) => name.to_string(),
                _ => unreachable!(),
            })), v)
        },
        k: expression ws token!(PFatArrow) wss v: expression => ((k, v)),
    }

    block: Block = {
        brace_block,
        do_block,
        lambda,
    }
    brace_block: Block = {
        token!(PLBrace) wss p: opt!(block_parameter_list) wss
            b: compound_statement wss token!(PRBrace) => {
            Block {
                params: p.unwrap_or_default(),
                body: b,
                lambda: false,
            }
        }
    }
    do_block: Block = {
        token!(Kdo) wss p: opt!(block_parameter_list) wss
            b: compound_statement wss token!(Kend) => {
            Block {
                params: p.unwrap_or_default(),
                body: b,
                lambda: false,
            }
        }
    }
    lambda: Block = {
        token!(PArrow) p: opt!(do_parse!(wss >> p: lambda_parameter >> (p))) ws token!(PLBrace)
            wss b: compound_statement wss token!(PRBrace) => {
            Block {
                params: p.unwrap_or_default(),
                body: b,
                lambda: true,
            }
        },
        token!(PArrow) p: opt!(do_parse!(wss >> p: lambda_parameter >> (p))) ws token!(Kdo)
            wss b: compound_statement wss token!(Kend) => {
            Block {
                params: p.unwrap_or_default(),
                body: b,
                lambda: true,
            }
        },
    }
    lambda_parameter: Parameters = {
        token!(PLParen) wss p: opt!(parameter_list) wss token!(PRParen) => (p.unwrap_or_default()),
        parameter_list,
    }
    block_parameter_list: Parameters = {
        token!(OBitOr) wss p: opt!(parameter_list) wss token!(OBitOr) => (p.unwrap_or_default()),
    }

    parameter_list: Parameters = {
        a: parameter b: many0!(do_parse!(ws >> token!(PComma) >> wss >> p: parameter >> (p))) => {
            iter::once(a).chain(b.into_iter()).collect()
        }
    }
    parameter: Parameter = {
        i: token!(ILocal) a: opt!(do_parse!(
            ws >> a: alt!(
                do_parse!(token!(OAssign) >> wss >> e: expression >> ((true, Some(e)))),
                do_parse!(token!(PColon) >> wss >> e: opt!(expression) >> ((false, e))),
                err: Parameter
            ) >> (a)
        )) => {
            match a {
                Some((true, Some(e))) => Parameter::Optional(i.clone().into(), e),
                Some((true, None)) => unreachable!(),
                Some((false, e)) => Parameter::Keyword(i.clone().into(), e),
                None => Parameter::Mandatory(i.clone().into()),
            }
        }
    }

    body_statement: BodyStatement = {
        b: compound_statement
            wss r: many0!(rescue_clause)
            wss e: opt!(else_clause)
            wss f: opt!(ensure_clause) => [i => {
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
    then_clause: StatementList = {
        opt!(separator) wss token!(Kthen) wss c: compound_statement => c,
        separator wss c: compound_statement => c,
    }
    else_clause: StatementList = {
        token!(Kelse) wss c: compound_statement => c,
    }
    elsif_clause: (Expression, StatementList) = {
        token!(Kelsif) wss c: expression ws b: then_clause => ((c, b)),
    }
    rescue_clause: Rescue = {
        token!(Krescue)
            ws e: opt!(exception_class_list)
            wss v: opt!(variable) // TEMP
            ws t: then_clause => {
            Rescue {
                classes: e,
                variable: v.map(|v| LeftHandSide::Var(v)),
                body: t,
            }
        }
    }
    exception_class_list: MultiRightHandSide = {
        // TEMP
        a: expression b: many0!(do_parse!(ws >> token!(PComma) >> wss >> b: expression >> (b))) => {
            MultiRightHandSide {
                items: iter::once(a).chain(b.into_iter()).collect(),
                splat: None,
            }
        }
    }
    ensure_clause: StatementList = {
        token!(Kensure) wss c: compound_statement => c,
    }

    if_expression: Expression = {
        token!(Kif) wss c: expression
            ws t: then_clause
            wss p: many0!(elsif_clause)
            wss e: opt!(else_clause)
            wss token!(Kend) => {
            Expression::If {
                cond: Box::new(c),
                then: t,
                elsif: p,
                else_: e,
            }
        }
    }

    while_expression: Expression = {
        token!(Kwhile) wss c: expression
            ws t: do_clause
            wss token!(Kend) => {
            Expression::While(Box::new(c), t)
        }
    }
    do_clause: StatementList = {
        token!(Kdo) ws opt!(separator) wss c: compound_statement => c,
        separator wss c: compound_statement => c,
    }

    for_expression: Expression = {
        token!(Kfor) wss v: variable
            ws token!(Kin)
            wss e: expression
            ws b: do_clause
            wss token!(Kend) => {
            Expression::For(vec![MultiLHSItem::LHS(LeftHandSide::Var(v))], Box::new(e), b)
        }
    }

    method_definition: Expression = {
        token!(Kdef) wss n: method_name ws p: method_parameters wss b: body_statement
            wss token!(Kend) => {
            Expression::Method {
                name: n,
                params: p,
                body: b,
            }
        }
    }
    method_parameters: Parameters = {
        token!(PLParen) wss p: opt!(parameter_list) wss token!(PRParen) => (p.unwrap_or_default()),
        p: opt!(parameter_list) ws separator => (p.unwrap_or_default()),
    }

    class_definition: Expression = {
        token!(Kclass) wss p: const_def_path
            ws s: opt!(do_parse!(
                token!(OLt) >> wss >> s: expression >> (s)
            ))
            ws separator
            wss b: body_statement
            wss token!(Kend) => {
            Expression::Class {
                path: p,
                superclass: s.map(Box::new),
                body: b,
            }
        }
    }
    const_def_path: DefPath = {
        token!(PDblColon) wss i: token!(IConstant) => (DefPath::Root(i.clone().into())),
        e: expression => [ i => {
            match e {
                Expression::SubConst(member, name) => Ok((i, DefPath::Member(member, name))),
                _ => Err(ParseError::Unexpected(i.without_state(), Expected::ConstDefPath))
            }
        }],
        i: token!(IConstant) => (DefPath::Current(i.clone().into()))
    }

    module_definition: Expression = {
        token!(Kmodule) wss p: const_def_path
            ws separator
            wss b: body_statement
            wss token!(Kend) => {
            Expression::Module {
                path: p,
                body: b,
            }
        }
    }

    singleton_class_definition: Expression = {
        token!(Kclass) ws token!(OShl) wss e: expression
            ws separator
            wss b: body_statement
            wss token!(Kend) => {
            Expression::SingletonClass {
                expr: Box::new(e),
                body: b,
            }
        }
    }
}
