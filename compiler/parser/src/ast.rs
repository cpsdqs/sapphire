//! Abstract syntax tree nodes.

use crate::lex::{Token, UnsignedNumeric};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span(pub usize, pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<Option<T>> {
    pub fn map<F: Fn(T) -> U, U>(self, f: F) -> Spanned<Option<U>> {
        Spanned(self.0.map(f), self.1)
    }
}
impl<T: Default> Spanned<Option<T>> {
    pub fn unwrap_or_default(self) -> Spanned<T> {
        match self.0 {
            Some(inner) => Spanned(inner, self.1),
            None => Spanned(T::default(), self.1),
        }
    }
}

/// An identifier.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    Local(String),
    Global(String),
    Const(String),
    Class(String),
    Instance(String),
    MethodOnly(String),
    AssignmentMethod(String),
    Keyword(&'static str),
}

impl Ident {
    pub(crate) fn is_const(&self) -> bool {
        match self {
            Ident::Const(_) => true,
            _ => false,
        }
    }
}

impl<'input> From<&Token<'input>> for Ident {
    fn from(token: &Token<'input>) -> Ident {
        let spanned: Spanned<Ident> = Spanned(token, Span::default()).into();
        spanned.0
    }
}
impl<'input> From<Spanned<&Token<'input>>> for Ident {
    fn from(token: Spanned<&Token<'input>>) -> Ident {
        let spanned: Spanned<Ident> = token.into();
        spanned.0
    }
}
impl<'input> From<Spanned<&Token<'input>>> for Spanned<Ident> {
    fn from(token: Spanned<&Token<'input>>) -> Spanned<Ident> {
        use Token::*;
        let inner = match token.0 {
            ILocal(l) => Ident::Local(l.to_string()),
            IGlobal(g) => Ident::Global(g.to_string()),
            IConstant(c) => Ident::Const(c.to_string()),
            IClass(c) => Ident::Class(c.to_string()),
            IInstance(i) => Ident::Instance(i.to_string()),
            IMethodOnly(i, s) => Ident::MethodOnly(format!("{}{}", i, s)),
            IAssignmentLikeMethod(i) => Ident::AssignmentMethod(format!("{}=", i)),
            K__LINE__ => Ident::Keyword("__LINE__"),
            K__ENCODING__ => Ident::Keyword("__ENCODING__"),
            K__FILE__ => Ident::Keyword("__FILE__"),
            KBEGIN => Ident::Keyword("BEGIN"),
            KEND => Ident::Keyword("END"),
            Kalias => Ident::Keyword("alias"),
            Kand => Ident::Keyword("and"),
            Kbegin => Ident::Keyword("begin"),
            Kbreak => Ident::Keyword("break"),
            Kcase => Ident::Keyword("case"),
            Kclass => Ident::Keyword("class"),
            Kdef => Ident::Keyword("def"),
            Kdefined => Ident::Keyword("defined"),
            Kdo => Ident::Keyword("do"),
            Kelse => Ident::Keyword("else"),
            Kelsif => Ident::Keyword("elsif"),
            Kend => Ident::Keyword("end"),
            Kensure => Ident::Keyword("ensure"),
            Kfor => Ident::Keyword("for"),
            Kfalse => Ident::Keyword("false"),
            Kif => Ident::Keyword("if"),
            Kin => Ident::Keyword("in"),
            Kmodule => Ident::Keyword("module"),
            Knext => Ident::Keyword("next"),
            Knil => Ident::Keyword("nil"),
            Knot => Ident::Keyword("not"),
            Kor => Ident::Keyword("or"),
            Kredo => Ident::Keyword("redo"),
            Krescue => Ident::Keyword("rescue"),
            Kretry => Ident::Keyword("retry"),
            Kreturn => Ident::Keyword("return"),
            Kself => Ident::Keyword("self"),
            Ksuper => Ident::Keyword("super"),
            Kthen => Ident::Keyword("then"),
            Ktrue => Ident::Keyword("true"),
            Kundef => Ident::Keyword("undef"),
            Kunless => Ident::Keyword("unless"),
            Kuntil => Ident::Keyword("until"),
            Kwhen => Ident::Keyword("when"),
            Kwhile => Ident::Keyword("while"),
            Kyield => Ident::Keyword("yield"),
            OBitXor => Ident::Keyword("^"),
            OBitAnd => Ident::Keyword("&"),
            OBitOr => Ident::Keyword("|"),
            OCmp => Ident::Keyword("<=>"),
            OCaseEq => Ident::Keyword("==="),
            OEq => Ident::Keyword("=="),
            OMatch => Ident::Keyword("=~"),
            OGeq => Ident::Keyword(">="),
            OShr => Ident::Keyword(">>"),
            OGt => Ident::Keyword(">"),
            OLeq => Ident::Keyword("<="),
            OShl => Ident::Keyword("<<"),
            OLt => Ident::Keyword("<"),
            OUPlus => Ident::Keyword("+@"),
            OUMinus => Ident::Keyword("-@"),
            OAdd => Ident::Keyword("+"),
            OSub => Ident::Keyword("-"),
            OMul => Ident::Keyword("*"),
            ODiv => Ident::Keyword("/"),
            ORem => Ident::Keyword("%"),
            OPow => Ident::Keyword("**"),
            OBitInv => Ident::Keyword("~"),
            OAssignIndex => Ident::Keyword("[]="),
            OIndex => Ident::Keyword("[]"),
            token => panic!("don’t know how to turn {:?} into an identifier", token),
        };
        Spanned(inner, token.1)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ident::Local(s)
            | Ident::Const(s)
            | Ident::MethodOnly(s)
            | Ident::AssignmentMethod(s) => write!(f, "{}", s),
            Ident::Global(s) => write!(f, "${}", s),
            Ident::Class(s) => write!(f, "@@{}", s),
            Ident::Instance(s) => write!(f, "@{}", s),
            Ident::Keyword(s) => write!(f, "{}", s),
        }
    }
}

/// Assignment operations, such as `&&=`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignmentOp {
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

impl<'input> From<Spanned<&Token<'input>>> for Spanned<AssignmentOp> {
    fn from(token: Spanned<&Token<'input>>) -> Spanned<AssignmentOp> {
        let inner = match token.0 {
            Token::OAssignAnd => AssignmentOp::And,
            Token::OAssignOr => AssignmentOp::Or,
            Token::OAssignBitAnd => AssignmentOp::BitAnd,
            Token::OAssignBitOr => AssignmentOp::BitOr,
            Token::OAssignBitXor => AssignmentOp::BitXor,
            Token::OAssignShl => AssignmentOp::Shl,
            Token::OAssignShr => AssignmentOp::Shr,
            Token::OAssignAdd => AssignmentOp::Add,
            Token::OAssignSub => AssignmentOp::Sub,
            Token::OAssignMul => AssignmentOp::Mul,
            Token::OAssignDiv => AssignmentOp::Div,
            Token::OAssignRem => AssignmentOp::Rem,
            Token::OAssignPow => AssignmentOp::Pow,
            token => panic!(
                "don’t know how to turn {:?} into an assignment operation",
                token
            ),
        };
        Spanned(inner, token.1)
    }
}

/// Binary operations, such as `==`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Neq,
    NMatch,
    KeywordAnd,
    KeywordOr,
    And,
    Or,
    BitXor,
    BitAnd,
    BitOr,
    Cmp,
    CaseEq,
    Eq,
    Match,
    Geq,
    Shr,
    Gt,
    Leq,
    Shl,
    Lt,
    Add,
    Sub,
    Pow,
    Mul,
    Div,
    Rem,
}

impl<'input> From<Spanned<&Token<'input>>> for Spanned<BinaryOp> {
    fn from(token: Spanned<&Token<'input>>) -> Spanned<BinaryOp> {
        let inner = match token.0 {
            Token::ONeq => BinaryOp::Neq,
            Token::ONMatch => BinaryOp::NMatch,
            Token::OAnd => BinaryOp::And,
            Token::OOr => BinaryOp::Or,
            Token::OBitXor => BinaryOp::BitXor,
            Token::OBitAnd => BinaryOp::BitAnd,
            Token::OBitOr => BinaryOp::BitOr,
            Token::OCmp => BinaryOp::Cmp,
            Token::OCaseEq => BinaryOp::CaseEq,
            Token::OEq => BinaryOp::Eq,
            Token::OMatch => BinaryOp::Match,
            Token::OGeq => BinaryOp::Geq,
            Token::OShr => BinaryOp::Shr,
            Token::OGt => BinaryOp::Gt,
            Token::OLeq => BinaryOp::Leq,
            Token::OShl => BinaryOp::Shl,
            Token::OLt => BinaryOp::Lt,
            Token::OAdd => BinaryOp::Add,
            Token::OSub => BinaryOp::Sub,
            Token::OPow => BinaryOp::Pow,
            Token::OMul => BinaryOp::Mul,
            Token::ODiv => BinaryOp::Div,
            Token::ORem => BinaryOp::Rem,
            Token::Kor => BinaryOp::KeywordOr,
            Token::Kand => BinaryOp::KeywordAnd,
            token => panic!(
                "don’t know how to turn {:?} into a binary operation",
                token
            ),
        };
        Spanned(inner, token.1)
    }
}

/// A list of statements.
pub type StatementList = Vec<Spanned<Statement>>;

/// An identifier reference; used in statements like `undef`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentRef {
    Symbol(Spanned<String>),
    Ident(Spanned<Ident>),
}

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    /// An expression statement.
    Expr(Spanned<Expression>),
    /// `alias .0 .1`
    Alias(Spanned<IdentRef>, Spanned<IdentRef>),
    /// `undef .0`
    Undef(Vec<Spanned<IdentRef>>),
    /// `.0 if .1`
    IfMod(Box<Spanned<Statement>>, Spanned<Expression>),
    /// `.0 unless .1`
    UnlessMod(Box<Spanned<Statement>>, Spanned<Expression>),
    /// `.0 while .1`
    WhileMod(Box<Spanned<Statement>>, Spanned<Expression>),
    /// `.0 until .1`
    UntilMod(Box<Spanned<Statement>>, Spanned<Expression>),
    /// `.0 rescue .1`
    RescueMod(Box<Spanned<Statement>>, Box<Spanned<Statement>>),
    /// `.0 = .1`
    AssignVar(Spanned<Ident>, Spanned<Expression>),
    /// `member::name = value`
    ///
    /// if member is None, then it’s `::name`
    AssignConst {
        member: Option<Spanned<Expression>>,
        name: Spanned<Ident>,
        value: Spanned<Expression>,
    },
    /// `.0[.1] = .2`
    AssignIndex(Spanned<Expression>, Spanned<Arguments>, Spanned<Expression>),
    /// `.0..1 = .2`
    AssignMethod(Spanned<Expression>, Spanned<Ident>, Spanned<Expression>),
    /// `member.name op value`
    AssignOp {
        member: Option<Spanned<Expression>>,
        name: Spanned<Ident>,
        op: Spanned<AssignmentOp>,
        value: Spanned<Expression>,
    },
    /// `member[index] op value`
    AssignIndexOp {
        expr: Spanned<Expression>,
        index: Spanned<Arguments>,
        op: Spanned<AssignmentOp>,
        value: Spanned<Expression>,
    },
    /// `.0 = .1`
    MultiAssign(Spanned<MultiLeftHandSide>, Spanned<MultiRightHandSide>),
}

/// A body statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BodyStatement {
    pub body: Spanned<StatementList>,
    pub rescue: Vec<Spanned<Rescue>>,
    pub else_: Option<Spanned<StatementList>>,
    pub ensure: Option<Spanned<StatementList>>,
}

/// A rescue clause.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rescue {
    /// Exception class list
    pub classes: Option<Spanned<MultiRightHandSide>>,
    /// => variable
    pub variable: Spanned<Option<LeftHandSide>>,
    pub body: Spanned<StatementList>,
}

/// When argument.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhenArgument {
    pub args: Vec<Spanned<Expression>>,
    pub array: Option<Spanned<Expression>>,
}

/// Definition path for classes and modules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefPath {
    Member(Box<Spanned<Expression>>, Spanned<Ident>),
    Root(Spanned<Ident>),
    Current(Spanned<Ident>),
}

/// The value of a numeric literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumericValue {
    Decimal(String),
    Octal(String),
    Binary(String),
    Hexadecimal(String),
    Float {
        decimal: (String, String),
        exp_positive: bool,
        exp: String,
    },
}

impl<'input> From<UnsignedNumeric<'input>> for NumericValue {
    fn from(num: UnsignedNumeric<'input>) -> NumericValue {
        use UnsignedNumeric::*;

        match num {
            OctInt(i) => NumericValue::Octal(i.into()),
            HexInt(i) => NumericValue::Hexadecimal(i.into()),
            BinInt(i) => NumericValue::Binary(i.into()),
            DecInt(i) => NumericValue::Decimal(i.into()),
            FloatExp(int, dec, sign, exp) => NumericValue::Float {
                decimal: (int.into(), dec.into()),
                exp_positive: sign == '+',
                exp: exp.into(),
            },
            Float(int, dec) => NumericValue::Float {
                decimal: (int.into(), dec.into()),
                exp_positive: true,
                exp: "0".into(),
            },
        }
    }
}

/// A fragment of a quoted literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum QuotedFragment {
    String(String),
    /// `#$ident`, `#@@ident`, or `#@ident`
    Ident(Ident),
    /// `#{ compound_statement }`
    Interpolated(StatementList),
}

/// A literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Number { positive: bool, value: NumericValue },
    String(String),
    QuotedString(Vec<QuotedFragment>),
    Symbol(String),
}

/// Expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Variable(Spanned<Ident>),
    /// `::.0`
    RootConst(Spanned<Ident>),
    /// `.0..1` or `.0::.1`
    SubConst(Box<Spanned<Expression>>, Spanned<Ident>),
    Literal(Spanned<Literal>),
    Block(Spanned<Block>),
    Nil(Spanned<()>),
    True(Spanned<()>),
    False(Spanned<()>),
    SelfExpr(Spanned<()>),
    /// `[.0]`
    ArrayConstructor(Option<Arguments>),
    /// `{.0}`
    HashConstructor(Vec<(Spanned<Expression>, Spanned<Expression>)>),
    /// `not .0`
    Not(Box<Spanned<Expression>>),
    /// `-.0`
    UMinus(Box<Spanned<Expression>>),
    /// `+.0`
    UPlus(Box<Spanned<Expression>>),
    /// `~.0`
    BitInv(Box<Spanned<Expression>>),
    /// `.0 .1 .2`
    BinOp(Box<Spanned<Expression>>, Spanned<BinaryOp>, Box<Spanned<Expression>>),
    /// `if cond; then; elsif; else end`
    If {
        cond: Box<Spanned<Expression>>,
        then: Spanned<StatementList>,
        elsif: Vec<Spanned<(Spanned<Expression>, Spanned<StatementList>)>>,
        else_: Option<Spanned<StatementList>>,
    },
    /// `cond ? then : else`
    Ternary {
        cond: Box<Spanned<Expression>>,
        then: Box<Spanned<Expression>>,
        else_: Box<Spanned<Expression>>,
    },
    /// `unless cond; then; else end`
    Unless {
        cond: Box<Spanned<Expression>>,
        then: Spanned<StatementList>,
        else_: Option<Spanned<StatementList>>,
    },
    /// `case expr; cases; else end`
    Case {
        expr: Option<Box<Spanned<Expression>>>,
        cases: Vec<(Spanned<WhenArgument>, Spanned<StatementList>)>,
        else_: Option<Spanned<StatementList>>,
    },
    /// `while .0; .1 end`
    While(Box<Spanned<Expression>>, Spanned<StatementList>),
    /// `until .0; .1 end`
    Until(Box<Spanned<Expression>>, Spanned<StatementList>),
    /// `for .0 in .1; .2 end`
    For(Spanned<MultiLeftHandSide>, Box<Spanned<Expression>>, Spanned<StatementList>),
    Begin(Spanned<BodyStatement>),
    /// `member.name(args)`
    Call {
        member: Option<Box<Spanned<Expression>>>,
        name: Spanned<Ident>,
        args: Spanned<Arguments>,
    },
    /// `.0[.1]`
    Index(Box<Spanned<Expression>>, Spanned<Arguments>),
    /// `defined? .0`
    Defined(Box<Spanned<Expression>>),
    /// `super(.0)`
    Super(Spanned<Arguments>),
    /// `return .0`
    Return(Option<Spanned<Arguments>>),
    /// `break .0`
    Break(Option<Spanned<Arguments>>),
    /// `yield .0`
    Yield(Option<Spanned<Arguments>>),
    /// `next .0`
    Next(Option<Spanned<Arguments>>),
    Redo,
    Retry,
    /// `(.0)`
    Statements(Spanned<StatementList>),
    /// `.0...1` or `.0....1`
    Range {
        start: Option<Box<Spanned<Expression>>>,
        end: Option<Box<Spanned<Expression>>>,
        inclusive: bool,
    },
    /// `module path; body end`
    Module {
        path: Spanned<DefPath>,
        body: Spanned<BodyStatement>,
    },
    /// `class path < superclass; body end`
    Class {
        path: Spanned<DefPath>,
        superclass: Option<Box<Spanned<Expression>>>,
        body: Spanned<BodyStatement>,
    },
    /// `def name params; body end`
    Method {
        name: Spanned<Ident>,
        params: Spanned<Parameters>,
        body: Spanned<BodyStatement>,
    },
    /// `class << expr; body end`
    SingletonClass {
        expr: Box<Spanned<Expression>>,
        body: Spanned<BodyStatement>,
    },
    /// `def expr.name params; body end`
    SingletonMethod {
        expr: Box<Spanned<Expression>>,
        name: Spanned<Ident>,
        params: Spanned<Parameters>,
        body: Spanned<BodyStatement>,
    },
    /// `.0 = .1`
    AssignVar(Spanned<Ident>, Box<Spanned<Expression>>),
    /// `member::name = value`
    ///
    /// if member is None, then it’s `::name`
    AssignConst {
        member: Option<Box<Spanned<Expression>>>,
        name: Spanned<Ident>,
        value: Box<Spanned<Expression>>,
    },
    /// `.0[.1] = .2`
    AssignIndex(Box<Spanned<Expression>>, Spanned<Arguments>, Box<Spanned<Expression>>),
    /// `.0..1 = .2`
    AssignMethod(Box<Spanned<Expression>>, Spanned<Ident>, Box<Spanned<Expression>>),
    /// `member.name op value`
    AssignOp {
        member: Option<Box<Spanned<Expression>>>,
        name: Spanned<Ident>,
        op: Spanned<AssignmentOp>,
        value: Box<Spanned<Expression>>,
    },
    /// `expr[index] op value`
    AssignIndexOp {
        expr: Box<Spanned<Expression>>,
        index: Spanned<Arguments>,
        op: Spanned<AssignmentOp>,
        value: Box<Spanned<Expression>>,
    },
    /// `.0 = .1 rescue .2`
    AssignRescue(Box<LeftHandSide>, Box<Spanned<Expression>>, Box<Spanned<Expression>>),
}

/// Left hand side in a variable assignment.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LeftHandSide {
    Var(Spanned<Ident>),
    Index(Spanned<Expression>, Option<Arguments>),
    Member(Spanned<Expression>, Spanned<Ident>),
    RootConst(Spanned<Ident>),
}

/// Left hand side in a multiple assignment statement.
pub type MultiLeftHandSide = Vec<Spanned<MultiLHSItem>>;

/// A left hand side item in a multiple assignment statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MultiLHSItem {
    LHS(Spanned<LeftHandSide>),
    /// `*.0`
    Packing(Spanned<Option<LeftHandSide>>),
    /// `(.0)`
    Group(Spanned<MultiLeftHandSide>),
}

/// Right hand side in a multiple assignment statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultiRightHandSide {
    pub items: Vec<Spanned<Expression>>,
    pub splat: Option<Spanned<Expression>>,
}

/// A block or lambda.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub params: Spanned<Parameters>,
    pub body: Spanned<StatementList>,
    pub lambda: bool,
}

/// Method or indexing arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Arguments {
    pub items: Vec<Spanned<Argument>>,
    pub hash: Vec<Spanned<(Expression, Expression)>>,
    pub block: Option<Box<Spanned<Expression>>>,
}

impl Arguments {
    /// True if there are no arguments.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty() && self.hash.is_empty() && self.block.is_none()
    }
}

/// A positional argument.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    Expr(Spanned<Expression>),
    Splat(Spanned<Expression>),
}

// TODO: enforce order; these do actually have one
/// A list of method parameters.
pub type Parameters = Vec<Spanned<Parameter>>;

/// Method parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Parameter {
    Mandatory(Spanned<Ident>),
    Optional(Spanned<Ident>, Spanned<Expression>),
    Keyword(Spanned<Ident>, Option<Spanned<Expression>>),
    Block(Spanned<Ident>),
    Splat(Spanned<Option<Ident>>),
}
