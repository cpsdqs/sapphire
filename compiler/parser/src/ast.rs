//! Abstract syntax tree nodes.

use crate::lex::{Token, UnsignedNumeric};
use std::fmt;

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

impl<'input> From<Token<'input>> for Ident {
    fn from(token: Token<'input>) -> Ident {
        use Token::*;
        match token {
            ILocal(l) => Ident::Local(l.into()),
            IGlobal(g) => Ident::Global(g.into()),
            IConstant(c) => Ident::Const(c.into()),
            IClass(c) => Ident::Class(c.into()),
            IInstance(i) => Ident::Instance(i.into()),
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
        }
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

impl<'input> From<Token<'input>> for AssignmentOp {
    fn from(token: Token<'input>) -> AssignmentOp {
        match token {
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
        }
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

impl<'input> From<Token<'input>> for BinaryOp {
    fn from(token: Token<'input>) -> BinaryOp {
        match token {
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
            token => panic!("don’t know how to turn {:?} into a binary operation", token),
        }
    }
}

impl<'input> From<AssignmentOp> for BinaryOp {
    fn from(op: AssignmentOp) -> BinaryOp {
        match op {
            AssignmentOp::And => BinaryOp::And,
            AssignmentOp::Or => BinaryOp::Or,
            AssignmentOp::BitAnd => BinaryOp::BitAnd,
            AssignmentOp::BitOr => BinaryOp::BitOr,
            AssignmentOp::BitXor => BinaryOp::BitXor,
            AssignmentOp::Shl => BinaryOp::Shl,
            AssignmentOp::Shr => BinaryOp::Shr,
            AssignmentOp::Add => BinaryOp::Add,
            AssignmentOp::Sub => BinaryOp::Sub,
            AssignmentOp::Mul => BinaryOp::Mul,
            AssignmentOp::Div => BinaryOp::Div,
            AssignmentOp::Rem => BinaryOp::Rem,
            AssignmentOp::Pow => BinaryOp::Pow,
        }
    }
}

/// A list of statements.
pub type StatementList = Vec<Statement>;

/// An identifier reference; used in statements like `undef`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentRef {
    Symbol(String),
    Ident(Ident),
}

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    /// An expression statement.
    Expr(Expression),
    /// `alias .0 .1`
    Alias(IdentRef, IdentRef),
    /// `undef .0`
    Undef(Vec<IdentRef>),
    /// `.0 if .1`
    IfMod(Box<Statement>, Expression),
    /// `.0 unless .1`
    UnlessMod(Box<Statement>, Expression),
    /// `.0 while .1`
    WhileMod(Box<Statement>, Expression),
    /// `.0 until .1`
    UntilMod(Box<Statement>, Expression),
    /// `.0 rescue .1`
    RescueMod(Box<Statement>, Box<Statement>),
    /// `.0 = .1`
    AssignVar(Ident, Expression),
    /// `member::name = value`
    ///
    /// if member is None, then it’s `::name`
    AssignConst {
        member: Option<Expression>,
        name: Ident,
        value: Expression,
    },
    /// `.0[.1] = .2`
    AssignIndex(Expression, Arguments, Expression),
    /// `.0..1 = .2`
    AssignMethod(Expression, Ident, Expression),
    /// `member.name op value`
    AssignOp {
        member: Option<Expression>,
        name: Ident,
        op: AssignmentOp,
        value: Expression,
    },
    /// `member[index] op value`
    AssignIndexOp {
        expr: Expression,
        index: Arguments,
        op: AssignmentOp,
        value: Expression,
    },
    /// `.0 = .1`
    MultiAssign(MultiLeftHandSide, MultiRightHandSide),
}

/// A body statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BodyStatement {
    pub body: StatementList,
    pub rescue: Vec<Rescue>,
    pub else_: Option<StatementList>,
    pub ensure: Option<StatementList>,
}

/// A rescue clause.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rescue {
    /// Exception class list
    pub classes: Option<MultiRightHandSide>,
    /// => variable
    pub variable: Option<LeftHandSide>,
    pub body: StatementList,
}

/// When argument.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhenArgument {
    pub args: Vec<Expression>,
    pub array: Option<Expression>,
}

/// Definition path for classes and modules.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefPath {
    Member(Box<Expression>, Ident),
    Root(Ident),
    Current(Ident),
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
    Variable(Ident),
    /// `::.0`
    RootConst(Ident),
    /// `.0..1` or `.0::.1`
    SubConst(Box<Expression>, Ident),
    Literal(Literal),
    Block(Block),
    Nil,
    True,
    False,
    SelfExpr,
    /// `[.0]`
    ArrayConstructor(Option<Arguments>),
    /// `{.0}`
    HashConstructor(Vec<(Expression, Expression)>),
    /// `not .0`
    Not(Box<Expression>),
    /// `-.0`
    UMinus(Box<Expression>),
    /// `+.0`
    UPlus(Box<Expression>),
    /// `~.0`
    BitInv(Box<Expression>),
    /// `.0 .1 .2`
    BinOp(Box<Expression>, BinaryOp, Box<Expression>),
    /// `if cond; then; elsif; else end`
    If {
        cond: Box<Expression>,
        then: StatementList,
        elsif: Vec<(Expression, StatementList)>,
        else_: Option<StatementList>,
    },
    /// `cond ? then : else`
    Ternary {
        cond: Box<Expression>,
        then: Box<Expression>,
        else_: Box<Expression>,
    },
    /// `unless cond; then; else end`
    Unless {
        cond: Box<Expression>,
        then: StatementList,
        else_: Option<StatementList>,
    },
    /// `case expr; cases; else end`
    Case {
        expr: Option<Box<Expression>>,
        cases: Vec<(WhenArgument, StatementList)>,
        else_: Option<StatementList>,
    },
    /// `while .0; .1 end`
    While(Box<Expression>, StatementList),
    /// `until .0; .1 end`
    Until(Box<Expression>, StatementList),
    /// `for .0 in .1; .2 end`
    For(MultiLeftHandSide, Box<Expression>, StatementList),
    Begin(BodyStatement),
    /// `member.name(args)`
    Call {
        member: Option<Box<Expression>>,
        name: Ident,
        args: Arguments,
    },
    /// `.0[.1]`
    Index(Box<Expression>, Arguments),
    /// `defined? .0`
    Defined(Box<Expression>),
    /// `super(.0)`
    Super(Arguments),
    /// `return .0`
    Return(Option<Arguments>),
    /// `break .0`
    Break(Option<Arguments>),
    /// `yield .0`
    Yield(Option<Arguments>),
    /// `next .0`
    Next(Option<Arguments>),
    Redo,
    Retry,
    /// `(.0)`
    Statements(StatementList),
    /// `.0...1` or `.0....1`
    Range {
        start: Option<Box<Expression>>,
        end: Option<Box<Expression>>,
        inclusive: bool,
    },
    /// `module path; body end`
    Module {
        path: DefPath,
        body: BodyStatement,
    },
    /// `class path < superclass; body end`
    Class {
        path: DefPath,
        superclass: Option<Box<Expression>>,
        body: BodyStatement,
    },
    /// `def name params; body end`
    Method {
        name: Ident,
        params: Parameters,
        body: BodyStatement,
    },
    /// `class << expr; body end`
    SingletonClass {
        expr: Box<Expression>,
        body: BodyStatement,
    },
    /// `def expr.name params; body end`
    SingletonMethod {
        expr: Box<Expression>,
        name: Ident,
        params: Parameters,
        body: BodyStatement,
    },
    /// `.0 = .1`
    AssignVar(Ident, Box<Expression>),
    /// `member::name = value`
    ///
    /// if member is None, then it’s `::name`
    AssignConst {
        member: Option<Box<Expression>>,
        name: Ident,
        value: Box<Expression>,
    },
    /// `.0[.1] = .2`
    AssignIndex(Box<Expression>, Arguments, Box<Expression>),
    /// `.0..1 = .2`
    AssignMethod(Box<Expression>, Ident, Box<Expression>),
    /// `member.name op value`
    AssignOp {
        member: Option<Box<Expression>>,
        name: Ident,
        op: AssignmentOp,
        value: Box<Expression>,
    },
    /// `expr[index] op value`
    AssignIndexOp {
        expr: Box<Expression>,
        index: Arguments,
        op: AssignmentOp,
        value: Box<Expression>,
    },
    /// `.0 = .1 rescue .2`
    AssignRescue(Box<LeftHandSide>, Box<Expression>, Box<Expression>),
}

/// Left hand side in a variable assignment.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LeftHandSide {
    Var(Ident),
    Index(Expression, Option<Arguments>),
    Member(Expression, Ident),
    RootConst(Ident),
}

/// Left hand side in a multiple assignment statement.
pub type MultiLeftHandSide = Vec<MultiLHSItem>;

/// A left hand side item in a multiple assignment statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MultiLHSItem {
    LHS(LeftHandSide),
    /// `*.0`
    Packing(Option<LeftHandSide>),
    /// `(.0)`
    Group(MultiLeftHandSide),
}

/// Right hand side in a multiple assignment statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultiRightHandSide {
    pub items: Vec<Expression>,
    pub splat: Option<Expression>,
}

/// A block or lambda.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub params: Parameters,
    pub body: StatementList,
    pub lambda: bool,
}

/// Method or indexing arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Arguments {
    pub items: Vec<Argument>,
    pub hash: Vec<(Expression, Expression)>,
    pub block: Option<Box<Expression>>,
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
    Expr(Expression),
    Splat(Expression),
}

// TODO: enforce order; these do actually have one
/// A list of method parameters.
pub type Parameters = Vec<Parameter>;

/// Method parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Parameter {
    Mandatory(Ident),
    Optional(Ident, Expression),
    Keyword(Ident, Option<Expression>),
    Block(Ident),
    Splat(Option<Ident>),
}
