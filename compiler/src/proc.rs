//! Procs.

use crate::SymbolTable;
use smallvec::SmallVec;
use std::fmt::{self, Write};
use std::sync::Arc;

pub const RESERVED_REGISTERS: usize = 3;
pub const NIL: usize = 0;
pub const VOID: usize = 1;
pub const SELF: usize = 2;

/// A procedure containing a set of VM instructions.
pub struct Proc<T: SymbolTable, U> {
    /// Proc debug name.
    pub name: T::Symbol,
    /// Number of registers.
    pub registers: usize,
    /// Register index at which the block parameter is stored.
    /// If None, the parent value should be used.
    pub block_idx: Option<usize>,
    /// Static values.
    pub statics: Vec<Static<T, U>>,
    /// Addressing mode.
    pub mode: AddressingMode,
    /// Bytecode.
    pub code: Vec<u8>,
    /// Parameters.
    pub params: Params<T>,
    /// Parent registers, if this is a block.
    pub parent_registers: Vec<U>,
}

impl<T: SymbolTable, U> Proc<T, U>
where
    Static<T, U>: Clone,
    Params<T>: Clone,
{
    pub fn clone_with_parents(&self, parents: Vec<U>) -> Proc<T, U> {
        Proc {
            name: self.name,
            registers: self.registers,
            block_idx: self.block_idx,
            statics: self.statics.clone(),
            mode: self.mode,
            code: self.code.clone(),
            params: self.params.clone(),
            parent_registers: parents,
        }
    }
}

impl<T: SymbolTable, U> fmt::Debug for Proc<T, U>
where
    T: fmt::Debug,
    U: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut code = String::new();
        for byte in &self.code {
            if !code.is_empty() {
                write!(code, " ").unwrap();
            }
            write!(code, "{:02X}", byte).unwrap();
        }

        f.debug_struct("Proc")
            .field("name", &self.name)
            .field("registers", &self.registers)
            .field("block_idx", &self.block_idx)
            .field("statics", &self.statics)
            .field("mode", &self.mode)
            .field("code", &code)
            .finish()
    }
}

impl<T: SymbolTable, U: PartialEq> PartialEq for Proc<T, U> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.registers == other.registers
            && self.block_idx == other.block_idx
            && self.statics == other.statics
            && self.mode == other.mode
            && self.code == other.code
            && self.params == other.params
            && self.parent_registers == other.parent_registers
    }
}

/// A static value.
#[derive(Debug)]
pub enum Static<T: SymbolTable, U> {
    Int(i64),
    Float(f64),
    Str(String),
    Sym(T::Symbol),
    Proc(Arc<Proc<T, U>>),
}

impl<T: SymbolTable, U> Clone for Static<T, U> {
    fn clone(&self) -> Self {
        match self {
            Static::Int(i) => Static::Int(*i),
            Static::Float(i) => Static::Float(*i),
            Static::Str(i) => Static::Str(i.clone()),
            Static::Sym(i) => Static::Sym(*i),
            Static::Proc(i) => Static::Proc(Arc::clone(i)),
        }
    }
}

impl<T: SymbolTable, U: PartialEq> PartialEq for Static<T, U> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Static::Int(a), Static::Int(b)) => a == b,
            (Static::Float(a), Static::Float(b)) => a == b,
            (Static::Str(a), Static::Str(b)) => a == b,
            (Static::Sym(a), Static::Sym(b)) => a == b,
            (Static::Proc(a), Static::Proc(b)) => a == b,
            _ => false,
        }
    }
}

/// The proc register addressing mode.
/// Usually u8, but if there are more than 256 registers (or 256 statics) it’ll switch to u16.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressingMode {
    U8,
    U16,
}

impl AddressingMode {
    /// Byte length of an address with the current mode.
    pub fn addr_len(&self) -> usize {
        match self {
            AddressingMode::U8 => 1,
            AddressingMode::U16 => 2,
        }
    }
}

macro_rules! def_op {
    ($($op:ident = $val:expr,)+) => {
        /// VM operations.
        pub struct Op(u8);

        mod _hidden {
            #[repr(u8)]
            #[allow(non_camel_case_types)]
            enum __enum_for_compile_time_verification_of_uniqueness {
                $($op = $val,)+
            }
        }

        impl Op {
            $(pub const $op: u8 = $val;)+
        }

        impl fmt::Display for Op {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $(Op($val) => write!(f, "{}", stringify!($op)),)+
                    Op(i) => write!(f, "<{:02x}>", i),
                }
            }
        }
    }
}

def_op! {
    LOAD_ROOT = 0x01,
    LOAD_TRUE = 0x02,
    LOAD_FALSE = 0x03,
    LOAD_GLOBAL = 0x04,
    LOAD_CONST = 0x05,
    LOAD_CLASS_VAR = 0x06,
    LOAD_IVAR = 0x07,
    LOAD_STRING = 0x08,
    APPEND_STRING = 0x09,
    LOAD_SYMBOL = 0x0A,
    LOAD_I64 = 0x0B,
    LOAD_FLOAT = 0x0C,
    LOAD_BLOCK = 0x0D,
    LOAD_PARENT = 0x0E,
    ARG = 0x12,
    ARG_ASSOC = 0x13,
    ARG_BLOCK = 0x14,
    READ_ARGS = 0x15,
    CALL = 0x10,
    CALL_ONE = 0x17,
    SUPER = 0x11,
    NOT = 0x16,
    JUMP = 0x20,
    JUMP_IF = 0x21,
    JUMP_IF_NOT = 0x22,
    RETURN = 0x23,
    ASSIGN = 0x30,
    ASSIGN_GLOBAL = 0x31,
    ASSIGN_CONST = 0x32,
    ASSIGN_CLASS_VAR = 0x33,
    ASSIGN_IVAR = 0x34,
    ASSIGN_PARENT = 0x35,
    BEGIN_RESCUE = 0x40,
    RESCUE_MATCH = 0x41,
    RESCUE_BIND = 0x42,
    CONTINUE_RESCUE = 0x43,
    END_RESCUE = 0x44,
    DEFINED_CONST = 0x36,
    DEFINED_GLOBAL = 0x37,
    DEFINED_CLASS_VAR = 0x38,
    DEFINED_IVAR = 0x39,
    DEF_MODULE = 0x50,
    DEF_CLASS = 0x51,
    DEF_METHOD = 0x52,
    DEF_SINGLETON_CLASS = 0x53,
    DEF_SINGLETON_METHOD = 0x54,
    PARAM_FALLBACK = 0x55,
}

/// A proc parameter with the register into which the value should be read.
#[derive(Debug, Clone, PartialEq)]
pub enum Param<T: fmt::Debug + Copy + PartialEq> {
    /// A mandatory parameter.
    Mandatory(u16),
    /// An optional parameter. The method body will contain a fallback routine.
    Optional(u16),
    /// A splat parameter. There can only be one.
    Splat(u16),
    /// `(key, register, mandatory)`
    Hash(SmallVec<[(T, u16, bool); 8]>),
}

/// A set of proc parameters.
#[derive(Debug)]
pub struct Params<T: SymbolTable> {
    pub params: SmallVec<[Param<T::Symbol>; 8]>,
    /// The register for the block parameter.
    pub block: u16,
}

impl<T: SymbolTable> Clone for Params<T> {
    fn clone(&self) -> Self {
        Params {
            params: SmallVec::clone(&self.params),
            block: self.block,
        }
    }
}

impl<T: SymbolTable> PartialEq for Params<T> {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && self.block == other.block
    }
}

#[derive(Debug)]
pub struct ConstProc {
    pub symbols: &'static [(usize, &'static str)],
    pub name: usize,
    pub registers: usize,
    pub block_idx: Option<usize>,
    pub statics: &'static [ConstStatic],
    pub mode: AddressingMode,
    pub code: &'static [u8],
    pub params: ConstParams,
}

#[derive(Debug)]
pub enum ConstStatic {
    Int(i64),
    Float(f64),
    Str(&'static str),
    Sym(usize),
    Proc(ConstProc),
}

#[derive(Debug)]
pub struct ConstParams {
    pub params: &'static [ConstParam],
    pub block: u16,
}

#[derive(Debug)]
pub enum ConstParam {
    Mandatory(u16),
    Optional(u16),
    Splat(u16),
    Hash(&'static [(usize, u16, bool)]),
}

impl ConstProc {
    pub fn new<T: SymbolTable, U>(&self, symbols: &mut T) -> Proc<T, U> {
        Proc {
            name: symbols.symbol(self.symbol_name(self.name)),
            registers: self.registers,
            block_idx: self.block_idx,
            statics: self.statics.iter().map(|s| s.new(self, symbols)).collect(),
            mode: self.mode,
            code: self.code.to_vec(),
            params: self.params.new(self, symbols),
            parent_registers: Vec::new(),
        }
    }

    fn symbol_name(&self, id: usize) -> &str {
        self.symbols.iter().find(|(i, _)| *i == id).unwrap().1
    }
}

impl ConstStatic {
    fn new<T: SymbolTable, U>(&self, proc: &ConstProc, symbols: &mut T) -> Static<T, U> {
        match self {
            ConstStatic::Int(i) => Static::Int(*i),
            ConstStatic::Float(i) => Static::Float(*i),
            ConstStatic::Str(i) => Static::Str(i.to_string()),
            ConstStatic::Sym(i) => Static::Sym(symbols.symbol(proc.symbol_name(*i))),
            ConstStatic::Proc(i) => Static::Proc(Arc::new(i.new(symbols))),
        }
    }
}

impl ConstParams {
    fn new<T: SymbolTable>(&self, proc: &ConstProc, symbols: &mut T) -> Params<T> {
        Params {
            params: self
                .params
                .iter()
                .map(|param| param.new(proc, symbols))
                .collect(),
            block: self.block,
        }
    }
}

impl ConstParam {
    fn new<T: SymbolTable>(&self, proc: &ConstProc, symbols: &mut T) -> Param<T::Symbol> {
        match self {
            ConstParam::Mandatory(i) => Param::Mandatory(*i),
            ConstParam::Optional(i) => Param::Optional(*i),
            ConstParam::Splat(i) => Param::Splat(*i),
            ConstParam::Hash(hash) => Param::Hash(
                hash.iter()
                    .map(|(id, r, m)| (symbols.symbol(proc.symbol_name(*id)), *r, *m))
                    .collect(),
            ),
        }
    }
}
