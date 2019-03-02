//! Ruby procs.

use crate::symbol::Symbol;
use std::fmt::{self, Write};
use std::sync::Arc;

pub const RESERVED_REGISTERS: usize = 4;
pub const PARENT: usize = 0;
pub const NIL: usize = 1;
pub const VOID: usize = 2;
pub const SELF: usize = 3;

/// A procedure containing a set of VM instructions.
#[derive(PartialEq)]
pub struct Proc {
    /// Proc debug name.
    pub name: Symbol,
    /// Number of registers.
    pub registers: usize,
    /// Static values.
    pub statics: Vec<Static>,
    /// Addressing mode.
    pub mode: AddressingMode,
    /// Bytecode.
    pub code: Vec<u8>,
}

impl fmt::Debug for Proc {
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
            .field("statics", &self.statics)
            .field("mode", &self.mode)
            .field("code", &code)
            .finish()
    }
}

/// A static value.
#[derive(Debug, Clone, PartialEq)]
pub enum Static {
    Int(i64),
    Float(f64),
    Str(String),
    Sym(Symbol),
    Proc(Arc<Proc>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressingMode {
    U8,
    U16,
}

macro_rules! def_op {
    ($($op:ident = $val:expr,)+) => {
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
    LOAD_PROC = 0x0D,
    ARG = 0x12,
    ARG_ASSOC = 0x13,
    ARG_SPLAT = 0x14,
    ARG_BLOCK = 0x15,
    CALL = 0x10,
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
    BEGIN_RESCUE = 0x40,
    RESCUE_MATCH = 0x41,
    RESCUE_BIND = 0x42,
    END_RESCUE = 0x43,
    DEFINED_CONST = 0x35,
    DEFINED_GLOBAL = 0x36,
    DEFINED_CLASS_VAR = 0x37,
    DEFINED_IVAR = 0x38,
    YIELD = 0x17,
    DEF_MODULE = 0x50,
    DEF_CLASS = 0x51,
    DEF_METHOD = 0x52,
    DEF_SINGLETON_CLASS = 0x53,
    DEF_SINGLETON_METHOD = 0x54,
}
