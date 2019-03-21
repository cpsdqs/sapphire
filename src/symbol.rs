//! Symbols and symbol tables.

use crate::context::Context;
use crate::object::{send, Arguments, Object, SendError};
use crate::thread::Thread;
use crate::value::Value;
use sapphire_compiler::SymbolTable;
use std::any::Any;
use std::collections::HashMap;

/// A symbol.
///
/// These are bound to a [`Context`] (or more specifically, to a [`Symbols`] table) but this will
/// not be verified at runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

impl Object for Symbol {
    fn get(&self, _: Symbol) -> Option<Value> {
        None
    }
    fn set(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
        Err(())
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        match name {
            Symbol::CLASS => Ok(Value::Ref(thread.context().symbol_class().clone())),
            name => send(
                Value::Symbol(*self),
                thread.context().symbol_class().clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        format!(":{}", context.symbols().symbol_name(*self).unwrap_or("?"))
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}

macro_rules! def_common_symbols {
    ($name:ident = $str:expr, $($rest:tt)+) => {
        impl Symbol {
            #[doc = "```text"]
            #[doc = $str]
            #[doc = "```"]
            pub const $name: Symbol = Symbol(0);
            def_common_symbols!(__def_consts 1, $($rest)+);
        }

        macro_rules! init_symbol_table {
            ($table:expr) => {
                $table.insert($str.to_string(), Symbol(0));
                def_common_symbols!(__def_table 1, $table, $($rest)+);
            }
        }
    };
    (__def_consts $c:expr, $name:ident = $str:expr, $($rest:tt)+) => {
        #[doc = "```text"]
        #[doc = $str]
        #[doc = "```"]
        pub const $name: Symbol = Symbol($c);
        def_common_symbols!(__def_consts $c + 1, $($rest)+);
    };
    (__def_consts $c:expr, $name:ident = $str:expr,) => {
        #[doc = "```text"]
        #[doc = $str]
        #[doc = "```"]
        pub const $name: Symbol = Symbol($c);
    };
    (__def_table $c:expr, $table:expr, $name:ident = $str:expr, $($rest:tt)+) => {
        $table.insert($str.to_string(), Symbol($c));
        def_common_symbols!(__def_table $c + 1, $table, $($rest)+)
    };
    (__def_table $c:expr, $table:expr, $name:ident = $str:expr,) => {
        $table.insert($str.to_string(), Symbol($c));
    };
}

def_common_symbols! {
    SEND = "send",
    CLASS = "class",
    SUPERCLASS = "superclass",
    NEW = "new",
    INITIALIZE = "initialize",
    METHOD = "method",
    DEFINE_METHOD = "define_method",
    METHOD_MISSING = "method_missing",
    ADD = "+",
    SUB = "-",
    MUL = "*",
    DIV = "/",
    REM = "%",
    UPLUS = "+@",
    UMINUS = "-@",
    EQ = "==",
    CASE_EQ = "===",
    NEQ = "!=",
    MATCH = "=~",
    NMATCH = "!~",
    SHL = "<<",
    SHR = ">>",
    GEQ = ">=",
    LEQ = "<=",
    GT = ">",
    LT = "<",
    CMP = "<=>",
    BIT_AND = "&",
    BIT_OR = "|",
    BIT_XOR = "^",
    BIT_INV = "~",
    INDEX = "[]",
    INDEX_EQ = "[]=",
    TO_S = "to_s",
    TO_I = "to_i",
    IS_A = "is_a?",
    INCLUDE = "include",
    RAISE = "raise",
    BLOCK_GIVEN = "block_given?",
    SINGLETON_CLASS = "singleton_class",
    DOES_INCLUDE = "include?",
    SAPPHIRE_ALLOCATE = "sapphire_allocate",
    CALL = "call",
    INSPECT = "inspect",
}

/// A symbol table.
#[derive(Debug)]
pub struct Symbols {
    table: HashMap<String, Symbol>,
}

impl Symbols {
    pub fn new() -> Symbols {
        let mut table = HashMap::new();
        init_symbol_table!(table);
        Symbols { table }
    }

    /// Returns the symbol with the given name.
    pub fn symbol<T: AsRef<str>>(&mut self, name: T) -> Symbol {
        match self.table.get(name.as_ref()) {
            Some(sym) => *sym,
            None => {
                let sym = Symbol(self.table.len());
                self.table.insert(name.as_ref().to_string(), sym);
                sym
            }
        }
    }

    /// Looks up the symbol name (worst case O(n)).
    pub fn symbol_name(&self, symbol: Symbol) -> Option<&str> {
        self.table
            .iter()
            .find(|(_, v)| **v == symbol)
            .map(|(k, _)| &**k)
    }
}

impl SymbolTable for Symbols {
    type Symbol = Symbol;
    fn symbol(&mut self, name: &str) -> Symbol {
        self.symbol(name)
    }
    fn symbol_name(&self, symbol: Symbol) -> Option<&str> {
        self.symbol_name(symbol)
    }
}
