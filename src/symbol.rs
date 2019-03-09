use crate::context::Context;
use crate::heap::Ref;
use crate::object::{Object, ObjectType};
use crate::value::Value;
use std::any::Any;
use std::collections::HashMap;

/// A symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

impl Object for Symbol {
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Object
    }
    fn class(&self, context: &Context) -> Ref<Object> {
        context.symbol_class().clone()
    }
    fn get_ivar(&self, _: Symbol) -> Option<Value> {
        None
    }
    fn set_ivar(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
        Err(())
    }
}

macro_rules! def_common_symbols {
    ($name:ident = $str:expr, $($rest:tt)+) => {
        impl Symbol {
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
        pub const $name: usize = $c;
        def_common_symbols!(__def_consts $c + 1, $($rest)+);
    };
    (__def_consts $c:expr, $name:ident = $str:expr,) => {
        pub const $name: usize = $c;
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

    /// Looks up the symbol name.
    pub fn symbol_name(&self, symbol: Symbol) -> Option<&str> {
        self.table
            .iter()
            .find(|(_, v)| **v == symbol)
            .map(|(k, _)| &**k)
    }
}
