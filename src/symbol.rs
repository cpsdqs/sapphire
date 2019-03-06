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

/// A symbol table.
#[derive(Debug)]
pub struct Symbols {
    table: HashMap<String, Symbol>,
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols {
            table: HashMap::new(),
        }
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
