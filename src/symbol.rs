use std::collections::HashMap;

/// A symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

/// A symbol table.
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
