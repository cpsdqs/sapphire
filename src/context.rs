//! Execution contexts.

use crate::compile;
use crate::exception::Exceptions;
use crate::heap::Ref;
use crate::object::{init_root, Arguments, Object, RbObject};
use crate::proc::Proc;
use crate::symbol::{Symbol, Symbols};
use crate::thread::Thread;
use crate::value::Value;
use crate::{kernel, module};
use fnv::FnvHashMap;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::sync::Arc;

/// An execution context: contains globals, a symbol table, and a root object.
#[derive(Debug)]
pub struct Context {
    symbols: RwLock<Symbols>,
    globals: Ref<FnvHashMap<Symbol, Value>>,
    root: Ref<Object>,

    nil_class: Ref<Object>,
    bool_class: Ref<Object>,
    fixnum_class: Ref<Object>,
    float_class: Ref<Object>,
    symbol_class: Ref<Object>,
    string_class: Ref<Object>,
    proc_class: Ref<Object>,
    object_class: Ref<Object>,
    class_class: Ref<Object>,
    module_class: Ref<Object>,
    exceptions: Exceptions,
}

impl Context {
    /// Creates a new context.
    pub fn new() -> Arc<Context> {
        let mut symbols = Symbols::new();
        let (object_class, class_class, module_class) = init_root(&mut symbols);
        let exceptions = Exceptions::new(&mut symbols, object_class.clone(), class_class.clone());

        let context = Arc::new(Context {
            symbols: RwLock::new(symbols),
            globals: Ref::new_generic(FnvHashMap::default()),
            root: RbObject::new(object_class.clone()),
            nil_class: object_class.clone(),    // TODO
            bool_class: object_class.clone(),   // TODO
            fixnum_class: object_class.clone(), // TODO
            float_class: object_class.clone(),  // TODO
            symbol_class: object_class.clone(), // TODO
            string_class: object_class.clone(), // TODO
            proc_class: object_class.clone(),   // TODO
            object_class: object_class,
            class_class: class_class,
            module_class: module_class,
            exceptions,
        });

        kernel::init(&context);
        module::init(&context);

        let proc = compile!(file "src/init.rb").new(&mut *context.symbols.write());
        let mut thread = Thread::new(Arc::clone(&context));
        thread
            .call(
                Value::Nil,
                Proc::Sapphire(Arc::new(proc)),
                Arguments::empty(),
            )
            .unwrap();

        context
    }

    /// Returns the symbol table.
    pub fn symbols(&self) -> RwLockReadGuard<Symbols> {
        self.symbols.read()
    }

    /// Returns the symbol table.
    pub fn symbols_mut(&self) -> RwLockWriteGuard<Symbols> {
        self.symbols.write()
    }
}

macro_rules! impl_getters {
    ($($(#[$a:meta])* $name:ident: $ty:ty,)+) => {
        impl Context {
            $(
                $(#[$a])*
                pub fn $name(&self) -> &$ty {
                    &self.$name
                }
            )+
        }
    }
}

impl_getters! {
    /// Returns the set of global variables.
    globals: Ref<FnvHashMap<Symbol, Value>>,
    /// Returns the root object, i.e. top-level `self`.
    root: Ref<Object>,
    nil_class: Ref<Object>,
    bool_class: Ref<Object>,
    fixnum_class: Ref<Object>,
    float_class: Ref<Object>,
    symbol_class: Ref<Object>,
    string_class: Ref<Object>,
    proc_class: Ref<Object>,
    object_class: Ref<Object>,
    class_class: Ref<Object>,
    module_class: Ref<Object>,
    exceptions: Exceptions,
}
