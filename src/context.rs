//! Execution contexts.

use crate::compile;
use crate::exception::Exceptions;
use crate::heap::Ref;
use crate::object::{init_root, Arguments, ClassName, Object, RbClass, RbObject};
use crate::proc::Proc;
use crate::symbol::{Symbol, Symbols};
use crate::thread::Thread;
use crate::value::Value;
use crate::{collections, kernel, module, proc};
use fnv::FnvHashMap;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::sync::Arc;

/// An execution context: contains globals, a symbol table, and a root object.
#[derive(Debug)]
pub struct Context {
    symbols: RwLock<Symbols>,
    globals: Ref<FnvHashMap<Symbol, Value>>,
    root: Ref<dyn Object>,

    nil_class: Ref<dyn Object>,
    true_class: Ref<dyn Object>,
    false_class: Ref<dyn Object>,
    numeric_class: Ref<dyn Object>,
    integer_class: Ref<dyn Object>,
    fixnum_class: Ref<dyn Object>,
    float_class: Ref<dyn Object>,
    symbol_class: Ref<dyn Object>,
    string_class: Ref<dyn Object>,
    proc_class: Ref<dyn Object>,
    array_class: Ref<dyn Object>,
    hash_class: Ref<dyn Object>,
    object_class: Ref<dyn Object>,
    class_class: Ref<dyn Object>,
    module_class: Ref<dyn Object>,
    exceptions: Exceptions,
}

impl Context {
    /// Creates a new context.
    pub fn new() -> Arc<Context> {
        let mut symbols = Symbols::new();
        let (object_class, class_class, module_class) = init_root(&mut symbols);
        let exceptions = Exceptions::new(&mut symbols, object_class.clone(), class_class.clone());

        macro_rules! classes {
            (def => $($var:ident: $name:tt < $super:expr;)+) => {
                $(
                let $var = RbClass::new_unchecked(
                    ClassName::Name(symbols.symbol($name)),
                    $super.clone(),
                    class_class.clone(),
                );
                )+
            };
            ($context:expr => $object_class:expr => $($var:ident: $name:expr;)+) => {
                $(
                $object_class
                    .set($context.symbols.write().symbol($name), Value::Ref($context.$var.clone()))
                    .unwrap();
                )+
            };
        }

        classes! {
            def =>
            nil_class: "NilClass" < object_class;
            true_class: "TrueClass" < object_class;
            false_class: "FalseClass" < object_class;
            numeric_class: "Numeric" < object_class;
            integer_class: "Integer" < numeric_class;
            fixnum_class: "Fixnum" < integer_class;
            float_class: "Float" < numeric_class;
            symbol_class: "Symbol" < object_class;
            string_class: "String" < object_class;
            proc_class: "Proc" < object_class;
            array_class: "Array" < object_class;
            hash_class: "Hash" < object_class;
        }

        let context = Arc::new(Context {
            symbols: RwLock::new(symbols),
            globals: Ref::new_generic(FnvHashMap::default()),
            root: RbObject::new(object_class.clone()),
            nil_class,
            true_class,
            false_class,
            numeric_class,
            integer_class,
            fixnum_class,
            float_class,
            symbol_class,
            string_class,
            proc_class,
            array_class,
            hash_class,
            object_class: object_class.clone(),
            class_class,
            module_class,
            exceptions,
        });

        {
            let mut object_class = object_class.get();
            classes! {
                context => object_class =>
                nil_class: "NilClass";
                true_class: "TrueClass";
                false_class: "FalseClass";
                numeric_class: "Numeric";
                integer_class: "Integer";
                fixnum_class: "Fixnum";
                float_class: "Float";
                symbol_class: "Symbol";
                string_class: "String";
                proc_class: "Proc";
                array_class: "Array";
                hash_class: "Hash";
            }
        }

        kernel::init(&context);
        module::init(&context);
        proc::init(&context);
        collections::init(&context);

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
    root: Ref<dyn Object>,
    nil_class: Ref<dyn Object>,
    true_class: Ref<dyn Object>,
    false_class: Ref<dyn Object>,
    fixnum_class: Ref<dyn Object>,
    float_class: Ref<dyn Object>,
    symbol_class: Ref<dyn Object>,
    string_class: Ref<dyn Object>,
    proc_class: Ref<dyn Object>,
    array_class: Ref<dyn Object>,
    hash_class: Ref<dyn Object>,
    object_class: Ref<dyn Object>,
    class_class: Ref<dyn Object>,
    module_class: Ref<dyn Object>,
    exceptions: Exceptions,
}
