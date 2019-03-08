use crate::heap::Ref;
use crate::object::{init_root, Object, RbObject};
use crate::symbol::{Symbol, Symbols};
use crate::value::Value;
use fnv::FnvHashMap;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};

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
}

impl Context {
    pub fn new() -> Context {
        let mut symbols = Symbols::new();
        let (object_class, class_class, module_class) = init_root(&mut symbols);

        Context {
            symbols: RwLock::new(symbols),
            globals: Ref::new_generic(FnvHashMap::default()),
            root: Ref::new(RbObject::new(object_class.clone())),
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
        }
    }

    pub fn symbols(&self) -> RwLockReadGuard<Symbols> {
        self.symbols.read()
    }

    pub fn symbols_mut(&self) -> RwLockWriteGuard<Symbols> {
        self.symbols.write()
    }
}

macro_rules! impl_getters {
    ($($name:ident: $ty:ty,)+) => {
        impl Context {
            $(
                pub fn $name(&self) -> &$ty {
                    &self.$name
                }
            )+
        }
    }
}

impl_getters! {
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
}
