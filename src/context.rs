use crate::heap::Ref;
use crate::object::Object;
use crate::symbol::{Symbol, Symbols};
use crate::value::Value;
use fnv::FnvHashMap;

pub struct Context {
    symbols: Symbols,
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
    symbols: Symbols,
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
