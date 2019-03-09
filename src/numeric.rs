use crate::context::Context;
use crate::heap::Ref;
use crate::object::{Arguments, Object, SendError};
use crate::proc::Proc;
use crate::symbol::Symbol;
use crate::thread::Thread;
use crate::value::Value;
use fnv::FnvHashMap;
use std::any::Any;
use std::sync::Arc;

pub struct FixnumClass {
    class_vars: FnvHashMap<Symbol, Value>,
    modules: Vec<Ref<Object>>,
    extra_methods: FnvHashMap<Symbol, Arc<Proc>>,
}

impl FixnumClass {
    pub fn new() -> FixnumClass {
        FixnumClass {
            class_vars: FnvHashMap::default(),
            modules: Vec::new(),
            extra_methods: FnvHashMap::default(),
        }
    }
}

impl Object for FixnumClass {
    fn get(&self, name: Symbol) -> Option<Value> {
        self.class_vars.get(&name).map(|value| value.clone())
    }
    fn set(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.class_vars.insert(name, value);
        Ok(())
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        unimplemented!("send")
    }
    fn inspect(&self, context: &Context) -> String {
        format!("Fixnum")
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}
