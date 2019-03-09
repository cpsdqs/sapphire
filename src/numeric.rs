use crate::context::Context;
use crate::heap::Ref;
use crate::object::{Class, Module, Object, ObjectRef, ObjectRefMut, ObjectType};
use crate::proc::Proc;
use crate::symbol::Symbol;
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
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
    fn as_module(&self) -> Option<ObjectRef<Module>> {
        Some(ObjectRef::Ref(self))
    }
    fn as_module_mut(&mut self) -> Option<ObjectRefMut<Module>> {
        Some(ObjectRefMut::Ref(self))
    }
    fn as_class(&self) -> Option<ObjectRef<Class>> {
        Some(ObjectRef::Ref(self))
    }
    fn as_class_mut(&mut self) -> Option<ObjectRefMut<Class>> {
        Some(ObjectRefMut::Ref(self))
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Class
    }
    fn class(&self, context: &Context) -> Ref<Object> {
        context.class_class().clone()
    }
    fn get_ivar(&self, name: Symbol) -> Option<Value> {
        self.class_vars.get(&name).map(|value| value.clone())
    }
    fn set_ivar(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.class_vars.insert(name, value);
        Ok(())
    }
}

impl Module for FixnumClass {
    fn def_method(&mut self, name: Symbol, body: Arc<Proc>) -> Result<(), ()> {
        self.extra_methods.insert(name, body);
        Ok(())
    }
    fn resolve_method(&mut self, name: Symbol) -> Option<Arc<Proc>> {
        match name {
            Symbol::ADD => unimplemented!(),
            _ => unimplemented!(),
        }
    }
    fn include_module(&mut self, module: Ref<Object>) -> Result<(), ()> {
        self.modules.push(module);
        Ok(())
    }
}

impl Class for FixnumClass {
    fn superclass(&self, context: &Context) -> Ref<Object> {
        context.object_class().clone()
    }
}
