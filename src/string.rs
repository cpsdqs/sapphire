use crate::context::Context;
use crate::heap::Ref;
use crate::object::{Object, ObjectType};
use crate::symbol::Symbol;
use crate::value::Value;
use std::any::Any;

impl Object for String {
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
        context.string_class().clone()
    }
    fn get_ivar(&self, _: Symbol) -> Option<Value> {
        None
    }
    fn set_ivar(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
        Err(())
    }
    fn inspect(&self, _: &Context) -> String {
        format!("{:?}", self)
    }
}
