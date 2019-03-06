use crate::context::Context;
use crate::heap::Ref;
use crate::object::{Object, ObjectType};
use crate::proc::Proc;
use crate::symbol::Symbol;
use std::any::Any;
use std::sync::Arc;

#[derive(Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Fixnum(i64),
    Float(f64),
    Symbol(Symbol),
    String(String),
    Proc(Arc<Proc>),
    Ref(Ref<dyn Object>),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }
}

impl Object for Value {
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
        unimplemented!()
    }
    fn get_ivar(&self, _: Symbol) -> Option<Value> {
        None
    }
    fn set_ivar(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
        Err(())
    }
}

macro_rules! impl_object_for_primitive {
    ($ty:ty, $class:ident) => {
        impl Object for $ty {
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
                context.$class().clone()
            }
            fn get_ivar(&self, _: Symbol) -> Option<Value> {
                None
            }
            fn set_ivar(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
                Err(())
            }
        }
    };
}

impl_object_for_primitive!((), nil_class);
impl_object_for_primitive!(bool, bool_class);
impl_object_for_primitive!(i64, fixnum_class);
impl_object_for_primitive!(f64, float_class);
