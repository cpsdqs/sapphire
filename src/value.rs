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
        match self {
            Value::Nil => ().object_type(),
            Value::Bool(b) => b.object_type(),
            Value::Fixnum(i) => i.object_type(),
            Value::Float(f) => f.object_type(),
            Value::Symbol(s) => s.object_type(),
            Value::String(s) => s.object_type(),
            Value::Proc(p) => p.object_type(),
            Value::Ref(o) => o.get().object_type(),
        }
    }
    fn class(&self, context: &Context) -> Ref<Object> {
        match self {
            Value::Nil => ().class(context),
            Value::Bool(b) => b.class(context),
            Value::Fixnum(i) => i.class(context),
            Value::Float(f) => f.class(context),
            Value::Symbol(s) => s.class(context),
            Value::String(s) => s.class(context),
            Value::Proc(p) => p.class(context),
            Value::Ref(o) => o.get().class(context),
        }
    }
    fn get_ivar(&self, name: Symbol) -> Option<Value> {
        match self {
            Value::Nil => ().get_ivar(name),
            Value::Bool(b) => b.get_ivar(name),
            Value::Fixnum(i) => i.get_ivar(name),
            Value::Float(f) => f.get_ivar(name),
            Value::Symbol(s) => s.get_ivar(name),
            Value::String(s) => s.get_ivar(name),
            Value::Proc(p) => p.get_ivar(name),
            Value::Ref(o) => o.get().get_ivar(name),
        }
    }
    fn set_ivar(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        match self {
            Value::Nil => ().set_ivar(name, value),
            Value::Bool(b) => b.set_ivar(name, value),
            Value::Fixnum(i) => i.set_ivar(name, value),
            Value::Float(f) => f.set_ivar(name, value),
            Value::Symbol(s) => s.set_ivar(name, value),
            Value::String(s) => s.set_ivar(name, value),
            Value::Proc(p) => p.set_ivar(name, value),
            Value::Ref(o) => o.get().set_ivar(name, value),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        match self {
            Value::Nil => ().inspect(context),
            Value::Bool(b) => b.inspect(context),
            Value::Fixnum(i) => i.inspect(context),
            Value::Float(f) => f.inspect(context),
            Value::Symbol(s) => s.inspect(context),
            Value::String(s) => s.inspect(context),
            Value::Proc(p) => p.inspect(context),
            Value::Ref(o) => o.get().inspect(context),
        }
    }
}

macro_rules! impl_object_for_primitive {
    ($ty:ty, $class:ident, $($inspect:tt)*) => {
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
            $($inspect)*
        }
    };
}

impl_object_for_primitive!((), nil_class, fn inspect(&self, _: &Context) -> String {
    String::from("nil")
});
impl_object_for_primitive!(bool, bool_class, fn inspect(&self, _: &Context) -> String {
    format!("{:?}", self)
});
impl_object_for_primitive!(i64, fixnum_class, fn inspect(&self, _: &Context) -> String {
    format!("{:?}", self)
});
impl_object_for_primitive!(f64, float_class, fn inspect(&self, _: &Context) -> String {
    format!("{:?}", self)
});
