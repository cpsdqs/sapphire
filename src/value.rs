use crate::context::Context;
use crate::heap::Ref;
use crate::object::{Arguments, Object, SendError};
use crate::proc::Proc;
use crate::symbol::Symbol;
use crate::thread::Thread;
use std::any::Any;
use std::sync::Arc;

#[derive(Debug, Clone)]
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
    fn get(&self, name: Symbol) -> Option<Value> {
        match self {
            Value::Nil => ().get(name),
            Value::Bool(b) => b.get(name),
            Value::Fixnum(i) => i.get(name),
            Value::Float(f) => f.get(name),
            Value::Symbol(s) => s.get(name),
            Value::String(s) => s.get(name),
            Value::Proc(p) => p.get(name),
            Value::Ref(o) => o.get().get(name),
        }
    }
    fn set(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        match self {
            Value::Nil => ().set(name, value),
            Value::Bool(b) => b.set(name, value),
            Value::Fixnum(i) => i.set(name, value),
            Value::Float(f) => f.set(name, value),
            Value::Symbol(s) => s.set(name, value),
            Value::String(s) => s.set(name, value),
            Value::Proc(p) => p.set(name, value),
            Value::Ref(o) => o.get().set(name, value),
        }
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        match self {
            Value::Nil => ().send(name, args, thread),
            Value::Bool(b) => b.send(name, args, thread),
            Value::Fixnum(i) => i.send(name, args, thread),
            Value::Float(f) => f.send(name, args, thread),
            Value::Symbol(s) => s.send(name, args, thread),
            Value::String(s) => s.send(name, args, thread),
            Value::Proc(p) => p.send(name, args, thread),
            Value::Ref(o) => o.get().send(name, args, thread),
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
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
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
            fn send(&mut self, _: Symbol, _: Arguments, _: &mut Thread) -> Result<Value, SendError> {
                unimplemented!("send")
            }
            fn get(&self, _: Symbol) -> Option<Value> {
                None
            }
            fn set(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
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
