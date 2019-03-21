//! Value types.

use crate::context::Context;
use crate::heap::Ref;
use crate::object::{send, Arguments, Object, SendError};
use crate::proc::Proc;
use crate::symbol::Symbol;
use crate::thread::Thread;
use std::any::Any;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::{mem, slice};

/// A value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Fixnum(i64),
    Float(f64),
    Symbol(Symbol),
    String(String),
    Proc(Proc),
    Ref(Ref<dyn Object>),
}

impl Value {
    /// Returns true if the value isn’t `nil` or `false`.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }

    /// Returns a hash of the immediate value.
    pub fn hash(&self) -> i64 {
        let self_ptr = self as *const Value as *const u8;
        let self_buf = unsafe { slice::from_raw_parts(self_ptr, mem::size_of::<Value>()) };
        let mut hasher = DefaultHasher::new();
        self_buf.hash(&mut hasher);
        unsafe { mem::transmute::<u64, i64>(hasher.finish()) }
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

impl Object for () {
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        match name {
            Symbol::CLASS => Ok(Value::Ref(thread.context().nil_class().clone())),
            name => send(
                Value::Nil,
                thread.context().nil_class().clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn get(&self, _: Symbol) -> Option<Value> {
        None
    }
    fn set(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
        Err(())
    }
    fn inspect(&self, _: &Context) -> String {
        String::from("nil")
    }
}

impl Object for bool {
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        match name {
            Symbol::CLASS => match self {
                true => Ok(Value::Ref(thread.context().true_class().clone())),
                false => Ok(Value::Ref(thread.context().false_class().clone())),
            },
            _ => send(
                Value::Bool(*self),
                match self {
                    true => thread.context().true_class().clone(),
                    false => thread.context().false_class().clone(),
                },
                name,
                args,
                thread,
            ),
        }
    }
    fn get(&self, _: Symbol) -> Option<Value> {
        None
    }
    fn set(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
        Err(())
    }
    fn inspect(&self, _: &Context) -> String {
        format!("{}", self)
    }
}
