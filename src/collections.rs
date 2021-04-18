//! Collection type implementations.

use crate::context::Context;
use crate::exception::Exception;
use crate::heap::{Ref, Weak};
use crate::object::{send, Arguments, Object, RbClass, SendError};
use crate::proc::Proc;
use crate::read_args;
use crate::symbol::Symbol;
use crate::thread::Thread;
use crate::value::Value;
use std::any::Any;
use std::sync::Arc;

/// A Ruby array.
#[derive(Debug, Clone)]
pub struct Array {
    inner: Vec<Value>,
    self_ref: Weak<dyn Object>,
}

impl Array {
    /// Creates a new array.
    pub fn new() -> Ref<dyn Object> {
        Self::new_with(Vec::new())
    }

    /// Wraps a Vec to create an array.
    pub fn new_with(inner: Vec<Value>) -> Ref<dyn Object> {
        let array = Ref::new(Array {
            inner,
            self_ref: Weak::new_null(),
        });
        let self_ref = array.downgrade();
        Object::downcast_mut::<Array>(&mut *array.get())
            .unwrap()
            .self_ref = self_ref;
        array
    }
}

impl Object for Array {
    fn get(&self, _: Symbol) -> Option<Value> {
        None
    }
    fn set(&mut self, _: Symbol, _: Value) -> Result<(), ()> {
        Err(())
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        match name {
            Symbol::CLASS => Ok(Value::Ref(thread.context().array_class().clone())),
            Symbol::SHL => {
                read_args!(args, thread; obj);
                self.inner.push(obj);
                Ok(Value::Ref(self.self_ref.upgrade().unwrap()))
            }
            Symbol::INDEX => {
                read_args!(args, thread; first: Fixnum, second: Fixnum?);
                if let Some(second) = second {
                    let mut start = first;
                    let mut len = second;
                    if start < 0 {
                        start += self.inner.len() as i64;
                    }
                    if start < 0 || start > self.inner.len() as i64 || len < 0 {
                        return Ok(Value::Nil);
                    }
                    if len > self.inner.len() as i64 - start {
                        len = self.inner.len() as i64 - start;
                    }
                    let slice = self.inner[start as usize..(start + len) as usize].to_vec();
                    Ok(Value::Ref(Array::new_with(slice)))
                } else {
                    let mut index = first;
                    if index < 0 {
                        index += self.inner.len() as i64;
                    }
                    if index < 0 || index >= self.inner.len() as i64 {
                        return Ok(Value::Nil);
                    }
                    Ok(self.inner[index as usize].clone())
                }
            }
            Symbol::INDEX_EQ => {
                read_args!(args, thread; index: Fixnum, value, splice?);
                if let Some(splice) = splice {
                    let mut start = index;
                    let mut len = match value {
                        Value::Fixnum(len) => len,
                        value => {
                            return Err(SendError::Exception(Value::Ref(Exception::new(
                                format!(
                                    "Expected a Fixnum, got {}",
                                    value.inspect(thread.context())
                                ),
                                thread.trace(),
                                thread.context().exceptions().argument_error.clone(),
                            ))));
                        }
                    };
                    if start < 0 {
                        start += self.inner.len() as i64;
                    }
                    if start < 0 || start > self.inner.len() as i64 || len < 0 {
                        return Ok(Value::Nil);
                    }
                    if len > self.inner.len() as i64 - start {
                        len = self.inner.len() as i64 - start;
                    }
                    if start < self.inner.len() as i64 {
                        for _ in 1..len {
                            self.inner.remove(start as usize);
                        }
                        self.inner[start as usize] = splice.clone();
                    } else {
                        self.inner.push(splice.clone());
                    }
                    Ok(splice)
                } else {
                    let mut index = index;
                    if index < 0 {
                        index += self.inner.len() as i64;
                    }
                    if index < 0 || index > self.inner.len() as i64 {
                        return Ok(Value::Nil);
                    }
                    if index < self.inner.len() as i64 {
                        self.inner[index as usize] = value.clone();
                    } else {
                        self.inner.push(value.clone());
                    }
                    Ok(value)
                }
            }
            name => send(
                Value::Ref(self.self_ref.upgrade().unwrap()),
                thread.context().array_class().clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        let mut contents = String::new();
        for item in &self.inner {
            if !contents.is_empty() {
                contents += ", ";
            }
            contents += &item.inspect(context);
        }
        format!("[{}]", contents)
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

/// Initializes collection methods in the given context.
pub fn init(context: &Arc<Context>) {
    let mut array_ref = context.array_class().get();
    let array: &mut RbClass = Object::downcast_mut(&mut *array_ref).unwrap();

    array.def_method(Symbol::SAPPHIRE_ALLOCATE, Proc::Native(alloc_array));
    array.def_method(Symbol::INITIALIZE, Proc::Native(initialize));

    array.ensure_singleton_class(context);
    let mut array_singleton_ref = array.singleton_class().unwrap().get();
    let array_singleton: &mut RbClass = Object::downcast_mut(&mut *array_singleton_ref).unwrap();

    array_singleton.def_method(Symbol::INDEX, Proc::Native(create_array));
}

fn alloc_array(_: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; _class: Ref, *);

    Ok(Value::Ref(Array::new()))
}

fn initialize(_recv: Value, _args: Arguments, _thread: &mut Thread) -> Result<Value, SendError> {
    // TODO
    Ok(Value::Nil)
}

fn create_array(_: Value, args: Arguments, _: &mut Thread) -> Result<Value, SendError> {
    let items = args.args.collect();
    Ok(Value::Ref(Array::new_with(items)))
}
