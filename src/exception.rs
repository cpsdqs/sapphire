//! Exceptions.

use crate::context::Context;
use crate::heap::{Ref, Weak};
use crate::object::{send, Arguments, ClassName, Object, RbClass, SendError};
use crate::proc::Proc;
use crate::read_args;
use crate::symbol::{Symbol, Symbols};
use crate::thread::Thread;
use crate::value::Value;
use fnv::FnvHashMap;
use std::any::Any;
use std::mem;

/// An exception.
#[derive(Debug, Clone)]
pub struct Exception {
    /// The exception message.
    pub message: String,
    /// The backtrace.
    pub trace: Vec<TraceItem>,

    // Standard object properties
    ivars: FnvHashMap<Symbol, Value>,
    class: Ref<Object>,
    self_ref: Weak<Object>,
}

impl Exception {
    pub fn new(message: String, trace: Vec<TraceItem>, class: Ref<Object>) -> Ref<Object> {
        let this = Ref::new(Exception {
            message,
            trace,
            ivars: FnvHashMap::default(),
            class,
            self_ref: unsafe { mem::uninitialized() },
        });
        let weak = this.downgrade();
        mem::forget(mem::replace(
            &mut Object::downcast_mut::<Exception>(&mut *this.get())
                .unwrap()
                .self_ref,
            weak,
        ));
        this
    }
}

/// A backtrace item.
#[derive(Debug, Clone, PartialEq)]
pub struct TraceItem {
    pub proc: Proc,
    pub pc: usize,
}

impl Object for Exception {
    fn get(&self, name: Symbol) -> Option<Value> {
        self.ivars.get(&name).map(|value| value.clone())
    }
    fn set(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.ivars.insert(name, value);
        Ok(())
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        match name {
            Symbol::TO_S => Ok(Value::String(self.message.clone())),
            _ => send(
                Value::Ref(self.self_ref.upgrade().unwrap()),
                self.class.clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        let class = self.class.get().inspect(context);
        format!("<{}:{:?} {:?}>", class, self as *const Self, self.message)
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}

/// Standard exception classes.
#[derive(Debug)]
pub struct Exceptions {
    pub exception: Ref<Object>,
    pub standard_error: Ref<Object>,
    pub zero_division_error: Ref<Object>,
    pub name_error: Ref<Object>,
    pub no_method_error: Ref<Object>,
    pub argument_error: Ref<Object>,
    pub type_error: Ref<Object>,
}

impl Exceptions {
    pub fn new(
        symbols: &mut Symbols,
        object_class: Ref<Object>,
        class_class: Ref<Object>,
    ) -> Exceptions {
        // TODO: use macros or ruby for this
        macro_rules! def_exceptions {
            ($($name:ident, $sym:expr, $super:expr;)+ $block:block) => {
                $(
                let $name = RbClass::new_unchecked(
                    ClassName::Name(symbols.symbol($sym)),
                    $super.clone(),
                    class_class.clone(),
                );
                )+

                {
                    let mut object_class = object_class.get();
                    $(
                    object_class
                        .set(symbols.symbol($sym), Value::Ref($name.clone()))
                        .unwrap();
                    )+
                }

                $block

                Exceptions {
                    $($name,)+
                }
            }
        }

        def_exceptions! {
            exception, "Exception", object_class;
            standard_error, "StandardError", exception;
            zero_division_error, "ZeroDivisionError", standard_error;
            name_error, "NameError", standard_error;
            no_method_error, "NoMethodError", name_error;
            argument_error, "ArgumentError", standard_error;
            type_error, "TypeError", standard_error;
            {
                let mut exception_ref = exception.get();
                let exception: &mut RbClass = Object::downcast_mut(&mut *exception_ref).unwrap();

                exception.def_method(Symbol::SAPPHIRE_ALLOCATE, Proc::Native(allocate_exception));
                exception.def_method(Symbol::INITIALIZE, Proc::Native(init_exception));
            }
        }
    }
}

fn allocate_exception(_: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; class: Ref, *);
    Ok(Value::Ref(Exception::new(
        String::new(),
        thread.trace(),
        class,
    )))
}

fn init_exception(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; message: String);

    match recv {
        Value::Ref(recv) => match Object::downcast_mut::<Exception>(&mut *recv.get()) {
            Some(exception) => exception.message = message,
            _ => (),
        },
        _ => {
            return Err(SendError::Exception(Value::Ref(Exception::new(
                format!("invalid primitive exception"),
                thread.trace(),
                thread.context().exceptions().type_error.clone(),
            ))));
        }
    }

    Ok(Value::Nil)
}
