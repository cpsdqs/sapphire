//! Exceptions.

use crate::context::Context;
use crate::heap::{Ref, Weak};
use crate::object::{send, Arguments, Object, RbClass, SendError};
use crate::proc::Proc;
use crate::symbol::{Symbol, Symbols};
use crate::thread::Thread;
use crate::value::Value;
use fnv::FnvHashMap;
use std::any::Any;
use std::mem;
use std::sync::Arc;

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
    pub proc: Arc<Proc>,
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
}

impl Exceptions {
    pub fn new(
        symbols: &mut Symbols,
        object_class: Ref<Object>,
        class_class: Ref<Object>,
    ) -> Exceptions {
        let exception = RbClass::new_unchecked(
            symbols.symbol("Exception"),
            object_class.clone(),
            class_class.clone(),
        );
        let standard_error = RbClass::new_unchecked(
            symbols.symbol("StandardError"),
            exception.clone(),
            class_class.clone(),
        );
        let zero_division_error = RbClass::new_unchecked(
            symbols.symbol("ZeroDivisionError"),
            standard_error.clone(),
            class_class.clone(),
        );
        let name_error = RbClass::new_unchecked(
            symbols.symbol("NameError"),
            standard_error.clone(),
            class_class.clone(),
        );
        let no_method_error = RbClass::new_unchecked(
            symbols.symbol("NoMethodError"),
            name_error.clone(),
            class_class,
        );

        {
            let mut object_class = object_class.get();
            object_class
                .set(symbols.symbol("Exception"), Value::Ref(exception.clone()))
                .unwrap();
            object_class
                .set(
                    symbols.symbol("StandardError"),
                    Value::Ref(standard_error.clone()),
                )
                .unwrap();
            object_class
                .set(
                    symbols.symbol("ZeroDivisionError"),
                    Value::Ref(zero_division_error.clone()),
                )
                .unwrap();
            object_class
                .set(symbols.symbol("NameError"), Value::Ref(name_error.clone()))
                .unwrap();
            object_class
                .set(
                    symbols.symbol("NoMethodError"),
                    Value::Ref(no_method_error.clone()),
                )
                .unwrap();
        }

        Exceptions {
            exception,
            standard_error,
            zero_division_error,
            name_error,
            no_method_error,
        }
    }
}
