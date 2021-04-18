//! String type implementation.

use crate::context::Context;
use crate::exception::Exception;
use crate::object::{send, Arguments, Object, SendError};
use crate::read_args;
use crate::symbol::Symbol;
use crate::thread::Thread;
use crate::value::Value;
use std::any::Any;

impl Object for String {
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
            Symbol::ADD => {
                read_args!(args, thread; operand);

                match operand {
                    Value::String(rhs) => {
                        let mut new_str = self.clone();
                        new_str += &rhs;
                        Ok(Value::String(new_str))
                    }
                    mut other => match other.send(Symbol::TO_S, Arguments::empty(), thread)? {
                        Value::String(rhs) => {
                            let mut new_str = self.clone();
                            new_str += &rhs;
                            Ok(Value::String(new_str))
                        }
                        _ => Err(SendError::Exception(Value::Ref(Exception::new(
                            format!("second operand couldn’t be coerced to a string"),
                            thread.trace(),
                            thread.context().exceptions().type_error.clone(),
                        )))),
                    },
                }
            }
            name => send(
                Value::String(self.clone()),
                thread.context().string_class().clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn inspect(&self, _: &Context) -> String {
        format!("{:?}", self)
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}
