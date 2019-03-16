//! String type implementation.

use crate::context::Context;
use crate::object::{Arguments, Object, SendError};
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
        _name: Symbol,
        _args: Arguments,
        _thread: &mut Thread,
    ) -> Result<Value, SendError> {
        unimplemented!("send")
    }
    fn inspect(&self, _: &Context) -> String {
        format!("{:?}", self)
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}
