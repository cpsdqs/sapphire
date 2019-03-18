//! Procs.

use crate::context::Context;
use crate::object::{Arguments, Object, SendError};
use crate::symbol::{Symbol, Symbols};
use crate::thread::{Register, Thread};
use crate::value::Value;
use std::any::Any;
use std::sync::Arc;

pub use sapphire_compiler::{
    AddressingMode, Op, Proc as CProc, Static as CStatic, NIL, SELF, VOID,
};

pub type Proc = CProc<Symbols, Register>;
pub type Static = CStatic<Symbols, Register>;

impl Object for Arc<Proc> {
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
    fn inspect(&self, context: &Context) -> String {
        format!(
            "<Proc {}>",
            context.symbols().symbol_name(self.name).unwrap()
        )
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}
