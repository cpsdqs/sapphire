//! Procs.

use crate::context::Context;
use crate::exception::Exception;
use crate::object::{send, Arguments, Object, RbClass, SendError};
use crate::read_args;
use crate::symbol::{Symbol, Symbols};
use crate::thread::{Register, Thread};
use crate::value::Value;
use std::any::Any;
use std::fmt;
use std::sync::Arc;

pub use sapphire_compiler::{
    AddressingMode, Op, Proc as CProc, Static as CStatic, NIL, SELF, VOID,
};

#[derive(Clone)]
pub enum Proc {
    Sapphire(Arc<CProc<Symbols, Register>>),
    Native(fn(this: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError>),
}
pub type Static = CStatic<Symbols, Register>;

impl Proc {
    pub fn sapphire(&self) -> Option<&CProc<Symbols, Register>> {
        match self {
            Proc::Sapphire(v) => Some(v),
            _ => None,
        }
    }
}

impl Object for Proc {
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
            Symbol::CLASS => Ok(Value::Ref(thread.context().proc_class().clone())),
            name => send(
                Value::Proc(self.clone()),
                thread.context().proc_class().clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        match self {
            Proc::Sapphire(proc) => format!(
                "<Proc {}>",
                context.symbols().symbol_name(proc.name).unwrap_or("?")
            ),
            Proc::Native(proc) => format!("<Proc {:?}>", *proc as *const ()),
        }
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}

impl fmt::Debug for Proc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Proc::Sapphire(proc) => write!(f, "{:?}", proc),
            Proc::Native(proc) => write!(f, "Proc(native @ {:?})", *proc as *const ()),
        }
    }
}

impl PartialEq for Proc {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Proc::Sapphire(a), Proc::Sapphire(b)) => a == b,
            (Proc::Native(a), Proc::Native(b)) => *a as *const () == *b as *const (),
            _ => false,
        }
    }
}

pub fn init(context: &Arc<Context>) {
    let mut proc_ref = context.module_class().get();
    let proc: &mut RbClass = Object::downcast_mut(&mut *proc_ref).unwrap();

    proc.def_method(Symbol::SAPPHIRE_ALLOCATE, Proc::Native(allocate_proc));
    proc.def_method(Symbol::INITIALIZE, Proc::Native(init_proc));
}

fn allocate_proc(_: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; &block);
    Ok(Value::Proc(block))
}

fn init_proc(_: Value, _: Arguments, _: &mut Thread) -> Result<Value, SendError> {
    Ok(Value::Nil)
}
