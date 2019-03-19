use crate::context::Context;
use crate::exception::Exception;
use crate::object::{Arguments, Object, RbModule, SendError};
use crate::proc::Proc;
use crate::read_args;
use crate::symbol::Symbol;
use crate::thread::Thread;
use crate::value::Value;
use std::iter;
use std::sync::Arc;

pub fn init(context: &Arc<Context>) {
    let name = context.symbols_mut().symbol("Kernel");
    let module = RbModule::new(name, context);

    context
        .object_class()
        .get()
        .set(name, Value::Ref(module.clone()))
        .unwrap();
    let mut thread = Thread::new(Arc::clone(&context));
    context
        .object_class()
        .get()
        .send(
            Symbol::INCLUDE,
            Arguments::new(&mut iter::once(Value::Ref(module.clone())), None),
            &mut thread,
        )
        .unwrap();

    {
        let mut module_ref = module.get();
        let module: &mut RbModule = Object::downcast_mut(&mut *module_ref).unwrap();

        module.def_method(Symbol::RAISE, Proc::Native(raise));
        module.def_method(Symbol::BLOCK_GIVEN, Proc::Native(is_block_given));

        let mut symbols = context.symbols_mut();
        module.def_method(symbols.symbol("object_id"), Proc::Native(object_id));
        module.def_method(symbols.symbol("nil?"), Proc::Native(is_nil));
    }
}

fn raise(_: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; exception);
    Err(SendError::Exception(exception))
}

fn is_block_given(_: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; -);
    Ok(Value::Bool(thread.is_block_given()))
}

fn object_id(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; -);
    Ok(Value::Fixnum(recv.object_id()))
}

fn is_nil(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; -);
    match recv {
        Value::Nil => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}
