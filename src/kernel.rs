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
        module.def_method(Symbol::IS_A, Proc::Native(is_a));
        module.def_method(Symbol::METHOD_MISSING, Proc::Native(method_missing));
        module.def_method(Symbol::EQ, Proc::Native(eq));
        module.def_method(Symbol::CASE_EQ, Proc::Native(case_eq));
        module.def_method(Symbol::SEND, Proc::Native(send));

        let mut symbols = context.symbols_mut();
        module.def_method(symbols.symbol("object_id"), Proc::Native(hash));
        module.def_method(symbols.symbol("hash"), Proc::Native(hash));
        module.def_method(symbols.symbol("nil?"), Proc::Native(is_nil));
        module.def_method(symbols.symbol("kind_of?"), Proc::Native(is_a));
        module.def_method(symbols.symbol("instance_of?"), Proc::Native(instance_of));
        module.def_method(symbols.symbol("inspect"), Proc::Native(inspect));
        module.def_method(symbols.symbol("eql?"), Proc::Native(eq));
        module.def_method(symbols.symbol("equal?"), Proc::Native(eq));
        module.def_method(symbols.symbol("loop"), Proc::Native(k_loop));
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

fn hash(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; -);
    Ok(Value::Fixnum(recv.hash()))
}

fn eq(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; operand);
    Ok(Value::Bool(recv == operand))
}

fn case_eq(mut recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; operand);
    if recv == operand {
        return Ok(Value::Bool(true));
    }
    Ok(Value::Bool(
        recv.send(
            Symbol::EQ,
            Arguments::new(&mut iter::once(operand), None),
            thread,
        )?
        .is_truthy(),
    ))
}

fn send(mut recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; method_name: Symbol, *);
    recv.send(method_name, args, thread)
}

fn is_nil(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; -);
    match recv {
        Value::Nil => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_a(mut recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; other: Ref);

    let recv_class = match recv.send(Symbol::CLASS, Arguments::empty(), thread)? {
        Value::Ref(class) => class,
        _ => return Ok(Value::Bool(false)),
    };

    if recv_class == other {
        return Ok(Value::Bool(true));
    }

    let other_is_class = other
        .get()
        .send(
            Symbol::IS_A,
            Arguments::new(
                &mut iter::once(Value::Ref(thread.context().class_class().clone())),
                None,
            ),
            thread,
        )?
        .is_truthy();

    let mut current = recv_class;
    loop {
        if other_is_class && current == other {
            return Ok(Value::Bool(true));
        } else if !other_is_class {
            let includes = current
                .get()
                .send(
                    Symbol::DOES_INCLUDE,
                    Arguments::new(&mut iter::once(Value::Ref(other.clone())), None),
                    thread,
                )?
                .is_truthy();
            if includes {
                return Ok(Value::Bool(true));
            }
        }
        let superclass = match current
            .get()
            .send(Symbol::SUPERCLASS, Arguments::empty(), thread)?
        {
            Value::Ref(class) => class,
            _ => return Ok(Value::Bool(false)),
        };
        if superclass == current {
            return Ok(Value::Bool(false));
        } else {
            current = superclass;
        }
    }
}

fn instance_of(mut recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; other: Ref);

    let recv_class = match recv.send(Symbol::CLASS, Arguments::empty(), thread)? {
        Value::Ref(class) => class,
        _ => return Ok(Value::Bool(false)),
    };

    Ok(Value::Bool(recv_class == other))
}

fn method_missing(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    match args.args.next() {
        Some(Value::Symbol(name)) => Err(SendError::Exception(Value::Ref(Exception::new(
            format!(
                "method {} is missing for {}",
                thread.context().symbols().symbol_name(name).unwrap_or("?"),
                recv.inspect(thread.context())
            ),
            thread.trace(),
            thread.context().exceptions().no_method_error.clone(),
        )))),
        _ => Err(SendError::Exception(Value::Ref(Exception::new(
            format!("first method_missing argument is not a symbol"),
            thread.trace(),
            thread.context().exceptions().type_error.clone(),
        )))),
    }
}

fn inspect(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; -);
    Ok(Value::String(recv.inspect(thread.context())))
}

fn k_loop(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; &block);

    loop {
        thread.call(recv.clone(), block.clone(), Arguments::empty())?;
    }
}
