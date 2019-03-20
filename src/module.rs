use crate::context::Context;
use crate::exception::Exception;
use crate::heap::Ref;
use crate::object::{Arguments, Object, RbClass, SendError};
use crate::proc::Proc;
use crate::read_args;
use crate::symbol::Symbol;
use crate::thread::Thread;
use crate::value::Value;
use std::iter;
use std::sync::Arc;

pub fn init(context: &Arc<Context>) {
    let mut module_ref = context.module_class().get();
    let module: &mut RbClass = Object::downcast_mut(&mut *module_ref).unwrap();

    module.def_method(Symbol::LT, Proc::Native(lt));
    module.def_method(Symbol::LEQ, Proc::Native(leq));
    module.def_method(Symbol::GT, Proc::Native(gt));
    module.def_method(Symbol::GEQ, Proc::Native(geq));
    module.def_method(Symbol::CMP, Proc::Native(cmp));
    module.def_method(Symbol::CASE_EQ, Proc::Native(case_eq));
}

fn assert_is_module(other: &Ref<Object>, thread: &mut Thread) -> Result<(), SendError> {
    let is_module = other
        .get()
        .send(
            Symbol::IS_A,
            Arguments::new(
                &mut iter::once(Value::Ref(thread.context().module_class().clone())),
                None,
            ),
            thread,
        )?
        .is_truthy();

    if !is_module {
        return Err(SendError::Exception(Value::Ref(Exception::new(
            format!("second operand is not a module"),
            thread.trace(),
            thread.context().exceptions().type_error.clone(),
        ))));
    }

    Ok(())
}

fn cmp(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; other: Ref);

    assert_is_module(&other, thread)?;

    match recv {
        Value::Ref(recv) => {
            if recv == other {
                return Ok(Value::Fixnum(0));
            }

            let mut current = recv.clone();
            let is_subclass = loop {
                let superclass =
                    current
                        .get()
                        .send(Symbol::SUPERCLASS, Arguments::empty(), thread)?;
                if let Value::Ref(superclass) = superclass {
                    if superclass == current {
                        break false;
                    }
                    if superclass == other {
                        break true;
                    }
                    current = superclass;
                } else {
                    break false;
                }
            };

            if is_subclass {
                return Ok(Value::Fixnum(-1));
            }

            let mut current = other;
            let is_superclass = loop {
                let superclass =
                    current
                        .get()
                        .send(Symbol::SUPERCLASS, Arguments::empty(), thread)?;
                if let Value::Ref(superclass) = superclass {
                    if superclass == current {
                        break false;
                    }
                    if superclass == recv {
                        break true;
                    }
                    current = superclass;
                } else {
                    break false;
                }
            };

            if is_superclass {
                Ok(Value::Fixnum(1))
            } else {
                Ok(Value::Nil)
            }
        }
        _ => Ok(Value::Nil),
    }
}

fn lt(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    match cmp(recv, args, thread)? {
        Value::Fixnum(0) | Value::Fixnum(1) => Ok(Value::Bool(false)),
        Value::Fixnum(-1) => Ok(Value::Bool(true)),
        _ => Ok(Value::Nil),
    }
}

fn leq(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    match cmp(recv, args, thread)? {
        Value::Fixnum(1) => Ok(Value::Bool(false)),
        Value::Fixnum(0) | Value::Fixnum(-1) => Ok(Value::Bool(true)),
        _ => Ok(Value::Nil),
    }
}

fn gt(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    match cmp(recv, args, thread)? {
        Value::Fixnum(0) | Value::Fixnum(-1) => Ok(Value::Bool(false)),
        Value::Fixnum(1) => Ok(Value::Bool(true)),
        _ => Ok(Value::Nil),
    }
}

fn geq(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    match cmp(recv, args, thread)? {
        Value::Fixnum(-1) => Ok(Value::Bool(false)),
        Value::Fixnum(0) | Value::Fixnum(1) => Ok(Value::Bool(true)),
        _ => Ok(Value::Nil),
    }
}

fn case_eq(recv: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    read_args!(args, thread; other: Ref);

    let is_instance = other
        .get()
        .send(
            Symbol::IS_A,
            Arguments::new(&mut iter::once(recv), None),
            thread,
        )?
        .is_truthy();

    Ok(Value::Bool(is_instance))
}
