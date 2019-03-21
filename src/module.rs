use crate::compile;
use crate::context::Context;
use crate::exception::Exception;
use crate::heap::Ref;
use crate::object::{Arguments, Object, RbClass, RbObject, SendError};
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

    let mut symbols = context.symbols_mut();
    module.def_method(
        symbols.symbol("attr"),
        Proc::Sapphire(Arc::new(
            compile!("def attr name; attr_reader name; end").new(&mut symbols),
        )),
    );
    module.def_method(symbols.symbol("attr_accessor"), Proc::Native(attr_accessor));
    module.def_method(symbols.symbol("attr_reader"), Proc::Native(attr_reader));
    module.def_method(symbols.symbol("attr_writer"), Proc::Native(attr_writer));

    let mut class_ref = context.class_class().get();
    let class: &mut RbClass = Object::downcast_mut(&mut *class_ref).unwrap();
    class.def_method(Symbol::NEW, Proc::Native(new));
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

macro_rules! attr_accessor {
    ($name:ident, $($reader:ident)?, $($writer:ident)?) => {
        fn $name(
            mut recv: Value,
            args: Arguments,
            thread: &mut Thread,
        ) -> Result<Value, SendError> {
            for arg in args.args {
                let name = match arg {
                    Value::Symbol(name) => name,
                    _ => {
                        return Err(SendError::Exception(Value::Ref(Exception::new(
                            format!("attr_accessor argument is not a symbol"),
                            thread.trace(),
                            thread.context().exceptions().argument_error.clone(),
                        ))));
                    }
                };
                let name_str = thread
                    .context()
                    .symbols()
                    .symbol_name(name)
                    .unwrap_or("?")
                    .to_string();

                // TODO: check if name is local ident or constant ident

                let mut name_with_eq = name_str;
                name_with_eq += "=";
                #[allow(unused_variables)]
                let name_with_eq = thread.context().symbols_mut().symbol(name_with_eq);

                $(
                let $reader = compile!("def name; @name; end")
                    .new_with(&mut |symbol| match symbol {
                        "name" => name,
                        s => panic!("unexpected symbol request {:?}", s),
                    });
                )?

                $(
                let $writer = compile!("def name= value; @name = value; end")
                    .new_with(&mut |symbol| match symbol {
                        "name=" => name_with_eq,
                        "name" => name,
                        s => panic!("unexpected symbol request {:?}", s),
                    });
                )?

                $(
                recv.send(
                    Symbol::DEFINE_METHOD,
                    Arguments::new(
                        &mut iter::once(Value::Symbol(name)),
                        Some(Value::Proc(Proc::Sapphire(Arc::new($reader)))),
                    ),
                    thread,
                )?;
                )?

                $(
                recv.send(
                    Symbol::DEFINE_METHOD,
                    Arguments::new(
                        &mut iter::once(Value::Symbol(name_with_eq)),
                        Some(Value::Proc(Proc::Sapphire(Arc::new($writer)))),
                    ),
                    thread,
                )?;
                )?
            }

            Ok(Value::Nil)
        }
    }
}

attr_accessor!(attr_accessor, reader, writer);
attr_accessor!(attr_reader, reader,);
attr_accessor!(attr_writer, , writer);

fn new(recv: Value, mut args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
    let class = match recv {
        Value::Ref(class) => class,
        _ => {
            return Err(SendError::Exception(Value::Ref(Exception::new(
                format!("cannot call Class#new on a primitive"),
                thread.trace(),
                thread.context().exceptions().type_error.clone(),
            ))));
        }
    };

    let allocator = class.get().send(
        Symbol::METHOD,
        Arguments::new(
            &mut iter::once(Value::Symbol(Symbol::SAPPHIRE_ALLOCATE)),
            None,
        ),
        thread,
    )?;

    let mut object = match allocator {
        Value::Proc(Proc::Native(proc)) => {
            struct AllocArgs<'a, 'b: 'a>(&'a mut Arguments<'b>, Option<Value>);
            impl<'a, 'b> Iterator for AllocArgs<'a, 'b> {
                type Item = Value;
                fn next(&mut self) -> Option<Value> {
                    if let Some(class) = self.1.take() {
                        Some(class)
                    } else {
                        self.0.args.next()
                    }
                }
            }

            let block = args.block.clone();

            thread.call(
                Value::Nil,
                Proc::Native(proc),
                Arguments::new(&mut AllocArgs(&mut args, Some(Value::Ref(class))), block),
            )?
        }
        _ => Value::Ref(RbObject::new(class)),
    };
    object.send(Symbol::INITIALIZE, args, thread)?;
    Ok(object)
}
