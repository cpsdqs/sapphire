use crate::context::Context;
use crate::heap::Ref;
use crate::object::{Arguments, Object, SendError};
use crate::proc::Proc;
use crate::symbol::Symbol;
use crate::thread::Thread;
use crate::value::Value;
use fnv::FnvHashMap;
use std::any::Any;
use std::sync::Arc;

pub struct FixnumClass {
    class_vars: FnvHashMap<Symbol, Value>,
    modules: Vec<Ref<Object>>,
    extra_methods: FnvHashMap<Symbol, Arc<Proc>>,
}

impl FixnumClass {
    pub fn new() -> FixnumClass {
        FixnumClass {
            class_vars: FnvHashMap::default(),
            modules: Vec::new(),
            extra_methods: FnvHashMap::default(),
        }
    }
}

impl Object for FixnumClass {
    fn get(&self, name: Symbol) -> Option<Value> {
        self.class_vars.get(&name).map(|value| value.clone())
    }
    fn set(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.class_vars.insert(name, value);
        Ok(())
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        unimplemented!("send")
    }
    fn inspect(&self, context: &Context) -> String {
        format!("Fixnum")
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}

impl Object for i64 {
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
            Symbol::CLASS => Ok(Value::Ref(thread.context().fixnum_class().clone())),
            Symbol::ADD | Symbol::SUB | Symbol::MUL | Symbol::DIV | Symbol::REM => {
                // TODO: proper argument validation
                match args.args.get(0) {
                    // TODO: use checked operations
                    Some(Value::Fixnum(rhs)) => match name {
                        Symbol::ADD => Ok(Value::Fixnum(*self + rhs)),
                        Symbol::SUB => Ok(Value::Fixnum(*self - rhs)),
                        Symbol::MUL => Ok(Value::Fixnum(*self * rhs)),
                        Symbol::DIV => Ok(Value::Fixnum(*self / rhs)),
                        Symbol::REM => Ok(Value::Fixnum(*self % rhs)),
                        _ => unreachable!(),
                    },
                    Some(Value::Float(rhs)) => match name {
                        Symbol::ADD => Ok(Value::Float(*self as f64 + rhs)),
                        Symbol::SUB => Ok(Value::Float(*self as f64 - rhs)),
                        Symbol::MUL => Ok(Value::Float(*self as f64 * rhs)),
                        Symbol::DIV => Ok(Value::Float(*self as f64 / rhs)),
                        Symbol::REM => Ok(Value::Float(*self as f64 % rhs)),
                        _ => unreachable!(),
                    },
                    _ => unimplemented!("argument error"),
                }
            }
            Symbol::EQ => match args.args.get(0) {
                Some(Value::Fixnum(rhs)) => Ok(Value::Bool(self == rhs)),
                Some(_) => Ok(Value::Bool(false)),
                _ => unimplemented!("argument error"),
            },
            _ => unimplemented!("use normal send"),
        }
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
