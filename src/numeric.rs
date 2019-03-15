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
        _name: Symbol,
        _args: Arguments,
        _thread: &mut Thread,
    ) -> Result<Value, SendError> {
        unimplemented!("send")
    }
    fn inspect(&self, _: &Context) -> String {
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
            Symbol::UPLUS => Ok(Value::Fixnum(*self)),
            Symbol::UMINUS => Ok(Value::Fixnum(-*self)),
            Symbol::BIT_INV => Ok(Value::Fixnum(!*self)),
            Symbol::ADD
            | Symbol::SUB
            | Symbol::MUL
            | Symbol::DIV
            | Symbol::REM
            | Symbol::LEQ
            | Symbol::LT
            | Symbol::GEQ
            | Symbol::GT
            | Symbol::CMP
            | Symbol::BIT_AND
            | Symbol::BIT_OR
            | Symbol::BIT_XOR
            | Symbol::SHL
            | Symbol::SHR => {
                // TODO: proper argument validation
                match args.args.next() {
                    // TODO: use checked operations
                    Some(Value::Fixnum(rhs)) => match name {
                        Symbol::ADD => Ok(Value::Fixnum(*self + rhs)),
                        Symbol::SUB => Ok(Value::Fixnum(*self - rhs)),
                        Symbol::MUL => Ok(Value::Fixnum(*self * rhs)),
                        Symbol::DIV => Ok(Value::Fixnum(*self / rhs)),
                        Symbol::REM => Ok(Value::Fixnum(*self % rhs)),
                        Symbol::LEQ => Ok(Value::Bool(*self <= rhs)),
                        Symbol::GEQ => Ok(Value::Bool(*self >= rhs)),
                        Symbol::LT => Ok(Value::Bool(*self < rhs)),
                        Symbol::GT => Ok(Value::Bool(*self > rhs)),
                        Symbol::CMP => Ok(Value::Fixnum((*self - rhs).signum())),
                        Symbol::BIT_AND => Ok(Value::Fixnum(*self & rhs)),
                        Symbol::BIT_OR => Ok(Value::Fixnum(*self | rhs)),
                        Symbol::BIT_XOR => Ok(Value::Fixnum(*self ^ rhs)),
                        Symbol::SHL => Ok(Value::Fixnum(*self << rhs)),
                        Symbol::SHR => Ok(Value::Fixnum(*self >> rhs)),
                        _ => unreachable!(),
                    },
                    Some(Value::Float(rhs)) => match name {
                        Symbol::ADD => Ok(Value::Float(*self as f64 + rhs)),
                        Symbol::SUB => Ok(Value::Float(*self as f64 - rhs)),
                        Symbol::MUL => Ok(Value::Float(*self as f64 * rhs)),
                        Symbol::DIV => Ok(Value::Float(*self as f64 / rhs)),
                        Symbol::REM => Ok(Value::Float(*self as f64 % rhs)),
                        Symbol::LEQ => Ok(Value::Bool(*self as f64 <= rhs)),
                        Symbol::GEQ => Ok(Value::Bool(*self as f64 >= rhs)),
                        Symbol::LT => Ok(Value::Bool((*self as f64) < rhs)),
                        Symbol::GT => Ok(Value::Bool((*self as f64) > rhs)),
                        Symbol::CMP => Ok(Value::Fixnum((*self as f64 - rhs).signum() as i64)),
                        Symbol::BIT_AND => Ok(Value::Fixnum(*self & rhs as i64)),
                        Symbol::BIT_OR => Ok(Value::Fixnum(*self | rhs as i64)),
                        Symbol::BIT_XOR => Ok(Value::Fixnum(*self ^ rhs as i64)),
                        Symbol::SHL => Ok(Value::Fixnum(*self << rhs as i64)),
                        Symbol::SHR => Ok(Value::Fixnum(*self >> rhs as i64)),
                        _ => unreachable!(),
                    },
                    _ => unimplemented!("coerce"),
                }
            }
            Symbol::EQ => match args.args.next() {
                Some(Value::Fixnum(rhs)) => Ok(Value::Bool(*self == rhs)),
                Some(Value::Float(rhs)) => Ok(Value::Bool(*self as f64 == rhs)),
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

impl Object for f64 {
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
            Symbol::UPLUS => Ok(Value::Float(*self)),
            Symbol::UMINUS => Ok(Value::Float(-*self)),
            Symbol::ADD | Symbol::SUB | Symbol::MUL | Symbol::DIV | Symbol::REM => {
                // TODO: proper argument validation
                match args.args.next() {
                    // TODO: use checked operations
                    Some(Value::Fixnum(rhs)) => match name {
                        Symbol::ADD => Ok(Value::Float(*self + rhs as f64)),
                        Symbol::SUB => Ok(Value::Float(*self - rhs as f64)),
                        Symbol::MUL => Ok(Value::Float(*self * rhs as f64)),
                        Symbol::DIV => Ok(Value::Float(*self / rhs as f64)),
                        Symbol::REM => Ok(Value::Float(*self % rhs as f64)),
                        _ => unreachable!(),
                    },
                    Some(Value::Float(rhs)) => match name {
                        Symbol::ADD => Ok(Value::Float(*self + rhs)),
                        Symbol::SUB => Ok(Value::Float(*self - rhs)),
                        Symbol::MUL => Ok(Value::Float(*self * rhs)),
                        Symbol::DIV => Ok(Value::Float(*self / rhs)),
                        Symbol::REM => Ok(Value::Float(*self % rhs)),
                        _ => unreachable!(),
                    },
                    _ => unimplemented!("coerce"),
                }
            }
            Symbol::EQ => match args.args.next() {
                Some(Value::Float(rhs)) => Ok(Value::Bool(*self == rhs)),
                Some(Value::Fixnum(rhs)) => Ok(Value::Bool(*self == rhs as f64)),
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
