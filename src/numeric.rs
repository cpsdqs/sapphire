//! Numeric type implementations.

use crate::context::Context;
use crate::exception::Exception;
use crate::object::{send, Arguments, Object, SendError};
use crate::read_args;
use crate::symbol::Symbol;
use crate::thread::Thread;
use crate::value::Value;
use std::any::Any;

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
            Symbol::CLASS => {
                read_args!(args, thread; -);
                Ok(Value::Ref(thread.context().fixnum_class().clone()))
            }
            Symbol::UPLUS => {
                read_args!(args, thread; -);
                Ok(Value::Fixnum(*self))
            }
            Symbol::UMINUS => {
                read_args!(args, thread; -);
                Ok(Value::Fixnum(-*self))
            }
            Symbol::BIT_INV => {
                read_args!(args, thread; -);
                Ok(Value::Fixnum(!*self))
            }
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
                read_args!(args, thread; rhs);
                match rhs {
                    // TODO: use checked operations
                    Value::Fixnum(rhs) => match name {
                        Symbol::ADD => Ok(Value::Fixnum(*self + rhs)),
                        Symbol::SUB => Ok(Value::Fixnum(*self - rhs)),
                        Symbol::MUL => Ok(Value::Fixnum(*self * rhs)),
                        Symbol::DIV => {
                            if rhs == 0 {
                                Err(SendError::Exception(Value::Ref(Exception::new(
                                    "attempt to divide by zero".into(),
                                    thread.trace(),
                                    thread.context().exceptions().zero_division_error.clone(),
                                ))))
                            } else {
                                Ok(Value::Fixnum(*self / rhs))
                            }
                        }
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
                    Value::Float(rhs) => match name {
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
            Symbol::EQ => {
                read_args!(args, thread; rhs);
                match rhs {
                    Value::Fixnum(rhs) => Ok(Value::Bool(*self == rhs)),
                    Value::Float(rhs) => Ok(Value::Bool(*self as f64 == rhs)),
                    _ => Ok(Value::Bool(false)),
                }
            }
            _ => send(
                Value::Fixnum(*self),
                thread.context().fixnum_class().clone(),
                name,
                args,
                thread,
            ),
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
            Symbol::CLASS => Ok(Value::Ref(thread.context().float_class().clone())),
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
            _ => send(
                Value::Float(*self),
                thread.context().fixnum_class().clone(),
                name,
                args,
                thread,
            ),
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
