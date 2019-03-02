use crate::heap::Ref;
use crate::object::Object;
use crate::proc::Proc;
use crate::symbol::Symbol;
use std::sync::Arc;

pub enum Value {
    Bool(bool),
    Fixnum(i64),
    Float(f64),
    Symbol(Symbol),
    String(String),
    Proc(Arc<Proc>),
    Object(Ref<dyn Object>),
}
