use crate::context::Context;
use crate::heap::Ref;
use crate::proc::Proc;
use crate::symbol::Symbol;
use crate::value::Value;
use smallvec::SmallVec;
use std::any::Any;

/// Object types.
pub enum ObjectType {
    /// Any sort of object that is not of another type.
    Object,

    /// A class object. Its class must be the object `Class`.
    Class,

    /// A module object. Its class must be the object `Module`.
    Module,
}

/// An object.
pub trait Object: Send {
    /// Helper method for downcasting.
    fn as_any(&self) -> &Any;

    /// Helper method for downcasting.
    fn as_any_mut(&mut self) -> &mut Any;

    fn as_const_node(&self) -> Option<&ConstNode> {
        None
    }
    fn as_const_node_mut(&mut self) -> Option<&mut ConstNode> {
        None
    }
    fn as_class(&self) -> Option<&Class> {
        None
    }
    fn as_class_mut(&mut self) -> Option<&mut Class> {
        None
    }
    fn as_module(&self) -> Option<&Module> {
        None
    }
    fn as_module_mut(&mut self) -> Option<&mut Module> {
        None
    }

    /// Returns the object type.
    fn object_type(&self) -> ObjectType;

    /// Returns the object’s class.
    fn class(&self, context: &Context) -> Ref<Object>;

    /// Returns an instance variable.
    fn get_ivar(&self, name: Symbol) -> Option<Value>;

    /// Sets an instance variable.
    fn set_ivar(&mut self, name: Symbol, value: Value) -> Result<(), ()>;
}

/// A node in the “const tree.”
pub trait ConstNode: Send {
    /// Returns a constant.
    fn get_const(&self, name: Symbol) -> Option<Value>;

    /// Attempts to set a constant.
    fn set_const(&mut self, name: Symbol, value: Value) -> Result<(), ()>;
}

/// A class object.
pub trait Class: Object + ConstNode {
    /// Resolves a method.
    fn resolve_method(&mut self, name: Symbol) -> Option<Proc>;
}

/// A module object.
pub trait Module: Object + ConstNode {
    /// Resolves a method.
    fn resolve_method(&mut self, name: Symbol) -> Option<Proc>;
}

impl Object {
    /// Attempts to downcast the [Object] into the type T.
    pub fn downcast_ref<T: Any + Send>(this: &Object) -> Option<&T> {
        this.as_any().downcast_ref::<T>()
    }

    /// Attempts to downcast the [Object] into a mutable reference to the type T.
    pub fn downcast_mut<T: Any + Send>(this: &mut Object) -> Option<&mut T> {
        this.as_any_mut().downcast_mut::<T>()
    }
}

/// Method call arguments.
pub struct Arguments {
    pub args: SmallVec<[Value; 32]>,
    pub block: Option<Value>,
}
