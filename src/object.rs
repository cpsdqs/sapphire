use crate::context::Context;
use crate::heap::Ref;
use crate::proc::Proc;
use crate::symbol::Symbol;
use crate::value::Value;
use fnv::FnvHashMap;
use smallvec::SmallVec;
use std::any::Any;
use std::sync::Arc;

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

    fn as_module(&self) -> Option<&Module> {
        None
    }
    fn as_module_mut(&mut self) -> Option<&mut Module> {
        None
    }
    fn as_class(&self) -> Option<&Class> {
        None
    }
    fn as_class_mut(&mut self) -> Option<&mut Class> {
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

    fn inspect(&self, _context: &Context) -> String {
        format!("<?:{:?}>", self as *const _)
    }
}

/// A module object.
pub trait Module: Send {
    /// Returns a constant.
    fn get_const(&self, name: Symbol) -> Option<Value>;

    /// Attempts to set a constant.
    fn set_const(&mut self, name: Symbol, value: Value) -> Result<(), ()>;

    /// Defines a method on this module.
    fn def_method(&mut self, name: Symbol, body: Arc<Proc>) -> Result<(), ()>;

    /// Resolves a method.
    fn resolve_method(&mut self, name: Symbol) -> Option<Arc<Proc>>;

    /// Includes a module.
    fn include_module(&mut self, module: Ref<Object>) -> Result<(), ()>;
}

/// A class object.
pub trait Class: Module {
    fn superclass(&self, context: &Context) -> Ref<Object>;
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

/// A Ruby-defined class.
pub struct RbClass {
    name: Symbol,
    class_vars: FnvHashMap<Symbol, Value>,
    modules: SmallVec<[Ref<Object>; 16]>,
    methods: FnvHashMap<Symbol, Arc<Proc>>,
    method_cache: FnvHashMap<Symbol, Arc<Proc>>,
    class: Ref<Object>,
    superclass: Ref<Object>,
}

impl RbClass {
    pub fn new(name: Symbol, superclass: Ref<Object>, context: &Context) -> RbClass {
        RbClass {
            name,
            class_vars: FnvHashMap::default(),
            modules: SmallVec::new(),
            methods: FnvHashMap::default(),
            method_cache: FnvHashMap::default(),
            class: context.class_class().clone(),
            superclass,
        }
    }
}

impl Object for RbClass {
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Class
    }
    fn class(&self, _: &Context) -> Ref<Object> {
        self.class.clone()
    }
    fn get_ivar(&self, name: Symbol) -> Option<Value> {
        self.class_vars.get(&name).map(|value| value.clone())
    }
    fn set_ivar(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.class_vars.insert(name, value);
        Ok(())
    }
}

impl Module for RbClass {
    fn get_const(&self, name: Symbol) -> Option<Value> {
        self.class_vars.get(&name).map(|value| value.clone())
    }
    fn set_const(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.class_vars.insert(name, value);
        Ok(())
    }
    fn def_method(&mut self, name: Symbol, body: Arc<Proc>) -> Result<(), ()> {
        self.methods.insert(name, body);
        Ok(())
    }
    fn resolve_method(&mut self, _name: Symbol) -> Option<Arc<Proc>> {
        unimplemented!("resolve method")
    }
    fn include_module(&mut self, module: Ref<Object>) -> Result<(), ()> {
        self.modules.push(module);
        Ok(())
    }
}

impl Class for RbClass {
    fn superclass(&self, _: &Context) -> Ref<Object> {
        self.superclass.clone()
    }
}
