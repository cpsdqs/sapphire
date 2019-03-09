use crate::context::Context;
use crate::heap::Ref;
use crate::proc::Proc;
use crate::symbol::{Symbol, Symbols};
use crate::value::Value;
use fnv::FnvHashMap;
use parking_lot::MutexGuard;
use smallvec::SmallVec;
use std::any::Any;
use std::mem;
use std::ops::{Deref, DerefMut};
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

pub enum ObjectRef<'a, T: ?Sized> {
    Ref(&'a T),
    Guard(MutexGuard<'a, Object>),
}
pub enum ObjectRefMut<'a, T: ?Sized> {
    Ref(&'a mut T),
    Guard(MutexGuard<'a, Object>),
}
macro_rules! impl_object_ref {
    ($o:ident, $t:tt, $f:ident) => {
        impl<'a> Deref for $o<'a, dyn $t> {
            type Target = dyn $t;
            fn deref(&self) -> &Self::Target {
                match self {
                    $o::Ref(r) => *r,
                    $o::Guard(r) => unsafe { mem::transmute::<&$t, &$t>(&*r.$f().unwrap()) },
                }
            }
        }
    };
    (mut $o:ident, $t:tt, $f:ident) => {
        impl<'a> DerefMut for $o<'a, dyn $t> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                match self {
                    $o::Ref(r) => *r,
                    $o::Guard(r) => unsafe {
                        mem::transmute::<&mut $t, &mut $t>(&mut *r.$f().unwrap())
                    },
                }
            }
        }
    };
}
impl_object_ref!(ObjectRef, Module, as_module);
impl_object_ref!(ObjectRef, Class, as_class);
impl_object_ref!(ObjectRefMut, Module, as_module);
impl_object_ref!(ObjectRefMut, Class, as_class);
impl_object_ref!(mut ObjectRefMut, Module, as_module_mut);
impl_object_ref!(mut ObjectRefMut, Class, as_class_mut);

/// An object.
pub trait Object: Send {
    /// Helper method for downcasting.
    fn as_any(&self) -> &Any;

    /// Helper method for downcasting.
    fn as_any_mut(&mut self) -> &mut Any;

    fn as_module(&self) -> Option<ObjectRef<Module>> {
        None
    }
    fn as_module_mut(&mut self) -> Option<ObjectRefMut<Module>> {
        None
    }
    fn as_class(&self) -> Option<ObjectRef<Class>> {
        None
    }
    fn as_class_mut(&mut self) -> Option<ObjectRefMut<Class>> {
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
#[derive(Debug)]
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
    fn as_module(&self) -> Option<ObjectRef<Module>> {
        Some(ObjectRef::Ref(self))
    }
    fn as_module_mut(&mut self) -> Option<ObjectRefMut<Module>> {
        Some(ObjectRefMut::Ref(self))
    }
    fn as_class(&self) -> Option<ObjectRef<Class>> {
        Some(ObjectRef::Ref(self))
    }
    fn as_class_mut(&mut self) -> Option<ObjectRefMut<Class>> {
        Some(ObjectRefMut::Ref(self))
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

/// A generic object.
pub struct RbObject {
    ivars: FnvHashMap<Symbol, Value>,
    class: Ref<Object>,
}

impl RbObject {
    pub fn new(class: Ref<Object>) -> RbObject {
        RbObject {
            ivars: FnvHashMap::default(),
            class,
        }
    }
}

impl Object for RbObject {
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
    fn object_type(&self) -> ObjectType {
        ObjectType::Object
    }
    fn class(&self, _: &Context) -> Ref<Object> {
        self.class.clone()
    }
    fn get_ivar(&self, name: Symbol) -> Option<Value> {
        self.ivars.get(&name).map(|value| value.clone())
    }
    fn set_ivar(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.ivars.insert(name, value);
        Ok(())
    }
}

/// Creates the object class, class class, and module class.
pub fn init_root(symbols: &mut Symbols) -> (Ref<Object>, Ref<Object>, Ref<Object>) {
    let object_class: Ref<Object> = Ref::new(RbClass {
        name: symbols.symbol("Object"),
        class_vars: FnvHashMap::default(),
        modules: SmallVec::new(),
        methods: FnvHashMap::default(),
        method_cache: FnvHashMap::default(),
        class: unsafe { mem::uninitialized() },
        superclass: unsafe { mem::uninitialized() },
    });
    let module_class: Ref<Object> = Ref::new(RbClass {
        name: symbols.symbol("Module"),
        class_vars: FnvHashMap::default(),
        modules: SmallVec::new(),
        methods: FnvHashMap::default(),
        method_cache: FnvHashMap::default(),
        class: unsafe { mem::uninitialized() },
        superclass: object_class.clone(),
    });
    let class_class: Ref<Object> = Ref::new(RbClass {
        name: symbols.symbol("Class"),
        class_vars: FnvHashMap::default(),
        modules: SmallVec::new(),
        methods: FnvHashMap::default(),
        method_cache: FnvHashMap::default(),
        class: unsafe { mem::uninitialized() },
        superclass: module_class.clone(),
    });

    let object_class_ref = object_class.clone();
    let class_class_ref = class_class.clone();

    {
        let mut object_class_i = object_class.get();
        let mut module_class_i = module_class.get();
        let mut class_class_i = class_class.get();
        let object_class = Object::downcast_mut::<RbClass>(&mut *object_class_i).unwrap();
        let module_class = Object::downcast_mut::<RbClass>(&mut *module_class_i).unwrap();
        let class_class = Object::downcast_mut::<RbClass>(&mut *class_class_i).unwrap();
        mem::forget(mem::replace(
            &mut object_class.class,
            class_class_ref.clone(),
        ));
        mem::forget(mem::replace(&mut object_class.superclass, object_class_ref));
        mem::forget(mem::replace(
            &mut module_class.class,
            class_class_ref.clone(),
        ));
        mem::forget(mem::replace(&mut class_class.class, class_class_ref));
    }

    (object_class, class_class, module_class)
}
