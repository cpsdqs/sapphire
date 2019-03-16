//! Objects.

use crate::context::Context;
use crate::heap::{Ref, Weak};
use crate::proc::Proc;
use crate::symbol::{Symbol, Symbols};
use crate::thread::{Thread, ThreadError};
use crate::value::Value;
use fnv::FnvHashMap;
use smallvec::SmallVec;
use std::any::Any;
use std::sync::Arc;
use std::{iter, mem};

/// A generic object.
pub trait Object: Send + Any {
    /// Gets an ivar.
    fn get(&self, name: Symbol) -> Option<Value>;
    /// Sets an ivar.
    fn set(&mut self, name: Symbol, value: Value) -> Result<(), ()>;
    /// Sends a message to the object.
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError>;
    /// Inspects the object.
    fn inspect(&self, context: &Context) -> String;

    // TODO: remove these when TypeId::type_id() is stable
    /// Helper method for downcasting.
    fn as_any(&self) -> &Any;
    /// Helper method for downcasting.
    fn as_any_mut(&mut self) -> &mut Any;
}

impl Object {
    /// Attempts to downcast the object to the given type.
    pub fn downcast_ref<T: 'static + Object>(this: &dyn Object) -> Option<&T> {
        this.as_any().downcast_ref()
    }

    /// Attempts to downcast the object to the given type.
    pub fn downcast_mut<T: 'static + Object>(this: &mut dyn Object) -> Option<&mut T> {
        this.as_any_mut().downcast_mut()
    }
}

/// Method send errors.
#[derive(Debug)]
pub enum SendError {
    MethodMissing,
    Exception(Value),
    Thread(ThreadError),
}

impl From<ThreadError> for SendError {
    fn from(this: ThreadError) -> SendError {
        SendError::Thread(this)
    }
}

/// Method call arguments.
pub struct Arguments<'a> {
    pub args: &'a mut dyn Iterator<Item = Value>,
    pub block: Option<Value>,
}

impl<'a> Arguments<'a> {
    pub fn new(args: &'a mut dyn Iterator<Item = Value>, block: Option<Value>) -> Arguments<'a> {
        Arguments { args, block }
    }

    pub fn empty() -> Arguments<'static> {
        struct Empty;
        impl Iterator for Empty {
            type Item = Value;
            fn next(&mut self) -> Option<Value> {
                None
            }
        }
        lazy_static::lazy_static! {
            static ref EMPTY: Box<Empty> = Box::new(Empty);
        }

        Arguments {
            args: unsafe { &mut *(&**EMPTY as *const Empty as *mut Empty) },
            block: None,
        }
    }
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

    /// Resolves the given method.
    pub fn method(&mut self, name: Symbol, thread: &mut Thread) -> Option<Arc<Proc>> {
        if let Some(proc) = self.method_cache.get(&name) {
            return Some(Arc::clone(proc));
        }
        let out = if let Some(proc) = self.methods.get(&name) {
            Some(Arc::clone(proc))
        } else {
            let mut method = None;
            for module in self.modules.iter().rev() {
                let mut args = iter::once(Value::Symbol(name));
                match module
                    .get()
                    .send(Symbol::METHOD, Arguments::new(&mut args, None), thread)
                {
                    Ok(Value::Proc(m)) => {
                        method = Some(m);
                        break;
                    }
                    Ok(_) => continue,
                    Err(err) => unimplemented!("handle {:?}", err),
                }
            }
            if let Some(method) = method {
                Some(method)
            } else if !self.is_own_superclass() {
                let mut args = iter::once(Value::Symbol(name));
                match self.superclass.get().send(
                    Symbol::METHOD,
                    Arguments::new(&mut args, None),
                    thread,
                ) {
                    Ok(Value::Proc(m)) => Some(m),
                    Ok(Value::Nil) => None,
                    Ok(_) => unimplemented!("handle invalid return value"),
                    Err(err) => unimplemented!("handle {:?}", err),
                }
            } else {
                None
            }
        };
        if let Some(out) = &out {
            self.method_cache.insert(name, Arc::clone(out));
        }
        out
    }

    fn is_own_superclass(&self) -> bool {
        // comparing the pointers directly will always return false
        // (possibly because superclass.as_ptr() returns a fat pointer)
        self.superclass.as_ptr() as *const () == self as *const _ as *const ()
    }
}

impl Object for RbClass {
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
        match name {
            // TODO: proper argument validation
            Symbol::METHOD => match args.args.next() {
                Some(Value::Symbol(name)) => Ok(self
                    .method(name, thread)
                    .map(Value::Proc)
                    .unwrap_or(Value::Nil)),
                _ => unimplemented!("raise ArgumentError"),
            },
            Symbol::DEFINE_METHOD => match (args.args.next(), args.block) {
                (Some(Value::Symbol(name)), Some(Value::Proc(ref proc))) => {
                    self.methods.insert(name, proc.clone());
                    Ok(Value::Nil)
                }
                _ => unimplemented!("raise ArgumentError"),
            },
            Symbol::SUPERCLASS => Ok(Value::Ref(self.superclass.clone())),
            _ => Err(SendError::MethodMissing),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        format!(
            "<Class:{}>",
            context.symbols().symbol_name(self.name).unwrap_or("?")
        )
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}

/// Default send implementation: will try to call the method or `method_missing`.
pub fn send(
    mut this: Value,
    class: &mut Object,
    name: Symbol,
    args: Arguments,
    thread: &mut Thread,
) -> Result<Value, SendError> {
    let res = class.send(
        Symbol::METHOD,
        Arguments::new(&mut iter::once(Value::Symbol(name)), None),
        thread,
    );
    match res {
        Ok(Value::Proc(method)) => Ok(thread.call(this, method, args)?),
        Ok(Value::Nil) => {
            if name != Symbol::METHOD_MISSING {
                struct MethodMissingArgs<'a>(Option<Value>, &'a mut dyn Iterator<Item = Value>);
                impl<'a> Iterator for MethodMissingArgs<'a> {
                    type Item = Value;
                    fn next(&mut self) -> Option<Value> {
                        match self.0.take() {
                            Some(i) => Some(i),
                            None => self.1.next(),
                        }
                    }
                }

                let block = args.block;
                let mut args = MethodMissingArgs(Some(Value::Symbol(name)), args.args);

                this.send(
                    Symbol::METHOD_MISSING,
                    Arguments::new(&mut args, block),
                    thread,
                )
            } else {
                unimplemented!("method_missing is missing")
            }
        }
        res => unimplemented!("handle unexpected class.method response: {:?}", res),
    }
}

/// A generic object.
pub struct RbObject {
    ivars: FnvHashMap<Symbol, Value>,
    class: Ref<Object>,
    self_ref: Weak<Object>,
}

impl RbObject {
    pub fn new(class: Ref<Object>) -> Ref<Object> {
        let this = Ref::new(RbObject {
            ivars: FnvHashMap::default(),
            class,
            self_ref: unsafe { mem::uninitialized() },
        });
        let weak = this.downgrade();
        mem::forget(mem::replace(
            &mut Object::downcast_mut::<RbObject>(&mut *this.get())
                .unwrap()
                .self_ref,
            weak,
        ));
        this
    }
}

impl Object for RbObject {
    fn get(&self, name: Symbol) -> Option<Value> {
        self.ivars.get(&name).map(|value| value.clone())
    }
    fn set(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.ivars.insert(name, value);
        Ok(())
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        send(
            Value::Ref(self.self_ref.upgrade().unwrap()),
            &mut *self.class.get(),
            name,
            args,
            thread,
        )
    }
    fn inspect(&self, context: &Context) -> String {
        let class = self.class.get().inspect(context);
        format!("<{}:{:?}>", class, self as *const Self)
    }
    fn as_any(&self) -> &Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut Any {
        self
    }
}

/// Creates the object class, class class, and module class (return value is in that order).
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

    let object_class_ref = object_class.clone();
    object_class
        .get()
        .set(symbols.symbol("Object"), Value::Ref(object_class_ref))
        .unwrap();
    object_class
        .get()
        .set(symbols.symbol("Class"), Value::Ref(class_class.clone()))
        .unwrap();
    object_class
        .get()
        .set(symbols.symbol("Module"), Value::Ref(module_class.clone()))
        .unwrap();

    (object_class, class_class, module_class)
}
