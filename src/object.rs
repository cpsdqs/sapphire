//! Objects.

use crate::context::Context;
use crate::exception::Exception;
use crate::heap::{Ref, Weak};
use crate::proc::Proc;
use crate::symbol::{Symbol, Symbols};
use crate::thread::{Thread, ThreadError};
use crate::value::Value;
use fnv::FnvHashMap;
use smallvec::SmallVec;
use std::any::Any;
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
#[derive(Debug, Clone)]
pub enum SendError {
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

#[macro_export]
macro_rules! read_args {
    ($args:expr, $thread:expr; -) => {
        read_args!(__impl $args, $thread, 0;)
    };
    ($args:expr, $thread:expr; $($rest:tt)*) => {
        read_args!(__impl $args, $thread, 0; $($rest)*)
    };
    (__impl $args:expr, $thread:expr, $c:expr; $var:ident: $ty:ident, $($rest:tt)*) => {
        let $var = match $args.args.next() {
            Some(Value::$ty(arg)) => arg,
            Some(value) => {
                return Err(SendError::Exception(Value::Ref(Exception::new(
                    format!(
                        "Expected a {}, got {}",
                        stringify!($ty),
                        value.inspect($thread.context())
                    ),
                    $thread.trace(),
                    $thread.context().exceptions().argument_error.clone(),
                ))));
            }
            None => {
                return Err(SendError::Exception(Value::Ref(Exception::new(
                    format!("Expected an argument for {}", stringify!($var)),
                    $thread.trace(),
                    $thread.context().exceptions().argument_error.clone(),
                ))));
            }
        };
        read_args!(__impl $args, $thread, $c + 1; $($rest)*);
    };
    (__impl $args:expr, $thread:expr, $c:expr; $var:ident, $($rest:tt)*) => {
        let $var = match $args.args.next() {
            Some(arg) => arg,
            None => {
                return Err(SendError::Exception(Value::Ref(Exception::new(
                    format!("Expected an argument for {}", stringify!($var)),
                    $thread.trace(),
                    $thread.context().exceptions().argument_error.clone(),
                ))));
            }
        };
        read_args!(__impl $args, $thread, $c + 1; $($rest)*);
    };
    (__impl $args:expr, $thread:expr, $c:expr; $var:ident: $ty:ident) => {
        read_args!(__impl $args, $thread, $c; $var: $ty,);
    };
    (__impl $args:expr, $thread:expr, $c:expr; $var:ident) => {
        read_args!(__impl $args, $thread, $c; $var,);
    };
    (__impl $args:expr, $thread:expr, $c:expr; &$block:ident) => {
        let $block = match $args.block {
            Some(Value::Proc(arg)) => arg,
            Some(value) => {
                return Err(SendError::Exception(Value::Ref(Exception::new(
                    format!("Expected a block argument, got {}", value.inspect($thread.context())),
                    $thread.trace(),
                    $thread.context().exceptions().argument_error.clone(),
                ))));
            }
            None => {
                return Err(SendError::Exception(Value::Ref(Exception::new(
                    format!("Expected a block argument"),
                    $thread.trace(),
                    $thread.context().exceptions().argument_error.clone(),
                ))));
            }
        };
        read_args!(__impl $args, $thread, $c;);
    };
    (__impl $args:expr, $thread:expr, $c:expr; *) => {
    };
    (__impl $args:expr, $thread:expr, $c:expr;) => {
        match $args.args.next() {
            Some(_) => return Err(SendError::Exception(Value::Ref(Exception::new(
                match $c {
                    0 => format!("Expected no arguments"),
                    1 => format!("Expected at most one argument"),
                    c => format!("Expected at most {} arguments", c),
                },
                $thread.trace(),
                $thread.context().exceptions().argument_error.clone(),
            )))),
            None => ()
        }
    }
}

/// A generic module.
pub struct RbModule {
    name: Symbol,
    vars: FnvHashMap<Symbol, Value>,
    modules: SmallVec<[Ref<Object>; 16]>,
    methods: FnvHashMap<Symbol, Proc>,
    method_cache: FnvHashMap<Symbol, Proc>,
    class: Ref<Object>,
    singleton_class: Option<Ref<Object>>,
    self_ref: Weak<Object>,
}

impl RbModule {
    pub fn new(name: Symbol, context: &Context) -> Ref<Object> {
        Self::new_unchecked(name, context.module_class().clone())
    }

    pub(crate) fn new_unchecked(name: Symbol, class: Ref<Object>) -> Ref<Object> {
        let this = Ref::new(RbModule {
            name,
            vars: FnvHashMap::default(),
            modules: SmallVec::new(),
            methods: FnvHashMap::default(),
            method_cache: FnvHashMap::default(),
            class,
            singleton_class: None,
            self_ref: unsafe { mem::uninitialized() },
        });
        let this_ref = this.downgrade();
        mem::forget(mem::replace(
            &mut Object::downcast_mut::<RbModule>(&mut *this.get())
                .unwrap()
                .self_ref,
            this_ref,
        ));
        this
    }

    /// Resolves the given method.
    pub fn method(&mut self, name: Symbol, thread: &mut Thread) -> Option<Proc> {
        if let Some(proc) = self.method_cache.get(&name) {
            return Some(proc.clone());
        }
        let out = if let Some(proc) = self.methods.get(&name) {
            Some(proc.clone())
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
                    Err(_) => (),
                }
            }
            method
        };
        if let Some(out) = &out {
            self.method_cache.insert(name, out.clone());
        }
        out
    }

    pub fn def_method(&mut self, name: Symbol, method: Proc) {
        self.methods.insert(name, method);
    }

    fn ensure_singleton_class(&mut self, context: &Context) {
        if self.singleton_class.is_some() {
            return;
        }

        self.singleton_class = Some(RbClass::new_singleton(
            self.self_ref.clone(),
            self.class.clone(),
            context,
        ));
    }
}

impl Object for RbModule {
    fn get(&self, name: Symbol) -> Option<Value> {
        self.vars.get(&name).map(|value| value.clone())
    }
    fn set(&mut self, name: Symbol, value: Value) -> Result<(), ()> {
        self.vars.insert(name, value);
        Ok(())
    }
    fn send(
        &mut self,
        name: Symbol,
        args: Arguments,
        thread: &mut Thread,
    ) -> Result<Value, SendError> {
        match name {
            Symbol::METHOD => {
                read_args!(args, thread; name: Symbol);
                Ok(self
                    .method(name, thread)
                    .map(Value::Proc)
                    .unwrap_or(Value::Nil))
            }
            Symbol::DEFINE_METHOD => {
                read_args!(args, thread; name: Symbol, &proc);
                self.def_method(name, proc);
                Ok(Value::Nil)
            }
            Symbol::INCLUDE => {
                read_args!(args, thread; module: Ref);
                self.modules.push(module);
                Ok(Value::Nil)
            }
            Symbol::CLASS => {
                read_args!(args, thread; -);
                Ok(Value::Ref(self.class.clone()))
            }
            Symbol::SINGLETON_CLASS => {
                read_args!(args, thread; -);
                self.ensure_singleton_class(thread.context());
                Ok(Value::Ref(self.singleton_class.as_ref().unwrap().clone()))
            }
            name => send(
                Value::Ref(self.self_ref.upgrade().unwrap()),
                self.singleton_class.as_ref().unwrap_or(&self.class).clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        format!(
            "<Module:{}>",
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

pub(crate) enum ClassName {
    Name(Symbol),
    Singleton(Weak<Object>),
}

/// A generic class.
pub struct RbClass {
    name: ClassName,
    class_vars: FnvHashMap<Symbol, Value>,
    modules: SmallVec<[Ref<Object>; 16]>,
    methods: FnvHashMap<Symbol, Proc>,
    method_cache: FnvHashMap<Symbol, Proc>,
    class: Ref<Object>,
    singleton_class: Option<Ref<Object>>,
    superclass: Ref<Object>,
    self_ref: Weak<Object>,
}

impl RbClass {
    pub fn new(name: Symbol, superclass: Ref<Object>, context: &Context) -> Ref<Object> {
        Self::new_unchecked(
            ClassName::Name(name),
            superclass,
            context.class_class().clone(),
        )
    }

    fn new_singleton(receiver: Weak<Object>, class: Ref<Object>, context: &Context) -> Ref<Object> {
        Self::new_unchecked(
            ClassName::Singleton(receiver),
            class,
            context.class_class().clone(),
        )
    }

    pub(crate) fn new_unchecked(
        name: ClassName,
        superclass: Ref<Object>,
        class: Ref<Object>,
    ) -> Ref<Object> {
        let this = Ref::new(RbClass {
            name,
            class_vars: FnvHashMap::default(),
            modules: SmallVec::new(),
            methods: FnvHashMap::default(),
            method_cache: FnvHashMap::default(),
            class,
            singleton_class: None,
            superclass,
            self_ref: unsafe { mem::uninitialized() },
        });
        let this_ref = this.downgrade();
        mem::forget(mem::replace(
            &mut Object::downcast_mut::<RbClass>(&mut *this.get())
                .unwrap()
                .self_ref,
            this_ref,
        ));
        this
    }

    /// Resolves the given method.
    pub fn method(&mut self, name: Symbol, thread: &mut Thread) -> Option<Proc> {
        if let Some(proc) = self.method_cache.get(&name) {
            return Some(proc.clone());
        }
        let out = if let Some(proc) = self.methods.get(&name) {
            Some(proc.clone())
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
                    Err(_) => (),
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
                    Ok(_) => None,
                    Err(_) => None,
                }
            } else {
                None
            }
        };
        if let Some(out) = &out {
            self.method_cache.insert(name, out.clone());
        }
        out
    }

    pub fn def_method(&mut self, name: Symbol, method: Proc) {
        self.methods.insert(name, method);
    }

    fn is_own_superclass(&self) -> bool {
        // comparing the pointers directly will always return false
        // (possibly because superclass.as_ptr() returns a fat pointer)
        self.superclass.as_ptr() as *const () == self as *const _ as *const ()
    }

    fn is_singleton_class(&self) -> bool {
        match self.name {
            ClassName::Singleton(_) => true,
            _ => false,
        }
    }

    fn ensure_singleton_class(&mut self, context: &Context) {
        if self.singleton_class.is_some() {
            return;
        }

        self.singleton_class = Some(RbClass::new_singleton(
            self.self_ref.clone(),
            self.class.clone(),
            context,
        ));
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
            Symbol::NEW if self.is_singleton_class() => {
                Err(SendError::Exception(Value::Ref(Exception::new(
                    format!("cannot instantiate a singleton class"),
                    thread.trace(),
                    thread.context().exceptions().type_error.clone(),
                ))))
            }
            Symbol::METHOD => {
                read_args!(args, thread; name: Symbol);
                Ok(self
                    .method(name, thread)
                    .map(Value::Proc)
                    .unwrap_or(Value::Nil))
            }
            Symbol::DEFINE_METHOD => {
                read_args!(args, thread; name: Symbol, &proc);
                self.def_method(name, proc);
                Ok(Value::Nil)
            }
            Symbol::INCLUDE => {
                read_args!(args, thread; module: Ref);
                self.modules.push(module);
                Ok(Value::Nil)
            }
            Symbol::SUPERCLASS => {
                read_args!(args, thread; -);
                Ok(Value::Ref(self.superclass.clone()))
            }
            Symbol::CLASS => {
                read_args!(args, thread; -);
                Ok(Value::Ref(self.class.clone()))
            }
            Symbol::SINGLETON_CLASS => {
                read_args!(args, thread; -);
                self.ensure_singleton_class(thread.context());
                Ok(Value::Ref(self.singleton_class.as_ref().unwrap().clone()))
            }
            name => send(
                Value::Ref(self.self_ref.upgrade().unwrap()),
                self.singleton_class.as_ref().unwrap_or(&self.class).clone(),
                name,
                args,
                thread,
            ),
        }
    }
    fn inspect(&self, context: &Context) -> String {
        match &self.name {
            ClassName::Name(name) => format!(
                "<Class:{}>",
                context.symbols().symbol_name(*name).unwrap_or("?")
            ),
            ClassName::Singleton(owner) => match owner.upgrade() {
                Some(obj) => format!("<SingletonClass:{}>", obj.get().inspect(context)),
                None => format!("<SingletonClass:[dropped]>"),
            },
        }
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
    class: Ref<Object>,
    name: Symbol,
    args: Arguments,
    thread: &mut Thread,
) -> Result<Value, SendError> {
    let res = class.get().send(
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
                let exception = Exception::new(
                    "method_missing is missing".into(),
                    thread.trace(),
                    thread.context().exceptions().no_method_error.clone(),
                );
                Err(SendError::Exception(Value::Ref(exception)))
            }
        }
        Ok(_) => unimplemented!("raise exception"),
        Err(err) => Err(err),
    }
}

/// A generic object.
pub struct RbObject {
    ivars: FnvHashMap<Symbol, Value>,
    class: Ref<Object>,
    singleton_class: Option<Ref<Object>>,
    self_ref: Weak<Object>,
}

impl RbObject {
    pub fn new(class: Ref<Object>) -> Ref<Object> {
        let this = Ref::new(RbObject {
            ivars: FnvHashMap::default(),
            class,
            singleton_class: None,
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

    fn ensure_singleton_class(&mut self, context: &Context) {
        if self.singleton_class.is_some() {
            return;
        }

        self.singleton_class = Some(RbClass::new_singleton(
            self.self_ref.clone(),
            self.class.clone(),
            context,
        ));
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
        match name {
            Symbol::CLASS => Ok(Value::Ref(self.class.clone())),
            Symbol::SINGLETON_CLASS => {
                self.ensure_singleton_class(thread.context());
                Ok(Value::Ref(self.singleton_class.as_ref().unwrap().clone()))
            }
            name => send(
                Value::Ref(self.self_ref.upgrade().unwrap()),
                self.singleton_class.as_ref().unwrap_or(&self.class).clone(),
                name,
                args,
                thread,
            ),
        }
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
        name: ClassName::Name(symbols.symbol("Object")),
        class_vars: FnvHashMap::default(),
        modules: SmallVec::new(),
        methods: FnvHashMap::default(),
        method_cache: FnvHashMap::default(),
        class: unsafe { mem::uninitialized() },
        singleton_class: None,
        superclass: unsafe { mem::uninitialized() },
        self_ref: unsafe { mem::uninitialized() },
    });
    let module_class: Ref<Object> = Ref::new(RbClass {
        name: ClassName::Name(symbols.symbol("Module")),
        class_vars: FnvHashMap::default(),
        modules: SmallVec::new(),
        methods: FnvHashMap::default(),
        method_cache: FnvHashMap::default(),
        class: unsafe { mem::uninitialized() },
        singleton_class: None,
        superclass: object_class.clone(),
        self_ref: unsafe { mem::uninitialized() },
    });
    let class_class: Ref<Object> = Ref::new(RbClass {
        name: ClassName::Name(symbols.symbol("Class")),
        class_vars: FnvHashMap::default(),
        modules: SmallVec::new(),
        methods: FnvHashMap::default(),
        method_cache: FnvHashMap::default(),
        class: unsafe { mem::uninitialized() },
        singleton_class: None,
        superclass: module_class.clone(),
        self_ref: unsafe { mem::uninitialized() },
    });

    let object_class_ref = object_class.clone();
    let class_class_ref = class_class.clone();
    let module_class_ref = module_class.clone();

    {
        let mut object_class_i = object_class.get();
        let mut module_class_i = module_class.get();
        let mut class_class_i = class_class.get();
        let object_class = Object::downcast_mut::<RbClass>(&mut *object_class_i).unwrap();
        let module_class = Object::downcast_mut::<RbClass>(&mut *module_class_i).unwrap();
        let class_class = Object::downcast_mut::<RbClass>(&mut *class_class_i).unwrap();

        mem::forget(mem::replace(
            &mut object_class.self_ref,
            object_class_ref.downgrade(),
        ));
        mem::forget(mem::replace(
            &mut class_class.self_ref,
            class_class_ref.downgrade(),
        ));
        mem::forget(mem::replace(
            &mut module_class.self_ref,
            module_class_ref.downgrade(),
        ));

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

        fn init(_: Value, args: Arguments, thread: &mut Thread) -> Result<Value, SendError> {
            read_args!(args, thread; -);
            Ok(Value::Nil)
        }
        object_class.def_method(Symbol::INITIALIZE, Proc::Native(init));
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
