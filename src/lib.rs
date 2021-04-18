//! Sort of a Ruby VM.
//!
//! # Interfacing with Rust
//! Anything that can be a Ruby value implements the [`Object`](object::Object) trait, so calling
//! Ruby methods from Rust is as simple as calling [`send`](object::Object::send) on the object.
//!
//! On the flip side, defining new types for Ruby to use is as simple as implementing `Object`
//! on a Rust type and turning it into an `Object` trait object.
//! When defining methods, using `read_args!` may be useful for argument validation.
//!
//! ## Defining Classes for Rust Types
//! To play nice with Ruby’s class and module system, an `Object` implementation should ideally be
//! accompanied by a class definition for the new type. Said class should probably also be added
//! as a constant to the `Object` class.
//!
//! However, if the class was defined using [`RbClass`](object::RbClass), calling `new` on the
//! class will return a new [`RbObject`](object::RbObject).
//! To prevent this without having to override `new` on every subclass, the magic *instance* method
//! [`sapphire_allocate`](symbol::Symbol::SAPPHIRE_ALLOCATE) can be defined to return the custom
//! Rust type. This method will be called with `nil` as the receiver by the default implementation
//! of `new`. Note that the `sapphire_allocate` implementation must have a
//! [`Proc::Native`](proc::Proc::Native) body.

pub extern crate sapphire_compiler as compiler;
#[doc(hidden)]
pub extern crate sapphire_compiler_macro as compiler_macro;

pub mod collections;
pub mod context;
pub mod exception;
pub mod heap;
pub mod kernel;
pub mod module;
pub mod numeric;
pub mod object;
pub mod proc;
pub mod string;
pub mod symbol;
pub mod thread;
pub mod value;

pub use context::Context;
pub use object::Object;
pub use symbol::Symbol;
pub use thread::Thread;
pub use value::Value;

/// Compiles ruby source code into sapphire bytecode.
///
/// The source must be a method definition. Either use `compile!("def ... end")` or point to a file
/// relative to the crate’s `Cargo.toml` by doing `compile!(file "path/to/file.rb")`.
///
/// Also see [ConstProc](sapphire_compiler::ConstProc).
///
/// # Examples
/// ```
/// # use sapphire::context::Context;
/// # use sapphire::thread::Thread;
/// # use sapphire::object::Arguments;
/// # use sapphire::value::Value;
/// # use sapphire::proc::Proc;
/// # use std::sync::Arc;
/// // Ruby source code that will be compiled to byte code at compile-time
/// let compiled = sapphire::compile!("def one_plus_one; 1 + 1; end");
///
/// // Create an execution context
/// let context = Context::new();
///
/// // instantiate `compiled` as a Proc in the new context
/// let compiled_in_context = Proc::Sapphire(Arc::new(compiled.new(&mut *context.symbols_mut())));
///
/// let mut thread = Thread::new(Arc::clone(&context));
///
/// // call the method with no arguments on the root object
/// let result = thread.call(
///     Value::Ref(context.root().clone()),
///     compiled_in_context,
///     Arguments::empty(),
/// ).unwrap();
///
/// // 1 + 1 = 2
/// assert_eq!(result, Value::Fixnum(2));
/// ```
#[macro_export]
macro_rules! compile {
    (file $code:expr) => {{
        mod inner {
            use $crate::compiler as sapphire_compiler;
            $crate::compiler_macro::compile!(COMPILED, file $code);
        }
        inner::COMPILED
    }};
    ($code:expr) => {{
        mod inner {
            use $crate::compiler as sapphire_compiler;
            $crate::compiler_macro::compile!(COMPILED, $code);
        }
        inner::COMPILED
    }};
}
