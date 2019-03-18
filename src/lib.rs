//! Ruby VM.

pub extern crate sapphire_compiler as compiler;
#[doc(hidden)]
pub extern crate sapphire_compiler_macro as compiler_macro;

pub mod context;
pub mod exception;
pub mod heap;
pub mod numeric;
pub mod object;
pub mod proc;
pub mod string;
pub mod symbol;
pub mod thread;
pub mod value;

/// Compiles ruby source code into sapphire bytecode.
///
/// The source must be a method definition.
///
/// # Examples
/// ```
/// # use sapphire::context::Context;
/// # use sapphire::thread::Thread;
/// # use sapphire::object::Arguments;
/// # use sapphire::value::Value;
/// # use std::sync::Arc;
/// // Ruby source code that will be compiled to byte code at compile-time
/// let compiled = sapphire::compile!("def one_plus_one; 1 + 1; end");
///
/// // Create an execution context
/// let context = Arc::new(Context::new());
///
/// // instantiate `compiled` as a Proc in the new context
/// let compiled_in_context = Arc::new(compiled.new(&mut *context.symbols_mut()));
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
/// match result {
///     Value::Fixnum(value) => assert_eq!(value, 2),
///     _ => panic!("wrong data type"),
/// }
/// ```
#[macro_export]
macro_rules! compile {
    ($code:expr) => {
        {
            mod inner {
                use $crate::compiler as sapphire_compiler;
                $crate::compiler_macro::compile!(COMPILED, $code);
            }
            inner::COMPILED
        }
    };
}
