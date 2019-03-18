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

#[macro_export]
macro_rules! compile {
    ($name:ident, $code:expr) => {
        use $crate::compiler as sapphire_compiler;
        $crate::compiler_macro::compile!($name, $code);
    };
}
