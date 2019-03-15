use sapphire::compiler::compile_ir;
use sapphire::context::Context;
use sapphire::object::Object;
use sapphire::symbol::Symbols;
use sapphire::thread::{Thread, ThreadResult};
use std::io::{self, Read, Write};
use std::process::exit;
use std::sync::Arc;

fn main() {
    if atty::isnt(atty::Stream::Stdin) {
        let mut input = String::new();
        io::stdin()
            .read_to_string(&mut input)
            .expect("Failed to read stdin");
        let mut symbols = Symbols::new();
        let compiled_ir = match compile_ir("stdin", input, &mut symbols) {
            Ok(compiled) => compiled,
            Err(err) => {
                eprintln!("Failed to compile:\n{}", err.fmt_ansi());
                exit(1);
            }
        };
        unimplemented!("run {}", compiled_ir.fmt_with_symbols(&symbols));
    } else {
        let context = Arc::new(Context::new());
        loop {
            print!("\x1b[38;5;248msapphire (IR,byte,exec)> \x1b[m");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read stdin");
            let a = ::std::time::Instant::now();
            let compiled_ir = match compile_ir("stdin", input, &mut context.symbols_mut()) {
                Ok(compiled_ir) => compiled_ir,
                Err(err) => {
                    eprintln!("Failed to compile:\n{}", err.fmt_ansi());
                    continue;
                }
            };
            println!("{}", compiled_ir.fmt_with_symbols(&context.symbols()));
            let proc = Arc::new(compiled_ir.into());
            println!("{:?}", proc);
            let b = ::std::time::Instant::now();
            let mut thread = Thread::new_root(Arc::clone(&context), proc);
            loop {
                match thread.next() {
                    ThreadResult::NotReady => (),
                    ThreadResult::Ready(value) => {
                        println!("-> \x1b[32m{}\x1b[m", value.inspect(&context));
                        break;
                    }
                    ThreadResult::Err(err) => {
                        eprintln!("{:?} in {:?}", err, thread);
                        break;
                    }
                }
            }
            let c = ::std::time::Instant::now();
            println!("time: {:?} {:?}", b - a, c - b);
        }
    }
}
