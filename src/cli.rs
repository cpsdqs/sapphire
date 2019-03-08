use sapphire::compiler::compile_ir;
use sapphire::context::Context;
use sapphire::proc::Proc;
use sapphire::symbol::Symbols;
use sapphire::thread::Thread;
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
            print!("sapphire (IR)> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read stdin");
            let compiled_ir = match compile_ir("stdin", input, &mut context.symbols_mut()) {
                Ok(compiled_ir) => compiled_ir,
                Err(err) => {
                    eprintln!("Failed to compile:\n{}", err.fmt_ansi());
                    continue;
                }
            };
            println!("{}", compiled_ir.fmt_with_symbols(&context.symbols()));
            let mut thread = Thread::new_root(Arc::clone(&context), Arc::new(compiled_ir.into()));
            loop {
                match thread.next() {
                    Some(Ok(())) => (),
                    Some(Err(err)) => {
                        eprintln!("{:?}", err);
                        break;
                    }
                    None => break,
                }
            }
        }
    }
}
