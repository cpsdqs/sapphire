use sapphire::compiler::compile_ir;
use sapphire::symbol::Symbols;
use std::io::{self, Read};
use std::process::exit;

fn main() {
    let mut input = String::new();
    match io::stdin().read_to_string(&mut input) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Failed to read standard input:\n{}", err);
            exit(1);
        }
    }
    let mut symbols = Symbols::new();
    let compiled_ir = match compile_ir("stdin", input, &mut symbols) {
        Ok(compiled) => compiled,
        Err(err) => {
            eprintln!("Failed to compile:\n{}", err.fmt_ansi());
            exit(1);
        }
    };
    unimplemented!("run {}", compiled_ir.fmt_with_symbols(&symbols));
}
