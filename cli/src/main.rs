use std::process::exit;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use sapphire::compiler::{compile, compile_ir};
use sapphire::compiler::parser::lex::Lexer;
use sapphire::compiler::parser::parse::parse;
use sapphire::context::Context;
use sapphire::object::{Arguments, Object, SendError};
use sapphire::proc::Proc;
use sapphire::thread::Thread;
use sapphire::value::Value;
use std::sync::Arc;
use std::time::Instant;
use std::io::{self, Read};

fn main() {
    if atty::is(atty::Stream::Stdin) {
        let mut rl: Editor<()> = Editor::new();
        let mut ast = false;
        let mut ir = false;
        let mut byte = false;
        let mut exec = true;
        let mut time = true;

        let context = Arc::new(Context::new());

        loop {
            let mut prompt = String::from("\x1b[38;5;248msapphire (");
            if ast {
                prompt.push('a');
            }
            if ir {
                prompt.push('i');
            }
            if byte {
                prompt.push('b');
            }
            if exec {
                prompt.push('e');
            }
            if time {
                prompt.push('t');
            }
            prompt.push_str(")> \x1b[m");

            match rl.readline(&prompt) {
                Ok(line) => {
                    rl.add_history_entry(line.as_ref());

                    match &*line {
                        ".ast" => {
                            ast = !ast;
                            print_enabled_disabled(ast, "AST");
                        }
                        ".ir" => {
                            ir = !ir;
                            print_enabled_disabled(ir, "IR");
                        }
                        ".byte" => {
                            byte = !byte;
                            print_enabled_disabled(byte, "Bytecode");
                        }
                        ".exec" => {
                            exec = !exec;
                            print_enabled_disabled(exec, "Execution");
                        }
                        ".time" => {
                            time = !time;
                            print_enabled_disabled(time, "Time");
                        }
                        _ => {
                            if ast {
                                let tokens: Vec<_> = Lexer::new(&line).collect();
                                match parse(&tokens) {
                                    Ok(ast) => eprintln!("{:#?}", ast),
                                    Err(_) => (), // will be displayed below
                                }
                            }

                            let proc = match compile_ir("main", line, &mut *context.symbols_mut()) {
                                Ok(ir) => ir,
                                Err(err) => {
                                    eprintln!("{}", err.fmt_ansi());
                                    continue;
                                }
                            };

                            if ir {
                                eprintln!("{}", proc.fmt_with_symbols(&context.symbols()));
                            }

                            let proc = Proc::Sapphire(Arc::new(proc.into()));

                            if byte {
                                eprintln!("{:?}", proc);
                            }

                            if exec {
                                let mut thread = Thread::new(Arc::clone(&context));
                                let start = Instant::now();
                                match thread.call(
                                    Value::Ref(context.root().clone()),
                                    proc,
                                    Arguments::empty(),
                                ) {
                                    Ok(res) => {
                                        println!("-> \x1b[32m{}\x1b[m", res.inspect(&context))
                                    }
                                    Err(err) => match err {
                                        SendError::Exception(exception) => {
                                            eprintln!("Exception: {}", exception.inspect(&context));
                                        }
                                        SendError::Thread(err) => {
                                            eprintln!("Thread error: {:?}", err);
                                        }
                                    },
                                }
                                let end = Instant::now();

                                eprintln!("took {:?}", end - start);
                            }
                        }
                    }
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
                Err(err) => panic!("{}", err),
            }
        }
    } else {
        let mut code = String::new();
        io::stdin().read_to_string(&mut code).unwrap();

        let context = Arc::new(Context::new());
        let proc = match compile("main", code, &mut *context.symbols_mut()) {
            Ok(proc) => Proc::Sapphire(Arc::new(proc)),
            Err(err) => {
                eprintln!("Failed to compile:\n{}", err.fmt_ansi());
                exit(1);
            }
        };
        let mut thread = Thread::new(Arc::clone(&context));
        match thread.call(Value::Ref(context.root().clone()), proc, Arguments::empty()) {
            Ok(_) => (),
            Err(err) => match err {
                SendError::Exception(exception) => {
                    eprintln!("Exception: {}", exception.inspect(&context));
                }
                SendError::Thread(err) => {
                    eprintln!("Thread error: {:?}", err);
                }
            }
        }
    }
}

fn print_enabled_disabled(state: bool, name: &'static str) {
    if state {
        eprintln!("{} enabled", name);
    } else {
        eprintln!("{} disabled", name);
    }
}
