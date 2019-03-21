use rustyline::error::ReadlineError;
use rustyline::Editor;
use sapphire::compiler::parser::lex::Lexer;
use sapphire::compiler::parser::parse::parse;
use sapphire::compiler::{compile, compile_ir};
use sapphire::context::Context;
use sapphire::object::{Arguments, Object, SendError};
use sapphire::proc::Proc;
use sapphire::symbol::Symbol;
use sapphire::thread::Thread;
use sapphire::value::Value;
use std::io::{self, Read};
use std::mem;
use std::process::exit;
use std::sync::Arc;
use std::time::Instant;

fn main() {
    if atty::is(atty::Stream::Stdin) {
        let mut rl: Editor<()> = Editor::new();
        let mut ast = false;
        let mut ir = false;
        let mut byte = false;
        let mut exec = true;
        let mut time = false;

        let context = Arc::new(Context::new());
        let mut line_accum = String::new();
        let mut multiline_mode = false;

        loop {
            let mut prompt = String::new();
            if multiline_mode {
                prompt += "\x1b[38;5;248m.. \x1b[m";
            } else {
                prompt += "\x1b[38;5;248msapphire (";
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
            }

            match rl.readline(&prompt) {
                Ok(mut line) => {
                    if line == "." && !multiline_mode {
                        multiline_mode = true;
                        continue;
                    } else if line == "." {
                        multiline_mode = false;
                        line = mem::replace(&mut line_accum, String::new());
                    }

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
                            if multiline_mode {
                                line_accum += &line;
                                line_accum += "\n";
                                continue;
                            }

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
                                    Ok(mut res) => {
                                        let inspected = match res.send(
                                            Symbol::INSPECT,
                                            Arguments::empty(),
                                            &mut thread,
                                        ) {
                                            Ok(Value::String(inspected)) => inspected,
                                            _ => res.inspect(&context),
                                        };
                                        println!("-> \x1b[32m{}\x1b[m", inspected);
                                    }
                                    Err(err) => match err {
                                        SendError::Exception(exception) => {
                                            eprintln!(
                                                "\x1b[31mException: {}\x1b[m",
                                                exception.inspect(&context)
                                            );
                                        }
                                        SendError::Thread(err) => {
                                            eprintln!("\x1b[31;1mThread error: {:?}\x1b[m", err);
                                        }
                                    },
                                }
                                let end = Instant::now();

                                if time {
                                    eprintln!("took {:?}", end - start);
                                }
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
            },
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
