use rustyline::error::ReadlineError;
use rustyline::Editor;
use sapphire::compiler::compile_ir;
use sapphire::context::Context;
use sapphire::object::{Arguments, Object};
use sapphire::parser::lex::Lexer;
use sapphire::parser::parse::parse;
use sapphire::thread::Thread;
use sapphire::value::Value;
use std::sync::Arc;
use std::time::Instant;

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
                            print_enabled_disabled(ir, "AST");
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

                            let proc = match compile_ir("main", line, &mut context.symbols_mut()) {
                                Ok(ir) => ir,
                                Err(err) => {
                                    eprintln!("{}", err.fmt_ansi());
                                    continue;
                                }
                            };

                            if ir {
                                eprintln!("{}", proc.fmt_with_symbols(&context.symbols()));
                            }

                            let proc = proc.into();

                            if byte {
                                eprintln!("{:?}", proc);
                            }

                            if exec {
                                let mut thread = Thread::new(Arc::clone(&context));
                                let start = Instant::now();
                                match thread.call(
                                    Value::Ref(context.root().clone()),
                                    Arc::new(proc),
                                    Arguments::empty(),
                                ) {
                                    Ok(res) => {
                                        println!("-> \x1b[32m{}\x1b[m", res.inspect(&context))
                                    }
                                    Err(err) => eprintln!("Thread error: {:?}", err),
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
        unimplemented!("read from stdin and execute")
    }
}

fn print_enabled_disabled(state: bool, name: &'static str) {
    if state {
        eprintln!("{} enabled", name);
    } else {
        eprintln!("{} disabled", name);
    }
}
