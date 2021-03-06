extern crate proc_macro;
use proc_macro::TokenStream as PTokenStream;
use proc_macro2::{Literal, Punct, Spacing, TokenStream, TokenTree};
use quote::quote;
use quote::TokenStreamExt;
use sapphire_compiler::{AddressingMode, Param, Params, Proc, Static, SymbolTable};
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use syn::parse::{Parse, ParseStream};
use syn::*;

struct Input {
    out: Ident,
    code: LitStr,
    _comma: Token![,],
    arg: Result<Ident>,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        Ok(Input {
            out: input.parse()?,
            _comma: input.parse()?,
            arg: input.parse(),
            code: input.parse()?,
        })
    }
}

#[derive(Debug)]
struct Symbols {
    symbols: HashMap<String, usize>,
    sym_counter: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Symbol(usize);

impl SymbolTable for Symbols {
    type Symbol = Symbol;
    fn symbol(&mut self, name: &str) -> Symbol {
        let mut sym_counter = self.sym_counter;
        let value = self.symbols.entry(name.to_string()).or_insert_with(|| {
            sym_counter += 1;
            sym_counter - 1
        });
        self.sym_counter = sym_counter;
        Symbol(*value)
    }
    fn symbol_name(&self, symbol: Symbol) -> Option<&str> {
        self.symbols
            .iter()
            .find(|(_, sym)| **sym == symbol.0)
            .map(|(name, _)| &**name)
    }
}

/// Use `sapphire::compile!` instead.
#[proc_macro]
pub fn compile(tokens: PTokenStream) -> PTokenStream {
    let input = parse_macro_input!(tokens as Input);

    let out = input.out;

    let file = match input.arg {
        Ok(ident) => match &*format!("{}", ident) {
            "file" => true,
            arg => panic!("invalid argument {}", arg),
        },
        Err(_) => false,
    };

    let mut include_str = None;

    let code = if file {
        let cwd = env::var("CARGO_MANIFEST_DIR").unwrap_or(".".into());
        let path = Path::new(&cwd).join(input.code.value());
        let mut buf = String::new();
        let path_str = path.to_string_lossy();
        include_str = Some(quote! {
            include_str!(#path_str)
        });
        File::open(path.clone())
            .and_then(|mut file| file.read_to_string(&mut buf))
            .expect(&format!("Failed to read file {:?}", path));
        buf
    } else {
        input.code.value()
    };

    if !code.starts_with("def") {
        panic!("code must be a method definition");
    }

    let mut symbols = Symbols {
        symbols: HashMap::new(),
        sym_counter: 0,
    };

    let super_proc: Proc<Symbols, ()> = match sapphire_compiler::compile("_", code, &mut symbols) {
        Ok(proc) => proc,
        Err(err) => panic!("Failed to compile:\n{}", err),
    };

    let proc = super_proc
        .statics
        .into_iter()
        .find(|item| match item {
            Static::Proc(_) => true,
            _ => false,
        })
        .map(|item| match item {
            Static::Proc(proc) => proc.clone_with_parents(Vec::new()),
            _ => unreachable!(),
        })
        .expect("expected super proc to contain a proc");

    let symbols = quote_symbols(symbols);
    let proc = quote_proc(proc, symbols);
    let include_str = match include_str {
        Some(include_str) => quote! { const _INCLUDE_STR: &str = #include_str; },
        None => quote! {},
    };
    let out = quote! {
        #include_str
        pub const #out: sapphire_compiler::ConstProc = #proc;
    };
    out.into()
}

fn quote_symbols(symbols: Symbols) -> TokenStream {
    let mut out = TokenStream::new();
    for (v, k) in symbols.symbols {
        let k_lit = Literal::usize_suffixed(k);
        let v_lit = Literal::string(&v);
        out.append_all(quote! {
            (#k_lit, #v_lit),
        });
    }
    quote! { &[ #out ] }
}

fn quote_proc(proc: Proc<Symbols, ()>, symbols: TokenStream) -> TokenStream {
    let name = Literal::usize_suffixed(proc.name.0);
    let registers = Literal::usize_suffixed(proc.registers);
    let block_idx = match proc.block_idx {
        Some(idx) => {
            let idx = Literal::usize_suffixed(idx);
            quote! { Some(#idx) }
        }
        None => quote! { None },
    };
    let mode = match proc.mode {
        AddressingMode::U8 => quote! { sapphire_compiler::AddressingMode::U8 },
        AddressingMode::U16 => quote! { sapphire_compiler::AddressingMode::U16 },
    };
    let statics = quote_statics(proc.statics);
    let code = quote_code(proc.code);
    let params = quote_params(proc.params);
    quote! {
        sapphire_compiler::ConstProc {
            symbols: #symbols,
            name: #name,
            registers: #registers,
            block_idx: #block_idx,
            statics: #statics,
            mode: #mode,
            code: #code,
            params: #params,
        }
    }
}

fn quote_statics(statics: Vec<Static<Symbols, ()>>) -> TokenStream {
    let mut out = TokenStream::new();
    for item in statics {
        match item {
            Static::Int(value) => {
                let lit = Literal::i64_suffixed(value);
                out.append_all(quote! { sapphire_compiler::ConstStatic::Int(#lit) });
            }
            Static::Float(value) => {
                let lit = Literal::f64_suffixed(value);
                out.append_all(quote! { sapphire_compiler::ConstStatic::Float(#lit) });
            }
            Static::Str(value) => {
                let lit = Literal::string(&value);
                out.append_all(quote! { sapphire_compiler::ConstStatic::Str(#lit) });
            }
            Static::Sym(value) => {
                let lit = Literal::usize_suffixed(value.0);
                out.append_all(quote! { sapphire_compiler::ConstStatic::Sym(#lit) });
            }
            Static::Proc(proc) => {
                let lit = quote_proc(proc.clone_with_parents(Vec::new()), quote! { &[] });
                out.append_all(quote! { sapphire_compiler::ConstStatic::Proc(#lit) });
            }
        }
        out.append(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
    }
    quote! { &[ #out ] }
}

fn quote_code(code: Vec<u8>) -> TokenStream {
    let mut out = TokenStream::new();

    for byte in code {
        out.append(Literal::u8_suffixed(byte));
        out.append(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
    }

    quote! { &[ #out ] }
}

fn quote_params(params: Params<Symbols>) -> TokenStream {
    let mut items = TokenStream::new();
    for param in params.params {
        items.append_all(quote_param(param));
        items.append(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
    }
    let block = Literal::u16_suffixed(params.block);
    quote! {
        sapphire_compiler::ConstParams {
            params: &[ #items ],
            block: #block,
        }
    }
}

fn quote_param(param: Param<Symbol>) -> TokenStream {
    match param {
        Param::Mandatory(i) => {
            let lit = Literal::u16_suffixed(i);
            quote! { sapphire_compiler::ConstParam::Mandatory(#lit) }
        }
        Param::Optional(i) => {
            let lit = Literal::u16_suffixed(i);
            quote! { sapphire_compiler::ConstParam::Optional(#lit) }
        }
        Param::Splat(i) => {
            let lit = Literal::u16_suffixed(i);
            quote! { sapphire_compiler::ConstParam::Splat(#lit) }
        }
        Param::Hash(items) => {
            let mut out = TokenStream::new();
            for (k, r, m) in items {
                let k_lit = Literal::usize_suffixed(k.0);
                let r_lit = Literal::u16_suffixed(r);
                let m_lit = match m {
                    true => quote! { true },
                    false => quote! { false },
                };
                out.append_all(quote! { (#k_lit, #r_lit, #m_lit) });
            }
            quote! { sapphire_compiler::ConstParam::Hash(&[ #out ]) }
        }
    }
}
