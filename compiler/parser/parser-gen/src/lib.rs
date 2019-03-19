//! Parser generator.
#![recursion_limit="128"]

extern crate proc_macro;

use petgraph::algo::kosaraju_scc;
use petgraph::graph::Graph;
use proc_macro::TokenStream as PTokenStream;
use proc_macro2::*;
use quote::quote_spanned;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap};
use std::hash::{Hash, Hasher};
use std::{fmt, iter, mem};

/// Generates nom-style parsers from the given symbols and takes care of left recursion.
///
/// # Example
/// ```ignore
/// enum Expression {
///     Add(Box<Expression>, Box<Expression>),
///     Identifier,
/// }
///
/// parser! {
///     expr: Expression = {
///         a: expr token!(Plus) b: expr => {
///             Expression::Add(Box::new(Expression::Identifier), Box::new(b))
///         },
///         identifier => (Expression::Identifier),
///     }
///
///     identifier: () = {
///         token!(Identifier) => ()
///     }
/// }
/// ```
///
/// The generated code will look something like this:
///
/// ```ignore
/// fn expr(i: Cursor) -> IResult<Expression> {
///     tail_rules!(i,
///         tail_accum_6258150132132877961 = do_parse!(
///             identifier >> (Expression::Identifier)
///         );
///         tail_rule!(
///             tail_accum_6258150132132877961 -> a >>
///             token!(Plus) >>
///             b: expr >> {
///                 Expression::Add(Box::new(Expression::Identifier), Box::new(b))
///             }
///         ),;
///         err: Expr
///     )
/// }
/// fn identifier (i: Cursor) -> IResult<()> {
///     call_macro!(i,
///         do_parse!(token!(Identifier) >> ())
///     )
/// }
/// ```
#[proc_macro]
pub fn parser(tokens: PTokenStream) -> PTokenStream {
    let tokens: TokenStream = tokens.into();
    let mut tokens = tokens.into_iter();

    // symbols in order of definition
    let mut symbols = BTreeMap::new();

    // a graph containing all symbols, for detecting left-recursion.
    // If the first item of a production rule points to another symbol, an edge will be created.
    let mut lr_graph = Graph::new();

    // symbol name (string) -> symbol node in lr_graph
    let mut symbol_nodes = HashMap::new();

    loop {
        match Symbol::parse(&mut tokens) {
            Ok(Some(symbol)) => {
                let node = lr_graph.add_node(1);
                symbol_nodes.insert(symbol.name.to_string(), node);
                symbols.insert(node, symbol);
            }
            Ok(None) => break,
            Err(e) => {
                // collect some tokens after because Span debug info is kinda useless
                let mut after = String::new();
                for _ in 0..8 {
                    if let Some(next) = tokens.next() {
                        after += &format!(" {}", next);
                    }
                }
                panic!("Failed to parse: {} (before{})", e, after)
            }
        }
    }

    let mut symbol_indices = HashMap::new();

    loop {
        // recreate edges every loop because they were probably changed
        lr_graph.clear_edges();

        // add edges for all rules with another symbol as the first item
        for (node, symbol) in &symbols {
            for rule in &symbol.rules {
                if let Some(PatternItem::Symbol(ident)) = rule.first_pattern_item() {
                    let target = match symbol_nodes.get(&ident.to_string()) {
                        Some(node) => *node,
                        None => panic!(
                            "symbol {} referenced in {} does not exist",
                            ident, symbol.name
                        ),
                    };
                    lr_graph.add_edge(*node, target, 1);
                }
            }
        }

        // left recursion can now be found by finding all cycles in the graph
        let loops = kosaraju_scc(&lr_graph);

        if symbol_indices.is_empty() {
            // removing left recursion requires the symbols to be ordered in some way.
            // Items with a high index will be modified the least, so the topological ordering
            // provided by kosaraju_scc is quite convenient to ensure minimal changes.
            let mut index = 0;
            for loop_items in &loops {
                for loop_item in loop_items {
                    symbol_indices.insert(*loop_item, index);
                    index += 1;
                }
            }
        }

        let mut did_change_something = false;

        // Symbols whose rules were “spilled” into another symbol may never have their function
        // called, but they’re still relevant to the code---so they’ll be marked with
        // #[allow(dead_code)]
        let mut mark_may_be_dead_code = Vec::new();

        for loop_items in loops {
            let remove_direct_recursion = if loop_items.len() == 1 {
                // check for self-recursion
                if lr_graph.contains_edge(loop_items[0], loop_items[0]) {
                    true
                } else {
                    // the loop contains only one item and it’s not recursive: there’s nothing
                    // to do here
                    continue;
                }
            } else {
                false
            };

            for node in loop_items {
                let node_index = symbol_indices.get(&node).unwrap();
                let symbol = symbols.get(&node).unwrap();
                let mut remove_direct_recursion = remove_direct_recursion;

                // remove indirect left-recursion
                let mut rules_to_replace = Vec::new();
                for (index, rule) in symbol.rules.iter().enumerate() {
                    if let Some(PatternItem::Symbol(ident)) = rule.first_pattern_item() {
                        let assigned_name = &rule.pattern[0].0;
                        let target = symbol_nodes.get(&ident.to_string()).unwrap();
                        let target_index = symbol_indices.get(target).unwrap();

                        if target_index > node_index {
                            // target index is higher than the node index:
                            // the target symbol’s rules will be “spilled” into this one

                            did_change_something = true;
                            remove_direct_recursion = true;
                            mark_may_be_dead_code.push(*target);

                            let mut spilled_rules = Vec::new();

                            let target_symbol = symbols.get(target).unwrap();
                            for target_rule in &target_symbol.rules {
                                let mut pattern = rule.pattern[1..].to_vec();
                                pattern.insert(
                                    0,
                                    (
                                        assigned_name.clone(),
                                        PatternItem::Rule(target_rule.clone()),
                                    ),
                                );

                                spilled_rules.push(Rule {
                                    pattern,
                                    handler: rule.handler.clone(),
                                });
                            }

                            rules_to_replace.push((index, spilled_rules));
                        }
                    }
                }

                let symbol = symbols.get_mut(&node).unwrap();

                let mut offset = 0;
                for (index, new_rules) in rules_to_replace {
                    symbol.rules.remove(index + offset);
                    for rule in new_rules.into_iter() {
                        symbol.rules.insert(index + offset, rule);
                        offset += 1;
                    }
                    offset -= 1;
                }

                // remove direct left-recursion
                if remove_direct_recursion {
                    // unlike the CFG left-recursion removal algorithm, no extra symbols need to
                    // be created here because of special treatment for “tail rules”

                    let mut tail_rules = Vec::new();
                    let mut self_rules = Vec::new();

                    for rule in symbol.rules.iter() {
                        if let Some(PatternItem::Symbol(ident)) = rule.first_pattern_item() {
                            let target = symbol_nodes.get(&ident.to_string()).unwrap();
                            if target == &node {
                                if rule.is_real_tail_rule() {
                                    tail_rules.push(rule.clone());
                                }
                            } else {
                                self_rules.push(rule.clone());
                            }
                        } else {
                            self_rules.push(rule.clone());
                        }
                    }

                    symbol.rules = self_rules;
                    symbol.tail_rules.append(&mut tail_rules);
                }
            }
        }

        for node in mark_may_be_dead_code {
            symbols.get_mut(&node).unwrap().may_be_dead_code = true;
        }

        if !did_change_something {
            break;
        }
    }

    let tokens: TokenStream = symbols
        .into_iter()
        .flat_map(|(_, s)| s.gen().into_iter())
        .collect();
    tokens.into()
}

fn to_camel_case(snake_case: &str) -> String {
    snake_case
        .split("_")
        .filter(|s| !s.is_empty())
        .map(|slice| {
            let mut chars = slice.chars();
            let first = chars.next().unwrap().to_uppercase();
            first.chain(chars).collect::<String>()
        })
        .collect()
}

/// A non-terminal symbol.
#[derive(Debug)]
struct Symbol {
    name: Ident,
    ty: TokenTree,
    rules: Vec<Rule>,

    /// The symbol’s “tail rules.”
    /// Instead of removing direct left-recursion by converting
    ///     A -> Aa | b
    /// into
    ///     A -> b[B]
    ///     B -> a[B]
    /// this takes advantage of the fact that Rust is not constrained by CFGs and creates
    ///     A -> b (a)*
    /// Here, `a` would be one such tail rule.
    ///
    /// A “tail accumulator” variable is used to facilitate bottom-up parsing.
    tail_rules: Vec<Rule>,
    may_be_dead_code: bool,
}

impl Symbol {
    fn parse<T: Iterator<Item = TokenTree>>(tokens: &mut T) -> Result<Option<Symbol>, String> {
        let name = match tokens.next() {
            Some(TokenTree::Ident(name)) => name,
            Some(token) => return Err(format!("Expected symbol name at {:?}", token.span())),
            None => return Ok(None),
        };
        match tokens.next() {
            Some(TokenTree::Punct(ref p))
                if p.as_char() == ':' && p.spacing() == Spacing::Alone =>
            {
                ()
            }
            Some(token) => return Err(format!("Expected colon at {:?}", token)),
            None => return Err(format!("Expected colon at EOF")),
        }
        let ty = match tokens.next() {
            Some(t) => t,
            None => return Err("Expected type (in symbol name: ty) at EOF".into()),
        };
        match tokens.next() {
            Some(TokenTree::Punct(ref p)) if p.as_char() == '=' => (),
            Some(token) => return Err(format!("Expected = at {:?}", token)),
            None => return Err(format!("Expected = at EOF")),
        }
        let mut contents = match tokens.next() {
            Some(TokenTree::Group(g)) => g.stream().into_iter(),
            Some(token) => return Err(format!("Expected group at {:?}", token)),
            None => return Err(format!("Expected group at EOF")),
        };
        let mut rules = Vec::new();
        loop {
            match Rule::parse(&mut contents)? {
                Some(rule) => rules.push(rule),
                None => break,
            }
        }
        Ok(Some(Symbol {
            name,
            ty,
            rules,
            tail_rules: Vec::new(),
            may_be_dead_code: false,
        }))
    }

    fn gen(self) -> TokenStream {
        let span = self.name.span();
        let name = self.name;
        let err_name = Ident::new(&to_camel_case(&name.to_string()), span);
        let ty = self.ty;
        let tail_rules = self.tail_rules;

        if self.rules.is_empty() {
            panic!("{} has no production rules", name);
        }

        let mut s = DefaultHasher::new();
        name.hash(&mut s);
        let hash = s.finish();
        let accum = Ident::new(&format!("tail_accum_{}", hash), span);

        let has_tail_rules = !tail_rules.is_empty();
        let tail_rules: TokenStream = tail_rules
            .iter()
            .flat_map(|rule| {
                rule.gen_tail_rule(&accum, &err_name)
                    .into_iter()
                    .chain(iter::once(TokenTree::Punct(Punct::new(
                        ',',
                        Spacing::Alone,
                    ))))
            })
            .collect();

        let contents = if self.rules.len() == 1 {
            let contents = self.rules[0].gen(&err_name);

            if has_tail_rules {
                quote_spanned! { span =>
                    tail_rules!(i, #accum = #contents; #tail_rules; err: #err_name)
                }
            } else {
                quote_spanned! { span =>
                    call_macro!(i, #contents)
                }
            }
        } else {
            let contents: TokenStream = self
                .rules
                .into_iter()
                .flat_map(|rule| {
                    rule.gen(&err_name)
                        .into_iter()
                        .chain(iter::once(TokenTree::Punct(Punct::new(
                            ',',
                            Spacing::Alone,
                        ))))
                })
                .collect();

            if has_tail_rules {
                quote_spanned! { span =>
                    tail_rules!(i, #accum = alt!(
                        #contents
                        err: #err_name
                    ); #tail_rules; err: #err_name)
                }
            } else {
                quote_spanned! { span =>
                    alt!(
                        i,
                        #contents
                        err: #err_name
                    )
                }
            }
        };

        let attrs = if self.may_be_dead_code {
            quote_spanned! { span => #[allow(dead_code)] }
        } else {
            quote_spanned!(span =>)
        };

        let tokens = quote_spanned! { span =>
            #attrs
            fn #name(i: Cursor) -> IResult<Spanned<#ty>> {
                if let Some(err) = i.cached_err(stringify!(#name)) {
                    return Err(err);
                }
                let start_pos = i.pos();

                let res: IResult<#ty> = { #contents };

                if let Err(err) = &res {
                    i.cache_err(stringify!(#name), err.clone());
                }

                let end_pos = i.pos();
                let span = Span(start_pos, end_pos);

                res.map(|(cursor, node)| (cursor, Spanned(node, span)))
            }
        };
        tokens.into()
    }
}

#[derive(Debug, Clone)]
enum PatternItem {
    Macro(Ident, Group),
    Symbol(Ident),
    Rule(Rule),
}

impl PatternItem {
    fn gen(&self, err_name: &Ident) -> TokenStream {
        match self {
            PatternItem::Macro(ident, group) => quote_spanned! { ident.span() =>
                #ident!#group
            },
            PatternItem::Symbol(ident) => quote_spanned! { ident.span() =>
                #ident
            },
            PatternItem::Rule(rule) => rule.gen(err_name),
        }
    }
}

impl fmt::Display for PatternItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PatternItem::Macro(i, g) => write!(f, "{}!{}", i, g),
            PatternItem::Symbol(i) => write!(f, "{}", i),
            PatternItem::Rule(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone)]
struct Rule {
    pattern: Vec<(Option<Ident>, PatternItem)>,
    handler: TokenTree,
}

impl Rule {
    fn first_pattern_item(&self) -> Option<&PatternItem> {
        if let Some((_, first_item)) = self.pattern.get(0) {
            let mut first_item = first_item;
            while let PatternItem::Rule(inner) = first_item {
                match inner.pattern.get(0) {
                    Some(inner) => first_item = &inner.1,
                    None => return None,
                }
            }
            Some(first_item)
        } else {
            None
        }
    }

    fn parse<T: Iterator<Item = TokenTree>>(tokens: &mut T) -> Result<Option<Rule>, String> {
        let mut items: Vec<_> = tokens
            .take_while(|token| match token {
                TokenTree::Punct(ref p) if p.as_char() == ',' => false,
                _ => true,
            })
            .map(|i| Some(i))
            .collect();

        if items.is_empty() {
            return Ok(None);
        }

        if items.len() == 1 {
            // allow single symbol identifiers as a shorthand for `a: symbol => a`
            if let Some(Some(TokenTree::Ident(ident))) = items.get(0) {
                return Ok(Some(Rule {
                    pattern: vec![(
                        Some(Ident::new("a", ident.span())),
                        PatternItem::Symbol(ident.clone()),
                    )],
                    handler: TokenTree::Group(Group::new(
                        Delimiter::Brace,
                        quote_spanned! { ident.span() => a },
                    )),
                }));
            }
        }

        let handler = items.pop().unwrap().unwrap();

        // =>
        match items.pop().map(|i| i.unwrap()) {
            Some(TokenTree::Punct(ref p))
                if p.as_char() == '>' && p.spacing() == Spacing::Alone =>
            {
                ()
            }
            token => {
                return Err(format!(
                    "Expected => at {:?}",
                    token.unwrap_or(handler).span()
                ));
            }
        }
        match items.pop().map(|i| i.unwrap()) {
            Some(TokenTree::Punct(ref p))
                if p.as_char() == '=' && p.spacing() == Spacing::Joint =>
            {
                ()
            }
            token => {
                return Err(format!(
                    "Expected => at {:?}",
                    token.unwrap_or(handler).span()
                ));
            }
        }

        let mut pattern = Vec::new();
        let mut i = 0;

        macro_rules! next {
            () => {{
                i += 1;
                items.get(i - 1).map_or(None, |i| i.as_ref())
            }};
            (take) => {{
                i += 1;
                items
                    .get_mut(i - 1)
                    .map(|item| mem::replace(item, None).unwrap())
            }};
            (name) => {{
                let saved_pos = i;
                let mut matched = None;
                if let Some(TokenTree::Ident(_)) = next!() {
                    if let Some(TokenTree::Punct(p)) = next!() {
                        if p.as_char() == ':' && p.spacing() == Spacing::Alone {
                            i = saved_pos;
                            let ident = match next!(take) {
                                Some(TokenTree::Ident(i)) => i,
                                _ => unreachable!(),
                            };
                            next!(take);
                            matched = Some(ident);
                        }
                    }
                }
                if let None = matched {
                    i = saved_pos;
                }
                matched
            }};
            (call) => {{
                let saved_pos = i;
                let mut matched = None;
                if let Some(TokenTree::Ident(_)) = next!() {
                    if let Some(TokenTree::Punct(p)) = next!() {
                        if p.as_char() == '!' && p.spacing() == Spacing::Alone {
                            if let Some(TokenTree::Group(args)) = next!() {
                                if args.delimiter() == Delimiter::Parenthesis {
                                    i = saved_pos;
                                    let ident = match next!(take) {
                                        Some(TokenTree::Ident(i)) => i,
                                        _ => unreachable!(),
                                    };
                                    next!(take);
                                    let args = match next!(take) {
                                        Some(TokenTree::Group(g)) => g,
                                        _ => unreachable!(),
                                    };
                                    matched = Some((ident, args));
                                }
                            }
                        }
                    }
                }
                if let None = matched {
                    i = saved_pos;
                }
                matched
            }};
        }

        loop {
            if i >= items.len() {
                break;
            }

            let name = next!(name);
            let item = match next!(call) {
                Some((name, args)) => PatternItem::Macro(name, args),
                None => match next!(take) {
                    Some(TokenTree::Ident(ident)) => PatternItem::Symbol(ident),
                    Some(TokenTree::Group(group)) => {
                        match Rule::parse(&mut group.stream().into_iter())? {
                            Some(rule) => PatternItem::Rule(rule),
                            None => {
                                return Err(format!("Empty group in pattern at {:?}", group.span()));
                            }
                        }
                    }
                    Some(token) => {
                        return Err(format!(
                            "Expected identifier, macro call, or group at {:?}",
                            token.span()
                        ));
                    }
                    None => {
                        return Err(format!(
                            "Expected pattern item in rule at {:?}",
                            handler.span()
                        ));
                    }
                },
            };

            pattern.push((name, item));
        }

        Ok(Some(Rule { pattern, handler }))
    }

    fn gen_pattern(pattern: &[(Option<Ident>, PatternItem)], err_name: &Ident) -> TokenStream {
        pattern
            .iter()
            .map(|(name, item)| {
                let item = item.gen(err_name);
                match name {
                    Some(name) => quote_spanned!(name.span() => #name: #item >>),
                    None => quote_spanned!(err_name.span() => #item >>),
                }
            })
            .collect()
    }

    fn gen_pattern_with_first_replaced(
        pattern: &[(Option<Ident>, PatternItem)],
        accum: &Ident,
        err_name: &Ident,
    ) -> TokenStream {
        let mut is_first = true;
        pattern
            .iter()
            .map(|(name, item)| {
                if is_first {
                    is_first = false;

                    match name {
                        Some(name) => match item {
                            PatternItem::Rule(r) => {
                                let r = r.gen_tail_rule(accum, err_name);
                                quote_spanned!(err_name.span() => #name: #r >>)
                            }
                            _ => quote_spanned!(name.span() => #accum -> #name >>),
                        },
                        None => panic!("Unnamed left recursion in {} at {}", err_name, item),
                    }
                } else {
                    let item = item.gen(err_name);
                    match name {
                        Some(name) => quote_spanned!(name.span() => #name: #item >>),
                        None => quote_spanned!(err_name.span() => #item >>),
                    }
                }
            })
            .collect()
    }

    fn gen_tail_rule(&self, accum: &Ident, err_name: &Ident) -> TokenStream {
        let pat = &Self::gen_pattern_with_first_replaced(&self.pattern, accum, err_name);
        let handler = &self.handler;

        quote_spanned! { self.handler.span() =>
            tail_rule!(#pat #handler)
        }
    }

    fn gen(&self, err_name: &Ident) -> TokenStream {
        let pat = Self::gen_pattern(&self.pattern, err_name);
        let handler = &self.handler;

        quote_spanned! { self.handler.span() =>
            do_parse!(#pat #handler)
        }
    }

    /// Returns true if this is a real tail rule instead of just a `A -> A` rule with no actual
    /// tail.
    fn is_real_tail_rule(&self) -> bool {
        if self.pattern.len() > 1 {
            return true;
        }

        match self.pattern.get(0) {
            Some((_, PatternItem::Rule(rule))) => rule.is_real_tail_rule(),
            _ => false,
        }
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (name, item) in &self.pattern {
            if let Some(name) = name {
                write!(f, "{}: ", name)?;
            }
            write!(f, "{} ", item)?;
        }
        write!(f, "=> {})", self.handler)
    }
}
