//! Intermediate representation.

use crate::symbol::{Symbol, Symbols};
use fnv::{FnvHashMap, FnvHashSet};
use sapphire_parser::ast::*;
use std::collections::BTreeMap;
use std::{fmt, iter, mem};

/// A register variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Var {
    /// A local variable from the parent (e.g. in a block), with the depth of the parent
    /// (usually 1).
    Parent(usize, usize),
    /// A local variable in the current proc.
    Local(usize),
    /// A special white hole variable that will always contain nil.
    Nil,
    /// A special black hole variable into which unused output is directed.
    Void,
    /// A reference to the current self.
    SelfRef,
}

impl Var {
    /// Returns true if the variable is from the parent proc.
    fn is_from_parent(&self) -> bool {
        match self {
            Var::Parent(_, _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Var::Parent(i, depth) => write!(f, "{}var_{}", "^".repeat(*depth), i),
            Var::Local(i) => write!(f, "var_{}", i),
            Var::Nil => write!(f, "nil"),
            Var::Void => write!(f, "void"),
            Var::SelfRef => write!(f, "self"),
        }
    }
}

/// A jump label.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

/// A local variable scope (and some extras).
///
/// Scopes may have a parent scope, in which case variables from the parent scope are accessible.
/// This should only be used for blocks.
struct Scope<'a> {
    /// The parent scope. If this is None, symbols must be Some.
    parent: Option<&'a mut Scope<'a>>,
    /// Mapping from [Var]s to local variable names.
    variables: &'a mut FnvHashMap<Var, Symbol>,
    /// List of variables that may be captured in a block.
    captured_vars: &'a mut FnvHashSet<Var>,
    /// The symbols table. If this is None, parent must be Some.
    symbols: Option<&'a mut Symbols>,
    /// Flag that marks the existence of a child proc that can capture variables.
    may_be_captured: &'a mut bool,
    /// Variable counter for registers.
    var_counter: usize,
    /// If true, no variables will be created in the current scope. They will instead be created
    /// in the parent scope.
    no_local_vars: bool,
}

impl<'a> Scope<'a> {
    /// Creates a sub-scope with this scope as the parent.
    ///
    /// Safety: 'b must be shorter than 'a
    unsafe fn sub_scope<'b>(
        &mut self,
        variables: &'b mut FnvHashMap<Var, Symbol>,
        captured_vars: &'b mut FnvHashSet<Var>,
        may_be_captured: &'b mut bool,
    ) -> Scope<'b> {
        let borrowck_go_away = mem::transmute::<&mut Scope, &mut Scope>(self);
        Scope {
            parent: Some(borrowck_go_away),
            variables,
            captured_vars,
            symbols: None,
            may_be_captured,
            var_counter: 0,
            no_local_vars: false,
        }
    }

    /// Mark this scope as possibly being captured.
    fn may_be_captured(&mut self) {
        *self.may_be_captured = true;
    }

    /// Obtains a symbol by name.
    fn symbol(&mut self, symbol: &str) -> Symbol {
        if let Some(symbols) = &mut self.symbols {
            symbols.symbol(symbol)
        } else {
            self.parent.as_mut().unwrap().symbol(symbol)
        }
    }

    /// Obtains the symbol table.
    fn symbols(&mut self) -> &mut Symbols {
        if let Some(symbols) = &mut self.symbols {
            symbols
        } else {
            self.parent.as_mut().unwrap().symbols()
        }
    }

    /// Returns a new register variable.
    fn next_var(&mut self) -> Var {
        self.var_counter += 1;
        Var::Local(self.var_counter)
    }

    /// Returns a new label.
    fn next_label(&mut self) -> Label {
        self.var_counter += 1;
        Label(self.var_counter)
    }

    /// Defines a local variable.
    fn define_local_var(&mut self, name: Symbol) -> Var {
        if self.no_local_vars {
            self.parent.as_mut().unwrap().define_local_var(name);
            return self.parent.as_mut().unwrap().capture_var(name).unwrap();
        }

        if let Some(var) = self.local_var(name) {
            var
        } else {
            let var = self.next_var();
            self.variables.insert(var, name);
            var
        }
    }

    /// Should only be called by a child scope: captures a local variable.
    fn capture_var(&mut self, name: Symbol) -> Option<Var> {
        match self.local_var(name) {
            Some(Var::Parent(i, depth)) => Some(Var::Parent(i, depth + 1)),
            Some(Var::Local(i)) => {
                self.captured_vars.insert(Var::Local(i));
                Some(Var::Parent(i, 1))
            }
            v => v,
        }
    }

    /// Returns the local variable with the given name.
    fn local_var(&mut self, name: Symbol) -> Option<Var> {
        let var = self
            .variables
            .iter()
            .find(|(_, v)| **v == name)
            .map(|(k, _)| *k);

        match var {
            Some(var) => Some(var),
            None => self
                .parent
                .as_mut()
                .map_or(None, |parent| parent.capture_var(name)),
        }
    }
}

/// Errors that may occur when creating the IR.
/// Most of these are likely symptoms of an invalid AST.
#[derive(Debug)]
pub enum IRError {
    InvalidConstPath(Ident),
    InvalidMethodName(Ident),
    InvalidModuleName(Ident),
    InvalidLeftHandSide(Ident),
    InvalidMember(Ident),
    InvalidDefined,
    TooManySplats,
}

// TODO: spans?

impl fmt::Display for IRError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IRError::InvalidConstPath(ident) => write!(f, "invalid const path “{}”", ident),
            IRError::InvalidMethodName(ident) => write!(f, "invalid method name “{}”", ident),
            IRError::InvalidModuleName(ident) => write!(f, "invalid module name “{}”", ident),
            IRError::InvalidLeftHandSide(ident) => {
                write!(f, "invalid left-hand side “{}”", ident)
            }
            IRError::InvalidMember(ident) => write!(f, "invalid member “{}”", ident),
            IRError::InvalidDefined => write!(f, "invalid defined? (somewhere…)"),
            IRError::TooManySplats => write!(f, "too many splats (somewhere…)"),
        }
    }
}

/// IR operations.
#[derive(Debug)]
pub enum IROp {
    /// Loads the root (::) into the local variable.
    LoadRoot(Var),
    /// Loads a boolean value into the local variable.
    LoadBool(Var, bool),
    /// Loads a reference to a global variable into the local variable.
    LoadGlobal(Var, Symbol),
    /// Loads a reference to a constant into the local variable.
    LoadConst(Var, Var, Symbol),
    /// Loads a reference to a class variable into the local variable.
    LoadClassVar(Var, Symbol),
    /// Loads a reference to an instance variable into the local variable.
    LoadIVar(Var, Symbol),
    /// Loads a literal string into the local variable.
    LoadString(Var, String),
    /// Appends the second string to the first.
    AppendString(Var, Var),
    /// Loads a literal symbol into the local variable.
    LoadSymbol(Var, Symbol),
    /// Loads an i64 number into the local variable.
    LoadI64(Var, i64),
    /// Loads a float number into the local variable.
    LoadFloat(Var, f64),
    /// Loads a proc into the local variable.
    LoadProc(Var, IRProc),
    /// Pushes a positional argument.
    Arg(Var),
    /// Pushes an associative argument.
    ArgAssoc(Var, Var),
    /// Pushes a splat argument.
    ArgSplat(Var),
    /// Pushes a block argument.
    ArgBlock(Var),
    /// Calls a method and loads the result into a local variable.
    Call(Var, Var, Symbol),
    /// Calls super and loads the result into a local variable.
    Super(Var),
    /// Performs the not operation on the second argument and loads it into the first argument.
    Not(Var, Var),
    /// Label marker.
    Label(Label),
    /// Jumps to a label.
    Jump(Label),
    /// Jumps to a label if the variable is truthy.
    JumpIf(Var, Label),
    /// Jumps to a label if the variable is falsy.
    JumpIfNot(Var, Label),
    /// Returns with a value.
    Return(Var),
    /// Assigns the value of the second to the first.
    Assign(Var, Var),
    /// Assigns the value of the second to the first.
    AssignGlobal(Symbol, Var),
    /// Assigns the value of the second to the first.
    AssignConst(Symbol, Var),
    /// Assigns the value of the second to the first.
    AssignClassVar(Symbol, Var),
    /// Assigns the value of the second to the first.
    AssignIVar(Symbol, Var),
    /// Start of a rescuable section.
    BeginRescue(Label),
    /// Jumps to the label if the current exception matches.
    RescueMatch(Var, Label),
    /// Binds the exception to a variable.
    RescueBind(Var),
    /// End of a rescuable section.
    EndRescue,
    /// `defined?` for a constant variable.
    DefinedConst(Var, Symbol),
    /// `defined?` for a global variable.
    DefinedGlobal(Var, Symbol),
    /// `defined?` for a class variable.
    DefinedClassVar(Var, Symbol),
    /// `defined?` for an instance variable.
    DefinedIVar(Var, Symbol),
    /// `yield`
    Yield(Var),
    /// Defines a module.
    DefModule(Var, Symbol, IRProc),
    /// Defines a class.
    DefClass(Var, Symbol, Option<Var>, IRProc),
    /// Defines a method.
    // TODO: params?
    DefMethod(Symbol, IRProc),
    /// Defines a singleton class.
    DefSingletonClass(Var, IRProc),
    /// Defines a singleton method.
    // TODO: params?
    DefSingletonMethod(Var, Symbol, IRProc),
}

impl IROp {
    /// Iterates over all variables referenced in the IROp. If they are being written to, the write
    /// flag will be true.
    pub(super) fn for_each_var<F: FnMut(&mut Var, bool)>(&mut self, mut cb: F) {
        use IROp::*;
        match self {
            LoadRoot(var)
            | LoadBool(var, _)
            | LoadGlobal(var, _)
            | LoadClassVar(var, _)
            | LoadIVar(var, _)
            | LoadString(var, _)
            | LoadSymbol(var, _)
            | LoadI64(var, _)
            | LoadFloat(var, _)
            | LoadProc(var, _)
            | Super(var)
            | RescueBind(var)
            | Yield(var) => cb(var, true),
            LoadConst(out, inp, _)
            | AppendString(out, inp)
            | Call(out, inp, _)
            | Not(out, inp)
            | Assign(out, inp) => {
                cb(inp, false);
                cb(out, true);
            }
            Arg(var)
            | ArgSplat(var)
            | ArgBlock(var)
            | JumpIf(var, _)
            | JumpIfNot(var, _)
            | Return(var)
            | AssignGlobal(_, var)
            | AssignConst(_, var)
            | AssignClassVar(_, var)
            | AssignIVar(_, var)
            | RescueMatch(var, _)
            | DefinedConst(var, _)
            | DefinedGlobal(var, _)
            | DefinedClassVar(var, _)
            | DefinedIVar(var, _)
            | DefModule(var, _, _)
            | DefSingletonClass(var, _)
            | DefSingletonMethod(var, _, _) => cb(var, false),
            ArgAssoc(var, var2) => {
                cb(var, false);
                cb(var2, false);
            }
            Label(_) | Jump(_) | BeginRescue(_) | EndRescue | DefMethod(_, _) => (),
            DefClass(var, _, var2, _) => {
                cb(var, false);
                if let Some(var) = var2 {
                    cb(var, false);
                };
            }
        }
    }

    /// Returns true if the operation is useless, e.g. if it’s loading an i64 into [Var::Void].
    fn is_useless(&self) -> bool {
        use IROp::*;
        match self {
            LoadRoot(out)
            | LoadBool(out, _)
            | LoadGlobal(out, _)
            | LoadConst(out, _, _)
            | LoadClassVar(out, _)
            | LoadIVar(out, _)
            | LoadString(out, _)
            | LoadSymbol(out, _)
            | LoadI64(out, _)
            | LoadFloat(out, _)
            | LoadProc(out, _)
            | RescueBind(out)
            | DefinedConst(out, _)
            | DefinedGlobal(out, _)
            | DefinedClassVar(out, _)
            | DefinedIVar(out, _)
            | Not(out, _) => *out == Var::Void,
            Assign(out, inp) => *out == Var::Void || *out == *inp,
            AppendString(out, inp) => *out == Var::Void || *inp == Var::Nil,
            JumpIf(cond, _) => *cond == Var::Nil,
            Arg(_)
            | ArgAssoc(_, _)
            | ArgSplat(_)
            | ArgBlock(_)
            | Call(_, _, _)
            | Super(_)
            | Label(_)
            | Jump(_)
            | JumpIfNot(_, _)
            | Return(_)
            | AssignGlobal(_, _)
            | AssignConst(_, _)
            | AssignClassVar(_, _)
            | AssignIVar(_, _)
            | BeginRescue(_)
            | RescueMatch(_, _)
            | EndRescue
            | Yield(_)
            | DefModule(_, _, _)
            | DefClass(_, _, _, _)
            | DefMethod(_, _)
            | DefSingletonClass(_, _)
            | DefSingletonMethod(_, _, _) => false,
        }
    }

    /// Pretty-prints the IROp with the given symbol table.
    pub fn fmt_with_symbols(&self, symbols: &Symbols) -> String {
        macro_rules! sym {
            ($i:expr) => {
                match symbols.symbol_name(*$i) {
                    Some(name) => &name,
                    None => "[error getting symbol name]",
                }
            };
        }
        match self {
            IROp::LoadRoot(var) => format!("{} = (::);", var),
            IROp::LoadBool(var, value) => format!("{} = {};", var, value),
            IROp::LoadGlobal(var, name) => format!("{} = ${};", var, sym!(name)),
            IROp::LoadConst(var, parent, name) => format!("{} = {}::{};", var, parent, sym!(name)),
            IROp::LoadClassVar(var, name) => format!("{} = @@{};", var, sym!(name)),
            IROp::LoadIVar(var, name) => format!("{} = @{};", var, sym!(name)),
            IROp::LoadString(var, string) => format!("{} = {:?};", var, string),
            IROp::AppendString(var, other) => format!("{} += {}.to_s;", var, other),
            IROp::LoadSymbol(var, symbol) => format!("{} = :{};", var, sym!(symbol)),
            IROp::LoadI64(var, value) => format!("{} = {};", var, value),
            IROp::LoadFloat(var, value) => format!("{} = {};", var, value),
            IROp::LoadProc(var, proc) => format!("{} = {};", var, proc.fmt_with_symbols(symbols)),
            IROp::Arg(var) => format!("push_arg {};", var),
            IROp::ArgAssoc(key, val) => format!("push_arg {} => {};", key, val),
            IROp::ArgSplat(var) => format!("push_arg *{};", var),
            IROp::ArgBlock(var) => format!("push_arg &{};", var),
            IROp::Call(out, recv, name) => format!("{} = {}.{}();", out, recv, sym!(name)),
            IROp::Super(out) => format!("{} = super();", out),
            IROp::Not(out, var) => format!("{} = not {};", out, var),
            IROp::Label(label) => format!("{}", label),
            IROp::Jump(label) => format!("jump -> {};", label),
            IROp::JumpIf(cond, label) => format!("if {} jump -> {};", cond, label),
            IROp::JumpIfNot(cond, label) => format!("if not {} jump -> {};", cond, label),
            IROp::Return(var) => format!("return {};", var),
            IROp::Assign(lhs, rhs) => format!("{} = {};", lhs, rhs),
            IROp::AssignGlobal(name, rhs) => format!("${} = {};", sym!(name), rhs),
            IROp::AssignConst(name, rhs) => format!("{} = {};", sym!(name), rhs),
            IROp::AssignClassVar(name, rhs) => format!("@@{} = {};", sym!(name), rhs),
            IROp::AssignIVar(name, rhs) => format!("@{} = {};", sym!(name), rhs),
            IROp::BeginRescue(label) => format!("begin rescue (rescue -> {})", label),
            IROp::RescueMatch(class, label) => format!("rescue instanceof {} -> {}", class, label),
            IROp::RescueBind(var) => format!("rescue => {};", var),
            IROp::EndRescue => format!("end rescue"),
            IROp::DefinedConst(var, name) => format!("{} = defined? {};", var, sym!(name)),
            IROp::DefinedGlobal(var, name) => format!("{} = defined? ${};", var, sym!(name)),
            IROp::DefinedClassVar(var, name) => format!("{} = defined? @@{};", var, sym!(name)),
            IROp::DefinedIVar(var, name) => format!("{} = defined? ${};", var, sym!(name)),
            IROp::Yield(var) => format!("{} = yield();", var),
            IROp::DefModule(parent, name, proc) => format!(
                "module {}::{}: {}",
                parent,
                sym!(name),
                proc.fmt_with_symbols(symbols)
            ),
            IROp::DefClass(parent, name, superclass, proc) => match superclass {
                Some(superclass) => format!(
                    "class {}::{} < {}: {}",
                    parent,
                    sym!(name),
                    superclass,
                    proc.fmt_with_symbols(symbols)
                ),
                None => format!(
                    "class {}::{}: {}",
                    parent,
                    sym!(name),
                    proc.fmt_with_symbols(symbols)
                ),
            },
            IROp::DefMethod(name, proc) => {
                format!("def {}: {}", sym!(name), proc.fmt_with_symbols(symbols))
            }
            IROp::DefSingletonClass(expr, proc) => {
                format!("class << {}: {}", expr, proc.fmt_with_symbols(symbols))
            }
            IROp::DefSingletonMethod(expr, name, proc) => format!(
                "def {}.{}: {}",
                expr,
                sym!(name),
                proc.fmt_with_symbols(symbols)
            ),
        }
    }
}

/// An IR proc.
#[derive(Debug)]
pub struct IRProc {
    /// The name of the proc.
    /// This is usually the defined method name, class name, or an internal name.
    pub(super) name: Symbol,
    /// Mapping of register variables to local variable names.
    pub(super) variables: FnvHashMap<Var, Symbol>,
    /// List of captured variables.
    pub(super) captured_vars: FnvHashSet<Var>,
    /// If true, this proc may be captured in a block.
    pub(super) may_be_captured: bool,
    /// If true, this proc is a block.
    pub(super) is_block: bool,
    /// IR ops.
    pub(super) items: Vec<IROp>,
}

impl IRProc {
    /// Creates a new proc with the given list of statements.
    pub fn new(
        name: Symbol,
        statements: &[Statement],
        symbols: &mut Symbols,
    ) -> Result<IRProc, IRError> {
        let mut variables = FnvHashMap::default();
        let mut captured_vars = FnvHashSet::default();
        let mut items = Vec::new();
        let mut may_be_captured = false;

        let mut scope = Scope {
            parent: None,
            variables: &mut variables,
            captured_vars: &mut captured_vars,
            symbols: Some(symbols),
            may_be_captured: &mut may_be_captured,
            var_counter: 0,
            no_local_vars: false,
        };

        let out = scope.next_var();
        Self::expand_statements(statements, out, &mut scope, &mut items)?;
        items.push(IROp::Return(out));

        let mut proc = IRProc {
            name,
            variables,
            captured_vars,
            may_be_captured,
            is_block: false,
            items,
        };
        proc.optimize();
        Ok(proc)
    }

    /// Creates a proc with a body statement.
    fn new_with_body(
        name: Symbol,
        body: &BodyStatement,
        symbols: &mut Symbols,
    ) -> Result<IRProc, IRError> {
        let mut variables = FnvHashMap::default();
        let mut captured_vars = FnvHashSet::default();
        let mut items = Vec::new();
        let mut may_be_captured = false;

        let mut scope = Scope {
            parent: None,
            variables: &mut variables,
            captured_vars: &mut captured_vars,
            symbols: Some(symbols),
            may_be_captured: &mut may_be_captured,
            var_counter: 0,
            no_local_vars: false,
        };

        let out = Self::expand_body_statement(body, &mut scope, &mut items)?;
        items.push(IROp::Return(out));

        let mut proc = IRProc {
            name,
            variables,
            captured_vars,
            may_be_captured,
            is_block: false,
            items,
        };
        proc.optimize();
        Ok(proc)
    }

    /// Optimization:
    ///
    /// - collapsing register variables
    /// - collapsing redundant labels
    /// - stripping useless operations
    fn optimize(&mut self) {
        self.collapse_vars();
        self.collapse_labels();
        self.strip_useless();

        for item in &mut self.items {
            match item {
                IROp::LoadProc(_, proc)
                | IROp::DefModule(_, _, proc)
                | IROp::DefClass(_, _, _, proc)
                | IROp::DefMethod(_, proc)
                | IROp::DefSingletonClass(_, proc)
                | IROp::DefSingletonMethod(_, _, proc) => proc.optimize(),
                _ => (),
            }
        }
    }

    /// Collapses variables and replaces nil-reads and void-writes with [Var::Nil] and [Var::Void].
    ///
    /// “Collapsing variables” refers to merging two variables for which the proc could be
    /// partitioned into two sections such that in both sections only one of the two variables is
    /// used. Since nil-reads will be turned into [Var::Nil], this shouldn’t create any unexpected
    /// behavior.
    fn collapse_vars(&mut self) {
        // find first and last reads/writes of variables
        let mut first_rws: FnvHashMap<Var, (Option<usize>, Option<usize>)> = FnvHashMap::default();
        let mut last_rws: FnvHashMap<Var, (Option<usize>, Option<usize>)> = FnvHashMap::default();

        for (i, item) in self.items.iter_mut().enumerate() {
            item.for_each_var(|var, is_write| {
                first_rws
                    .entry(*var)
                    .and_modify(|(read, write)| {
                        if is_write {
                            if write.is_none() {
                                *write = Some(i)
                            }
                        } else {
                            if read.is_none() {
                                *read = Some(i)
                            }
                        }
                    })
                    .or_insert(if is_write {
                        (None, Some(i))
                    } else {
                        (Some(i), None)
                    });
                last_rws
                    .entry(*var)
                    .and_modify(|(read, write)| {
                        if is_write {
                            *write = Some(i);
                        } else {
                            *read = Some(i);
                        }
                    })
                    .or_insert(if is_write {
                        (None, Some(i))
                    } else {
                        (Some(i), None)
                    });
            });
        }

        // Mark which variables should be replaced with nil or void such that variables will
        // only exist from their first write to their last read
        let mut replace_with_nil_up_to = FnvHashMap::default();
        let mut replace_with_void_from = FnvHashMap::default();

        // List of first-used (created) and never-again--used (destroyed) variables in each
        // operation.
        let mut var_lifetimes: BTreeMap<usize, (Vec<Var>, Vec<Var>)> = BTreeMap::new();

        for (var, (first_read, first_write)) in first_rws {
            if self.captured_vars.contains(&var) || var.is_from_parent() {
                // Can’t remap a variable that’s used elsewhere
                continue;
            }

            match (first_read, first_write) {
                (Some(first_read), Some(first_write)) => {
                    if first_read < first_write {
                        // Read before it’s written to
                        replace_with_nil_up_to.insert(var, first_write);
                    }
                    var_lifetimes
                        .entry(first_write)
                        .and_modify(|(created, _)| created.push(var))
                        .or_insert((vec![var], Vec::new()));
                }
                (Some(_), None) => {
                    // Read, but never written to: will always be nil
                    replace_with_nil_up_to.insert(var, self.items.len());
                }
                (None, Some(_)) => {
                    // Written to, but never read: can be replaced with [Var::Void]
                    replace_with_void_from.insert(var, 0);
                }
                (None, None) => (),
            }
        }

        for (var, (last_read, last_write)) in last_rws {
            if let (Some(last_read), Some(last_write)) = (last_read, last_write) {
                if last_read < last_write {
                    // Written to after it won’t be read from again
                    replace_with_void_from.insert(var, last_read + 1);
                }
                var_lifetimes
                    .entry(last_read)
                    .and_modify(|(_, destroyed)| destroyed.push(var))
                    .or_insert((Vec::new(), vec![var]));
            }
        }

        // Variable “slots” with
        // (mapped-to variable, currently occupying variable, was occupied by a named variable)
        let mut var_slots: Vec<(Var, Option<Var>, bool)> = Vec::new();
        let mut var_mapping = FnvHashMap::default();

        for (_, (created, destroyed)) in var_lifetimes {
            // Free slots first.
            // Because making an operation read from and write to the same register is allowed,
            // this can save extra registers e.g. in an assignment `a = b` if b will never be used
            // again and a has never been used before.
            for var in destroyed {
                for (_, slot, _) in &mut var_slots {
                    if *slot == Some(var) {
                        *slot = None;
                    }
                }
            }

            for var in created {
                let mut needs_new_slot = true;
                let is_named_var = self.variables.contains_key(&var);
                for (slot_key, slot, slot_is_named) in var_slots.iter_mut() {
                    if is_named_var && *slot_is_named {
                        // don’t merge two named local variables
                        continue;
                    }
                    if slot.is_none() {
                        *slot = Some(var);
                        var_mapping.insert(var, *slot_key);
                        needs_new_slot = false;
                        if is_named_var {
                            *slot_is_named = true;
                        }
                    }
                }
                if needs_new_slot {
                    let slot_key = var;
                    var_mapping.insert(var, slot_key);
                    var_slots.push((slot_key, Some(slot_key), is_named_var));
                }
            }
        }

        // Actually perform the variable remapping
        for (index, item) in self.items.iter_mut().enumerate() {
            item.for_each_var(|var, _| {
                if let Some(i) = replace_with_nil_up_to.get(var) {
                    if index < *i {
                        *var = Var::Nil;
                    }
                }
                if let Some(i) = replace_with_void_from.get(var) {
                    if index >= *i {
                        *var = Var::Void;
                    }
                }
                if let Some(mapped) = var_mapping.get(var) {
                    *var = *mapped;
                }
            });
        }

        let mut variables = mem::replace(&mut self.variables, unsafe { mem::uninitialized() });
        variables = variables
            .into_iter()
            .map(|(var, sym)| {
                if let Some(mapped) = var_mapping.get(&var) {
                    (*mapped, sym)
                } else {
                    (var, sym)
                }
            })
            .collect();
        mem::forget(mem::replace(&mut self.variables, variables));
    }

    /// Collapses consecutive labels.
    fn collapse_labels(&mut self) {
        let mut remap = FnvHashMap::default();
        let mut i = 0;
        while i < self.items.len() {
            match self.items[i] {
                IROp::Label(label) => {
                    i += 1;
                    while let Some(IROp::Label(other_label)) = self.items.get(i) {
                        remap.insert(*other_label, label);
                        self.items.remove(i);
                    }
                }
                _ => i += 1,
            }
        }

        if remap.is_empty() {
            return;
        }

        for item in &mut self.items {
            match item {
                IROp::Jump(label)
                | IROp::JumpIf(_, label)
                | IROp::JumpIfNot(_, label)
                | IROp::BeginRescue(label)
                | IROp::RescueMatch(_, label) => {
                    if let Some(remapped) = remap.get(label) {
                        *label = *remapped;
                    }
                }
                _ => (),
            }
        }
    }

    /// Strips useless items.
    fn strip_useless(&mut self) {
        let mut items = mem::replace(&mut self.items, unsafe { mem::uninitialized() });
        items = items
            .into_iter()
            .filter(|item| !item.is_useless())
            .collect();
        mem::forget(mem::replace(&mut self.items, items));
    }

    fn expand_statement(
        statement: &Statement,
        out: Option<Var>,
        scope: &mut Scope,
        items: &mut Vec<IROp>,
    ) -> Result<(), IRError> {
        match statement {
            Statement::Expr(expr) => {
                let value = Self::expand_expr(expr, scope, items)?;
                if let Some(out) = out {
                    items.push(IROp::Assign(out, value));
                }
            }
            Statement::Alias(..) => unimplemented!("alias statement"),
            Statement::Undef(..) => unimplemented!("undef statement"),
            Statement::IfMod(statement, cond) => {
                let end_label = scope.next_label();
                let cond = Self::expand_expr(cond, scope, items)?;
                items.push(IROp::JumpIfNot(cond, end_label));
                Self::expand_statement(statement, out, scope, items)?;
                items.push(IROp::Label(end_label));
            }
            Statement::UnlessMod(statement, cond) => {
                let end_label = scope.next_label();
                let cond = Self::expand_expr(cond, scope, items)?;
                items.push(IROp::JumpIf(cond, end_label));
                Self::expand_statement(statement, out, scope, items)?;
                items.push(IROp::Label(end_label));
            }
            Statement::WhileMod(statement, cond) => {
                let top_label = scope.next_label();
                let end_label = scope.next_label();
                items.push(IROp::Label(top_label));
                let cond = Self::expand_expr(cond, scope, items)?;
                items.push(IROp::JumpIfNot(cond, end_label));
                Self::expand_statement(statement, out, scope, items)?;
                items.push(IROp::Jump(top_label));
                items.push(IROp::Label(end_label));
            }
            Statement::UntilMod(statement, cond) => {
                let top_label = scope.next_label();
                let end_label = scope.next_label();
                items.push(IROp::Label(top_label));
                let cond = Self::expand_expr(cond, scope, items)?;
                items.push(IROp::JumpIf(cond, end_label));
                Self::expand_statement(statement, out, scope, items)?;
                items.push(IROp::Jump(top_label));
                items.push(IROp::Label(end_label));
            }
            Statement::MultiAssign(lhs, rhs) => {
                if lhs.len() == 1 && rhs.items.len() == 1 && rhs.splat.is_none() {
                    match &lhs[0] {
                        MultiLHSItem::LHS(lhs) => match lhs {
                            LeftHandSide::Var(ident) => match ident {
                                Ident::Local(name) => {
                                    let name = scope.symbol(name);
                                    let var = scope.define_local_var(name);
                                    let value = Self::expand_expr(&rhs.items[0], scope, items)?;
                                    items.push(IROp::Assign(var, value));
                                }
                                _ => unimplemented!("multi assign"),
                            },
                            _ => unimplemented!("multi assign"),
                        },
                        _ => unimplemented!("multi assign"),
                    }
                } else {
                    unimplemented!("multi assign")
                }
            }
            _ => unimplemented!("statement"),
        }

        Ok(())
    }

    fn expand_statements(
        statements: &[Statement],
        out: Var,
        scope: &mut Scope,
        items: &mut Vec<IROp>,
    ) -> Result<(), IRError> {
        for (i, statement) in statements.iter().enumerate() {
            let is_last = i == statements.len() - 1;
            Self::expand_statement(
                statement,
                if is_last { Some(out) } else { None },
                scope,
                items,
            )?;
        }

        Ok(())
    }

    fn expand_expr(
        expr: &Expression,
        scope: &mut Scope,
        items: &mut Vec<IROp>,
    ) -> Result<Var, IRError> {
        match expr {
            Expression::Variable(ident) => Self::load_var(ident, scope, items),
            Expression::RootConst(ident) => match ident {
                Ident::Const(name) => {
                    let name = scope.symbol(name);
                    let out = scope.next_var();
                    items.push(IROp::LoadRoot(out));
                    items.push(IROp::LoadConst(out, out, name));
                    Ok(out)
                }
                _ => Err(IRError::InvalidConstPath(ident.clone())),
            },
            Expression::SubConst(expr, ident) => match ident {
                Ident::Const(name) => {
                    let name = scope.symbol(name);
                    let expr = Self::expand_expr(expr, scope, items)?;
                    let out = scope.next_var();
                    items.push(IROp::LoadConst(out, expr, name));
                    Ok(out)
                }
                _ => Err(IRError::InvalidConstPath(ident.clone())),
            },
            Expression::Literal(literal) => {
                let out = scope.next_var();
                match literal {
                    Literal::Number { positive, value } => {
                        if let Some(number) = Self::number_to_i64(*positive, value) {
                            items.push(IROp::LoadI64(out, number));
                        } else if let Some(number) = Self::number_to_float(*positive, value) {
                            items.push(IROp::LoadFloat(out, number));
                        } else {
                            unimplemented!("load float or bignum")
                        }
                    }
                    Literal::String(string) => {
                        items.push(IROp::LoadString(out, string.clone()));
                    }
                    Literal::QuotedString(fragments) => {
                        for (i, fragment) in fragments.iter().enumerate() {
                            if i == 0 {
                                match fragment {
                                    QuotedFragment::String(string) => {
                                        items.push(IROp::LoadString(out, string.clone()));
                                    }
                                    QuotedFragment::Ident(ident) => {
                                        let var = Self::load_var(ident, scope, items)?;
                                        items.push(IROp::LoadString(out, "".into()));
                                        items.push(IROp::AppendString(out, var));
                                    }
                                    QuotedFragment::Interpolated(statements) => {
                                        // FIXME: statements probably have values too idk
                                        let value = scope.next_var();
                                        Self::expand_statements(statements, value, scope, items)?;
                                        items.push(IROp::LoadString(out, "".into()));
                                        items.push(IROp::AppendString(out, value));
                                    }
                                }
                            } else {
                                match fragment {
                                    QuotedFragment::String(string) => {
                                        let tmp = scope.next_var();
                                        items.push(IROp::LoadString(tmp, string.clone()));
                                        items.push(IROp::AppendString(out, tmp));
                                    }
                                    QuotedFragment::Ident(ident) => {
                                        let var = Self::load_var(ident, scope, items)?;
                                        items.push(IROp::AppendString(out, var));
                                    }
                                    QuotedFragment::Interpolated(statements) => {
                                        // FIXME: statements probably have values too idk
                                        let value = scope.next_var();
                                        Self::expand_statements(statements, value, scope, items)?;
                                        items.push(IROp::AppendString(out, value));
                                    }
                                }
                            }
                        }
                    }
                    Literal::Symbol(symbol) => {
                        let symbol = scope.symbol(symbol);
                        items.push(IROp::LoadSymbol(out, symbol));
                    }
                }
                Ok(out)
            }
            Expression::Block(block) => {
                scope.may_be_captured();

                // TODO: lambdas?

                let mut variables = FnvHashMap::default();
                let mut captured_vars = FnvHashSet::default();
                let mut block_items = Vec::new();
                let mut may_be_captured = false;

                let mut block_scope = unsafe {
                    scope.sub_scope(&mut variables, &mut captured_vars, &mut may_be_captured)
                };
                let out = block_scope.next_var();
                Self::expand_statements(&block.body, out, &mut block_scope, &mut block_items)?;
                block_items.push(IROp::Return(out));

                let name = if block.lambda {
                    scope.symbol("[lambda]")
                } else {
                    scope.symbol("[block]")
                };

                let proc = IRProc {
                    name,
                    variables,
                    captured_vars,
                    may_be_captured,
                    is_block: true,
                    items: block_items,
                };

                let out = scope.next_var();
                items.push(IROp::LoadProc(out, proc));
                Ok(out)
            }
            Expression::Nil => {
                let out = scope.next_var();
                Ok(out)
            }
            Expression::SelfExpr => Ok(Var::SelfRef),
            Expression::True | Expression::False => {
                let is_true = expr == &Expression::True;
                let out = scope.next_var();
                items.push(IROp::LoadBool(out, is_true));
                Ok(out)
            }
            Expression::ArrayConstructor(args) => {
                let out = scope.next_var();
                items.push(IROp::LoadRoot(out));
                items.push(IROp::LoadConst(out, out, scope.symbol("Array")));
                if let Some(args) = args {
                    Self::expand_args(args, scope, items)?;
                }
                items.push(IROp::Call(out, out, scope.symbol("new")));
                Ok(out)
            }
            Expression::HashConstructor(args) => {
                let out = scope.next_var();
                items.push(IROp::LoadRoot(out));
                items.push(IROp::LoadConst(out, out, scope.symbol("Hash")));
                Self::expand_args(
                    &Arguments {
                        block: None,
                        hash: args.to_vec(),
                        items: Vec::new(),
                    },
                    scope,
                    items,
                )?;
                items.push(IROp::Call(out, out, scope.symbol("new")));
                Ok(out)
            }
            Expression::Not(expr) => {
                let expr = Self::expand_expr(expr, scope, items)?;
                items.push(IROp::Not(expr, expr));
                Ok(expr)
            }
            Expression::And(lhs, rhs) => {
                // short-circuited && implemented as follows:
                // result = LHS
                // if not result goto end
                // rhs = RHS
                // result = rhs
                // end:
                let result = Self::expand_expr(lhs, scope, items)?;
                let end_label = scope.next_label();
                items.push(IROp::JumpIfNot(result, end_label));
                let rhs = Self::expand_expr(rhs, scope, items)?;
                items.push(IROp::Assign(result, rhs));
                items.push(IROp::Label(end_label));
                Ok(result)
            }
            Expression::Or(lhs, rhs) => {
                // short-circuited || implemented as follows:
                // result = LHS
                // if result goto end
                // rhs = RHS
                // result = rhs
                // end:
                let result = Self::expand_expr(lhs, scope, items)?;
                let end_label = scope.next_label();
                items.push(IROp::JumpIf(result, end_label));
                let rhs = Self::expand_expr(rhs, scope, items)?;
                items.push(IROp::Assign(result, rhs));
                items.push(IROp::Label(end_label));
                Ok(result)
            }
            Expression::UMinus(expr) => {
                let out = Self::expand_expr(expr, scope, items)?;
                items.push(IROp::Call(out, out, scope.symbol("-@")));
                Ok(out)
            }
            Expression::UPlus(expr) => {
                let out = Self::expand_expr(expr, scope, items)?;
                items.push(IROp::Call(out, out, scope.symbol("+@")));
                Ok(out)
            }
            Expression::BitInv(expr) => {
                let out = Self::expand_expr(expr, scope, items)?;
                items.push(IROp::Call(out, out, scope.symbol("~")));
                Ok(out)
            }
            Expression::BinOp(lhs, op, rhs) => {
                use sapphire_parser::ast::BinaryOp::*;

                let lhs = Self::expand_expr(lhs, scope, items)?;
                let rhs = Self::expand_expr(rhs, scope, items)?;
                items.push(IROp::Arg(rhs));

                let op = match op {
                    Neq => "!=",
                    NMatch => "!~",
                    And => "&&",
                    Or => "||",
                    BitXor => "^",
                    BitAnd => "&",
                    BitOr => "|",
                    Cmp => "<=>",
                    CaseEq => "===",
                    Eq => "==",
                    Match => "=~",
                    Geq => ">=",
                    Shr => ">>",
                    Gt => ">",
                    Leq => "<=",
                    Shl => "<<",
                    Lt => "<",
                    Add => "+",
                    Sub => "-",
                    Pow => "**",
                    Mul => "*",
                    Div => "/",
                    Rem => "%",
                };

                items.push(IROp::Call(lhs, lhs, scope.symbol(op)));
                Ok(lhs)
            }
            Expression::If {
                cond,
                then,
                elsif,
                else_,
            } => {
                // if a then  |  JumpIfNot a, next
                //    b       |  b
                //            |  Jump end
                // elsif ...  |  next: ...
                //            |  end:
                let end_label = scope.next_label();
                let out = scope.next_var();
                for (cond, then) in
                    iter::once((&**cond, &**then)).chain(elsif.iter().map(|(c, t)| (&*c, &**t)))
                {
                    let next_label = scope.next_label();
                    let cond = Self::expand_expr(cond, scope, items)?;
                    items.push(IROp::JumpIfNot(cond, next_label));
                    Self::expand_statements(then, out, scope, items)?;
                    items.push(IROp::Jump(end_label));
                    items.push(IROp::Label(next_label));
                }

                if let Some(else_) = else_ {
                    Self::expand_statements(else_, out, scope, items)?;
                }

                items.push(IROp::Label(end_label));

                Ok(out)
            }
            Expression::Ternary { cond, then, else_ } => {
                let cond = Self::expand_expr(cond, scope, items)?;
                let else_label = scope.next_label();
                let end_label = scope.next_label();
                items.push(IROp::JumpIfNot(cond, else_label));
                let out = Self::expand_expr(then, scope, items)?;
                items.push(IROp::Jump(end_label));
                items.push(IROp::Label(else_label));
                let else_ = Self::expand_expr(else_, scope, items)?;
                items.push(IROp::Assign(out, else_));
                items.push(IROp::Label(end_label));
                Ok(out)
            }
            Expression::Unless { cond, then, else_ } => {
                let cond = Self::expand_expr(cond, scope, items)?;
                let else_label = scope.next_label();
                let end_label = scope.next_label();
                let out = scope.next_var();
                items.push(IROp::JumpIf(cond, else_label));
                Self::expand_statements(then, out, scope, items)?;
                items.push(IROp::Jump(end_label));
                items.push(IROp::Label(else_label));
                if let Some(else_) = else_ {
                    Self::expand_statements(else_, out, scope, items)?;
                }
                items.push(IROp::Label(end_label));
                Ok(out)
            }
            Expression::Case {
                expr: _,
                cases: _,
                else_: _,
            } => unimplemented!("case expression"),
            Expression::While(cond, body) => {
                let top_label = scope.next_label();
                let end_label = scope.next_label();
                items.push(IROp::Label(top_label));
                let cond = Self::expand_expr(cond, scope, items)?;
                items.push(IROp::JumpIfNot(cond, end_label));
                let out = scope.next_var();
                Self::expand_statements(body, out, scope, items)?;
                items.push(IROp::Jump(top_label));
                items.push(IROp::Label(end_label));
                let out = scope.next_var();
                Ok(out)
            }
            Expression::Until(cond, body) => {
                let top_label = scope.next_label();
                let end_label = scope.next_label();
                items.push(IROp::Label(top_label));
                let cond = Self::expand_expr(cond, scope, items)?;
                items.push(IROp::JumpIf(cond, end_label));
                let out = scope.next_var();
                Self::expand_statements(body, out, scope, items)?;
                items.push(IROp::Jump(top_label));
                items.push(IROp::Label(end_label));
                let out = scope.next_var();
                Ok(out)
            }
            Expression::For(lhs, expr, body) => {
                let simple_for = match &**expr {
                    Expression::Range {
                        start,
                        end,
                        inclusive,
                    } => match (start.as_ref().map(|b| &**b), end.as_ref().map(|b| &**b)) {
                        (
                            Some(Expression::Literal(Literal::Number {
                                positive: start_positive,
                                value: start_value,
                            })),
                            Some(Expression::Literal(Literal::Number {
                                positive: end_positive,
                                value: end_value,
                            })),
                        ) => {
                            let start = Self::number_to_i64(*start_positive, start_value);
                            let end = Self::number_to_i64(*end_positive, end_value);

                            if let (Some(start), Some(end)) = (start, end) {
                                if lhs.len() == 1 {
                                    match &lhs[0] {
                                        MultiLHSItem::LHS(LeftHandSide::Var(Ident::Local(
                                            name,
                                        ))) => Some((name, start, end, inclusive)),
                                        _ => None,
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    },
                    _ => None,
                };

                if let Some((name, start, end, inclusive)) = simple_for {
                    let name = scope.symbol(name);
                    let counter = scope.define_local_var(name);
                    items.push(IROp::LoadI64(counter, start));
                    if start < end || (start == end && *inclusive) {
                        let top_label = scope.next_label();
                        let end_label = scope.next_label();
                        let end_var = scope.next_var();
                        let loop_cond = scope.next_var();
                        let out = scope.next_var();
                        let one = scope.next_var();
                        items.push(IROp::LoadI64(end_var, end));
                        items.push(IROp::LoadI64(one, 1));
                        items.push(IROp::Label(top_label));
                        items.push(IROp::Arg(end_var));
                        items.push(IROp::Call(
                            loop_cond,
                            counter,
                            if *inclusive {
                                scope.symbol("<=")
                            } else {
                                scope.symbol("<")
                            },
                        ));
                        items.push(IROp::JumpIfNot(loop_cond, end_label));
                        Self::expand_statements(body, out, scope, items)?;
                        items.push(IROp::Arg(one));
                        items.push(IROp::Call(out, counter, scope.symbol("+=")));
                        items.push(IROp::Jump(top_label));
                        items.push(IROp::Label(end_label));
                    }
                    Ok(Var::Nil)
                } else {
                    let mut block_items = Vec::new();

                    // TODO: assign block params
                    let mut variables = FnvHashMap::default();
                    let mut captured_vars = FnvHashSet::default();
                    let mut may_be_captured = false;

                    let mut for_scope = unsafe {
                        scope.sub_scope(&mut variables, &mut captured_vars, &mut may_be_captured)
                    };
                    for_scope.no_local_vars = true;
                    let out = scope.next_var();
                    Self::expand_statements(body, out, &mut for_scope, &mut block_items)?;
                    scope.may_be_captured();

                    let block = IRProc {
                        name: scope.symbol("[for block]"),
                        items: block_items,
                        captured_vars: FnvHashSet::default(),
                        may_be_captured: false,
                        is_block: true,
                        variables: FnvHashMap::default(),
                    };

                    let expr = Self::expand_expr(expr, scope, items)?;
                    let block_var = scope.next_var();
                    items.push(IROp::LoadProc(block_var, block));
                    items.push(IROp::ArgBlock(block_var));
                    items.push(IROp::Call(expr, expr, scope.symbol("each")));

                    Ok(Var::Nil)
                }
            }
            Expression::Begin(body) => Self::expand_body_statement(body, scope, items),
            Expression::Call { member, name, args } => {
                if let Some(member) = member {
                    let out = Self::expand_expr(member, scope, items)?;
                    Self::expand_args(args, scope, items)?;
                    let name = match name {
                        Ident::Local(s) | Ident::MethodOnly(s) | Ident::AssignmentMethod(s) => {
                            scope.symbol(s)
                        }
                        Ident::Keyword(s) => scope.symbol(s),
                        name => {
                            return Err(IRError::InvalidMethodName(name.clone()));
                        }
                    };
                    items.push(IROp::Call(out, out, name));
                    Ok(out)
                } else {
                    Self::expand_args(args, scope, items)?;
                    let out = scope.next_var();
                    match name {
                        Ident::Local(s) | Ident::MethodOnly(s) | Ident::AssignmentMethod(s) => {
                            items.push(IROp::Call(out, Var::SelfRef, scope.symbol(s)));
                        }
                        Ident::Keyword(s) => {
                            items.push(IROp::Call(out, Var::SelfRef, scope.symbol(s)));
                        }
                        Ident::Global(name) => {
                            items.push(IROp::LoadGlobal(out, scope.symbol(name)));
                            items.push(IROp::Call(out, out, scope.symbol("call")));
                        }
                        Ident::Const(name) => {
                            items.push(IROp::LoadConst(out, Var::SelfRef, scope.symbol(name)));
                            items.push(IROp::Call(out, out, scope.symbol("call")));
                        }
                        Ident::Class(name) => {
                            items.push(IROp::LoadClassVar(out, scope.symbol(name)));
                            items.push(IROp::Call(out, out, scope.symbol("call")));
                        }
                        Ident::Instance(name) => {
                            items.push(IROp::LoadIVar(out, scope.symbol(name)));
                            items.push(IROp::Call(out, out, scope.symbol("call")));
                        }
                    };
                    Ok(out)
                }
            }
            Expression::Index(expr, args) => {
                let expr = Self::expand_expr(expr, scope, items)?;
                Self::expand_args(args, scope, items)?;
                let out = scope.next_var();
                items.push(IROp::Call(out, expr, scope.symbol("[]")));
                Ok(out)
            }
            Expression::Defined(expr) => match &**expr {
                Expression::Variable(ident) => match ident {
                    Ident::Local(name) => {
                        let name = scope.symbol(name);
                        let out = scope.next_var();
                        if let Some(_) = scope.local_var(name) {
                            items.push(IROp::LoadString(out, "local-variable".into()));
                        }
                        Ok(out)
                    }
                    Ident::Const(name) => {
                        let name = scope.symbol(name);
                        let out = scope.next_var();
                        items.push(IROp::DefinedConst(out, name));
                        Ok(out)
                    }
                    Ident::Global(name) => {
                        let name = scope.symbol(name);
                        let out = scope.next_var();
                        items.push(IROp::DefinedGlobal(out, name));
                        Ok(out)
                    }
                    Ident::Class(name) => {
                        let name = scope.symbol(name);
                        let out = scope.next_var();
                        items.push(IROp::DefinedClassVar(out, name));
                        Ok(out)
                    }
                    Ident::Instance(name) => {
                        let name = scope.symbol(name);
                        let out = scope.next_var();
                        items.push(IROp::DefinedIVar(out, name));
                        Ok(out)
                    }
                    _ => Err(IRError::InvalidDefined),
                },
                _ => Err(IRError::InvalidDefined),
            },
            Expression::Super(args) => {
                Self::expand_args(args, scope, items)?;
                let out = scope.next_var();
                items.push(IROp::Super(out));
                Ok(out)
            }
            Expression::Return(args) => {
                if let Some(_args) = args {
                    unimplemented!("return with args")
                } else {
                    let nil = scope.next_var();
                    items.push(IROp::Return(nil));
                    Ok(nil)
                }
            }
            Expression::Break(_args) => unimplemented!("break"),
            Expression::Yield(args) => {
                if let Some(_args) = args {
                    unimplemented!("yield with args")
                } else {
                    let out = scope.next_var();
                    items.push(IROp::Yield(out));
                    Ok(out)
                }
            }
            Expression::Next(_args) => unimplemented!("next"),
            Expression::Redo => unimplemented!("redo"),
            Expression::Retry => unimplemented!("retry"),
            Expression::Statements(statements) => {
                let out = scope.next_var();
                Self::expand_statements(statements, out, scope, items)?;
                Ok(out)
            }
            Expression::Range {
                start,
                end,
                inclusive,
            } => {
                let start = if let Some(start) = start {
                    Self::expand_expr(start, scope, items)?
                } else {
                    scope.next_var() // nil
                };
                let end = if let Some(end) = end {
                    Self::expand_expr(end, scope, items)?
                } else {
                    scope.next_var() // nil
                };
                let incl = scope.next_var();
                items.push(IROp::LoadBool(incl, *inclusive));

                let out = scope.next_var();
                items.push(IROp::LoadRoot(out));
                items.push(IROp::LoadConst(out, out, scope.symbol("Range")));
                items.push(IROp::Arg(start));
                items.push(IROp::Arg(end));
                items.push(IROp::Arg(incl));
                items.push(IROp::Call(out, out, scope.symbol("new")));
                Ok(out)
            }
            Expression::Module { path, body } => {
                match path {
                    DefPath::Member(expr, name) => {
                        let expr = Self::expand_expr(expr, scope, items)?;
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        let body = IRProc::new_with_body(name, body, scope.symbols())?;
                        items.push(IROp::DefModule(expr, name, body));
                    }
                    DefPath::Root(name) => {
                        let root = scope.next_var();
                        items.push(IROp::LoadRoot(root));
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        let body = IRProc::new_with_body(name, body, scope.symbols())?;
                        items.push(IROp::DefModule(root, name, body));
                    }
                    DefPath::Current(name) => {
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        let body = IRProc::new_with_body(name, body, scope.symbols())?;
                        items.push(IROp::DefModule(Var::SelfRef, name, body));
                    }
                }
                Ok(Var::Nil)
            }
            Expression::Class {
                path,
                superclass,
                body,
            } => {
                let superclass = if let Some(superclass) = superclass {
                    Some(Self::expand_expr(superclass, scope, items)?)
                } else {
                    None
                };
                match path {
                    DefPath::Member(expr, name) => {
                        let expr = Self::expand_expr(expr, scope, items)?;
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        let body = IRProc::new_with_body(name, body, scope.symbols())?;
                        items.push(IROp::DefClass(expr, name, superclass, body));
                    }
                    DefPath::Root(name) => {
                        let root = scope.next_var();
                        items.push(IROp::LoadRoot(root));
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        let body = IRProc::new_with_body(name, body, scope.symbols())?;
                        items.push(IROp::DefClass(root, name, superclass, body));
                    }
                    DefPath::Current(name) => {
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        let body = IRProc::new_with_body(name, body, scope.symbols())?;
                        items.push(IROp::DefClass(Var::SelfRef, name, superclass, body));
                    }
                }
                Ok(Var::Nil)
            }
            Expression::Method { name, params, body } => {
                let name = match name {
                    Ident::Local(name)
                    | Ident::Const(name)
                    | Ident::MethodOnly(name)
                    | Ident::AssignmentMethod(name) => scope.symbol(name),
                    Ident::Keyword(name) => scope.symbol(name),
                    name => return Err(IRError::InvalidMethodName(name.clone())),
                };

                let mut did_find_splat = false;
                for param in params {
                    match param {
                        Parameter::Splat(..) => {
                            if !did_find_splat {
                                did_find_splat = true;
                            } else {
                                return Err(IRError::TooManySplats);
                            }
                        }
                        _ => (),
                    }
                }

                // TODO: deal with params

                let body = IRProc::new_with_body(name, body, scope.symbols())?;
                items.push(IROp::DefMethod(name, body));

                Ok(Var::Nil)
            }
            Expression::SingletonClass { expr, body } => {
                let expr = Self::expand_expr(expr, scope, items)?;
                let name = scope.symbol("[singleton class]");
                let body = IRProc::new_with_body(name, body, scope.symbols())?;
                items.push(IROp::DefSingletonClass(expr, body));
                Ok(Var::Nil)
            }
            Expression::SingletonMethod {
                expr,
                name,
                params,
                body,
            } => {
                let expr = Self::expand_expr(expr, scope, items)?;
                let name = match name {
                    Ident::Local(name)
                    | Ident::Const(name)
                    | Ident::MethodOnly(name)
                    | Ident::AssignmentMethod(name) => scope.symbol(name),
                    Ident::Keyword(name) => scope.symbol(name),
                    name => return Err(IRError::InvalidMethodName(name.clone())),
                };

                let mut did_find_splat = false;
                for param in params {
                    match param {
                        Parameter::Splat(..) => {
                            if !did_find_splat {
                                did_find_splat = true;
                            } else {
                                return Err(IRError::TooManySplats);
                            }
                        }
                        _ => (),
                    }
                }

                // TODO: deal with params
                let body = IRProc::new_with_body(name, body, scope.symbols())?;
                items.push(IROp::DefSingletonMethod(expr, name, body));
                Ok(Var::Nil)
            }
            _ => unimplemented!("expr"),
        }
    }

    fn expand_assignment(
        lhs: &LeftHandSide,
        rhs: Var,
        scope: &mut Scope,
        items: &mut Vec<IROp>,
    ) -> Result<Var, IRError> {
        match lhs {
            LeftHandSide::Var(ident) => match ident {
                Ident::Local(name) => {
                    let name = scope.symbol(name);
                    let var = scope.define_local_var(name);
                    items.push(IROp::Assign(var, rhs));
                    Ok(rhs)
                }
                Ident::Global(name) => {
                    let name = scope.symbol(name);
                    items.push(IROp::AssignGlobal(name, rhs));
                    Ok(rhs)
                }
                Ident::Const(name) => {
                    let name = scope.symbol(name);
                    items.push(IROp::AssignConst(name, rhs));
                    Ok(rhs)
                }
                Ident::Class(name) => {
                    let name = scope.symbol(name);
                    items.push(IROp::AssignClassVar(name, rhs));
                    Ok(rhs)
                }
                Ident::Instance(name) => {
                    let name = scope.symbol(name);
                    items.push(IROp::AssignIVar(name, rhs));
                    Ok(rhs)
                }
                lhs => {
                    return Err(IRError::InvalidLeftHandSide(lhs.clone()));
                }
            },
            LeftHandSide::Index(expr, args) => {
                let expr = Self::expand_expr(expr, scope, items)?;
                if let Some(args) = args {
                    Self::expand_args(args, scope, items)?;
                }
                items.push(IROp::Call(expr, expr, scope.symbol("[]")));
                items.push(IROp::Assign(expr, rhs));
                Ok(rhs)
            }
            LeftHandSide::Member(expr, ident) => {
                let expr = Self::expand_expr(expr, scope, items)?;
                match ident {
                    Ident::Local(name)
                    | Ident::Const(name)
                    | Ident::MethodOnly(name)
                    | Ident::AssignmentMethod(name) => {
                        let name = scope.symbol(name);
                        items.push(IROp::Call(expr, expr, name));
                    }
                    Ident::Keyword(name) => {
                        let name = scope.symbol(name);
                        items.push(IROp::Call(expr, expr, name));
                    }
                    ident => return Err(IRError::InvalidMember(ident.clone())),
                }
                items.push(IROp::Assign(expr, rhs));
                Ok(rhs)
            }
            LeftHandSide::RootConst(ident) => match ident {
                Ident::Const(name) => {
                    let name = scope.symbol(name);
                    let lhs = scope.next_var();
                    items.push(IROp::LoadRoot(lhs));
                    items.push(IROp::LoadConst(lhs, lhs, name));
                    Ok(rhs)
                }
                ident => return Err(IRError::InvalidConstPath(ident.clone())),
            },
        }
    }

    fn expand_body_statement(
        body: &BodyStatement,
        scope: &mut Scope,
        items: &mut Vec<IROp>,
    ) -> Result<Var, IRError> {
        let rescue_label = scope.next_label();
        let else_label = scope.next_label();
        let ensure_label = scope.next_label();
        items.push(IROp::BeginRescue(rescue_label));
        let out = scope.next_var();
        Self::expand_statements(&body.body, out, scope, items)?;
        items.push(IROp::Jump(else_label));
        items.push(IROp::EndRescue);
        items.push(IROp::Label(rescue_label));
        for rescue in &body.rescue {
            let end_label = scope.next_label();
            if let Some(classes) = &rescue.classes {
                let begin_label = scope.next_label();
                for item in &classes.items {
                    let exception_class = Self::expand_expr(item, scope, items)?;
                    items.push(IROp::RescueMatch(exception_class, begin_label));
                }
                items.push(IROp::Jump(end_label));
                items.push(IROp::Label(begin_label));
            }
            let out = scope.next_var();
            if let Some(binding) = &rescue.variable {
                match binding {
                    LeftHandSide::Var(ident) => match ident {
                        Ident::Local(name) => {
                            let name = scope.symbol(name);
                            let var = scope.define_local_var(name);
                            items.push(IROp::RescueBind(var));
                        }
                        _ => unimplemented!("assign exception to non-local identifier"),
                    },
                    _ => unimplemented!("assign exception to non-variable"),
                }
            }
            Self::expand_statements(&rescue.body, out, scope, items)?;
            items.push(IROp::Jump(ensure_label));
            items.push(IROp::Label(end_label));
        }
        items.push(IROp::Label(else_label));
        if let Some(else_) = &body.else_ {
            Self::expand_statements(&else_, out, scope, items)?;
        }
        items.push(IROp::Label(ensure_label));
        if let Some(ensure) = &body.ensure {
            Self::expand_statements(&ensure, out, scope, items)?;
        }
        Ok(out)
    }

    fn expand_args(
        args: &Arguments,
        scope: &mut Scope,
        items: &mut Vec<IROp>,
    ) -> Result<(), IRError> {
        let mut arg_items = Vec::new();
        for item in args.items.iter().rev() {
            match item {
                Argument::Expr(expr) => {
                    let expr = Self::expand_expr(expr, scope, items)?;
                    arg_items.push(IROp::Arg(expr));
                }
                Argument::Splat(expr) => {
                    let expr = Self::expand_expr(expr, scope, items)?;
                    arg_items.push(IROp::ArgSplat(expr));
                }
            }
        }

        for (key, value) in &args.hash {
            let key = Self::expand_expr(key, scope, items)?;
            let value = Self::expand_expr(value, scope, items)?;
            arg_items.push(IROp::ArgAssoc(key, value));
        }

        if let Some(block) = &args.block {
            let block = Self::expand_expr(block, scope, items)?;
            arg_items.push(IROp::ArgBlock(block));
        }

        items.append(&mut arg_items);
        Ok(())
    }

    fn load_var(ident: &Ident, scope: &mut Scope, items: &mut Vec<IROp>) -> Result<Var, IRError> {
        match ident {
            Ident::Local(name) => {
                let name = scope.symbol(name);
                if let Some(var) = scope.local_var(name) {
                    Ok(var)
                } else {
                    let out = scope.next_var();
                    items.push(IROp::Call(out, Var::SelfRef, name));
                    Ok(out)
                }
            }
            Ident::Keyword(name) => {
                let name = scope.symbol(name);
                if let Some(var) = scope.local_var(name) {
                    Ok(var)
                } else {
                    let out = scope.next_var();
                    items.push(IROp::Call(out, Var::SelfRef, name));
                    Ok(out)
                }
            }
            Ident::Global(name) => {
                let name = scope.symbol(name);
                let out = scope.next_var();
                items.push(IROp::LoadGlobal(out, name));
                Ok(out)
            }
            Ident::Const(name) => {
                let name = scope.symbol(name);
                let out = scope.next_var();
                items.push(IROp::LoadConst(out, Var::SelfRef, name));
                Ok(out)
            }
            Ident::Class(name) => {
                let name = scope.symbol(name);
                let out = scope.next_var();
                items.push(IROp::LoadClassVar(out, name));
                Ok(out)
            }
            Ident::Instance(name) => {
                let name = scope.symbol(name);
                let out = scope.next_var();
                items.push(IROp::LoadIVar(out, name));
                Ok(out)
            }
            Ident::MethodOnly(name) | Ident::AssignmentMethod(name) => {
                let name = scope.symbol(name);
                let out = scope.next_var();
                items.push(IROp::Call(out, Var::SelfRef, name));
                Ok(out)
            }
        }
    }

    fn number_to_i64(positive: bool, value: &NumericValue) -> Option<i64> {
        macro_rules! checked {
            ($e:expr) => {
                match $e {
                    Some(e) => e,
                    None => return None,
                }
            };
        }

        let sign = if positive { 1 } else { -1 };

        match value {
            NumericValue::Binary(value) => {
                if value.len() > 63 {
                    return None;
                }

                let mut n: i64 = 0;
                for c in value.chars() {
                    n = checked!(n.checked_shl(1));
                    match c {
                        '0' => (),
                        '1' => n = checked!(n.checked_add(1)),
                        _ => return None,
                    }
                }

                n.checked_mul(sign)
            }
            NumericValue::Octal(value) => {
                // log_8(2^63) = 21
                if value.len() > 21 {
                    return None;
                }

                let mut n: i64 = 0;
                for c in value.chars() {
                    n = checked!(n.checked_shl(3));
                    match c {
                        '0' => (),
                        '1' => n = checked!(n.checked_add(1)),
                        '2' => n = checked!(n.checked_add(2)),
                        '3' => n = checked!(n.checked_add(3)),
                        '4' => n = checked!(n.checked_add(4)),
                        '5' => n = checked!(n.checked_add(5)),
                        '6' => n = checked!(n.checked_add(6)),
                        '7' => n = checked!(n.checked_add(7)),
                        _ => return None,
                    }
                }

                n.checked_mul(sign)
            }
            NumericValue::Decimal(value) => {
                // log_10(2^63) = 18.96
                if value.len() > 19 {
                    return None;
                }

                let mut n: i64 = 0;
                for c in value.chars() {
                    n = checked!(n.checked_mul(10));
                    match c {
                        '0' => (),
                        '1' => n = checked!(n.checked_add(1)),
                        '2' => n = checked!(n.checked_add(2)),
                        '3' => n = checked!(n.checked_add(3)),
                        '4' => n = checked!(n.checked_add(4)),
                        '5' => n = checked!(n.checked_add(5)),
                        '6' => n = checked!(n.checked_add(6)),
                        '7' => n = checked!(n.checked_add(7)),
                        '8' => n = checked!(n.checked_add(8)),
                        '9' => n = checked!(n.checked_add(9)),
                        _ => return None,
                    }
                }

                n.checked_mul(sign)
            }
            NumericValue::Hexadecimal(value) => {
                // log_16(2^63) = 15.75
                if value.len() > 16 {
                    return None;
                }

                let mut n: i64 = 0;
                for c in value.chars() {
                    n = checked!(n.checked_shl(4));
                    match c {
                        '0' => (),
                        '1' => n = checked!(n.checked_add(1)),
                        '2' => n = checked!(n.checked_add(2)),
                        '3' => n = checked!(n.checked_add(3)),
                        '4' => n = checked!(n.checked_add(4)),
                        '5' => n = checked!(n.checked_add(5)),
                        '6' => n = checked!(n.checked_add(6)),
                        '7' => n = checked!(n.checked_add(7)),
                        '8' => n = checked!(n.checked_add(8)),
                        '9' => n = checked!(n.checked_add(9)),
                        'a' | 'A' => n = checked!(n.checked_add(10)),
                        'b' | 'B' => n = checked!(n.checked_add(11)),
                        'c' | 'C' => n = checked!(n.checked_add(12)),
                        'd' | 'D' => n = checked!(n.checked_add(13)),
                        'e' | 'E' => n = checked!(n.checked_add(14)),
                        'f' | 'F' => n = checked!(n.checked_add(15)),
                        _ => return None,
                    }
                }

                n.checked_mul(sign)
            }
            NumericValue::Float { .. } => None,
        }
    }

    fn number_to_float(positive: bool, value: &NumericValue) -> Option<f64> {
        match value {
            NumericValue::Float {
                decimal: (integer_part, decimal_part),
                exp_positive,
                exp,
            } => {
                let mut n = 0.;

                for c in integer_part.chars() {
                    n *= 10.;
                    match c {
                        '0' => (),
                        '1' => n += 1.,
                        '2' => n += 2.,
                        '3' => n += 3.,
                        '4' => n += 4.,
                        '5' => n += 5.,
                        '6' => n += 6.,
                        '7' => n += 7.,
                        '8' => n += 8.,
                        '9' => n += 9.,
                        _ => return None,
                    }
                }

                for (i, c) in decimal_part.chars().enumerate() {
                    let scale = 10.0_f64.powf(-1. - (i as f64));
                    match c {
                        '0' => (),
                        '1' => n += 1. * scale,
                        '2' => n += 2. * scale,
                        '3' => n += 3. * scale,
                        '4' => n += 4. * scale,
                        '5' => n += 5. * scale,
                        '6' => n += 6. * scale,
                        '7' => n += 7. * scale,
                        '8' => n += 8. * scale,
                        '9' => n += 9. * scale,
                        _ => return None,
                    }
                }

                let mut e = 0.;
                for c in exp.chars() {
                    e *= 10.;
                    match c {
                        '0' => (),
                        '1' => e += 1.,
                        '2' => e += 2.,
                        '3' => e += 3.,
                        '4' => e += 4.,
                        '5' => e += 5.,
                        '6' => e += 6.,
                        '7' => e += 7.,
                        '8' => e += 8.,
                        '9' => e += 9.,
                        _ => return None,
                    }
                }
                e = 10.0_f64.powf(e);
                if !exp_positive {
                    e *= -1.;
                }

                n *= e;

                if !positive {
                    n *= -1.;
                }

                Some(n)
            }
            _ => None,
        }
    }

    pub fn fmt_with_symbols(&self, symbols: &Symbols) -> String {
        use std::fmt::Write;

        let mut f = String::from("Proc {\n");
        for (k, v) in &self.variables {
            let name = symbols.symbol_name(*v).unwrap_or("?");
            if self.captured_vars.contains(k) {
                write!(f, "    (local variable) {} -> {} (captured)\n", k, name).unwrap();
            } else {
                write!(f, "    (local variable) {} -> {}\n", k, name).unwrap();
            }
        }
        write!(f, "    may be captured: {}\n", self.may_be_captured).unwrap();

        for item in &self.items {
            for line in item.fmt_with_symbols(symbols).lines() {
                write!(f, "    {}\n", line).unwrap();
            }
        }
        write!(f, "}}").unwrap();
        f
    }
}