//! Intermediate representation.

use crate::symbol::{Symbol, Symbols};
use fnv::FnvHashMap;
use sapphire_parser::ast::*;
use std::{fmt, iter, mem};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Var(usize);

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "var_{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Label(usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

struct Scope<'a> {
    parent: Option<&'a mut Scope<'a>>,
    variables: &'a mut FnvHashMap<Var, Symbol>,
    symbols: Option<&'a mut Symbols>,
    may_be_captured: &'a mut bool,
    var_counter: usize,
}

impl<'a> Scope<'a> {
    /// Safety: 'b must be shorter than 'a
    unsafe fn sub_scope<'b>(
        &mut self,
        variables: &'b mut FnvHashMap<Var, Symbol>,
        may_be_captured: &'b mut bool,
    ) -> Scope<'b> {
        let borrowck_go_away = mem::transmute::<&mut Scope, &mut Scope>(self);
        Scope {
            parent: Some(borrowck_go_away),
            variables,
            symbols: None,
            may_be_captured,
            var_counter: 0,
        }
    }

    fn may_be_captured(&mut self) {
        *self.may_be_captured = true;
    }

    fn symbol(&mut self, symbol: &str) -> Symbol {
        if let Some(symbols) = &mut self.symbols {
            symbols.symbol(symbol)
        } else {
            self.parent.as_mut().unwrap().symbol(symbol)
        }
    }

    fn symbols(&mut self) -> &mut Symbols {
        if let Some(symbols) = &mut self.symbols {
            symbols
        } else {
            self.parent.as_mut().unwrap().symbols()
        }
    }

    fn next_var(&mut self) -> Var {
        self.var_counter += 1;
        Var(self.var_counter)
    }

    fn next_label(&mut self) -> Label {
        self.var_counter += 1;
        Label(self.var_counter)
    }

    fn define_local_var(&mut self, name: Symbol) -> Var {
        if let Some(var) = self.local_var(name) {
            var
        } else {
            let var = self.next_var();
            self.variables.insert(var, name);
            var
        }
    }

    fn local_var(&self, name: Symbol) -> Option<Var> {
        let var = self
            .variables
            .iter()
            .find(|(_, v)| **v == name)
            .map(|(k, _)| *k);

        match var {
            Some(var) => Some(var),
            None => self
                .parent
                .as_ref()
                .map_or(None, |parent| parent.local_var(name)),
        }
    }
}

#[derive(Debug)]
pub enum IRError {
    InvalidConstPath(Ident),
    InvalidMethodName(Ident),
    InvalidModuleName(Ident),
    InvalidDefined,
    TooManySplats,
}

#[derive(Debug)]
enum IROp {
    /// Loads the root (::) into the local variable.
    LoadRoot(Var),
    /// Loads self into the local variable.
    LoadSelf(Var),
    /// Loads a boolean value into the local variable.
    LoadBool(Var, bool),
    /// Loads a reference to a global variable into the local variable.
    LoadGlobal(Var, Symbol),
    /// Loads a reference to a constant into the local variable.
    LoadConst(Var, Option<Var>, Symbol),
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
    /// Loads a proc into the local variable.
    LoadProc(Var, Proc),
    /// Pushes a positional argument.
    Arg(Var),
    /// Pushes an associative argument.
    ArgAssoc(Var, Var),
    /// Pushes a splat argument.
    ArgSplat(Var),
    /// Pushes a block argument.
    ArgBlock(Var),
    /// Calls a method and loads the result into a local variable.
    Call(Var, Option<Var>, Symbol),
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
    /// Start of a rescuable section.
    BeginRescue(Label),
    /// Jumps to the label if the current exception matches.
    RescueMatch(Var, Label),
    /// Binds the exception to a variable.
    RescueBind(Var),
    /// End of a rescuable section.
    EndRescue,
    /// `defined?` for a local variable.
    DefinedLocal(Var, Symbol),
    /// `defined?` for a constant variable.
    DefinedConst(Var, Symbol),
    /// `defined?` for a global variable.
    DefinedGlobal(Var, Symbol),
    /// `defined?` for a class variable.
    DefinedClassVar(Var, Symbol),
    /// `defined?` for an instance variable.
    DefinedIVar(Var, Symbol),
    /// `break`
    Break,
    /// `yield`
    Yield,
    /// `next`
    Next,
    /// `redo`
    Redo,
    /// `retry`
    Retry,
    /// Defines a module.
    DefModule(Option<Var>, Symbol, Proc),
    /// Defines a class.
    DefClass(Option<Var>, Symbol, Option<Var>, Proc),
    /// Defines a method.
    // TODO: params?
    DefMethod(Symbol, Proc),
}

impl IROp {
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
            IROp::LoadSelf(var) => format!("{} = self;", var),
            IROp::LoadBool(var, value) => format!("{} = {};", var, value),
            IROp::LoadGlobal(var, name) => format!("{} = ${};", var, sym!(name)),
            IROp::LoadConst(var, parent, name) => match parent {
                Some(parent) => format!("{} = {}::{};", var, parent, sym!(name)),
                None => format!("{} = {} (const);", var, sym!(name)),
            },
            IROp::LoadClassVar(var, name) => format!("{} = @@{};", var, sym!(name)),
            IROp::LoadIVar(var, name) => format!("{} = @{};", var, sym!(name)),
            IROp::LoadString(var, string) => format!("{} = {:?};", var, string),
            IROp::AppendString(var, other) => format!("{} += {}.to_s;", var, other),
            IROp::LoadSymbol(var, symbol) => format!("{} = :{};", var, sym!(symbol)),
            IROp::LoadI64(var, value) => format!("{} = {};", var, value),
            IROp::LoadProc(var, proc) => format!("{} = {};", var, proc.fmt_with_symbols(symbols)),
            IROp::Arg(var) => format!("push_arg {};", var),
            IROp::ArgAssoc(key, val) => format!("push_arg {} => {};", key, val),
            IROp::ArgSplat(var) => format!("push_arg *{};", var),
            IROp::ArgBlock(var) => format!("push_arg &{};", var),
            IROp::Call(out, recv, name) => match recv {
                Some(recv) => format!("{} = {}.{}();", out, recv, sym!(name)),
                None => format!("{} = {}();", out, sym!(name)),
            },
            IROp::Super(out) => format!("{} = super();", out),
            IROp::Not(out, var) => format!("{} = not {};", out, var),
            IROp::Label(label) => format!("{}", label),
            IROp::Jump(label) => format!("jump -> {};", label),
            IROp::JumpIf(cond, label) => format!("if {} jump -> {};", cond, label),
            IROp::JumpIfNot(cond, label) => format!("if not {} jump -> {};", cond, label),
            IROp::Return(var) => format!("return {};", var),
            IROp::Assign(lhs, rhs) => format!("{} = {};", lhs, rhs),
            IROp::BeginRescue(label) => format!("begin rescue (rescue -> {})", label),
            IROp::RescueMatch(class, label) => format!("rescue instanceof {} -> {}", class, label),
            IROp::RescueBind(var) => format!("rescue => {};", var),
            IROp::EndRescue => format!("end rescue"),
            IROp::DefinedLocal(var, name) => format!("{} = defined? {};", var, sym!(name)),
            IROp::DefinedConst(var, name) => format!("{} = defined? {};", var, sym!(name)),
            IROp::DefinedGlobal(var, name) => format!("{} = defined? ${};", var, sym!(name)),
            IROp::DefinedClassVar(var, name) => format!("{} = defined? @@{};", var, sym!(name)),
            IROp::DefinedIVar(var, name) => format!("{} = defined? ${};", var, sym!(name)),
            IROp::Break => format!("break();"),
            IROp::Yield => format!("yield();"),
            IROp::Next => format!("next();"),
            IROp::Redo => format!("redo;"),
            IROp::Retry => format!("retry;"),
            IROp::DefModule(parent, name, proc) => match parent {
                Some(parent) => format!(
                    "module {}::{}: {}",
                    parent,
                    sym!(name),
                    proc.fmt_with_symbols(symbols)
                ),
                None => format!("module {}: {}", sym!(name), proc.fmt_with_symbols(symbols)),
            },
            IROp::DefClass(parent, name, superclass, proc) => match (parent, superclass) {
                (Some(parent), Some(superclass)) => format!(
                    "class {}::{} < {}: {}",
                    parent,
                    sym!(name),
                    superclass,
                    proc.fmt_with_symbols(symbols)
                ),
                (Some(parent), None) => format!(
                    "class {}::{}: {}",
                    parent,
                    sym!(name),
                    proc.fmt_with_symbols(symbols)
                ),
                (None, Some(superclass)) => format!(
                    "class {} < {}: {}",
                    sym!(name),
                    superclass,
                    proc.fmt_with_symbols(symbols)
                ),
                (None, None) => {
                    format!("module {}: {}", sym!(name), proc.fmt_with_symbols(symbols))
                }
            },
            IROp::DefMethod(name, proc) => {
                format!("def {}: {}", sym!(name), proc.fmt_with_symbols(symbols))
            }
        }
    }
}

// TODO: convert things like `a = b; c = a;` into `c = b`

#[derive(Debug)]
pub struct Proc {
    variables: FnvHashMap<Var, Symbol>,
    may_be_captured: bool,
    items: Vec<IROp>,
}

impl Proc {
    pub fn new(statements: &[Statement], symbols: &mut Symbols) -> Result<Proc, IRError> {
        let mut variables = FnvHashMap::default();
        let mut items = Vec::new();
        let mut may_be_captured = false;

        let mut scope = Scope {
            parent: None,
            variables: &mut variables,
            symbols: Some(symbols),
            may_be_captured: &mut may_be_captured,
            var_counter: 0,
        };

        let out = scope.next_var();
        Self::expand_statements(statements, out, &mut scope, &mut items)?;
        items.push(IROp::Return(out));

        Ok(Proc {
            variables,
            may_be_captured,
            items,
        })
    }

    fn new_with_body(body: &BodyStatement, symbols: &mut Symbols) -> Result<Proc, IRError> {
        let mut variables = FnvHashMap::default();
        let mut items = Vec::new();
        let mut may_be_captured = false;

        let mut scope = Scope {
            parent: None,
            variables: &mut variables,
            symbols: Some(symbols),
            may_be_captured: &mut may_be_captured,
            var_counter: 0,
        };

        let out = Self::expand_body_statement(body, &mut scope, &mut items)?;
        items.push(IROp::Return(out));

        Ok(Proc {
            variables,
            may_be_captured,
            items,
        })
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
                    items.push(IROp::LoadConst(out, Some(out), name));
                    Ok(out)
                }
                _ => Err(IRError::InvalidConstPath(ident.clone())),
            },
            Expression::SubConst(expr, ident) => match ident {
                Ident::Const(name) => {
                    let name = scope.symbol(name);
                    let expr = Self::expand_expr(expr, scope, items)?;
                    let out = scope.next_var();
                    items.push(IROp::LoadConst(out, Some(expr), name));
                    Ok(out)
                }
                _ => Err(IRError::InvalidConstPath(ident.clone())),
            },
            Expression::Literal(literal) => {
                let out = scope.next_var();
                match literal {
                    Literal::Number {
                        positive,
                        value,
                    } => {
                        if let Some(number) = Self::number_to_i64(*positive, value) {
                            items.push(IROp::LoadI64(out, number));
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

                let mut variables = FnvHashMap::default();
                let mut block_items = Vec::new();
                let mut may_be_captured = false;

                let mut block_scope =
                    unsafe { scope.sub_scope(&mut variables, &mut may_be_captured) };
                let out = block_scope.next_var();
                Self::expand_statements(&block.body, out, &mut block_scope, &mut block_items)?;
                block_items.push(IROp::Return(out));

                let proc = Proc {
                    variables,
                    may_be_captured,
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
            Expression::SelfExpr => {
                let out = scope.next_var();
                items.push(IROp::LoadSelf(out));
                Ok(out)
            }
            Expression::True | Expression::False => {
                let is_true = expr == &Expression::True;
                let out = scope.next_var();
                items.push(IROp::LoadBool(out, is_true));
                Ok(out)
            }
            Expression::ArrayConstructor(args) => {
                let out = scope.next_var();
                items.push(IROp::LoadRoot(out));
                items.push(IROp::LoadConst(out, Some(out), scope.symbol("Array")));
                if let Some(args) = args {
                    Self::expand_args(args, scope, items)?;
                }
                items.push(IROp::Call(out, Some(out), scope.symbol("new")));
                Ok(out)
            }
            Expression::HashConstructor(args) => {
                let out = scope.next_var();
                items.push(IROp::LoadRoot(out));
                items.push(IROp::LoadConst(out, Some(out), scope.symbol("Hash")));
                Self::expand_args(
                    &Arguments {
                        block: None,
                        hash: args.to_vec(),
                        items: Vec::new(),
                    },
                    scope,
                    items,
                )?;
                items.push(IROp::Call(out, Some(out), scope.symbol("new")));
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
                items.push(IROp::Call(out, Some(out), scope.symbol("-@")));
                Ok(out)
            }
            Expression::UPlus(expr) => {
                let out = Self::expand_expr(expr, scope, items)?;
                items.push(IROp::Call(out, Some(out), scope.symbol("+@")));
                Ok(out)
            }
            Expression::BitInv(expr) => {
                let out = Self::expand_expr(expr, scope, items)?;
                items.push(IROp::Call(out, Some(out), scope.symbol("~")));
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

                items.push(IROp::Call(lhs, Some(lhs), scope.symbol(op)));
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
                            Some(counter),
                            if *inclusive {
                                scope.symbol("<=")
                            } else {
                                scope.symbol("<")
                            },
                        ));
                        items.push(IROp::JumpIfNot(loop_cond, end_label));
                        Self::expand_statements(body, out, scope, items)?;
                        items.push(IROp::Arg(one));
                        items.push(IROp::Call(out, Some(counter), scope.symbol("+=")));
                        items.push(IROp::Jump(top_label));
                        items.push(IROp::Label(end_label));
                    }
                    Ok(scope.next_var())
                } else {
                    let mut block_items = Vec::new();

                    // TODO: assign block params

                    let out = scope.next_var();
                    Self::expand_statements(body, out, scope, &mut block_items)?;

                    let block = Proc {
                        items: block_items,
                        may_be_captured: false,
                        variables: FnvHashMap::default(),
                    };

                    let expr = Self::expand_expr(expr, scope, items)?;
                    let block_var = scope.next_var();
                    items.push(IROp::LoadProc(block_var, block));
                    items.push(IROp::ArgBlock(block_var));
                    items.push(IROp::Call(expr, Some(expr), scope.symbol("each")));

                    Ok(scope.next_var())
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
                    items.push(IROp::Call(out, Some(out), name));
                    Ok(out)
                } else {
                    Self::expand_args(args, scope, items)?;
                    let out = scope.next_var();
                    match name {
                        Ident::Local(s) | Ident::MethodOnly(s) | Ident::AssignmentMethod(s) => {
                            items.push(IROp::Call(out, None, scope.symbol(s)));
                        }
                        Ident::Keyword(s) => {
                            items.push(IROp::Call(out, None, scope.symbol(s)));
                        }
                        Ident::Global(name) => {
                            items.push(IROp::LoadGlobal(out, scope.symbol(name)));
                            items.push(IROp::Call(out, Some(out), scope.symbol("call")));
                        }
                        Ident::Const(name) => {
                            items.push(IROp::LoadConst(out, None, scope.symbol(name)));
                            items.push(IROp::Call(out, Some(out), scope.symbol("call")));
                        }
                        Ident::Class(name) => {
                            items.push(IROp::LoadClassVar(out, scope.symbol(name)));
                            items.push(IROp::Call(out, Some(out), scope.symbol("call")));
                        }
                        Ident::Instance(name) => {
                            items.push(IROp::LoadIVar(out, scope.symbol(name)));
                            items.push(IROp::Call(out, Some(out), scope.symbol("call")));
                        }
                    };
                    Ok(out)
                }
            }
            Expression::Index(expr, args) => {
                let expr = Self::expand_expr(expr, scope, items)?;
                Self::expand_args(args, scope, items)?;
                let out = scope.next_var();
                items.push(IROp::Call(out, Some(expr), scope.symbol("[]")));
                Ok(out)
            }
            Expression::Defined(expr) => match &**expr {
                Expression::Variable(ident) => match ident {
                    Ident::Local(name) => {
                        let name = scope.symbol(name);
                        let out = scope.next_var();
                        items.push(IROp::DefinedLocal(out, name));
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
            Expression::Break(args) => {
                if let Some(_args) = args {
                    unimplemented!("break with args")
                } else {
                    items.push(IROp::Break);
                    Ok(scope.next_var())
                }
            }
            Expression::Yield(args) => {
                if let Some(_args) = args {
                    unimplemented!("yield with args")
                } else {
                    items.push(IROp::Yield);
                    Ok(scope.next_var())
                }
            }
            Expression::Next(args) => {
                if let Some(_args) = args {
                    unimplemented!("next with args")
                } else {
                    items.push(IROp::Next);
                    Ok(scope.next_var())
                }
            }
            Expression::Redo => {
                items.push(IROp::Redo);
                Ok(scope.next_var())
            }
            Expression::Retry => {
                items.push(IROp::Retry);
                Ok(scope.next_var())
            }
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
                items.push(IROp::LoadConst(out, Some(out), scope.symbol("Range")));
                items.push(IROp::Arg(start));
                items.push(IROp::Arg(end));
                items.push(IROp::Arg(incl));
                items.push(IROp::Call(out, Some(out), scope.symbol("new")));
                Ok(out)
            }
            Expression::Module { path, body } => {
                let body = Proc::new_with_body(body, scope.symbols())?;
                match path {
                    DefPath::Member(expr, name) => {
                        let expr = Self::expand_expr(expr, scope, items)?;
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        items.push(IROp::DefModule(Some(expr), name, body));
                    }
                    DefPath::Root(name) => {
                        let root = scope.next_var();
                        items.push(IROp::LoadRoot(root));
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        items.push(IROp::DefModule(Some(root), name, body));
                    }
                    DefPath::Current(name) => {
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        items.push(IROp::DefModule(None, name, body));
                    }
                }
                Ok(scope.next_var())
            }
            Expression::Class {
                path,
                superclass,
                body,
            } => {
                let body = Proc::new_with_body(body, scope.symbols())?;
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
                        items.push(IROp::DefClass(Some(expr), name, superclass, body));
                    }
                    DefPath::Root(name) => {
                        let root = scope.next_var();
                        items.push(IROp::LoadRoot(root));
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        items.push(IROp::DefClass(Some(root), name, superclass, body));
                    }
                    DefPath::Current(name) => {
                        let name = scope.symbol(match name {
                            Ident::Const(name) => name,
                            name => return Err(IRError::InvalidModuleName(name.clone())),
                        });
                        items.push(IROp::DefClass(None, name, superclass, body));
                    }
                }
                Ok(scope.next_var())
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

                let body = Proc::new_with_body(body, scope.symbols())?;
                items.push(IROp::DefMethod(name, body));

                Ok(scope.next_var())
            }
            _ => unimplemented!("expr"),
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
                    items.push(IROp::Call(out, None, name));
                    Ok(out)
                }
            }
            Ident::Keyword(name) => {
                let name = scope.symbol(name);
                if let Some(var) = scope.local_var(name) {
                    Ok(var)
                } else {
                    let out = scope.next_var();
                    items.push(IROp::Call(out, None, name));
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
                items.push(IROp::LoadConst(out, None, name));
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
                items.push(IROp::Call(out, None, name));
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

    pub fn fmt_with_symbols(&self, symbols: &Symbols) -> String {
        use std::fmt::Write;

        let mut f = String::from("Proc {\n");
        for (k, v) in &self.variables {
            write!(
                f,
                "    (local variable) {} -> {}\n",
                k,
                symbols.symbol_name(*v).unwrap_or("?")
            )
            .unwrap();
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
