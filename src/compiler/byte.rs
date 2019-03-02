use crate::compiler::ir::{IROp, IRProc, Var};
use crate::proc::{AddressingMode, Op, Proc, Static, NIL, PARENT, RESERVED_REGISTERS, SELF, VOID};
use fnv::FnvHashMap;
use std::sync::Arc;

impl Into<Proc> for IRProc {
    fn into(mut self) -> Proc {
        let mut code = Vec::with_capacity(self.items.len() * 2);
        let mut registers = FnvHashMap::default();
        let mut labels = FnvHashMap::default();

        let mut register_count = RESERVED_REGISTERS;
        let mut i = 0;
        for op in &mut self.items {
            op.for_each_var(|var, _| match var {
                Var::Local(i) => {
                    registers.entry(*i).or_insert_with(|| {
                        let i = register_count;
                        register_count += 1;
                        i
                    });
                }
                Var::Parent(..) | Var::Nil | Var::Void | Var::SelfRef => (),
            });

            match op {
                IROp::Label(label) => {
                    labels.insert(*label, i);
                }
                _ => i += 1,
            }
        }

        let mode = if register_count > 256 || self.items.len() > 256 {
            AddressingMode::U16
        } else {
            AddressingMode::U8
        };

        let mut statics = Vec::new();

        macro_rules! push_addr {
            ($v:expr) => {
                match mode {
                    AddressingMode::U8 => code.push($v as u8),
                    AddressingMode::U16 => {
                        let bytes = ($v as u16).to_ne_bytes();
                        code.push(bytes[0]);
                        code.push(bytes[1]);
                    }
                }
            };
        }

        macro_rules! addr_var {
            ($var:expr) => {
                match $var {
                    Var::Nil => push_addr!(NIL),
                    Var::Void => push_addr!(VOID),
                    Var::SelfRef => push_addr!(SELF),
                    Var::Local(i) => push_addr!(registers[&i]),
                    Var::Parent(i, depth) => {
                        for _ in 0..depth {
                            push_addr!(PARENT);
                        }
                        push_addr!(i);
                    }
                }
            };
        }

        macro_rules! addr_static {
            ($val:expr) => {
                let val = $val;
                if let Some(pos) = statics.iter().position(|v| v == &val) {
                    push_addr!(pos);
                } else {
                    push_addr!(statics.len());
                    statics.push(val);
                }
            };
        }

        for op in self.items {
            match op {
                IROp::LoadRoot(var) => {
                    code.push(Op::LOAD_ROOT);
                    addr_var!(var);
                }
                IROp::LoadBool(var, b) => {
                    if b {
                        code.push(Op::LOAD_TRUE);
                    } else {
                        code.push(Op::LOAD_FALSE);
                    }
                    addr_var!(var);
                }
                IROp::LoadGlobal(var, name) => {
                    code.push(Op::LOAD_GLOBAL);
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadConst(var, parent, name) => {
                    code.push(Op::LOAD_CONST);
                    addr_var!(var);
                    addr_var!(parent);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadClassVar(var, name) => {
                    code.push(Op::LOAD_CLASS_VAR);
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadIVar(var, name) => {
                    code.push(Op::LOAD_IVAR);
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadString(var, string) => {
                    code.push(Op::LOAD_STRING);
                    addr_var!(var);
                    addr_static!(Static::Str(string));
                }
                IROp::AppendString(out, var) => {
                    code.push(Op::APPEND_STRING);
                    addr_var!(out);
                    addr_var!(var);
                }
                IROp::LoadSymbol(out, sym) => {
                    code.push(Op::LOAD_SYMBOL);
                    addr_var!(out);
                    addr_static!(Static::Sym(sym));
                }
                IROp::LoadI64(out, val) => {
                    code.push(Op::LOAD_I64);
                    addr_var!(out);
                    addr_static!(Static::Int(val));
                }
                IROp::LoadFloat(out, val) => {
                    code.push(Op::LOAD_FLOAT);
                    addr_var!(out);
                    addr_static!(Static::Float(val));
                }
                IROp::LoadProc(out, proc) => {
                    code.push(Op::LOAD_PROC);
                    addr_var!(out);
                    addr_static!(Static::Proc(Arc::new(proc.into())));
                }
                IROp::Arg(var) => {
                    code.push(Op::ARG);
                    addr_var!(var);
                }
                IROp::ArgAssoc(key, value) => {
                    code.push(Op::ARG_ASSOC);
                    addr_var!(key);
                    addr_var!(value);
                }
                IROp::ArgSplat(var) => {
                    code.push(Op::ARG_SPLAT);
                    addr_var!(var);
                }
                IROp::ArgBlock(var) => {
                    code.push(Op::ARG_BLOCK);
                    addr_var!(var);
                }
                IROp::Call(var, recv, name) => {
                    code.push(Op::CALL);
                    addr_var!(var);
                    addr_var!(recv);
                    addr_static!(Static::Sym(name));
                }
                IROp::Super(var) => {
                    code.push(Op::SUPER);
                    addr_var!(var);
                }
                IROp::Not(out, value) => {
                    code.push(Op::NOT);
                    addr_var!(out);
                    addr_var!(value);
                }
                IROp::Label(_) => (),
                IROp::Jump(label) => {
                    code.push(Op::JUMP);
                    push_addr!(labels[&label]);
                }
                IROp::JumpIf(cond, label) => {
                    code.push(Op::JUMP_IF);
                    addr_var!(cond);
                    push_addr!(labels[&label]);
                }
                IROp::JumpIfNot(cond, label) => {
                    code.push(Op::JUMP_IF_NOT);
                    addr_var!(cond);
                    push_addr!(labels[&label]);
                }
                IROp::Return(var) => {
                    code.push(Op::RETURN);
                    addr_var!(var);
                }
                IROp::Assign(lhs, rhs) => {
                    code.push(Op::ASSIGN);
                    addr_var!(lhs);
                    addr_var!(rhs);
                }
                IROp::AssignGlobal(name, rhs) => {
                    code.push(Op::ASSIGN_GLOBAL);
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::AssignConst(name, rhs) => {
                    code.push(Op::ASSIGN_CONST);
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::AssignClassVar(name, rhs) => {
                    code.push(Op::ASSIGN_CLASS_VAR);
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::AssignIVar(name, rhs) => {
                    code.push(Op::ASSIGN_IVAR);
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::BeginRescue(label) => {
                    code.push(Op::BEGIN_RESCUE);
                    push_addr!(labels[&label]);
                }
                IROp::RescueMatch(class, label) => {
                    code.push(Op::RESCUE_MATCH);
                    addr_var!(class);
                    push_addr!(labels[&label]);
                }
                IROp::RescueBind(var) => {
                    code.push(Op::RESCUE_BIND);
                    addr_var!(var);
                }
                IROp::EndRescue => {
                    code.push(Op::END_RESCUE);
                }
                IROp::DefinedConst(var, name) => {
                    code.push(Op::DEFINED_CONST);
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::DefinedGlobal(var, name) => {
                    code.push(Op::DEFINED_GLOBAL);
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::DefinedClassVar(var, name) => {
                    code.push(Op::DEFINED_CLASS_VAR);
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::DefinedIVar(var, name) => {
                    code.push(Op::DEFINED_IVAR);
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::Yield(var) => {
                    code.push(Op::YIELD);
                    addr_var!(var);
                }
                IROp::DefModule(parent, name, proc) => {
                    code.push(Op::DEF_MODULE);
                    addr_var!(parent);
                    addr_static!(Static::Sym(name));
                    addr_static!(Static::Proc(Arc::new(proc.into())));
                }
                IROp::DefClass(parent, name, superclass, proc) => {
                    code.push(Op::DEF_CLASS);
                    addr_var!(parent);
                    addr_static!(Static::Sym(name));
                    addr_var!(superclass.unwrap_or(Var::Nil));
                    addr_static!(Static::Proc(Arc::new(proc.into())));
                }
                IROp::DefMethod(name, proc) => {
                    code.push(Op::DEF_METHOD);
                    addr_static!(Static::Sym(name));
                    addr_static!(Static::Proc(Arc::new(proc.into())));
                }
                IROp::DefSingletonClass(expr, proc) => {
                    code.push(Op::DEF_SINGLETON_CLASS);
                    addr_var!(expr);
                    addr_static!(Static::Proc(Arc::new(proc.into())));
                }
                IROp::DefSingletonMethod(expr, name, proc) => {
                    code.push(Op::DEF_SINGLETON_METHOD);
                    addr_var!(expr);
                    addr_static!(Static::Sym(name));
                    addr_static!(Static::Proc(Arc::new(proc.into())));
                }
            }
        }

        Proc {
            name: self.name,
            registers: register_count,
            statics,
            mode,
            code,
        }
    }
}
