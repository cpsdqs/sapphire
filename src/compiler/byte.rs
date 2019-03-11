use crate::compiler::ir::{IROp, IRParamType, IRProc, Var};
use crate::proc::*;
use fnv::FnvHashMap;
use smallvec::SmallVec;
use std::sync::Arc;

#[derive(Debug)]
enum Chunk {
    Byte(u8),
    Label(usize),
}

impl Into<Proc> for IRProc {
    fn into(self) -> Proc {
        self.into_proc(&Vec::new())
    }
}

impl IRProc {
    fn into_proc(mut self, parent_vars: &Vec<FnvHashMap<usize, usize>>) -> Proc {
        let mut code = Vec::with_capacity(self.items.len());
        let mut registers = FnvHashMap::default();
        let mut labels = FnvHashMap::default();

        let mut register_count = RESERVED_REGISTERS;
        for (i, op) in self.items.iter_mut().enumerate() {
            op.for_each_var(|var, _| match var {
                Var::Local(i) => {
                    registers.entry(*i).or_insert_with(|| {
                        let i = register_count;
                        register_count += 1;
                        i
                    });
                }
                Var::Nil | Var::Void | Var::SelfRef => (),
            });

            match op {
                IROp::Label(label) => {
                    labels.insert(*label, i);
                }
                _ => (),
            }
        }

        let mode = if register_count > 256 || self.items.len() > 256 {
            AddressingMode::U16
        } else {
            AddressingMode::U8
        };

        let mut next_parent_vars = parent_vars.clone();
        next_parent_vars.push(registers.clone());

        let mut statics = Vec::new();
        let mut op_offsets = Vec::new();
        let mut code_len = 0;

        macro_rules! push_addr {
            ($v:expr) => {{
                match mode {
                    AddressingMode::U8 => code.push(Chunk::Byte($v as u8)),
                    AddressingMode::U16 => {
                        let bytes = ($v as u16).to_ne_bytes();
                        code.push(Chunk::Byte(bytes[0]));
                        code.push(Chunk::Byte(bytes[1]));
                    }
                }
                code_len += mode.addr_len();
            }};
        }

        macro_rules! addr_var {
            ($var:expr) => {
                match $var {
                    Var::Nil => push_addr!(NIL),
                    Var::Void => push_addr!(VOID),
                    Var::SelfRef => push_addr!(SELF),
                    Var::Local(i) => push_addr!(registers[&i]),
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

        macro_rules! push_label {
            ($val:expr) => {
                code.push(Chunk::Label(labels[&$val]));
                code_len += mode.addr_len();
            };
        }

        for op in self.items {
            op_offsets.push(code_len);
            code_len += 1;
            match op {
                IROp::LoadRoot(var) => {
                    code.push(Chunk::Byte(Op::LOAD_ROOT));
                    addr_var!(var);
                }
                IROp::LoadBool(var, b) => {
                    if b {
                        code.push(Chunk::Byte(Op::LOAD_TRUE));
                    } else {
                        code.push(Chunk::Byte(Op::LOAD_FALSE));
                    }
                    addr_var!(var);
                }
                IROp::LoadGlobal(var, name) => {
                    code.push(Chunk::Byte(Op::LOAD_GLOBAL));
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadConst(var, parent, name) => {
                    code.push(Chunk::Byte(Op::LOAD_CONST));
                    addr_var!(var);
                    addr_var!(parent);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadClassVar(var, name) => {
                    code.push(Chunk::Byte(Op::LOAD_CLASS_VAR));
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadIVar(var, name) => {
                    code.push(Chunk::Byte(Op::LOAD_IVAR));
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::LoadString(var, string) => {
                    code.push(Chunk::Byte(Op::LOAD_STRING));
                    addr_var!(var);
                    addr_static!(Static::Str(string));
                }
                IROp::AppendString(out, var) => {
                    code.push(Chunk::Byte(Op::APPEND_STRING));
                    addr_var!(out);
                    addr_var!(var);
                }
                IROp::LoadSymbol(out, sym) => {
                    code.push(Chunk::Byte(Op::LOAD_SYMBOL));
                    addr_var!(out);
                    addr_static!(Static::Sym(sym));
                }
                IROp::LoadI64(out, val) => {
                    code.push(Chunk::Byte(Op::LOAD_I64));
                    addr_var!(out);
                    addr_static!(Static::Int(val));
                }
                IROp::LoadFloat(out, val) => {
                    code.push(Chunk::Byte(Op::LOAD_FLOAT));
                    addr_var!(out);
                    addr_static!(Static::Float(val));
                }
                IROp::LoadBlock(out, proc) => {
                    code.push(Chunk::Byte(Op::LOAD_BLOCK));
                    addr_var!(out);
                    addr_static!(Static::Proc(Arc::new(proc.into_proc(&next_parent_vars))));
                }
                IROp::LoadParent(out, id, depth) => {
                    code.push(Chunk::Byte(Op::LOAD_PARENT));
                    addr_var!(out);
                    // always u16 followed by u8
                    let addr = (parent_vars[parent_vars.len() - depth][&id] as u16).to_ne_bytes();
                    code.push(Chunk::Byte(addr[0]));
                    code.push(Chunk::Byte(addr[1]));
                    code.push(Chunk::Byte(depth as u8));
                }
                IROp::Arg(var) => {
                    code.push(Chunk::Byte(Op::ARG));
                    addr_var!(var);
                }
                IROp::ArgAssoc(key, value) => {
                    code.push(Chunk::Byte(Op::ARG_ASSOC));
                    addr_var!(key);
                    addr_var!(value);
                }
                IROp::ArgBlock(var) => {
                    code.push(Chunk::Byte(Op::ARG_BLOCK));
                    addr_var!(var);
                }
                IROp::Call(var, recv, name) => {
                    code.push(Chunk::Byte(Op::CALL));
                    addr_var!(var);
                    addr_var!(recv);
                    addr_static!(Static::Sym(name));
                }
                IROp::Super(var) => {
                    code.push(Chunk::Byte(Op::SUPER));
                    addr_var!(var);
                }
                IROp::Not(out, value) => {
                    code.push(Chunk::Byte(Op::NOT));
                    addr_var!(out);
                    addr_var!(value);
                }
                IROp::Jump(label) => {
                    code.push(Chunk::Byte(Op::JUMP));
                    push_label!(label);
                }
                IROp::JumpIf(cond, label) => {
                    code.push(Chunk::Byte(Op::JUMP_IF));
                    addr_var!(cond);
                    push_label!(label);
                }
                IROp::JumpIfNot(cond, label) => {
                    code.push(Chunk::Byte(Op::JUMP_IF_NOT));
                    addr_var!(cond);
                    push_label!(label);
                }
                IROp::Return(var) => {
                    code.push(Chunk::Byte(Op::RETURN));
                    addr_var!(var);
                }
                IROp::Assign(lhs, rhs) => {
                    code.push(Chunk::Byte(Op::ASSIGN));
                    addr_var!(lhs);
                    addr_var!(rhs);
                }
                IROp::AssignGlobal(name, rhs) => {
                    code.push(Chunk::Byte(Op::ASSIGN_GLOBAL));
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::AssignConst(name, rhs) => {
                    code.push(Chunk::Byte(Op::ASSIGN_CONST));
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::AssignClassVar(name, rhs) => {
                    code.push(Chunk::Byte(Op::ASSIGN_CLASS_VAR));
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::AssignIVar(name, rhs) => {
                    code.push(Chunk::Byte(Op::ASSIGN_IVAR));
                    addr_static!(Static::Sym(name));
                    addr_var!(rhs);
                }
                IROp::AssignParent(id, depth, rhs) => {
                    code.push(Chunk::Byte(Op::ASSIGN_PARENT));
                    // always u16 followed by u8
                    let addr = (parent_vars[parent_vars.len() - depth][&id] as u16).to_ne_bytes();
                    code.push(Chunk::Byte(addr[0]));
                    code.push(Chunk::Byte(addr[1]));
                    code.push(Chunk::Byte(depth as u8));
                    addr_var!(rhs);
                }
                IROp::BeginRescue(label) => {
                    code.push(Chunk::Byte(Op::BEGIN_RESCUE));
                    push_label!(label);
                }
                IROp::RescueMatch(class, label) => {
                    code.push(Chunk::Byte(Op::RESCUE_MATCH));
                    addr_var!(class);
                    push_label!(label);
                }
                IROp::RescueBind(var) => {
                    code.push(Chunk::Byte(Op::RESCUE_BIND));
                    addr_var!(var);
                }
                IROp::ContinueRescue => code.push(Chunk::Byte(Op::CONTINUE_RESCUE)),
                IROp::EndRescue => {
                    code.push(Chunk::Byte(Op::END_RESCUE));
                }
                IROp::DefinedConst(var, name) => {
                    code.push(Chunk::Byte(Op::DEFINED_CONST));
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::DefinedGlobal(var, name) => {
                    code.push(Chunk::Byte(Op::DEFINED_GLOBAL));
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::DefinedClassVar(var, name) => {
                    code.push(Chunk::Byte(Op::DEFINED_CLASS_VAR));
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::DefinedIVar(var, name) => {
                    code.push(Chunk::Byte(Op::DEFINED_IVAR));
                    addr_var!(var);
                    addr_static!(Static::Sym(name));
                }
                IROp::DefModule(parent, name, proc) => {
                    code.push(Chunk::Byte(Op::DEF_MODULE));
                    addr_var!(parent);
                    addr_static!(Static::Sym(name));
                    addr_static!(Static::Proc(Arc::new(proc.into_proc(&Vec::new()))));
                }
                IROp::DefClass(parent, name, superclass, proc) => {
                    code.push(Chunk::Byte(Op::DEF_CLASS));
                    addr_var!(parent);
                    addr_static!(Static::Sym(name));
                    addr_var!(superclass.unwrap_or(Var::Nil));
                    addr_static!(Static::Proc(Arc::new(proc.into_proc(&Vec::new()))));
                }
                IROp::DefMethod(name, proc) => {
                    code.push(Chunk::Byte(Op::DEF_METHOD));
                    addr_static!(Static::Sym(name));
                    addr_static!(Static::Proc(Arc::new(proc.into_proc(&Vec::new()))));
                }
                IROp::DefSingletonClass(expr, proc) => {
                    code.push(Chunk::Byte(Op::DEF_SINGLETON_CLASS));
                    addr_var!(expr);
                    addr_static!(Static::Proc(Arc::new(proc.into_proc(&Vec::new()))));
                }
                IROp::DefSingletonMethod(expr, name, proc) => {
                    code.push(Chunk::Byte(Op::DEF_SINGLETON_METHOD));
                    addr_var!(expr);
                    addr_static!(Static::Sym(name));
                    addr_static!(Static::Proc(Arc::new(proc.into_proc(&Vec::new()))));
                }
                IROp::ParamFallback(var, label) => {
                    code.push(Chunk::Byte(Op::PARAM_FALLBACK));
                    addr_var!(var);
                    push_label!(label);
                }
                IROp::Label(_) | IROp::Param(_) => code_len -= 1,
            }
        }

        struct IterOneOrTwoBytes(u8, Option<u8>, usize);
        impl Iterator for IterOneOrTwoBytes {
            type Item = u8;
            fn next(&mut self) -> Option<u8> {
                self.2 += 1;
                match self.2 - 1 {
                    0 => Some(self.0),
                    1 => self.1,
                    _ => None,
                }
            }
        }

        let code = code
            .into_iter()
            .flat_map(|chunk| match chunk {
                Chunk::Byte(byte) => IterOneOrTwoBytes(byte, None, 0),
                Chunk::Label(label) => {
                    let value = op_offsets[label];
                    match mode {
                        AddressingMode::U8 => IterOneOrTwoBytes(value as u8, None, 0),
                        AddressingMode::U16 => {
                            let bytes = (value as u16).to_ne_bytes();
                            IterOneOrTwoBytes(bytes[0], Some(bytes[1]), 0)
                        }
                    }
                }
            })
            .collect();

        let mut param_list = SmallVec::with_capacity(self.params.positional.len());

        for (var, ty) in self.params.positional {
            let var = match var {
                Var::Nil => NIL,
                Var::Void => VOID,
                Var::SelfRef => SELF,
                Var::Local(i) => registers[&i],
            };
            match ty {
                IRParamType::Mandatory => param_list.push(Param::Mandatory(var as u16)),
                IRParamType::Optional => param_list.push(Param::Optional(var as u16)),
                IRParamType::Splat => param_list.push(Param::Splat(var as u16)),
            }
        }

        if !self.params.keyword.is_empty() {
            let mut map = SmallVec::new();
            for (sym, var, ty) in self.params.keyword {
                let var = match var {
                    Var::Nil => NIL,
                    Var::Void => VOID,
                    Var::SelfRef => SELF,
                    Var::Local(i) => registers[&i],
                };
                map.push((sym, var as u16, ty == IRParamType::Mandatory));
            }
            param_list.push(Param::Hash(map));
        }

        let params = Params {
            params: param_list,
            block: self
                .params
                .block
                .map(|var| match var {
                    Var::Nil => NIL,
                    Var::Void => VOID,
                    Var::SelfRef => SELF,
                    Var::Local(i) => registers[&i],
                })
                .unwrap_or(VOID) as u16,
        };

        Proc {
            name: self.name,
            registers: register_count,
            statics,
            mode,
            code,
            params,
            parent_registers: Vec::new(),
        }
    }
}
