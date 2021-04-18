//! VM threads.

use crate::context::Context;
use crate::exception::{Exception, TraceItem};
use crate::heap::{Ref, RefGuard};
use crate::object::{Arguments, Object, RbClass, RbModule, SendError};
use crate::proc::{AddressingMode, CStatic, Op, Proc, Static, SELF, VOID};
use crate::symbol::Symbol;
use crate::value::Value;
use sapphire_compiler::Param;
use smallvec::SmallVec;
use std::collections::VecDeque;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::sync::Arc;
use std::{iter, mem};

lazy_static::lazy_static! {
    static ref ARGUMENT_NOT_GIVEN: Value = Value::Ref(Ref::new(Value::Nil));
}

/// A stack frame.
#[derive(Debug)]
struct Frame {
    proc: Proc,
    register: Register,
    block_given: bool,
}

type RegisterInner = SmallVec<[Value; 16]>;

/// Types of registers: local or detached.
/// Registers are detached if the local variable scope is captured in a block.
#[derive(Debug, Clone)]
pub enum Register {
    Local(RegisterInner),
    Detached(Ref<RegisterInner>),
}

impl Register {
    pub fn get(&self, index: usize) -> Value {
        match self {
            Register::Local(r) => r.index(index).clone(),
            Register::Detached(r) => r.get().index(index).clone(),
        }
    }
    pub fn set(&self, index: usize, value: Value) -> Result<(), ()> {
        match self {
            Register::Local(_) => Err(()),
            Register::Detached(r) => {
                *r.get().index_mut(index) = value;
                Ok(())
            }
        }
    }
}

/// Register reference.
enum RegisterMut<'a> {
    Local(&'a mut RegisterInner),
    Detached(RefGuard<'a, RegisterInner>),
}
impl<'a> Deref for RegisterMut<'a> {
    type Target = RegisterInner;
    fn deref(&self) -> &Self::Target {
        match self {
            RegisterMut::Local(r) => *r,
            RegisterMut::Detached(r) => &*r,
        }
    }
}
impl<'a> DerefMut for RegisterMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            RegisterMut::Local(r) => *r,
            RegisterMut::Detached(r) => &mut *r,
        }
    }
}
impl<'a> Index<usize> for RegisterMut<'a> {
    type Output = Value;
    fn index(&self, index: usize) -> &Self::Output {
        self.deref().index(index)
    }
}
impl<'a> IndexMut<usize> for RegisterMut<'a> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.deref_mut().index_mut(index)
    }
}
impl PartialEq for Register {
    fn eq(&self, _: &Register) -> bool {
        false
    }
}

impl Frame {
    fn new(proc: Proc, current_self: Value, block_given: bool) -> Frame {
        let registers = proc.sapphire().unwrap().registers;
        let mut register = SmallVec::with_capacity(registers);
        for _ in 0..registers {
            register.push(Value::Nil);
        }
        register[SELF] = current_self;
        Frame {
            register: Register::Local(register),
            proc,
            block_given,
        }
    }

    fn register_mut(&mut self) -> RegisterMut {
        match &mut self.register {
            Register::Local(register) => RegisterMut::Local(register),
            Register::Detached(register) => RegisterMut::Detached(register.get()),
        }
    }

    fn detach_register(&mut self) -> Register {
        match &self.register {
            Register::Local(..) => {
                let register = mem::replace(&mut self.register, unsafe { mem::zeroed() });
                if let Register::Local(register) = register {
                    mem::forget(mem::replace(
                        &mut self.register,
                        Register::Detached(Ref::new_generic(register)),
                    ));
                    self.register.clone()
                } else {
                    unreachable!()
                }
            }
            Register::Detached(..) => self.register.clone(),
        }
    }
}

/// A thread that will synchronously execute bytecode.
#[derive(Debug)]
pub struct Thread {
    context: Arc<Context>,
    frames: Stack<Frame>,
    pcs: Stack<usize>,
    rescues: Stack<(usize, usize)>,
    modules: Stack<Ref<dyn Object>>,
    pc: usize,
    arg_builder: ArgumentBuilder,
    exception: Option<Value>,
}

type OpResult = Result<Option<Value>, SendError>;

#[derive(Debug, Clone)]
enum ThreadResult {
    NotReady,
    Ready(Value),
    Err(SendError),
}

/// VM-level errors.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ThreadError {
    /// Recursion too deep.
    StackOverflow,
    /// Rescue attempt without any exception raised.
    InvalidRescueOp,
    /// Unrecognized opcode.
    InvalidOperation(u8),
    /// Bytecode: the referenced static value is of the wrong type.
    InvalidStatic,
    /// The parent register is not detached.
    InvalidLocalParentRegister,
    /// Bytecode: the referenced parent scope does not exist.
    InvalidParentDepth(usize),
    /// Bytecode ended unexpectedly.
    UnexpectedEnd,
}

impl Thread {
    fn alloc(context: Arc<Context>) -> Thread {
        Thread {
            context,
            frames: Stack::new(1024),
            pcs: Stack::new(1024),
            rescues: Stack::new(1024),
            modules: Stack::new(1024),
            pc: 0,
            arg_builder: ArgumentBuilder::new(),
            exception: None,
        }
    }

    /// Creates a new thread.
    pub fn new(context: Arc<Context>) -> Thread {
        let mut thread = Thread::alloc(context);
        thread
            .modules
            .push(thread.context.object_class().clone())
            .unwrap();
        thread
    }

    /// Returns the context to which this thread belongs.
    pub fn context(&self) -> &Context {
        &self.context
    }

    /// Creates a stack trace.
    pub fn trace(&self) -> Vec<TraceItem> {
        let mut trace = Vec::with_capacity(self.frames.buf.len());
        for (frame, pc) in self.frames.buf.iter().zip(self.pcs.buf.iter()) {
            trace.push(TraceItem {
                proc: frame.proc.clone(),
                pc: *pc,
            });
        }
        trace
    }

    /// Returns true if the current proc has a block given.
    pub fn is_block_given(&self) -> bool {
        for frame in self.frames.buf.iter().rev() {
            match frame.proc.sapphire().map_or(None, |p| p.block_idx) {
                None => continue,
                Some(_) => return frame.block_given,
            }
        }
        false
    }

    fn push_frame(
        &mut self,
        proc: Proc,
        new_self: Value,
        args: &Arguments,
    ) -> Result<(), ThreadError> {
        let block_given = args.block.is_some();
        self.frames.push(Frame::new(proc, new_self, block_given))?;
        self.pcs.push(self.pc)?;
        self.pc = 0;
        Ok(())
    }

    fn pop_frame(&mut self) -> Option<Frame> {
        let res = self.frames.pop();
        if let Some(pc) = self.pcs.pop() {
            self.pc = pc;
        }
        res
    }

    fn load_args(&mut self, args: Arguments) -> Result<(), SendError> {
        let frame = self.frames.top_mut().unwrap();
        let proc = match &frame.proc {
            Proc::Sapphire(proc) => Arc::clone(&proc),
            _ => panic!("Invalid proc type"),
        };

        let mut expected_at_most = 0;

        if let Some(nonlinear) = &proc.params.nonlinear {
            let args: SmallVec<[Value; 3]> = args.args.collect();
            let mut args_left = args.len();
            let mut args = args.into_iter();

            for (param, min_after) in proc
                .params
                .params
                .iter()
                .zip(nonlinear.min_args_after.iter())
            {
                if min_after > &args_left {
                    continue;
                }
                match param {
                    Param::Mandatory(i) => {
                        expected_at_most += 1;
                        frame.register_mut()[*i as usize] = match args.next() {
                            Some(value) => {
                                args_left -= 1;
                                value
                            }
                            None => {
                                return Err(SendError::Exception(Value::Ref(Exception::new(
                                    format!("missing mandatory argument"),
                                    self.trace(),
                                    self.context().exceptions().argument_error.clone(),
                                ))));
                            }
                        }
                    }
                    Param::Optional(i) => {
                        expected_at_most += 1;
                        frame.register_mut()[*i as usize] = match args.next() {
                            Some(value) => {
                                args_left -= 1;
                                value
                            }
                            None => ARGUMENT_NOT_GIVEN.clone(),
                        }
                    }
                    _ => unimplemented!("other param types"),
                }
            }
        } else {
            for param in &proc.params.params {
                match param {
                    Param::Mandatory(i) => {
                        expected_at_most += 1;
                        frame.register_mut()[*i as usize] = match args.args.next() {
                            Some(value) => value,
                            None => {
                                return Err(SendError::Exception(Value::Ref(Exception::new(
                                    format!("missing mandatory argument"),
                                    self.trace(),
                                    self.context().exceptions().argument_error.clone(),
                                ))));
                            }
                        }
                    }
                    Param::Optional(i) => {
                        expected_at_most += 1;
                        frame.register_mut()[*i as usize] = match args.args.next() {
                            Some(value) => value,
                            None => ARGUMENT_NOT_GIVEN.clone(),
                        }
                    }
                    _ => unimplemented!("other param types"),
                }
            }
        }

        if let Some(_) = args.args.next() {
            return Err(SendError::Exception(Value::Ref(Exception::new(
                match expected_at_most {
                    0 => format!("expected no arguments"),
                    1 => format!("expected at most one argument"),
                    n => format!("expected at most {} arguments", n),
                },
                self.trace(),
                self.context().exceptions().argument_error.clone(),
            ))));
        }

        if let Some(block) = args.block {
            frame.register_mut()[proc.params.block as usize] = block;
        }

        Ok(())
    }

    fn read_addr(&mut self) -> Result<usize, ThreadError> {
        let proc = &self.frames.top().expect("read_addr without frame").proc;
        let proc = proc.sapphire().unwrap();
        match proc.mode {
            AddressingMode::U8 => match proc.code.get(self.pc) {
                Some(i) => {
                    self.pc += 1;
                    Ok(*i as usize)
                }
                None => Err(ThreadError::UnexpectedEnd),
            },
            AddressingMode::U16 => match (proc.code.get(self.pc), proc.code.get(self.pc + 1)) {
                (Some(i), Some(j)) => {
                    self.pc += 2;
                    Ok(u16::from_le_bytes([*i, *j]) as usize)
                }
                _ => Err(ThreadError::UnexpectedEnd),
            },
        }
    }

    fn read_static(&mut self) -> Result<&Static, ThreadError> {
        let index = self.read_addr()?;
        Ok(&self.frames.top().unwrap().proc.sapphire().unwrap().statics[index])
    }

    fn read_symbol(&mut self) -> Result<Symbol, ThreadError> {
        match self.read_static()? {
            CStatic::Sym(sym) => Ok(*sym),
            _ => Err(ThreadError::InvalidStatic),
        }
    }

    fn read_parent_addr(&mut self) -> Result<(usize, usize), ThreadError> {
        let proc = &self
            .frames
            .top()
            .expect("read_parent_addr without frame")
            .proc;
        let proc = proc.sapphire().unwrap();
        match (
            proc.code.get(self.pc),
            proc.code.get(self.pc + 1),
            proc.code.get(self.pc + 2),
        ) {
            (Some(i), Some(j), Some(d)) => {
                self.pc += 3;
                let addr = u16::from_le_bytes([*i, *j]) as usize;
                let depth = *d as usize;
                Ok((addr, depth))
            }
            _ => Err(ThreadError::UnexpectedEnd),
        }
    }

    /// Runs a proc on this thread.
    pub fn call(
        &mut self,
        receiver: Value,
        proc: Proc,
        args: Arguments,
    ) -> Result<Value, SendError> {
        match proc {
            Proc::Sapphire(_) => {
                self.push_frame(proc, receiver, &args)?;
                self.load_args(args)?;
                let result = loop {
                    match self.next() {
                        ThreadResult::NotReady => (),
                        ThreadResult::Err(err) => break Err(err),
                        ThreadResult::Ready(value) => break Ok(value),
                    }
                };
                self.pop_frame();
                result
            }
            Proc::Native(f) => f(receiver, args, self),
        }
    }

    fn next(&mut self) -> ThreadResult {
        let code = match self.frames.top() {
            Some(frame) => match frame.proc.sapphire().unwrap().code.get(self.pc) {
                Some(code) => *code,
                None => return ThreadResult::Err(ThreadError::UnexpectedEnd.into()),
            },
            None => return ThreadResult::Ready(Value::Nil),
        };
        self.pc += 1;

        let res = match code {
            Op::LOAD_ROOT => self.op_load_root(),
            Op::LOAD_TRUE => self.op_load_true(),
            Op::LOAD_FALSE => self.op_load_false(),
            Op::LOAD_GLOBAL => self.op_load_global(),
            Op::LOAD_CONST => self.op_load_const(),
            Op::LOAD_CLASS_VAR => self.op_load_class_var(),
            Op::LOAD_IVAR => self.op_load_ivar(),
            Op::LOAD_STRING => self.op_load_string(),
            Op::APPEND_STRING => self.op_append_string(),
            Op::LOAD_SYMBOL => self.op_load_symbol(),
            Op::LOAD_I64 => self.op_load_i64(),
            Op::LOAD_FLOAT => self.op_load_float(),
            Op::LOAD_BLOCK => self.op_load_block(),
            Op::LOAD_PARENT => self.op_load_parent(),
            Op::ARG => self.op_arg(),
            Op::ARG_ASSOC => self.op_arg_assoc(),
            Op::ARG_BLOCK => self.op_arg_block(),
            Op::CALL => self.op_call(),
            Op::CALL_ONE => self.op_call_one(),
            Op::SUPER => self.op_super(),
            Op::NOT => self.op_not(),
            Op::JUMP => self.op_jump(),
            Op::JUMP_IF => self.op_jump_if(),
            Op::JUMP_IF_NOT => self.op_jump_if_not(),
            Op::RETURN => self.op_return(),
            Op::ASSIGN => self.op_assign(),
            Op::ASSIGN_GLOBAL => self.op_assign_global(),
            Op::ASSIGN_CONST => self.op_assign_const(),
            Op::ASSIGN_CLASS_VAR => self.op_assign_class_var(),
            Op::ASSIGN_IVAR => self.op_assign_ivar(),
            Op::ASSIGN_PARENT => self.op_assign_parent(),
            Op::BEGIN_RESCUE => self.op_begin_rescue(),
            Op::RESCUE_MATCH => self.op_rescue_match(),
            Op::RESCUE_BIND => self.op_rescue_bind(),
            Op::CONTINUE_UNWIND => self.op_continue_unwind(),
            Op::END_RESCUE => self.op_end_rescue(),
            Op::DEFINED_CONST => self.op_defined_const(),
            Op::DEFINED_GLOBAL => self.op_defined_global(),
            Op::DEFINED_CLASS_VAR => self.op_defined_class_var(),
            Op::DEFINED_IVAR => self.op_defined_ivar(),
            Op::DEF_MODULE => self.op_def_module(),
            Op::DEF_CLASS => self.op_def_class(),
            Op::DEF_METHOD => self.op_def_method(),
            Op::DEF_SINGLETON_CLASS => self.op_def_singleton_class(),
            Op::DEF_SINGLETON_METHOD => self.op_def_singleton_method(),
            Op::PARAM_FALLBACK => self.op_param_fallback(),
            code => return ThreadResult::Err(ThreadError::InvalidOperation(code).into()),
        };

        match res {
            Ok(Some(value)) => ThreadResult::Ready(value),
            Ok(None) => ThreadResult::NotReady,
            Err(SendError::Exception(exception)) => {
                let top_frame = self.frames.buf.len() - 1;
                if self
                    .rescues
                    .top()
                    .map_or(false, |(frame, _)| *frame == top_frame)
                {
                    // this proc has registered a rescue
                    self.exception = Some(exception);
                    self.pc = self.rescues.pop().unwrap().1;
                    ThreadResult::NotReady
                } else {
                    // unwind
                    ThreadResult::Err(SendError::Exception(exception))
                }
            }
            Err(SendError::Thread(err)) => ThreadResult::Err(SendError::Thread(err)),
        }
    }

    #[inline]
    fn op_load_root(&mut self) -> OpResult {
        let out = self.read_addr()?;
        self.frames.top_mut().unwrap().register_mut()[out] =
            Value::Ref(self.context.object_class().clone());
        Ok(None)
    }
    #[inline]
    fn op_load_true(&mut self) -> OpResult {
        let out = self.read_addr()?;
        self.frames.top_mut().unwrap().register_mut()[out] = Value::Bool(true);
        Ok(None)
    }
    #[inline]
    fn op_load_false(&mut self) -> OpResult {
        let out = self.read_addr()?;
        self.frames.top_mut().unwrap().register_mut()[out] = Value::Bool(false);
        Ok(None)
    }
    #[inline]
    fn op_load_global(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let name = self.read_symbol()?;
        match self.context.globals().get().get(&name) {
            Some(value) => self.frames.top_mut().unwrap().register_mut()[out] = value.clone(),
            None => self.frames.top_mut().unwrap().register_mut()[out] = Value::Nil,
        }
        Ok(None)
    }
    #[inline]
    fn op_load_const(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let parent = self.read_addr()?;
        let name = self.read_symbol()?;
        let register = &mut self.frames.top_mut().unwrap().register_mut();
        if parent == SELF {
            match self.modules.top().unwrap().get().get(name) {
                Some(value) => register[out] = value,
                None => register[out] = Value::Nil,
            }
        } else {
            let value = if let Some(value) = register[parent].get(name) {
                value
            } else {
                unimplemented!("const doesn’t exist")
            };
            register[out] = value;
        }
        Ok(None)
    }
    #[inline]
    fn op_load_class_var(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let name = self.read_symbol()?;
        let mut self_obj = self.frames.top_mut().unwrap().register_mut()[SELF].clone();
        let self_class = self_obj.send(Symbol::CLASS, Arguments::empty(), self)?;
        match self_class.get(name) {
            Some(value) => self.frames.top_mut().unwrap().register_mut()[out] = value,
            None => self.frames.top_mut().unwrap().register_mut()[out] = Value::Nil,
        }
        Ok(None)
    }
    #[inline]
    fn op_load_ivar(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let name = self.read_symbol()?;
        let mut register = self.frames.top_mut().unwrap().register_mut();
        match register[SELF].get(name) {
            Some(value) => register[out] = value,
            None => register[out] = Value::Nil,
        }
        Ok(None)
    }
    #[inline]
    fn op_load_string(&mut self) -> OpResult {
        let out = self.read_addr()?;
        match self.read_static()? {
            CStatic::Str(string) => {
                self.frames.top_mut().unwrap().register_mut()[out] = Value::String(string.clone())
            }
            _ => return Err(ThreadError::InvalidStatic.into()),
        }
        Ok(None)
    }
    #[inline]
    fn op_append_string(&mut self) -> OpResult {
        let recv = self.read_addr()?;
        let append = self.read_addr()?;
        let (append, mut recv_obj) = {
            let register = self.frames.top_mut().unwrap().register_mut();
            (register[append].clone(), register[recv].clone())
        };
        let out = recv_obj.send(
            Symbol::ADD,
            Arguments::new(&mut iter::once(append), None),
            self,
        )?;
        self.frames.top_mut().unwrap().register_mut()[recv] = out;
        Ok(None)
    }
    #[inline]
    fn op_load_symbol(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let sym = self.read_symbol()?;
        self.frames.top_mut().unwrap().register_mut()[out] = Value::Symbol(sym);
        Ok(None)
    }
    #[inline]
    fn op_load_i64(&mut self) -> OpResult {
        let out = self.read_addr()?;
        match self.read_static()? {
            CStatic::Int(value) => {
                self.frames.top_mut().unwrap().register_mut()[out] = Value::Fixnum(*value);
                Ok(None)
            }
            _ => Err(ThreadError::InvalidStatic.into()),
        }
    }
    #[inline]
    fn op_load_float(&mut self) -> OpResult {
        let out = self.read_addr()?;
        match self.read_static()? {
            CStatic::Float(value) => {
                self.frames.top_mut().unwrap().register_mut()[out] = Value::Float(*value);
                Ok(None)
            }
            _ => Err(ThreadError::InvalidStatic.into()),
        }
    }
    #[inline]
    fn op_load_block(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let frame = self.frames.top_mut().unwrap();
        let register = frame.detach_register();
        let mut parent_registers = frame.proc.sapphire().unwrap().parent_registers.clone();
        parent_registers.push(register);
        match self.read_static()? {
            CStatic::Proc(value) => {
                let proc = value.clone_with_parents(parent_registers);
                self.frames.top_mut().unwrap().register_mut()[out] =
                    Value::Proc(Proc::Sapphire(Arc::new(proc)));
                Ok(None)
            }
            _ => Err(ThreadError::InvalidStatic.into()),
        }
    }
    #[inline]
    fn op_load_parent(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let (var, depth) = self.read_parent_addr()?;
        let frame = self.frames.top_mut().unwrap();
        let parent_registers = &frame.proc.sapphire().unwrap().parent_registers;
        match parent_registers.get(parent_registers.len().wrapping_sub(depth)) {
            Some(register) => {
                frame.register_mut()[out] = register.get(var);
                Ok(None)
            }
            None => Err(ThreadError::InvalidParentDepth(depth).into()),
        }
    }
    #[inline]
    fn op_arg(&mut self) -> OpResult {
        let var = self.read_addr()?;
        self.arg_builder
            .push_arg(self.frames.top_mut().unwrap().register_mut()[var].clone());
        Ok(None)
    }
    #[inline]
    fn op_arg_assoc(&mut self) -> OpResult {
        let key = self.read_addr()?;
        let value = self.read_addr()?;
        let register = &self.frames.top_mut().unwrap().register_mut();
        self.arg_builder
            .push_hash(register[key].clone(), register[value].clone());
        Ok(None)
    }
    #[inline]
    fn op_arg_block(&mut self) -> OpResult {
        let var = self.read_addr()?;
        self.arg_builder
            .push_block(self.frames.top_mut().unwrap().register_mut()[var].clone());
        Ok(None)
    }
    #[inline]
    fn op_call(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let recv = self.read_addr()?;
        let mut recv = self.frames.top_mut().unwrap().register_mut()[recv].clone();
        let method = self.read_symbol()?;
        let args = self.arg_builder.take();
        let (mut arg_iter, block) = args.as_args();
        let value = recv.send(method, Arguments::new(&mut arg_iter, block), self)?;
        self.frames.top_mut().unwrap().register_mut()[out] = value;
        Ok(None)
    }
    #[inline]
    fn op_call_one(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let recv = self.read_addr()?;
        let method = self.read_symbol()?;
        let arg = self.read_addr()?;
        let (mut recv, mut arg_iter) = {
            let register = self.frames.top_mut().unwrap().register_mut();
            (register[recv].clone(), iter::once(register[arg].clone()))
        };
        let value = recv.send(method, Arguments::new(&mut arg_iter, None), self)?;
        self.frames.top_mut().unwrap().register_mut()[out] = value;
        Ok(None)
    }
    #[inline]
    fn op_super(&mut self) -> OpResult {
        unimplemented!()
    }
    #[inline]
    fn op_not(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let value = self.read_addr()?;
        let register = &mut self.frames.top_mut().unwrap().register_mut();
        register[out] = Value::Bool(!register[value].is_truthy());
        Ok(None)
    }
    #[inline]
    fn op_jump(&mut self) -> OpResult {
        self.pc = self.read_addr()?;
        Ok(None)
    }
    #[inline]
    fn op_jump_if(&mut self) -> OpResult {
        let cond = self.read_addr()?;
        let jump = self.read_addr()?;
        if self.frames.top_mut().unwrap().register_mut()[cond].is_truthy() {
            self.pc = jump;
        }
        Ok(None)
    }
    #[inline]
    fn op_jump_if_not(&mut self) -> OpResult {
        let cond = self.read_addr()?;
        let jump = self.read_addr()?;
        if !self.frames.top_mut().unwrap().register_mut()[cond].is_truthy() {
            self.pc = jump;
        }
        Ok(None)
    }
    #[inline]
    fn op_return(&mut self) -> OpResult {
        let value = self.read_addr()?;
        let value = self.frames.top_mut().unwrap().register_mut()[value].clone();
        Ok(Some(value))
    }
    #[inline]
    fn op_assign(&mut self) -> OpResult {
        let recv = self.read_addr()?;
        let value = self.read_addr()?;
        let register = &mut self.frames.top_mut().unwrap().register_mut();
        register[recv] = register[value].clone();
        Ok(None)
    }
    #[inline]
    fn op_assign_global(&mut self) -> OpResult {
        let name = self.read_symbol()?;
        let value = self.read_addr()?;
        let value = self.frames.top_mut().unwrap().register_mut()[value].clone();
        self.context.globals().get().insert(name, value);
        Ok(None)
    }
    #[inline]
    fn op_assign_const(&mut self) -> OpResult {
        unimplemented!()
    }
    #[inline]
    fn op_assign_class_var(&mut self) -> OpResult {
        let name = self.read_symbol()?;
        let var = self.read_addr()?;
        let mut self_obj = self.frames.top_mut().unwrap().register_mut()[SELF].clone();
        let mut class = self_obj.send(Symbol::CLASS, Arguments::empty(), self)?;
        let value = self.frames.top_mut().unwrap().register_mut()[var].clone();
        if let Err(()) = class.set(name, value) {
            unimplemented!("exception")
        }
        Ok(None)
    }
    #[inline]
    fn op_assign_ivar(&mut self) -> OpResult {
        let name = self.read_symbol()?;
        let var = self.read_addr()?;
        let mut register = self.frames.top_mut().unwrap().register_mut();
        let value = register[var].clone();
        if let Err(()) = register[SELF].set(name, value) {
            unimplemented!("exception")
        }
        Ok(None)
    }
    #[inline]
    fn op_assign_parent(&mut self) -> OpResult {
        let (out, depth) = self.read_parent_addr()?;
        let var = self.read_addr()?;
        let frame = self.frames.top_mut().unwrap();
        let parent_registers = &frame.proc.sapphire().unwrap().parent_registers;
        match parent_registers.get(parent_registers.len().wrapping_sub(depth)) {
            Some(register) => {
                register
                    .set(out, frame.register.get(var))
                    .map_err(|_| ThreadError::InvalidLocalParentRegister)?;
                Ok(None)
            }
            None => Err(ThreadError::InvalidParentDepth(depth).into()),
        }
    }
    #[inline]
    fn op_begin_rescue(&mut self) -> OpResult {
        let rescue_addr = self.read_addr()?;
        self.rescues
            .push((self.frames.buf.len() - 1, rescue_addr))?;
        Ok(None)
    }
    #[inline]
    fn op_rescue_match(&mut self) -> OpResult {
        let class = self.read_addr()?;
        let label = self.read_addr()?;
        if let Some(exception) = &self.exception {
            let mut exception = exception.clone();
            let class_obj = self.frames.top_mut().unwrap().register_mut()[class].clone();
            let mut args = iter::once(class_obj);
            let is_match =
                match exception.send(Symbol::IS_A, Arguments::new(&mut args, None), self)? {
                    Value::Bool(true) => true,
                    _ => false,
                };
            if is_match {
                self.pc = label;
            }
            Ok(None)
        } else {
            Err(SendError::Thread(ThreadError::InvalidRescueOp))
        }
    }
    #[inline]
    fn op_rescue_bind(&mut self) -> OpResult {
        let out = self.read_addr()?;
        if let Some(_) = &self.exception {
            self.frames.top_mut().unwrap().register_mut()[out] = self.exception.take().unwrap();
            Ok(None)
        } else {
            Err(SendError::Thread(ThreadError::InvalidRescueOp))
        }
    }
    #[inline]
    fn op_continue_unwind(&mut self) -> OpResult {
        if self.exception.is_some() {
            Err(SendError::Exception(self.exception.take().unwrap()))
        } else {
            Err(SendError::Thread(ThreadError::InvalidRescueOp))
        }
    }
    #[inline]
    fn op_end_rescue(&mut self) -> OpResult {
        self.rescues.pop();
        Ok(None)
    }
    #[inline]
    fn op_defined_const(&mut self) -> OpResult {
        unimplemented!()
    }
    #[inline]
    fn op_defined_global(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let name = self.read_symbol()?;
        let mut register = self.frames.top_mut().unwrap().register_mut();
        register[out] = Value::Bool(self.context.globals().get().contains_key(&name));
        Ok(None)
    }
    #[inline]
    fn op_defined_class_var(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let name = self.read_symbol()?;
        let mut self_obj = self.frames.top_mut().unwrap().register_mut()[SELF].clone();
        let class = self_obj.send(Symbol::CLASS, Arguments::empty(), self)?;
        self.frames.top_mut().unwrap().register_mut()[out] = Value::Bool(class.get(name).is_some());
        Ok(None)
    }
    #[inline]
    fn op_defined_ivar(&mut self) -> OpResult {
        let out = self.read_addr()?;
        let name = self.read_symbol()?;
        let mut register = self.frames.top_mut().unwrap().register_mut();
        register[out] = Value::Bool(register[SELF].get(name).is_some());
        Ok(None)
    }
    #[inline]
    fn op_def_module(&mut self) -> OpResult {
        let parent = self.read_addr()?;
        let name = self.read_symbol()?;
        let proc = match self.read_static()? {
            CStatic::Proc(proc) => Arc::clone(proc),
            _ => return Err(ThreadError::InvalidStatic.into()),
        };
        let module = if parent == VOID {
            self.modules.top_mut().unwrap().get().get(name)
        } else {
            self.frames.top_mut().unwrap().register_mut()[parent].get(name)
        };
        let module = if let Some(module) = module {
            module
        } else {
            let module = Value::Ref(RbModule::new(name, &self.context));
            let res = if parent == VOID {
                self.modules
                    .top_mut()
                    .unwrap()
                    .get()
                    .set(name, module.clone())
            } else {
                self.frames.top_mut().unwrap().register_mut()[parent].set(name, module.clone())
            };
            if let Err(()) = res {
                unimplemented!("exception")
            }
            module
        };
        match module.clone() {
            Value::Ref(module) => self.modules.push(module.clone())?,
            _ => unimplemented!("exception"),
        }
        self.call(module, Proc::Sapphire(proc), Arguments::empty())?;
        self.modules.pop();
        Ok(None)
    }
    #[inline]
    fn op_def_class(&mut self) -> OpResult {
        let parent = self.read_addr()?;
        let name = self.read_symbol()?;
        let superclass = self.read_addr()?;
        let superclass = if superclass == VOID {
            self.context.object_class().clone()
        } else {
            match &self.frames.top_mut().unwrap().register_mut()[superclass] {
                Value::Ref(r) => r.clone(), // TODO: maybe verify that it’s actually a class… somehow
                _ => unimplemented!("exception"),
            }
        };
        let proc = match self.read_static()? {
            CStatic::Proc(proc) => Arc::clone(proc),
            _ => return Err(ThreadError::InvalidStatic.into()),
        };
        let module = if parent == VOID {
            self.modules.top_mut().unwrap().get().get(name)
        } else {
            self.frames.top_mut().unwrap().register_mut()[parent].get(name)
        };
        let module = if let Some(module) = module {
            module
        } else {
            let module = Value::Ref(RbClass::new(name, superclass, &self.context));
            let res = if parent == VOID {
                self.modules
                    .top_mut()
                    .unwrap()
                    .get()
                    .set(name, module.clone())
            } else {
                self.frames.top_mut().unwrap().register_mut()[parent].set(name, module.clone())
            };
            if let Err(()) = res {
                unimplemented!("exception")
            }
            module
        };
        match module.clone() {
            Value::Ref(module) => self.modules.push(module.clone())?,
            _ => unimplemented!("exception"),
        }
        self.call(module, Proc::Sapphire(proc), Arguments::empty())?;
        self.modules.pop();
        Ok(None)
    }
    #[inline]
    fn op_def_method(&mut self) -> OpResult {
        let name = self.read_symbol()?;
        let proc = match self.read_static()? {
            CStatic::Proc(proc) => Arc::clone(proc),
            _ => return Err(ThreadError::InvalidStatic.into()),
        };
        let top_module = self.modules.top_mut().unwrap().clone();
        top_module.get().send(
            Symbol::DEFINE_METHOD,
            Arguments {
                args: &mut iter::once(Value::Symbol(name)),
                block: Some(Value::Proc(Proc::Sapphire(proc))),
            },
            self,
        )?;
        Ok(None)
    }
    #[inline]
    fn op_def_singleton_class(&mut self) -> OpResult {
        let obj = self.read_addr()?;
        let proc = match self.read_static()? {
            CStatic::Proc(proc) => Proc::Sapphire(Arc::clone(proc)),
            _ => return Err(ThreadError::InvalidStatic.into()),
        };
        let mut obj = self.frames.top_mut().unwrap().register_mut()[obj].clone();
        let singleton_class = obj.send(Symbol::SINGLETON_CLASS, Arguments::empty(), self)?;
        self.call(singleton_class, proc, Arguments::empty())?;
        Ok(None)
    }
    #[inline]
    fn op_def_singleton_method(&mut self) -> OpResult {
        let obj = self.read_addr()?;
        let name = self.read_symbol()?;
        let proc = match self.read_static()? {
            CStatic::Proc(proc) => Proc::Sapphire(Arc::clone(proc)),
            _ => return Err(ThreadError::InvalidStatic.into()),
        };
        let mut obj = self.frames.top_mut().unwrap().register_mut()[obj].clone();
        let mut singleton_class = obj.send(Symbol::SINGLETON_CLASS, Arguments::empty(), self)?;
        singleton_class.send(
            Symbol::DEFINE_METHOD,
            Arguments {
                args: &mut iter::once(Value::Symbol(name)),
                block: Some(Value::Proc(proc)),
            },
            self,
        )?;
        Ok(None)
    }
    #[inline]
    fn op_param_fallback(&mut self) -> OpResult {
        let var = self.read_addr()?;
        let jump = self.read_addr()?;
        if self.frames.top_mut().unwrap().register_mut()[var] != *ARGUMENT_NOT_GIVEN {
            self.pc = jump;
        }
        Ok(None)
    }
}

#[derive(Debug)]
struct Stack<T> {
    buf: Vec<T>,
    max_len: usize,
}

impl<T> Stack<T> {
    fn new(max_len: usize) -> Stack<T> {
        Stack {
            buf: Vec::with_capacity(max_len),
            max_len,
        }
    }

    fn push(&mut self, item: T) -> Result<(), ThreadError> {
        if self.buf.len() == self.max_len {
            return Err(ThreadError::StackOverflow);
        }
        self.buf.push(item);
        Ok(())
    }

    fn pop(&mut self) -> Option<T> {
        self.buf.pop()
    }

    fn top(&self) -> Option<&T> {
        self.buf.last()
    }

    fn top_mut(&mut self) -> Option<&mut T> {
        self.buf.last_mut()
    }
}

// TODO: ring buffer for arguments or something

#[derive(Debug)]
struct ArgumentBuilder {
    items: VecDeque<Value>,
    hash: Vec<(Value, Value)>,
    block: Option<Value>,
}

impl ArgumentBuilder {
    fn new() -> ArgumentBuilder {
        ArgumentBuilder {
            items: VecDeque::new(),
            hash: Vec::new(),
            block: None,
        }
    }

    fn push_arg(&mut self, arg: Value) {
        self.items.push_back(arg);
    }

    fn push_hash(&mut self, key: Value, value: Value) {
        self.hash.push((key, value));
    }

    fn push_block(&mut self, arg: Value) {
        self.block = Some(arg);
    }

    fn as_args(self) -> (ArgIter, Option<Value>) {
        let block = self.block;
        let items = ArgIter(self.items, self.hash, false);

        (items, block)
    }

    fn take(&mut self) -> ArgumentBuilder {
        mem::replace(self, ArgumentBuilder::new())
    }
}

struct ArgIter(VecDeque<Value>, Vec<(Value, Value)>, bool);

impl<'a> Iterator for ArgIter {
    type Item = Value;
    fn next(&mut self) -> Option<Value> {
        match self.0.remove(0) {
            Some(item) => Some(item),
            None => {
                if self.2 {
                    self.2 = false;
                    if !self.1.is_empty() {
                        unimplemented!("create hash and append")
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }
}
