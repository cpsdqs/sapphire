use crate::context::Context;
use crate::proc::{AddressingMode, Op, Proc, Static, VOID};
use crate::symbol::Symbol;
use crate::value::Value;
use smallvec::{Array, SmallVec};
use std::mem;
use std::sync::Arc;

struct Frame {
    proc: Arc<Proc>,
    register: SmallVec<[Value; 256]>,
}

impl Frame {
    fn new(proc: Arc<Proc>) -> Frame {
        Frame {
            register: SmallVec::with_capacity(proc.registers),
            proc,
        }
    }
}

pub struct Thread {
    context: Arc<Context>,
    frames: Stack<[Frame; 1024]>,
    pcs: Stack<[usize; 1024]>,
    rescues: Stack<[usize; 1024]>,
    pc: usize,
}

pub enum ThreadError {
    StackOverflow,
    InvalidOperation,
    InvalidStatic,
    UnexpectedEnd,
}

impl Thread {
    pub fn new_empty(context: Arc<Context>) -> Thread {
        Thread {
            context,
            frames: Stack::new(),
            pcs: Stack::new(),
            rescues: Stack::new(),
            pc: 0,
        }
    }

    fn push_frame(&mut self, proc: Arc<Proc>) -> Result<(), ThreadError> {
        self.frames.push(Frame::new(proc))?;
        self.pcs.push(self.pc)?;
        self.pc = 0;
        Ok(())
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
        if let Some(pc) = self.pcs.pop() {
            self.pc = pc;
        }
    }

    fn read_addr(&mut self) -> Result<usize, ThreadError> {
        let proc = &self.frames.top().expect("read_addr without frame").proc;

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
                    Ok(((*i as usize) << 8) & *j as usize)
                }
                _ => Err(ThreadError::UnexpectedEnd),
            },
        }
    }

    fn read_static(&mut self) -> Result<&Static, ThreadError> {
        let index = self.read_addr()?;
        Ok(&self.frames.top().unwrap().proc.statics[index])
    }

    fn read_symbol(&mut self) -> Result<Symbol, ThreadError> {
        match self.read_static()? {
            Static::Sym(sym) => Ok(*sym),
            _ => Err(ThreadError::InvalidStatic),
        }
    }

    fn next(&mut self) -> Result<Option<()>, ThreadError> {
        let code = match self.frames.top() {
            Some(frame) => match frame.proc.code.get(self.pc) {
                Some(code) => *code,
                None => return Err(ThreadError::UnexpectedEnd),
            },
            None => return Ok(None),
        };
        self.pc += 1;

        match code {
            Op::LOAD_ROOT => self.op_load_root()?,
            Op::LOAD_TRUE => self.op_load_true()?,
            Op::LOAD_FALSE => self.op_load_false()?,
            Op::LOAD_GLOBAL => self.op_load_global()?,
            Op::LOAD_CONST => self.op_load_const()?,
            Op::LOAD_CLASS_VAR => self.op_load_class_var()?,
            Op::LOAD_IVAR => self.op_load_ivar()?,
            Op::LOAD_STRING => self.op_load_string()?,
            Op::APPEND_STRING => self.op_append_string()?,
            Op::LOAD_SYMBOL => self.op_load_symbol()?,
            Op::LOAD_I64 => self.op_load_i64()?,
            Op::LOAD_FLOAT => self.op_load_float()?,
            Op::LOAD_PROC => self.op_load_proc()?,
            Op::ARG => self.op_arg()?,
            Op::ARG_ASSOC => self.op_arg_assoc()?,
            Op::ARG_SPLAT => self.op_arg_splat()?,
            Op::ARG_BLOCK => self.op_arg_block()?,
            Op::CALL => self.op_call()?,
            Op::SUPER => self.op_super()?,
            Op::NOT => self.op_not()?,
            Op::JUMP => self.op_jump()?,
            Op::JUMP_IF => self.op_jump_if()?,
            Op::JUMP_IF_NOT => self.op_jump_if_not()?,
            Op::RETURN => self.op_return()?,
            Op::ASSIGN => self.op_assign()?,
            Op::ASSIGN_GLOBAL => self.op_assign_global()?,
            Op::ASSIGN_CONST => self.op_assign_const()?,
            Op::ASSIGN_CLASS_VAR => self.op_assign_class_var()?,
            Op::ASSIGN_IVAR => self.op_assign_ivar()?,
            Op::BEGIN_RESCUE => self.op_begin_rescue()?,
            Op::RESCUE_MATCH => self.op_rescue_match()?,
            Op::RESCUE_BIND => self.op_rescue_bind()?,
            Op::END_RESCUE => self.op_end_rescue()?,
            Op::DEFINED_CONST => self.op_defined_const()?,
            Op::DEFINED_GLOBAL => self.op_defined_global()?,
            Op::DEFINED_CLASS_VAR => self.op_defined_class_var()?,
            Op::DEFINED_IVAR => self.op_defined_ivar()?,
            Op::YIELD => self.op_yield()?,
            Op::DEF_MODULE => self.op_def_module()?,
            Op::DEF_CLASS => self.op_def_class()?,
            Op::DEF_METHOD => self.op_def_method()?,
            Op::DEF_SINGLETON_CLASS => self.op_def_singleton_class()?,
            Op::DEF_SINGLETON_METHOD => self.op_def_singleton_method()?,
            _ => return Err(ThreadError::InvalidOperation),
        }

        Ok(Some(()))
    }

    // TODO: deal with parent vars
    #[inline]
    fn op_load_root(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        self.frames.top_mut().unwrap().register[out] = Value::Ref(self.context.root().clone());
        Ok(())
    }
    #[inline]
    fn op_load_true(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        self.frames.top_mut().unwrap().register[out] = Value::Bool(true);
        Ok(())
    }
    #[inline]
    fn op_load_false(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        self.frames.top_mut().unwrap().register[out] = Value::Bool(false);
        Ok(())
    }
    #[inline]
    fn op_load_global(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        let name = self.read_symbol()?;
        match self.context.globals().get().get(&name) {
            Some(value) => self.frames.top_mut().unwrap().register[out] = value.clone(),
            None => self.frames.top_mut().unwrap().register[out] = Value::Nil,
        }
        Ok(())
    }
    #[inline]
    fn op_load_const(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_load_class_var(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_load_ivar(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_load_string(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        match self.read_static()? {
            Static::Str(string) => {
                self.frames.top_mut().unwrap().register[out] = Value::String(string.clone())
            }
            _ => return Err(ThreadError::InvalidStatic),
        }
        Ok(())
    }
    #[inline]
    fn op_append_string(&mut self) -> Result<(), ThreadError> {
        let recv = self.read_addr()?;
        let append = self.read_addr()?;
        let register = &mut self.frames.top_mut().unwrap().register;
        let append = match &register[append] {
            Value::String(append) => append.clone(),
            _ => unimplemented!("stringify"),
        };
        match &mut register[recv] {
            Value::String(recv) => recv.push_str(&append),
            _ => unimplemented!("append string to non-string"),
        }
        Ok(())
    }
    #[inline]
    fn op_load_symbol(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        let sym = self.read_symbol()?;
        self.frames.top_mut().unwrap().register[out] = Value::Symbol(sym);
        Ok(())
    }
    #[inline]
    fn op_load_i64(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        match self.read_static()? {
            Static::Int(value) => {
                self.frames.top_mut().unwrap().register[out] = Value::Fixnum(*value);
                Ok(())
            }
            _ => Err(ThreadError::InvalidStatic)
        }
    }
    #[inline]
    fn op_load_float(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        match self.read_static()? {
            Static::Float(value) => {
                self.frames.top_mut().unwrap().register[out] = Value::Float(*value);
                Ok(())
            }
            _ => Err(ThreadError::InvalidStatic)
        }
    }
    #[inline]
    fn op_load_proc(&mut self) -> Result<(), ThreadError> {
        let out = self.read_addr()?;
        match self.read_static()? {
            Static::Proc(value) => {
                self.frames.top_mut().unwrap().register[out] = Value::Proc(Arc::clone(value));
                Ok(())
            }
            _ => Err(ThreadError::InvalidStatic)
        }
    }
    #[inline]
    fn op_arg(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_arg_assoc(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_arg_splat(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_arg_block(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_call(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_super(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_not(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_jump(&mut self) -> Result<(), ThreadError> {
        self.pc = self.read_addr()?;
        Ok(())
    }
    #[inline]
    fn op_jump_if(&mut self) -> Result<(), ThreadError> {
        let cond = self.read_addr()?;
        let jump = self.read_addr()?;
        if self.frames.top().unwrap().register[cond].is_truthy() {
            self.pc = jump;
        }
        Ok(())
    }
    #[inline]
    fn op_jump_if_not(&mut self) -> Result<(), ThreadError> {
        let cond = self.read_addr()?;
        let jump = self.read_addr()?;
        if !self.frames.top().unwrap().register[cond].is_truthy() {
            self.pc = jump;
        }
        Ok(())
    }
    #[inline]
    fn op_return(&mut self) -> Result<(), ThreadError> {
        let value = self.read_addr()?;
        let value = self.frames.top().unwrap().register[value].clone();
        self.pop_frame();
        self.frames.top_mut().unwrap().register[VOID] = value;
        Ok(())
    }
    #[inline]
    fn op_assign(&mut self) -> Result<(), ThreadError> {
        let recv = self.read_addr()?;
        let value = self.read_addr()?;
        let register = &mut self.frames.top_mut().unwrap().register;
        register[recv] = register[value].clone();
        Ok(())
    }
    #[inline]
    fn op_assign_global(&mut self) -> Result<(), ThreadError> {
        let name = self.read_symbol()?;
        let value = self.read_addr()?;
        let value = self.frames.top().unwrap().register[value].clone();
        self.context.globals().get().insert(name, value);
        Ok(())
    }
    #[inline]
    fn op_assign_const(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_assign_class_var(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_assign_ivar(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_begin_rescue(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_rescue_match(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_rescue_bind(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_end_rescue(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_defined_const(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_defined_global(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_defined_class_var(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_defined_ivar(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_yield(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_def_module(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_def_class(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_def_method(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_def_singleton_class(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
    #[inline]
    fn op_def_singleton_method(&mut self) -> Result<(), ThreadError> {
        unimplemented!()
    }
}

impl Iterator for Thread {
    type Item = Result<(), ThreadError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next().transpose()
    }
}

struct Stack<T> {
    buf: T,
    len: usize,
}

impl<T: Array> Stack<T> {
    fn new() -> Stack<T> {
        Stack {
            buf: unsafe { mem::uninitialized() },
            len: 0,
        }
    }

    fn push(&mut self, item: T::Item) -> Result<(), ThreadError> {
        if self.len == T::size() {
            return Err(ThreadError::StackOverflow);
        }
        unsafe { *self.buf.ptr_mut().offset(self.len as isize) = item };
        self.len += 1;
        Ok(())
    }

    fn pop(&mut self) -> Option<T::Item> {
        if self.len > 0 {
            self.len -= 1;
            let item = unsafe {
                mem::replace(
                    &mut *self.buf.ptr_mut().offset(self.len as isize),
                    mem::uninitialized(),
                )
            };
            Some(item)
        } else {
            None
        }
    }

    fn top(&self) -> Option<&T::Item> {
        if self.len > 0 {
            unsafe { Some(&*self.buf.ptr().offset(self.len as isize - 1)) }
        } else {
            None
        }
    }

    fn top_mut(&mut self) -> Option<&mut T::Item> {
        if self.len > 0 {
            unsafe { Some(&mut *self.buf.ptr_mut().offset(self.len as isize - 1)) }
        } else {
            None
        }
    }
}
