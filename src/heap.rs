use crate::object::Object;
use parking_lot::{ReentrantMutex, ReentrantMutexGuard};
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, Weak as WeakArc};

pub struct UnsafeMutableReentrantMutexGuard<'a, T: ?Sized>(ReentrantMutexGuard<'a, T>);

impl<'a, T: ?Sized> Deref for UnsafeMutableReentrantMutexGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.deref()
    }
}

impl<'a, T: ?Sized> DerefMut for UnsafeMutableReentrantMutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *(self.0.deref() as *const T as *mut T) }
    }
}

pub type RefGuard<'a, T> = UnsafeMutableReentrantMutexGuard<'a, T>;

#[derive(Default)]
pub struct Ref<T: ?Sized>(Arc<ReentrantMutex<T>>);

#[derive(Default)]
pub struct Weak<T: ?Sized>(WeakArc<ReentrantMutex<T>>);

impl Ref<Object> {
    pub fn new<T: Object + 'static>(this: T) -> Ref<Object> {
        Ref(Arc::new(ReentrantMutex::new(this)))
    }
}

impl<T> Ref<T> {
    pub fn new_generic(this: T) -> Ref<T> {
        Ref(Arc::new(ReentrantMutex::new(this)))
    }
}

impl<T: ?Sized> Ref<T> {
    pub fn get(&self) -> RefGuard<T> {
        UnsafeMutableReentrantMutexGuard(self.0.lock())
    }

    pub fn downgrade(&self) -> Weak<T> {
        Weak(Arc::downgrade(&self.0))
    }

    pub fn as_ptr(&self) -> *const T {
        &*self.get()
    }
}

impl<T: ?Sized> Weak<T> {
    pub fn upgrade(&self) -> Option<Ref<T>> {
        self.0.upgrade().map(Ref)
    }
}

impl<T: ?Sized> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref(Arc::clone(&self.0))
    }
}

impl<T: ?Sized> fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ref<{:?}>", &self.0 as *const Arc<_>)
    }
}
