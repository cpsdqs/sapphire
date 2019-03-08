use crate::object::Object;
use parking_lot::{Mutex, MutexGuard};
use std::sync::Arc;

#[derive(Debug, Default)]
pub struct Ref<T: ?Sized>(Arc<Mutex<T>>);

impl Ref<Object> {
    pub fn new<T: Object + 'static>(this: T) -> Ref<Object> {
        Ref(Arc::new(Mutex::new(this)))
    }
}

impl<T> Ref<T> {
    pub fn new_generic(this: T) -> Ref<T> {
        Ref(Arc::new(Mutex::new(this)))
    }
}

impl<T: ?Sized> Ref<T> {
    pub fn get(&self) -> MutexGuard<T> {
        self.0.lock()
    }
}

impl<T: ?Sized> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref(Arc::clone(&self.0))
    }
}
