use parking_lot::{Mutex, MutexGuard};
use std::sync::Arc;

pub struct Ref<T: ?Sized>(Arc<Mutex<T>>);

impl<T> Ref<T> {
    pub fn get(&self) -> MutexGuard<T> {
        self.0.lock()
    }
}
