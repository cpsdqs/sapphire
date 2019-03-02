use std::any::Any;

pub trait Object: Send {
    /// Helper method for downcasting.
    fn as_any(&self) -> &Any;

    /// Helper method for downcasting.
    fn as_any_mut(&mut self) -> &mut Any;

    // TODO
}

impl Object {
    /// Attempts to downcast the [Object] into the type T.
    pub fn downcast_ref<T: Any + Send>(this: &Object) -> Option<&T> {
        this.as_any().downcast_ref::<T>()
    }

    /// Attempts to downcast the [Object] into a mutable reference to the type T.
    pub fn downcast_mut<T: Any + Send>(this: &mut Object) -> Option<&mut T> {
        this.as_any_mut().downcast_mut::<T>()
    }
}
