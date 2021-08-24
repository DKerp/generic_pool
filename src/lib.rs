use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::rc::Rc;
use std::cell::RefCell;
use std::any::{Any, TypeId};
use std::ops::{Deref, DerefMut};



#[cfg(test)]
mod tests;



#[derive(Debug, Clone, Copy)]
pub struct PoolConfig {
    /// The maximum number of elements of a specific type to hold inside the pool.
    pub max: usize,
    /// The initial capacity for the storage of elements of a specific type.
    pub start: usize,
    /// How much capacity to add to the storage of elements of a specific type if the storage
    /// is full and has not yet reached the maximum.
    pub step: usize,
}

impl Default for PoolConfig {
    fn default() -> Self {
        Self {
            max: 100_000,
            start: 1_000,
            step: 1_000,
        }
    }
}

impl PoolConfig {
    fn next_step(&self, current_capacity: usize) -> Option<usize> {
        if current_capacity>=self.max {
            return None;
        }

        let remaining = self.max-current_capacity;

        if remaining>=self.step {
            return Some(self.step);
        } else {
            return Some(remaining);
        }
    }

    fn allocate_as_necessary<T>(&self, store: &mut Vec<T>) -> Result<(), ()> {
        if store.len()>=self.max {
            return Err(()); // The store is already full.
        }

        let capacity = store.capacity();

        if capacity==store.len() {
            if let Some(step) = self.next_step(capacity) {
                store.reserve(step);
            }
        }

        Ok(())
    }
}


pub struct DropGuard<T: Any> {
    inner: Option<T>,
    pool: Rc<RefCell<PoolInner<Box<dyn Any>>>>,
}

impl<T: Any> DropGuard<T> {
    fn new(obj: T, pool: &Rc<RefCell<PoolInner<Box<dyn Any>>>>) -> Self {
        let inner = Some(obj);
        let pool = Rc::clone(pool);
        Self {
            inner,
            pool,
        }
    }

    pub fn into_inner(mut self) -> T {
        self.inner.take().unwrap()
    }
}

impl<T: Any> Drop for DropGuard<T> {
    fn drop(&mut self) {
        if let Some(obj) = self.inner.take() {
            let obj = Box::new(obj);
            if let Ok(mut pool) = self.pool.try_borrow_mut() {
                pool.put::<T>(obj);
            }
        }
    }
}

impl<T: Any> AsRef<T> for DropGuard<T> {
    fn as_ref(&self) -> &T {
        self.inner.as_ref().unwrap()
    }
}

impl<T: Any> AsMut<T> for DropGuard<T> {
    fn as_mut(&mut self) -> &mut T {
        self.inner.as_mut().unwrap()
    }
}

impl<T: Any> Deref for DropGuard<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: Any> DerefMut for DropGuard<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}


pub struct SyncDropGuard<T: Any + Send + Sync> {
    inner: Option<T>,
    pool: Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>,
}

impl<T: Any + Send + Sync> SyncDropGuard<T> {
    fn new(obj: T, pool: &Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>) -> Self {
        let inner = Some(obj);
        let pool = Arc::clone(pool);
        Self {
            inner,
            pool,
        }
    }

    pub fn into_inner(mut self) -> T {
        self.inner.take().unwrap()
    }
}

impl<T: Any + Send + Sync> Drop for SyncDropGuard<T> {
    fn drop(&mut self) {
        if let Some(obj) = self.inner.take() {
            let obj = Box::new(obj);
            if let Ok(mut pool) = self.pool.write() {
                pool.put::<T>(obj);
            }
        }
    }
}

impl<T: Any + Send + Sync> AsRef<T> for SyncDropGuard<T> {
    fn as_ref(&self) -> &T {
        self.inner.as_ref().unwrap()
    }
}

impl<T: Any + Send + Sync> AsMut<T> for SyncDropGuard<T> {
    fn as_mut(&mut self) -> &mut T {
        self.inner.as_mut().unwrap()
    }
}

impl<T: Any + Send + Sync> Deref for SyncDropGuard<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: Any + Send + Sync> DerefMut for SyncDropGuard<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}



struct PoolInner<B> {
    store: HashMap<TypeId, Vec<B>>,
    config: HashMap<TypeId, PoolConfig>,
    default_config: PoolConfig,
}

impl<B> Default for PoolInner<B> {
    fn default() -> Self {
        Self {
            store: HashMap::new(),
            config: HashMap::new(),
            default_config: PoolConfig::default(),
        }
    }
}

impl<B> PoolInner<B> {
    pub fn get_config<T: Any>(&self) -> PoolConfig {
        let id = TypeId::of::<T>();

        match self.config.get(&id) {
            Some(config) => config.clone(),
            None => self.default_config,
        }
    }

    pub fn get_default_config(&self) -> PoolConfig {
        self.default_config
    }

    pub fn set_config<T: Any>(&mut self, config: PoolConfig) {
        let id = TypeId::of::<T>();

        self.config.insert(id, config);
    }

    pub fn set_default_config(&mut self, config: PoolConfig) {
        self.default_config = config;
    }

    pub fn get<T: Any>(&mut self) -> Option<B> {
        let id = TypeId::of::<T>();

        if let Some(list) = self.store.get_mut(&id) {
            return list.pop();
        }

        return None
    }

    pub fn put<T: Any>(&mut self, boxed_obj: B) {
        let id = TypeId::of::<T>();

        let config = match self.config.get(&id) {
            Some(config) => config,
            None => &self.default_config,
        };

        match self.store.get_mut(&id) {
            Some(list) => {
                if let Ok(_) = config.allocate_as_necessary(list) {
                    list.push(boxed_obj);
                }
            }
            None => {
                let mut list = Vec::<B>::with_capacity(config.start);
                list.push(boxed_obj);
                self.store.insert(id, list);
            }
        }
    }
}



#[derive(Default)]
pub struct Pool {
    inner: Rc<RefCell<PoolInner<Box<dyn Any>>>>,
}

impl Pool {
    pub fn get_config<T: Any>(&self) -> PoolConfig {
        self.inner.borrow().get_config::<T>()
    }

    pub fn get_default_config(&self) -> PoolConfig {
        self.inner.borrow().default_config
    }

    pub fn set_config<T: Any>(&mut self, config: PoolConfig) {
        self.inner.borrow_mut().set_config::<T>(config);
    }

    pub fn set_default_config(&mut self, config: PoolConfig) {
        self.inner.borrow_mut().default_config = config;
    }

    pub fn get<T: Any>(&mut self) -> Option<T> {
        if let Some(boxed_obj) = self.inner.borrow_mut().get::<T>() {
            if let Ok(element) = boxed_obj.downcast::<T>() {
                return Some(*element);
            }
        }

        None
    }

    pub fn get_or_default<T: Any + Default>(&mut self) -> T {
        match self.get::<T>() {
            Some(obj) => {
                return obj
            }
            None => {
                return T::default()
            }
        }
    }

    pub fn get_with_guard<T: Any>(&mut self) -> Option<DropGuard<T>> {
        match self.get::<T>() {
            Some(obj) => Some(DropGuard::new(obj, &self.inner)),
            None => None,
        }
    }

    pub fn get_or_default_with_guard<T: Any + Default>(&mut self) -> DropGuard<T> {
        let obj = self.get_or_default::<T>();

        DropGuard::new(obj, &self.inner)
    }

    pub fn put<T: Any>(&mut self, obj: T) {
        let obj = Box::new(obj);
        self.inner.borrow_mut().put::<T>(obj);
    }
}



#[derive(Default)]
pub struct SyncPool {
    inner: Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>,
}

impl Clone for SyncPool {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl SyncPool {
    pub fn get_config<T: Any + Send + Sync>(&mut self) -> PoolConfig {
        let inner = self.inner.read().unwrap();
        inner.get_config::<T>()
    }

    pub fn get_default_config(&mut self) -> PoolConfig {
        let inner = self.inner.read().unwrap();
        inner.get_default_config()
    }

    pub fn set_config<T: Any + Send + Sync>(&mut self, config: PoolConfig) {
        let mut inner = self.inner.write().unwrap();
        inner.set_config::<T>(config);
    }

    pub fn set_default_config(&mut self, config: PoolConfig) {
        let mut inner = self.inner.write().unwrap();
        inner.set_default_config(config);
    }

    pub fn get<T: Any + Send + Sync>(&self) -> Option<T> {
        let mut inner = self.inner.write().unwrap();
        if let Some(boxed_obj) = inner.get::<T>() {
            if let Ok(element) = boxed_obj.downcast::<T>() {
                return Some(*element);
            }
        }

        None
    }

    pub fn get_or_default<T: Any + Send + Sync + Default>(&self) -> T {
        match self.get::<T>() {
            Some(obj) => {
                return obj
            }
            None => {
                return T::default()
            }
        }
    }

    pub fn get_with_guard<T: Any + Send + Sync>(&mut self) -> Option<SyncDropGuard<T>> {
        match self.get::<T>() {
            Some(obj) => Some(SyncDropGuard::new(obj, &self.inner)),
            None => None,
        }
    }

    pub fn get_or_default_with_guard<T: Any + Send + Sync + Default>(&mut self) -> SyncDropGuard<T> {
        let obj = self.get_or_default::<T>();

        SyncDropGuard::new(obj, &self.inner)
    }

    pub fn put<T: Any + Send + Sync>(&self, obj: T) {
        let obj = Box::new(obj);
        let mut inner = self.inner.write().unwrap();
        inner.put::<T>(obj);
    }
}
