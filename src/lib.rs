//! A pool for recycling allocated objects for later reuse. Uses generic get/put methods so you can store (almost) any type in a single pool instance.
//!
//! The main advantage of this library compared to other pools is that it can store and retrieve an __abritrary__ number of __different objects__ seemlessly.
//! The pool itself does not contain any generics, only the get und put methods do. You initialise the pool and add any object to it that you want to recycle,
//! and when you need them later on you just tell the pool which type of object you want. The internal implementation does all the magic of selecting the correct
//! object type.
//!
//! This library is 100% pure Rust, has zero dependencies, uses no unstable or nighly only features and, most importantly, does not contain any unsafe code.
//!
//! ## Features
//!
//! - Provides a lightweigth __regular version__ as well as a thread-save __sync version__.
//! - Does optionally provide a __drop guard__ which will automatically add objects back to the store after they go out of scope.
//! - Allows configuring the maximum number of stored objects to prevent RAM exhaustion.
//! - Allows configuring the rate at which the internal capacity gets increased over time.
//! - Each configuration option can be set for each object type individually.
//! - You can also set the default configuration for object types for which no constum configuration was set.
//! - Provides optional auto-creation of objects which implement the [`Default`](https://doc.rust-lang.org/std/default/trait.Default.html) trait.
//! - Offers cheap [`Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html)ing, so you can easily use the same pool in many places.
//!
//! ## A quick example
//!
//! This example demonstrates the most basic usage. We define two different structs, `Person` and `Animal`.
//! We insert both of them into the pool and retrieve them later on. Note that the pool ensures that the correct object type gets returned.
//!
//! ```rust
//! use generic_pool::Pool;
//!
//! #[derive(Debug, Default, Clone, Eq, PartialEq)]
//! struct Person {
//!     name: String,
//!     age: i32,
//! }
//!
//! #[derive(Debug, Default, Clone, Eq, PartialEq)]
//! struct Animal {
//!     home: bool,
//!     name: String,
//!     age_in_month: u64,
//! }
//!
//! fn main() {
//!     // Initialize the pool. Note that there is no need to specify any generic type parameters.
//!     let mut pool = Pool::default();
//!
//!     let p = Person {
//!         name: String::from("Mark"),
//!         age: 100,
//!     };
//!
//!     let a = Animal {
//!         home: true,
//!         name: String::from("Meow"),
//!         age_in_month: 12,
//!     };
//!
//!     // Add copies of the objects to the pool. Note that the pool takes any object.
//!     pool.put(p.clone());
//!     pool.put(a.clone());
//!
//!     // Retrieve the objects from the pool.
//!     let person: Person = pool.get().unwrap(); // Returns `Option<Person>`.
//!     let animal: Animal = pool.get_or_default(); // Returns `Animal`, but requires `Animal` to implement Default.
//!
//!     // The objects we retrieve are exactly the ones we put into the pool earlier.
//!     assert_eq!(person, p);
//!     assert_eq!(animal, a);
//!
//!     // NOTE: You should implement some kind of reset logic for objects saved in the pool.
//!     // E.g.: person.reset()
//! }
//! ```
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::rc::Rc;
use std::cell::RefCell;
use std::any::{Any, TypeId};
use std::ops::{Deref, DerefMut};



#[cfg(test)]
mod tests;



/// The configuration options of the pools internal store.
///
/// The settings always apply to a specific type of object beeing stored in the pool, and never at
/// the pool as a whole. The settings are intended for limiting the memory usage of the pool.
///
/// We allow setting a [`max`](#structfield.max) capacity of the internal pool buffer. If the buffer is full
/// and another object gets added to the pool, the new object will get dropped.
///
/// If a certain buffer gets created, it will use the [`start`](#structfield.start) value to configure its
/// initial capacity. Whenever the capacity is reached and its length has not yet reached the
/// [`max`](#structfield.max) value, the [`step`](#structfield.step) value is used to determine by which amount
/// the capacity should be increased. Note that it will use a lower value then [`step`](#structfield.step)
/// if otherwise the capacity would exceed the [`max`](#structfield.max) value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Config {
    /// The maximum number of elements of a specific type to be hold inside the pool.
    ///
    /// __Default__: 100_000
    pub max: usize,
    /// The initial capacity for the storage of elements of a specific type.
    ///
    /// __Default__: 1_000
    pub start: usize,
    /// How much capacity to add to the storage of elements of a specific type if the storage
    /// is full and has not yet reached the maximum.
    ///
    /// __Default__: 1_000
    pub step: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max: 100_000,
            start: 1_000,
            step: 1_000,
        }
    }
}

impl Config {
    /// Determines if we need to increase the capacity, and if so, by which amount.
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

    /// Allocates more space if necessary, and returns an empty error if the
    /// storage is full.
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


/// A smart pointer which automatically puts the contained object back into the [`Pool`] on drop.
pub struct DropGuard<T: Any> {
    inner: Option<T>,
    pool: Rc<RefCell<PoolInner<Box<dyn Any>>>>,
}

impl<T: Any> DropGuard<T> {
    /// Creates a new DropGuard from an abritrary object and adds the reference to a regular pool.
    fn new(obj: T, pool: &Rc<RefCell<PoolInner<Box<dyn Any>>>>) -> Self {
        let inner = Some(obj);
        let pool = Rc::clone(pool);
        Self {
            inner,
            pool,
        }
    }

    /// Consume this guard and return the contained object.
    pub fn into_inner(mut self) -> T {
        self.inner.take().unwrap()
    }
}

/// Ensures the contained value gets automatically added back to the [`Pool`] it came from.
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


/// A smart pointer which automatically puts the contained object back into the [`SyncPool`] on drop.
pub struct SyncDropGuard<T: Any + Send + Sync> {
    inner: Option<T>,
    pool: Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>,
}

impl<T: Any + Send + Sync> SyncDropGuard<T> {
    /// Creates a new DropGuard from an abritrary object and adds the reference to a sync pool.
    fn new(obj: T, pool: &Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>) -> Self {
        let inner = Some(obj);
        let pool = Arc::clone(pool);
        Self {
            inner,
            pool,
        }
    }

    /// Consume this guard and return the contained object.
    pub fn into_inner(mut self) -> T {
        self.inner.take().unwrap()
    }
}

/// Ensures the contained value gets automatically added back to the [`SyncPool`] it came from.
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



/// The internal structure for all pools we use. Contains all the magic implementation details.
struct PoolInner<B> {
    store: HashMap<TypeId, Vec<B>>,
    config: HashMap<TypeId, Config>,
    default_config: Config,
}

impl<B> Default for PoolInner<B> {
    fn default() -> Self {
        Self {
            store: HashMap::new(),
            config: HashMap::new(),
            default_config: Config::default(),
        }
    }
}

impl<B> PoolInner<B> {
    pub fn get_config<T: Any>(&self) -> Config {
        let id = TypeId::of::<T>();

        match self.config.get(&id) {
            Some(config) => config.clone(),
            None => self.default_config,
        }
    }

    pub fn get_default_config(&self) -> Config {
        self.default_config
    }

    pub fn set_config<T: Any>(&mut self, config: Config) {
        let id = TypeId::of::<T>();

        self.config.insert(id, config);
    }

    pub fn set_default_config(&mut self, config: Config) {
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



/// A pool that allows storing abritrary objects.
#[derive(Default)]
pub struct Pool {
    inner: Rc<RefCell<PoolInner<Box<dyn Any>>>>,
}

/// The cloned [`Pool`] will still point to the same instance.
impl Clone for Pool {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl Pool {
    /// Create a new [`Pool`] with the provided default [`Config`].
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     // Use a non-default config.
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     assert_ne!(config, Config::default());
    ///
    ///     let mut pool = Pool::with_default_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.get_default_config());
    /// }
    /// ```
    pub fn with_default_config(config: Config) -> Self {
        let mut pool = Self::default();
        pool.set_default_config(config);

        pool
    }

    /// Retrieve the currently active [`Config`] for the provided object type.
    ///
    /// If you have not yet manually set a [`Config`] for the provided object type, this method
    /// will return the active default configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     let mut pool = Pool::default();
    ///
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     // Set the config for `Vec<u8>`.
    ///     pool.set_config::<Vec<u8>>(config); // NOTE: Config implements Copy.
    ///
    ///     // Retrieve the config for `Vec<u8>`.
    ///     // We would get back the active default config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.get_default_config());
    /// }
    /// ```
    pub fn get_config<T: Any>(&self) -> Config {
        self.inner.borrow().get_config::<T>()
    }

    /// Retrieve the currently active default [`Config`] for all object types which do not have
    /// a specific [`Config`] yet.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     // Use a non-default config.
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     assert_ne!(config, Config::default());
    ///
    ///     let mut pool = Pool::with_default_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.get_default_config());
    /// }
    /// ```
    pub fn get_default_config(&self) -> Config {
        self.inner.borrow().default_config
    }

    /// Update the [`Config`] for the provided object type.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     let mut pool = Pool::default();
    ///
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     // Set the config for `Vec<u8>`.
    ///     pool.set_config::<Vec<u8>>(config); // NOTE: Config implements Copy.
    ///
    ///     // Retrieve the config for `Vec<u8>`.
    ///     // We would get back the active default config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.get_default_config());
    /// }
    /// ```
    pub fn set_config<T: Any>(&mut self, config: Config) {
        self.inner.borrow_mut().set_config::<T>(config);
    }

    pub fn set_default_config(&mut self, config: Config) {
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


/// A thread-safe pool that allows storing abritrary objects.
#[derive(Default)]
pub struct SyncPool {
    inner: Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>,
}

/// The cloned [`SyncPool`] will still point to the same instance.
impl Clone for SyncPool {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl SyncPool {
    /// Create a new [`SyncPool`] with the provided default [`Config`].
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     // Use a non-default config.
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     assert_ne!(config, Config::default());
    ///
    ///     let mut pool = Pool::with_default_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.get_default_config());
    /// }
    /// ```
    pub fn with_default_config(config: Config) -> Self {
        let mut pool = Self::default();
        pool.set_default_config(config);

        pool
    }

    /// Retrieve the currently active [`Config`] for the provided object type.
    ///
    /// If you have not yet manually set a [`Config`] for the provided object type, this method
    /// will return the active default configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::SyncPool;
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::default();
    ///
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     // Set the config for `Vec<u8>`.
    ///     pool.set_config::<Vec<u8>>(config); // NOTE: Config implements Copy.
    ///
    ///     // Retrieve the config for `Vec<u8>`.
    ///     // We would get back the active default config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.get_default_config());
    /// }
    /// ```
    pub fn get_config<T: Any + Send + Sync>(&mut self) -> Config {
        let inner = self.inner.read().unwrap();
        inner.get_config::<T>()
    }

    /// Retrieve the currently active default [`Config`] for all object types which do not have
    /// a specific [`Config`] yet.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     // Use a non-default config.
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     assert_ne!(config, Config::default());
    ///
    ///     let mut pool = Pool::with_default_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.get_default_config());
    /// }
    /// ```
    pub fn get_default_config(&mut self) -> Config {
        let inner = self.inner.read().unwrap();
        inner.get_default_config()
    }

    /// Update the [`Config`] for the provided object type.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::SyncPool;
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::default();
    ///
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     // Set the config for `Vec<u8>`.
    ///     pool.set_config::<Vec<u8>>(config); // NOTE: Config implements Copy.
    ///
    ///     // Retrieve the config for `Vec<u8>`.
    ///     // We would get back the active default config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.get_default_config());
    /// }
    /// ```
    pub fn set_config<T: Any + Send + Sync>(&mut self, config: Config) {
        let mut inner = self.inner.write().unwrap();
        inner.set_config::<T>(config);
    }

    pub fn set_default_config(&mut self, config: Config) {
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
