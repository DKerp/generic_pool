//! A pool for recycling allocated objects for later reuse. Uses generic get/put methods so you can store (almost) any type in a single pool instance.
//!
//! The main advantage of this library compared to other pools is that it can store and retrieve an __abritrary__ number of __different objects__ seemlessly.
//! The pool itself does not contain any generics, only the get und put methods do. You initialise the pool and add any object to it that you want to recycle,
//! and when you need them later on you just tell the pool which type of object you want. The internal implementation does all the magic of selecting the correct
//! object type.
//!
//! This library is 100% pure Rust, uses no unstable or nighly only features and, most importantly, does not contain any unsafe code.
//! It also has zero dependencies on default, but has an optional serde feature which enables de-/serialization of the configuration.
//!
//! ## Features
//!
//! - Provides a lightweigth __regular version__ as well as a thread-save __sync version__.
//! - Does optionally provide a __drop guard__ which will automatically add objects back to the store after they go out of scope.
//! - Allows configuring the maximum number of stored objects to prevent RAM exhaustion.
//! - Allows configuring the rate at which the internal capacity gets increased over time.
//! - Each configuration option can be set for each object type individually.
//! - You can also set a fallback configuration for object types for which no constum configuration was set.
//! - Provides optional auto-creation of objects which implement the [`Default`](https://doc.rust-lang.org/std/default/trait.Default.html) trait.
//! - Offers cheap [`Clon`](https://doc.rust-lang.org/std/clone/trait.Clone.html)ing, so you can easily use the same pool in many places.
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
//!     let mut pool = Pool::new();
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
//! ## Security
//!
//! The pool does not modify the objects it receives or gives out in any way, and does thus in particular not reset objects properly.
//! __It is your responsibility to reset objects as appropriate either before adding them back to the store, or after receiving them.__
//! Otherwise you will likely open up security holes by accidentaly using data from earlier operations.
//!
//! ### A common bad example
//!
//! Many frontend APIs do also provide priviledged access if they receive the proper credentials. Depending on the implementation the admin flag
//! might only get explicitly set if some special credentials are send over. Without resetting the admin flag of recycled request objects this can
//! open up a big security hole.
//!
//! ```ignore
//! use generic_pool::Pool;
//!
//! struct FrontendRequest {
//!     admin_credentials: Option<String>,
//!     commands: Vec<String>,
//! }
//!
//! #[derive(Default)]
//! struct BackendRequest {
//!     is_admin: bool,
//!     commands: Vec<Command>,
//! }
//!
//! fn main() {
//!     let mut pool = Pool::new();
//!
//!     /* initialize API... */
//!
//!     loop {
//!         // Retrieve a frontend request.
//!         let frontend_req: FrontendRequest = get_frontend_req();
//!
//!         // Retrieve a recycled backend request object from the pool.
//!         let mut backend_req = pool.get_or_default_with_guard::<BackendRequest>();
//!
//!         /* HAZARD - The backend request object might still contain older commands. */
//!
//!         // Parse the commands and check if they are known and valid.
//!         for cmd in frountend_req.commands.iter() {
//!             match parse_cmd(cmd) {
//!                 Ok(parsed_cmd) => backend_req.commands.push(parsed_cmd),
//!                 Err(err) => return_error_to_client_and_abort(err),
//!             }
//!         }
//!
//!         /* HAZARD - the backend request might still have the admin flag set!!! */
//!
//!         if let Some(credentials) = &frontend_req.admin_credentials {
//!             match check_admin_credentials(credentials) {
//!                 Ok(_) => backend_req.is_admin = true,
//!                 Err(err) => return_error_to_client_and_abort(err),
//!             }
//!         }
//!
//!         // The backend might now receive unintended commands or even commands
//!         // from unpriviledged users with the admin flag set.
//!         send_backend_request(backend_req);
//!
//!         // NOTE: The drop guard of the backend request will put it back into the pool now.
//!     }
//! }
//! ```
//!
//! ### The solution
//!
//! Of course a simple solution would be the implement the whole parsing and checking process in such a way that all fields of any recycled object get always filled
//! with entirely new data, but this might not always be favorable. It can be difficult to check that all fields from all objects you recycle are overwritten before being used.
//!
//! The most secure solution is thus to write some explicit resetting logic for all objects you use and to make sure it gets called whenever you retrieve an object
//! from the pool.
//!
//! ```ignore
//! use generic_pool::{Pool, DropGuard};
//!
//! /// A local trait providing the reset logic for all recycled objects.
//! trait Reset {
//!     fn reset(self) -> Self; // See below why we use this pattern.
//! }
//!
//! struct FrontendRequest {
//!     admin_credentials: Option<String>,
//!     commands: Vec<String>,
//! }
//!
//! #[derive(Default)]
//! struct BackendRequest {
//!     is_admin: bool,
//!     commands: Vec<Command>,
//! }
//!
//! // We implement the trait on any drop guard by this.
//! impl<G: AsMut<BackendRequest>> Reset for G {
//!     fn reset(mut self) -> Self {
//!         let req = self.as_mut();
//!         req.is_admin = false;
//!         req.commands.clear();
//!
//!         self
//!     }
//! }
//!
//! fn main() {
//!     let mut pool = Pool::new();
//!
//!     /* initialize API... */
//!
//!     loop {
//!         // Retrieve a frontend request.
//!         let frontend_req: FrontendRequest = get_frontend_req();
//!
//!         // Retrieve a recycled backend request object from the pool.
//!         let mut backend_req = match pool.get_with_guard::<BackendRequest>() {
//!             Some(req) => req.reset(), // Is an expression, gives back req
//!             None => DropGuard::new(BackendRequest::default(), &pool), // No need to reset
//!         };
//!
//!         /* We can proceed safely now */
//!     }
//! }
//! ```
//!
//! ## Configuration
//!
//! You can configure the maximum size of the stores internal buffers, as well as their initial size and the rate at which the capacity increases.
//! These settings always apply to a specific type of object beeing stored in the pool, and never at
//! the pool as a whole. That is if you store e.g. 2 different object types and configure a default
//! maximum capacity of __1_000__, the pool will store up to __2_000__ objects. The settings are intended for limiting the memory usage of the pool.
//!
//! The pools fallback configuration will apply to all object types until you set a custom one for an object type. If you do not specify a costum
//! fallback config for the pool, the pool will use the default values for [`Config`].
//!
//! ### Example
//!
//! ```rust
//! use generic_pool::{Pool, Config};
//!
//! fn main() {
//!     // Use a non-default config.
//!     let config = Config {
//!         max: 1_000,
//!         step: 100,
//!         start: 500,
//!     };
//!
//!     assert_ne!(config, Config::default());
//!
//!     let mut pool = Pool::with_fallback_config(config); // NOTE Config implements Copy.
//!
//!     // Alternative:
//!     // let mut pool = Pool::new();
//!     // pool.set_fallback_config(config);
//!
//!     assert_eq!(config, pool.fallback_config());
//!     assert_eq!(config, pool.get_config::<Vec<u8>>());
//!
//!     // Create a costum config for `Vec<u8>`.
//!     let custom_config = Config {
//!         max: 100,
//!         step: 10,
//!         start: 10,
//!     };
//!
//!     pool.set_config::<Vec<u8>>(custom_config);
//!
//!     assert_eq!(custom_config, pool.get_config::<Vec<u8>>());
//! }
//! ```
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::rc::Rc;
use std::cell::RefCell;
use std::any::{Any, TypeId};
use std::ops::{Deref, DerefMut};
use std::fmt;
use std::hash::{Hash, Hasher};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};



#[cfg(test)]
mod tests;



/// The configuration options of the pools internal store.
///
/// The settings always apply to a specific type of object beeing stored in the pool, and never at
/// the pool as a whole. That is if you store e.g. 2 different object types and configure a default
/// maximum capacity of __1_000__, the pool will store up to __2_000__ objects.
/// The settings are intended for limiting the memory usage of the pool.
///
/// The pools fallback configuration will apply to all object types until you set a custom one for an object type.
/// If you do not specify a costum fallback config for the pool, the pool will use the default values for [`Config`].
///
/// We allow setting a [`max`](#structfield.max) capacity of the internal pool buffer. If the buffer is full
/// and another object gets added to the pool, the new object will get dropped.
///
/// If a certain buffer gets created, it will use the [`start`](#structfield.start) value to configure its
/// initial capacity. Whenever the capacity is reached and its length has not yet reached the
/// [`max`](#structfield.max) value, the [`step`](#structfield.step) value is used to determine by which amount
/// the capacity should be increased. Note that it will use a lower value then [`step`](#structfield.step)
/// if otherwise the capacity would exceed the [`max`](#structfield.max) value.
///
/// # Example
///
/// ```rust
/// use generic_pool::{Pool, Config};
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
///     let mut pool = Pool::with_fallback_config(config); // NOTE Config implements Copy.
///
///     // Alternative:
///     // let mut pool = Pool::new();
///     // pool.set_fallback_config(config);
///
///     assert_eq!(config, pool.fallback_config());
///     assert_eq!(config, pool.get_config::<Vec<u8>>());
///
///     // Create a costum config for `Vec<u8>`.
///     let custom_config = Config {
///         max: 100,
///         step: 10,
///         start: 10,
///     };
///
///     pool.set_config::<Vec<u8>>(custom_config);
///
///     assert_eq!(custom_config, pool.get_config::<Vec<u8>>());
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

impl fmt::Display for Config {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Config {{max: {}, start: {}, step: {}}})", self.max, self.start, self.step)
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
///
/// This version is not thread-safe. For the thread-safe version take a look at [`SyncDropGuard`].
#[derive(Debug)]
pub struct DropGuard<T: Any> {
    inner: Option<T>,
    pool: Rc<RefCell<PoolInner<Box<dyn Any>>>>,
}

impl<T: Any + Clone> Clone for DropGuard<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Clone::clone(&self.inner),
            pool: Rc::clone(&self.pool),
        }
    }
}

impl<T: Any + PartialEq> PartialEq for DropGuard<T> {
    fn eq(&self, other: &DropGuard<T>) -> bool {
        self.inner.as_ref().unwrap().eq(other.inner.as_ref().unwrap())
    }
}

impl<T: Any + Eq> Eq for DropGuard<T> {}

impl<T: Any + PartialOrd> PartialOrd for DropGuard<T> {
    fn partial_cmp(&self, other: &DropGuard<T>) -> Option<std::cmp::Ordering> {
        self.inner.as_ref().unwrap().partial_cmp(other.inner.as_ref().unwrap())
    }
}

impl<T: Any + Ord> Ord for DropGuard<T> {
    fn cmp(&self, other: &DropGuard<T>) -> std::cmp::Ordering {
        self.inner.as_ref().unwrap().cmp(other.inner.as_ref().unwrap())
    }
}

/// The hash value corresponds to the hash value of the contained object.
impl<T: Any + Hash> Hash for DropGuard<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.as_ref().unwrap().hash(state);
    }
}

impl<T: Any + fmt::Display> fmt::Display for DropGuard<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.inner.as_ref().unwrap(), f)
    }
}

impl<T: Any> DropGuard<T> {
    /// Creates a new [`DropGuard`] from an abritrary object and adds the reference to a regular [`Pool`].
    ///
    /// # Example
    ///
    /// Creating a [`DropGuard`] yourself might be usefull if you want to use objects which
    /// you want to manually create, but where you still want to have the auto-adding to the pool
    /// once they go out of scope.
    ///
    /// ```rust
    /// use generic_pool::{Pool, DropGuard};
    ///
    /// struct Buffer(Vec<u8>);
    ///
    /// impl Buffer {
    ///     fn len(&self) -> usize {
    ///         self.0.len()
    ///     }
    /// }
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
    ///
    ///     // You use buffers which have a length of exactly 1kb.
    ///     let buffer = match pool.get_with_guard::<Buffer>() {
    ///         Some(buffer) => buffer,
    ///         None => DropGuard::new(Buffer(vec![0u8; 1024]), &pool),
    ///     };
    ///
    ///     assert_eq!(buffer.len(), 1024);
    ///
    ///     assert!(pool.get::<Buffer>().is_none());
    ///     drop(buffer);
    ///     let buffer = pool.get::<Buffer>().unwrap();
    ///     assert_eq!(buffer.len(), 1024);
    /// }
    /// ```
    pub fn new(obj: T, pool: &Pool) -> Self {
        let inner = Some(obj);
        let pool = Rc::clone(&pool.inner);
        Self {
            inner,
            pool,
        }
    }

    /// Consume this guard and return the contained object.
    ///
    /// Note that this is an associated function and not a method. See the example.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::{Pool, DropGuard};
    ///
    /// struct Buffer(Vec<u8>);
    ///
    /// impl Buffer {
    ///     fn len(&self) -> usize {
    ///         self.0.len()
    ///     }
    /// }
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
    ///
    ///     // You use buffers which have a length of exactly 1kb.
    ///     let buffer = match pool.get_with_guard::<Buffer>() {
    ///         Some(buffer) => buffer,
    ///         None => DropGuard::new(Buffer(vec![0u8; 1024]), &pool),
    ///     };
    ///
    ///     // Maybe you want to use the buffer for something else.
    ///     let buffer: Buffer = DropGuard::into_inner(buffer);
    ///     let mut buffer: Vec<u8> = buffer.0;
    ///     buffer.clear();
    ///
    ///     assert_eq!(buffer.len(), 0);
    /// }
    /// ```
    pub fn into_inner(mut guard: DropGuard<T>) -> T {
        guard.inner.take().unwrap()
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
///
/// This version is thread-safe. For the not thread-safe version take a look at [`DropGuard`].
pub struct SyncDropGuard<T: Any + Send + Sync> {
    inner: Option<T>,
    pool: Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>,
}

impl<T: Any + Send + Sync + Clone> Clone for SyncDropGuard<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Clone::clone(&self.inner),
            pool: Arc::clone(&self.pool),
        }
    }
}

impl<T: Any + Send + Sync + PartialEq> PartialEq for SyncDropGuard<T> {
    fn eq(&self, other: &SyncDropGuard<T>) -> bool {
        self.inner.as_ref().unwrap().eq(other.inner.as_ref().unwrap())
    }
}

impl<T: Any + Send + Sync + Eq> Eq for SyncDropGuard<T> {}

impl<T: Any + Send + Sync + PartialOrd> PartialOrd for SyncDropGuard<T> {
    fn partial_cmp(&self, other: &SyncDropGuard<T>) -> Option<std::cmp::Ordering> {
        self.inner.as_ref().unwrap().partial_cmp(other.inner.as_ref().unwrap())
    }
}

impl<T: Any + Send + Sync + Ord> Ord for SyncDropGuard<T> {
    fn cmp(&self, other: &SyncDropGuard<T>) -> std::cmp::Ordering {
        self.inner.as_ref().unwrap().cmp(other.inner.as_ref().unwrap())
    }
}

/// The hash value corresponds to the hash value of the contained object.
impl<T: Any + Send + Sync + Hash> Hash for SyncDropGuard<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.as_ref().unwrap().hash(state);
    }
}

impl<T: Any + Send + Sync + fmt::Display> fmt::Display for SyncDropGuard<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.inner.as_ref().unwrap(), f)
    }
}

impl<T: Any + Send + Sync> SyncDropGuard<T> {
    /// Creates a new [`DropGuard`] from an abritrary object and adds the reference to a [`SyncPool`].
    ///
    /// # Example
    ///
    /// Creating a [`SyncDropGuard`] yourself might be usefull if you want to use objects which
    /// you want to manually create, but where you still want to have the auto-adding to the pool
    /// once they go out of scope.
    ///
    /// ```rust
    /// use generic_pool::{SyncPool, SyncDropGuard};
    ///
    /// struct Buffer(Vec<u8>);
    ///
    /// impl Buffer {
    ///     fn len(&self) -> usize {
    ///         self.0.len()
    ///     }
    /// }
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
    ///
    ///     // You use buffers which have a length of exactly 1kb.
    ///     let buffer = match pool.get_with_guard::<Buffer>() {
    ///         Some(buffer) => buffer,
    ///         None => SyncDropGuard::new(Buffer(vec![0u8; 1024]), &pool),
    ///     };
    ///
    ///     assert_eq!(buffer.len(), 1024);
    ///
    ///     assert!(pool.get::<Buffer>().is_none());
    ///     drop(buffer);
    ///     let buffer = pool.get::<Buffer>().unwrap();
    ///     assert_eq!(buffer.len(), 1024);
    /// }
    /// ```
    pub fn new(obj: T, pool: &SyncPool) -> Self {
        let inner = Some(obj);
        let pool = Arc::clone(&pool.inner);
        Self {
            inner,
            pool,
        }
    }

    /// Consume this guard and return the contained object.
    ///
    /// Note that this is an associated function and not a method. See the example.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::{SyncPool, SyncDropGuard};
    ///
    /// struct Buffer(Vec<u8>);
    ///
    /// impl Buffer {
    ///     fn len(&self) -> usize {
    ///         self.0.len()
    ///     }
    /// }
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
    ///
    ///     // You use buffers which have a length of exactly 1kb.
    ///     let buffer = match pool.get_with_guard::<Buffer>() {
    ///         Some(buffer) => buffer,
    ///         None => SyncDropGuard::new(Buffer(vec![0u8; 1024]), &pool),
    ///     };
    ///
    ///     // Maybe you want to use the buffer for something else.
    ///     let buffer: Buffer = SyncDropGuard::into_inner(buffer);
    ///     let mut buffer: Vec<u8> = buffer.0;
    ///     buffer.clear();
    ///
    ///     assert_eq!(buffer.len(), 0);
    /// }
    /// ```
    pub fn into_inner(mut guard: SyncDropGuard<T>) -> T {
        guard.inner.take().unwrap()
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
#[derive(Debug)]
struct PoolInner<B> {
    /// Contains the internal buffers, with each one holding one specific object type.
    store: HashMap<TypeId, Vec<B>>,
    /// Contains the custom configuration for each specific object type, if any.
    config: HashMap<TypeId, Config>,
    /// The default configuration for all object types which do not have a costum configuration.
    fallback_config: Config,
}

impl<B> Default for PoolInner<B> {
    fn default() -> Self {
        Self {
            store: HashMap::new(),
            config: HashMap::new(),
            fallback_config: Config::default(),
        }
    }
}

impl<B> fmt::Display for PoolInner<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f, "{{store: {} entries, config: {} entries, fallback_config: {}}}",
            self.store.len(), self.config.len(),
            self.fallback_config.to_string(),
        )
    }
}

impl<B> PoolInner<B> {
    /// Retrieve the configuration for a specific object type.
    pub fn get_config<T: Any>(&self) -> Config {
        let id = TypeId::of::<T>();

        match self.config.get(&id) {
            Some(config) => config.clone(),
            None => self.fallback_config,
        }
    }

    /// Retrieve the fallback configuration for all object types with no costum configuration set.
    pub fn fallback_config(&self) -> Config {
        self.fallback_config
    }

    /// Sets the costum configuration for a specific object type.
    pub fn set_config<T: Any>(&mut self, config: Config) {
        let id = TypeId::of::<T>();

        self.config.insert(id, config);
    }

    /// Deletes the costum configuration of a specific object type.
    pub fn delete_config<T: Any>(&mut self) {
        let id = TypeId::of::<T>();

        self.config.remove(&id);
    }

    /// Sets the fallback configuration for all object types with no costum configuration set.
    pub fn set_fallback_config(&mut self, config: Config) {
        self.fallback_config = config;
    }

    /// Tries to retrieve an instance of a specific object type from the internal buffer.
    pub fn get<T: Any>(&mut self) -> Option<B> {
        let id = TypeId::of::<T>();

        if let Some(list) = self.store.get_mut(&id) {
            return list.pop();
        }

        return None
    }

    /// Tries to add an instance of a specific object type into the internal buffer.
    pub fn put<T: Any>(&mut self, boxed_obj: B) {
        let id = TypeId::of::<T>();

        let config = match self.config.get(&id) {
            Some(config) => config,
            None => &self.fallback_config,
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
///
/// This version is not thread-safe. For the thread-safe version take a look at [`SyncPool`].
#[derive(Default)]
pub struct Pool {
    pub(crate) inner: Rc<RefCell<PoolInner<Box<dyn Any>>>>,
}

/// The cloned [`Pool`] will still point to the same instance.
impl Clone for Pool {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl fmt::Display for Pool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Pool {}", self.inner.borrow().to_string())
    }
}

impl Pool {
    /// Creates a new [`Pool`] with the default fallback configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{Pool, Config};
    ///
    /// fn main() {
    ///     let pool = Pool::new();
    ///     // Equivalent:
    ///     // let pool = Pool::default();
    ///
    ///     assert_eq!(pool.fallback_config(), Config::default());
    /// }
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new [`Pool`] with the provided [`Config`] as the fallback configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{Pool, Config};
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
    ///     let mut pool = Pool::with_fallback_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.fallback_config());
    /// }
    /// ```
    pub fn with_fallback_config(config: Config) -> Self {
        let mut pool = Self::default();
        pool.set_fallback_config(config);

        pool
    }

    /// Retrieve the currently active [`Config`] for the provided object type.
    ///
    /// If you have not manually set a [`Config`] for the provided object type, this method
    /// will return the fallback configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{Pool, Config};
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
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
    ///     // We would get back the fallback config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.fallback_config());
    /// }
    /// ```
    pub fn get_config<T: Any>(&self) -> Config {
        self.inner.borrow().get_config::<T>()
    }

    /// Retrieve the fallback [`Config`] for all object types which do not have
    /// a specific [`Config`] set.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{Pool, Config};
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
    ///     let mut pool = Pool::with_fallback_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.fallback_config());
    /// }
    /// ```
    pub fn fallback_config(&self) -> Config {
        self.inner.borrow().fallback_config
    }

    /// Update the [`Config`] for the provided object type.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{Pool, Config};
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
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
    ///     // We would get back the fallback config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.fallback_config());
    /// }
    /// ```
    pub fn set_config<T: Any>(&mut self, config: Config) {
        self.inner.borrow_mut().set_config::<T>(config);
    }

    /// Delete the costum [`Config`] for the provided object type.
    /// Afterwards the fallback config will apply again.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{Pool, Config};
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
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
    ///     assert_eq!(pool.get_config::<Vec<u8>>(), config);
    ///
    ///     // Delete the costum config. Afterwards the fallback config will apply.
    ///     pool.delete_config::<Vec<u8>>();
    ///
    ///     assert_ne!(pool.get_config::<Vec<u8>>(), config);
    ///     assert_eq!(pool.get_config::<Vec<u8>>(), pool.fallback_config());
    /// }
    /// ```
    pub fn delete_config<T: Any>(&mut self) {
        self.inner.borrow_mut().delete_config::<T>();
    }

    /// Set the fallback [`Config`] for all object types which do not have
    /// a specific [`Config`] set.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{Pool, Config};
    ///
    /// fn main() {
    ///     // Start with the default config.
    ///     let mut pool = Pool::new();
    ///
    ///     // Create a non-default config.
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     assert_ne!(config, pool.fallback_config());
    ///
    ///     pool.set_fallback_config(config);
    ///
    ///     assert_eq!(config, pool.fallback_config());
    /// }
    /// ```
    pub fn set_fallback_config(&mut self, config: Config) {
        self.inner.borrow_mut().fallback_config = config;
    }

    /// Try to retrieve an object of the specified type. Will give back `None` if the there are no
    /// objects of this type currently stored in the pool.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
    ///
    ///     let buffer = match pool.get::<Vec<u8>>() {
    ///         Some(mut buffer) => {
    ///             buffer.clear();
    ///             buffer
    ///         }
    ///         None => Vec::new(),
    ///     };
    ///
    ///     assert_eq!(buffer.len(), 0);
    /// }
    /// ```
    pub fn get<T: Any>(&mut self) -> Option<T> {
        if let Some(boxed_obj) = self.inner.borrow_mut().get::<T>() {
            if let Ok(element) = boxed_obj.downcast::<T>() {
                return Some(*element);
            }
        }

        None
    }

    /// Retrieve an object of the specified type. If there is no object of this type currently
    /// stored in the pool it will create one using its [`Default`] implementation.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
    ///
    ///     let buffer = pool.get_or_default::<Vec<u8>>();
    ///
    ///     assert_eq!(buffer.len(), 0);
    /// }
    /// ```
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

    /// Try to retrieve an object of the specified type with a drop guard. Will give back `None`
    /// if the there are no objects of this type currently stored in the pool.
    ///
    /// Once the returned [`DropGuard`] - if any - goes out of scope it will automatically put
    /// the contained object pack into the pool. Note that the object might still get dropped
    /// by the pool if the corresponding internal buffer is full.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::{Pool, DropGuard};
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
    ///
    ///     let buffer: DropGuard<Vec<u8>> = match pool.get_with_guard::<Vec<u8>>() {
    ///         Some(mut buffer) => {
    ///             // DropGuard is a smart pointer, so we can call Vec.clear() directly.
    ///             buffer.clear();
    ///             buffer
    ///         }
    ///         None => DropGuard::new(Vec::new(), &pool),
    ///     };
    ///
    ///     assert_eq!(buffer.len(), 0);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_none());
    ///
    ///     // Will put the buffer back into the pool.
    ///     drop(buffer);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_some());
    /// }
    /// ```
    pub fn get_with_guard<T: Any>(&mut self) -> Option<DropGuard<T>> {
        match self.get::<T>() {
            Some(obj) => Some(DropGuard::new(obj, self)),
            None => None,
        }
    }

    /// Retrieve an object of the specified type with a drop guard. If there is no object of this type currently
    /// stored in the pool it will create one using its [`Default`] implementation.
    ///
    /// Once the returned [`DropGuard`] goes out of scope it will automatically put
    /// the contained object pack into the pool. Note that the object might still get dropped
    /// by the pool if the corresponding internal buffer is full.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::{Pool, DropGuard};
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
    ///
    ///     let buffer: DropGuard<Vec<u8>> = pool.get_or_default_with_guard::<Vec<u8>>();
    ///
    ///     assert_eq!(buffer.len(), 0);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_none());
    ///
    ///     // Will put the buffer back into the pool.
    ///     drop(buffer);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_some());
    /// }
    /// ```
    pub fn get_or_default_with_guard<T: Any + Default>(&mut self) -> DropGuard<T> {
        let obj = self.get_or_default::<T>();

        DropGuard::new(obj, self)
    }

    /// Add an object to the pool. Unless the corresponding internal buffer is full, you can retrieve
    /// it later on by calling [`Pool.get()`](#method.get) or one of its variants.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::Pool;
    ///
    /// fn main() {
    ///     let mut pool = Pool::new();
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_none());
    ///
    ///     pool.put(vec![0u8; 1024]);
    ///
    ///     let buffer = pool.get::<Vec<u8>>().unwrap();
    ///
    ///     assert_eq!(buffer.len(), 1024);
    /// }
    /// ```
    pub fn put<T: Any>(&mut self, obj: T) {
        let obj = Box::new(obj);
        self.inner.borrow_mut().put::<T>(obj);
    }
}


/// A thread-safe pool that allows storing abritrary objects.
///
/// This version is thread-safe. For the not thread-safe version take a look at [`Pool`].
#[derive(Default)]
pub struct SyncPool {
    pub(crate) inner: Arc<RwLock<PoolInner<Box<dyn Any + Send + Sync>>>>,
}

/// The cloned [`SyncPool`] will still point to the same instance.
impl Clone for SyncPool {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl fmt::Display for SyncPool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SyncPool {}", self.inner.read().unwrap().to_string())
    }
}

impl SyncPool {
    /// Creates a new [`SyncPool`] with the default fallback configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{SyncPool, Config};
    ///
    /// fn main() {
    ///     let pool = SyncPool::new();
    ///     // Equivalent:
    ///     // let pool = SyncPool::default();
    ///
    ///     assert_eq!(pool.fallback_config(), Config::default());
    /// }
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new [`SyncPool`] with the provided [`Config`] as the fallback configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{SyncPool, Config};
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
    ///     let mut pool = SyncPool::with_fallback_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.fallback_config());
    /// }
    /// ```
    pub fn with_fallback_config(config: Config) -> Self {
        let mut pool = Self::default();
        pool.set_fallback_config(config);

        pool
    }

    /// Retrieve the currently active [`Config`] for the provided object type.
    ///
    /// If you have not manually set a [`Config`] for the provided object type, this method
    /// will return the fallback configuration.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{SyncPool, Config};
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
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
    ///     // We would get back the fallback config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.fallback_config());
    /// }
    /// ```
    pub fn get_config<T: Any + Send + Sync>(&mut self) -> Config {
        let inner = self.inner.read().unwrap();
        inner.get_config::<T>()
    }

    /// Retrieve the fallback [`Config`] for all object types which do not have
    /// a specific [`Config`] set.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{SyncPool, Config};
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
    ///     let mut pool = SyncPool::with_fallback_config(config); // NOTE Config implements Copy.
    ///
    ///     assert_eq!(config, pool.fallback_config());
    /// }
    /// ```
    pub fn fallback_config(&self) -> Config {
        let inner = self.inner.read().unwrap();
        inner.fallback_config()
    }

    /// Update the [`Config`] for the provided object type.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{SyncPool, Config};
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
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
    ///     // We would get back the fallback config without the line above.
    ///     let config_compare = pool.get_config::<Vec<u8>>();
    ///
    ///     assert_eq!(config_compare, config);
    ///     assert_ne!(config_compare, pool.fallback_config());
    /// }
    /// ```
    pub fn set_config<T: Any + Send + Sync>(&mut self, config: Config) {
        let mut inner = self.inner.write().unwrap();
        inner.set_config::<T>(config);
    }

    /// Delete the costum [`Config`] for the provided object type.
    /// Afterwards the fallback config will apply again.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{SyncPool, Config};
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
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
    ///     assert_eq!(pool.get_config::<Vec<u8>>(), config);
    ///
    ///     // Delete the costum config. Afterwards the fallback config will apply.
    ///     pool.delete_config::<Vec<u8>>();
    ///
    ///     assert_ne!(pool.get_config::<Vec<u8>>(), config);
    ///     assert_eq!(pool.get_config::<Vec<u8>>(), pool.fallback_config());
    /// }
    /// ```
    pub fn delete_config<T: Any>(&mut self) {
        let mut inner = self.inner.write().unwrap();
        inner.delete_config::<T>();
    }

    /// Set the fallback [`Config`] for all object types which do not have
    /// a specific [`Config`] set.
    ///
    /// # Example
    /// ```rust
    /// use generic_pool::{SyncPool, Config};
    ///
    /// fn main() {
    ///     // Start with the default config.
    ///     let mut pool = SyncPool::new();
    ///
    ///     // Create a non-default config.
    ///     let config = Config {
    ///         max: 1_000,
    ///         step: 100,
    ///         start: 500,
    ///     };
    ///
    ///     assert_ne!(config, pool.fallback_config());
    ///
    ///     pool.set_fallback_config(config);
    ///
    ///     assert_eq!(config, pool.fallback_config());
    /// }
    /// ```
    pub fn set_fallback_config(&mut self, config: Config) {
        let mut inner = self.inner.write().unwrap();
        inner.set_fallback_config(config);
    }

    /// Try to retrieve an object of the specified type. Will give back `None` if the there are no
    /// objects of this type currently stored in the pool.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::SyncPool;
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
    ///
    ///     let buffer = match pool.get::<Vec<u8>>() {
    ///         Some(mut buffer) => {
    ///             buffer.clear();
    ///             buffer
    ///         }
    ///         None => Vec::new(),
    ///     };
    ///
    ///     assert_eq!(buffer.len(), 0);
    /// }
    /// ```
    pub fn get<T: Any + Send + Sync>(&self) -> Option<T> {
        let mut inner = self.inner.write().unwrap();
        if let Some(boxed_obj) = inner.get::<T>() {
            if let Ok(element) = boxed_obj.downcast::<T>() {
                return Some(*element);
            }
        }

        None
    }

    /// Retrieve an object of the specified type. If there is no object of this type currently
    /// stored in the pool it will create one using its [`Default`] implementation.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::SyncPool;
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
    ///
    ///     let buffer = pool.get_or_default::<Vec<u8>>();
    ///
    ///     assert_eq!(buffer.len(), 0);
    /// }
    /// ```
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

    /// Try to retrieve an object of the specified type with a drop guard. Will give back `None`
    /// if the there are no objects of this type currently stored in the pool.
    ///
    /// Once the returned [`SyncDropGuard`] - if any - goes out of scope it will automatically put
    /// the contained object pack into the pool. Note that the object might still get dropped
    /// by the pool if the corresponding internal buffer is full.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::{SyncPool, SyncDropGuard};
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
    ///
    ///     let buffer: SyncDropGuard<Vec<u8>> = match pool.get_with_guard::<Vec<u8>>() {
    ///         Some(mut buffer) => {
    ///             // DropGuard is a smart pointer, so we can call Vec.clear() directly.
    ///             buffer.clear();
    ///             buffer
    ///         }
    ///         None => SyncDropGuard::new(Vec::new(), &pool),
    ///     };
    ///
    ///     assert_eq!(buffer.len(), 0);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_none());
    ///
    ///     // Will put the buffer back into the pool.
    ///     drop(buffer);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_some());
    /// }
    /// ```
    pub fn get_with_guard<T: Any + Send + Sync>(&mut self) -> Option<SyncDropGuard<T>> {
        match self.get::<T>() {
            Some(obj) => Some(SyncDropGuard::new(obj, self)),
            None => None,
        }
    }

    /// Retrieve an object of the specified type with a drop guard. If there is no object of this type currently
    /// stored in the pool it will create one using its [`Default`] implementation.
    ///
    /// Once the returned [`SyncDropGuard`] goes out of scope it will automatically put
    /// the contained object pack into the pool. Note that the object might still get dropped
    /// by the pool if the corresponding internal buffer is full.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::{SyncPool, SyncDropGuard};
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
    ///
    ///     let buffer: SyncDropGuard<Vec<u8>> = pool.get_or_default_with_guard::<Vec<u8>>();
    ///
    ///     assert_eq!(buffer.len(), 0);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_none());
    ///
    ///     // Will put the buffer back into the pool.
    ///     drop(buffer);
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_some());
    /// }
    /// ```
    pub fn get_or_default_with_guard<T: Any + Send + Sync + Default>(&mut self) -> SyncDropGuard<T> {
        let obj = self.get_or_default::<T>();

        SyncDropGuard::new(obj, self)
    }

    /// Add an object to the pool. Unless the corresponding internal buffer is full, you can retrieve
    /// it later on by calling [`SyncPool.get()`](#method.get) or one of its variants.
    ///
    /// # Example
    ///
    /// ```rust
    /// use generic_pool::SyncPool;
    ///
    /// fn main() {
    ///     let mut pool = SyncPool::new();
    ///
    ///     assert!(pool.get::<Vec<u8>>().is_none());
    ///
    ///     pool.put(vec![0u8; 1024]);
    ///
    ///     let buffer = pool.get::<Vec<u8>>().unwrap();
    ///
    ///     assert_eq!(buffer.len(), 1024);
    /// }
    /// ```
    pub fn put<T: Any + Send + Sync>(&self, obj: T) {
        let obj = Box::new(obj);
        let mut inner = self.inner.write().unwrap();
        inner.put::<T>(obj);
    }
}
