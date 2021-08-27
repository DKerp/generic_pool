# generic_pool
A pool for recycling allocated objects for later reuse. Uses generic get/put methods so you can store (almost) any type in a single pool instance.

The main advantage of this library compared to other pools is that it can store and retrieve an __abritrary__ number of __different objects__ seemlessly.
The pool itself does not contain any generics, only the get und put methods do. You initialise the pool and add any object to it that you want to recycle,
and when you need them later on you just tell the pool which type of object you want. The internal implementation does all the magic of selecting the correct
object type.

This library is 100% pure Rust, uses no unstable or nighly only features and, most importantly, does not contain any unsafe code.
It also has zero dependencies on default, but has an optional serde feature which enables de-/serialization of the configuration.

## Features

- Provides a lightweigth __regular version__ as well as a thread-save __sync version__.
- Does optionally provide a __drop guard__ which will automatically add objects back to the store after they go out of scope.
- Allows configuring the maximum number of stored objects to prevent RAM exhaustion.
- Allows configuring the rate at which the internal capacity gets increased over time.
- Each configuration option can be set for each object type individually.
- You can also set a fallback configuration for object types for which no constum configuration was set.
- Provides optional auto-creation of objects which implement the [`Default`](https://doc.rust-lang.org/std/default/trait.Default.html) trait.
- Offers cheap [`Clon`](https://doc.rust-lang.org/std/clone/trait.Clone.html)ing, so you can easily use the same pool in many places.

## A quick example

This example demonstrates the most basic usage. We define two different structs, `Person` and `Animal`.
We insert both of them into the pool and retrieve them later on. Note that the pool ensures that the correct object type gets returned.

```rust
use generic_pool::Pool;

#[derive(Debug, Default, Clone, Eq, PartialEq)]
struct Person {
    name: String,
    age: i32,
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
struct Animal {
    home: bool,
    name: String,
    age_in_month: u64,
}

fn main() {
    // Initialize the pool. Note that there is no need to specify any generic type parameters.
    let mut pool = Pool::default();

    let p = Person {
        name: String::from("Mark"),
        age: 100,
    };

    let a = Animal {
        home: true,
        name: String::from("Meow"),
        age_in_month: 12,
    };

    // Add copies of the objects to the pool. Note that the pool takes any object.
    pool.put(p.clone());
    pool.put(a.clone());

    // Retrieve the objects from the pool.
    let person: Person = pool.get().unwrap(); // Returns `Option<Person>`.
    let animal: Animal = pool.get_or_default(); // Returns `Animal`, but requires `Animal` to implement Default.

    // The objects we retrieve are exactly the ones we put into the pool earlier.
    assert_eq!(person, p);
    assert_eq!(animal, a);

    // NOTE: You should implement some kind of reset logic for objects saved in the pool.
    // E.g.: person.reset()
}
```

## How it works

This library makes use of the [`Any`](https://doc.rust-lang.org/std/any/trait.Any.html) trait which provides a globally unique [`TypeId`](https://doc.rust-lang.org/std/any/struct.TypeId.html) for each object.

This [`TypeId`](https://doc.rust-lang.org/std/any/struct.TypeId.html) gets used as the key in a [`HashMap`](https://doc.rust-lang.org/std/collections/struct.HashMap.html) where the values are [`Vec`](https://doc.rust-lang.org/std/vec/struct.Vec.html)'s of boxed [trait objects](https://doc.rust-lang.org/book/ch17-02-trait-objects.html) implementing the [`Any`](https://doc.rust-lang.org/std/any/trait.Any.html) trait.

Upon adding/getting an object from the pool the objects [`TypeId`](https://doc.rust-lang.org/std/any/struct.TypeId.html) gets used to retrieve the correct [`Vec`](https://doc.rust-lang.org/std/vec/struct.Vec.html) where the value gets pushed to or popped from.

When getting a value, we use [`Box.downcast()`](https://doc.rust-lang.org/std/boxed/struct.Box.html#method.downcast) to convert the boxed [trait object](https://doc.rust-lang.org/book/ch17-02-trait-objects.html) back to a boxed version of the concrete type. Afterwards we can simply move the concrete object out of the box by dereferencing it, see the accepted answer at [this Stackoverflow question](https://stackoverflow.com/questions/42264041/how-do-i-get-an-owned-value-out-of-a-box).

## Security

The pool does not modify the objects it receives or gives out in any way, and does thus in particular not reset objects properly.
__It is your responsibility to reset objects as appropriate either before adding them back to the store, or after receiving them.__
Otherwise you will likely open up security holes by accidentaly using data from earlier operations.

### A common bad example

Many frontend APIs do also provide priviledged access if they receive the proper credentials. Depending on the implementation the admin flag
might only get explicitly set if some special credentials are send over. Without resetting the admin flag of recycled request objects this can
open up a big security hole.

```rust
use generic_pool::Pool;

struct FrontendRequest {
    admin_credentials: Option<String>,
    commands: Vec<String>,
}

#[derive(Default)]
struct BackendRequest {
    is_admin: bool,
    commands: Vec<Command>,
}

fn main() {
    let mut pool = Pool::default();

    /* initialize API... */

    loop {
        // Retrieve a frontend request.
        let frontend_req: FrontendRequest = get_frontend_req();

        // Retrieve a recycled backend request object from the pool.
        let mut backend_req = pool.get_or_default_with_guard::<BackendRequest>();

        /* HAZARD - The backend request object might still contain older commands. */

        // Parse the commands and check if they are known and valid.
        for cmd in frountend_req.commands.iter() {
            match parse_cmd(cmd) {
                Ok(parsed_cmd) => backend_req.commands.push(parsed_cmd),
                Err(err) => return_error_to_client_and_abort(err),
            }
        }

        /* HAZARD - the backend request might still have the admin flag set!!! */

        if let Some(credentials) = &frontend_req.admin_credentials {
            match check_admin_credentials(credentials) {
                Ok(_) => backend_req.is_admin = true,
                Err(err) => return_error_to_client_and_abort(err),
            }
        }

        // The backend might now receive unintended commands or even commands
        // from unpriviledged users with the admin flag set.
        send_backend_request(backend_req);

        // NOTE: The drop guard of the backend request will put it back into the pool now.
    }
}
```

### The solution

Of course a simple solution would be the implement the whole parsing and checking process in such a way that all fields of any recycled object get always filled
with entirely new data, but this might not always be favorable. It can be difficult to check that all fields from all objects you recycle are overwritten before being used.

The most secure solution is thus to write some explicit resetting logic for all objects you use and to make sure it gets called whenever you retrieve an object
from the pool.

```rust
use generic_pool::{Pool, DropGuard};

/// A local trait providing the reset logic for all recycled objects.
trait Reset {
    fn reset(self) -> Self; // See below why we use this pattern.
}

struct FrontendRequest {
    admin_credentials: Option<String>,
    commands: Vec<String>,
}

#[derive(Default)]
struct BackendRequest {
    is_admin: bool,
    commands: Vec<Command>,
}

// We implement the trait on any drop guard by this.
impl<G: AsMut<BackendRequest>> Reset for G {
    fn reset(mut self) -> Self {
        let req = self.as_mut();
        req.is_admin = false;
        req.commands.clear();

        self
    }
}

fn main() {
    let mut pool = Pool::default();

    /* initialize API... */

    loop {
        // Retrieve a frontend request.
        let frontend_req: FrontendRequest = get_frontend_req();

        // Retrieve a recycled backend request object from the pool.
        let mut backend_req = match pool.get_with_guard::<BackendRequest>() {
            Some(req) => req.reset(), // Is an expression, gives back req
            None => DropGuard::new(BackendRequest::default(), &pool), // No need to reset
        };

        /* We can proceed safely now */
    }
}
```

## Configuration

You can configure the maximum size of the stores internal buffers, as well as their initial size and the rate at which the capacity increases.
These settings always apply to a specific type of object beeing stored in the pool, and never at
the pool as a whole. That is if you store e.g. 2 different object types and configure a default
maximum capacity of __1_000__, the pool will store up to __2_000__ objects. The settings are intended for limiting the memory usage of the pool.

The pools fallback configuration will apply to all object types until you set a custom one for an object type. If you do not specify a costum
fallback config for the pool, the pool will use the default values of the `Config` object.

### Example

```rust
use generic_pool::{Pool, Config};

fn main() {
    // Use a non-default config.
    let config = Config {
        max: 1_000,
        step: 100,
        start: 500,
    };

    assert_ne!(config, Config::default());

    let mut pool = Pool::with_default_config(config); // NOTE Config implements Copy.

    // Alternative:
    // let mut pool = Pool::default();
    // pool.set_default_config(config);

    assert_eq!(config, pool.get_default_config());
    assert_eq!(config, pool.get_config::<Vec<u8>>());

    // Create a costum config for `Vec<u8>`.
    let custom_config = Config {
        max: 100,
        step: 10,
        start: 10,
    };

    pool.set_config::<Vec<u8>>(custom_config);

    assert_eq!(custom_config, pool.get_config::<Vec<u8>>());
}
```

## License

This library is licensed under the [MIT license](http://opensource.org/licenses/MIT).

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in this library by you, shall be licensed as MIT, without any additional terms or conditions.
