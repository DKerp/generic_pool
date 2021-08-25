# generic_pool
A pool for recycling allocated objects for later reuse. Uses generic get/put methods so you can store (almost) any type in a single pool instance.

The main advantage of this library compared to other pools is that it can store and retrieve an __abritrary__ number of __different objects__ seemlessly.
The pool itself does not contain any generics, only the get und put methods do. You initialise the pool and add any object to it that you want to recycle,
and when you need them later on you just tell the pool which type of object you want. The internal implementation does all the magic of selecting the correct
object type.

This library is 100% pure Rust, has zero dependencies, uses no unstable or nighly only features and does not contain any unsafe code.

## Features

- Provides a lightwaigth __regular version__ as well as a thread-save __sync version__.
- Does optionally provide a __drop guard__ which will automatically add objects back to the store after they go out of scope.
- Allows configuring the maximum number of stored objects to prevent RAM exhaustion.
- Allows configuring the rate at which the internal capacity gets increased over time.
- Each configuration option can be set for each object type individually.
- You can also set the default configuration for object types for which no constum configuration was set.
- Provides optional auto-creation of objects which implement the [`Default`](https://doc.rust-lang.org/std/default/trait.Default.html) trait.
- Offers cheap [`Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html)ing, so you can easily use the same pool in many places.

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
