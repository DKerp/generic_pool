# generic_pool
A pool for recycling allocated objects for later reuse. Uses generics so you can store and retrieve (almost) any type in a single pool instance.

## TODO
I need to add the whole documentation. And a lot of tests of course. But the code is already useable.

## Example

This example demonstrates the most basic usage. We define two different structs, Person and Animal.
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

    pool.put(p.clone());
    pool.put(a.clone());

    let person: Person = pool.get_or_default();
    let animal: Animal = pool.get_or_default();

    assert_eq!(person, p);
    assert_eq!(animal, a);

    assert_eq!(pool.get::<Person>(), None);
    assert_eq!(pool.get::<Animal>(), None);

    println!("Test person: {:?}", person);
    println!("Test animal: {:?}", animal);
}
```
