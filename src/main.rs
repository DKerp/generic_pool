use generic_pool::Pool;

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
