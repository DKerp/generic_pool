use crate::*;


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


/// A local trait providing the reset logic for all recycled objects.
trait Reset {
    fn reset(self) -> Self; // See below why we use this pattern.
}

/// We implement the trait on any drop guard by this.
impl<G: AsMut<Animal>> Reset for G {
    fn reset(mut self) -> Self {
        let animal = self.as_mut();
        animal.home = false;
        animal.name.clear();
        animal.age_in_month = 0;

        self
    }
}


#[test]
fn simple_get() {
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
}

#[test]
fn get_with_guard() {
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

    let person: DropGuard<Person> = pool.get_with_guard().unwrap();
    let animal: DropGuard<Animal> = pool.get_with_guard().unwrap();

    assert_eq!(*person, p);
    assert_eq!(*animal, a);

    assert_eq!(pool.get::<Person>(), None);
    assert_eq!(pool.get::<Animal>(), None);

    drop(person);
    drop(animal);

    let person: DropGuard<Person> = pool.get_with_guard().unwrap();
    let animal: DropGuard<Animal> = pool.get_with_guard().unwrap();

    assert_eq!(*person, p);
    assert_eq!(*animal, a);

    assert_eq!(pool.get::<Person>(), None);
    assert_eq!(pool.get::<Animal>(), None);
}

#[test]
fn get_with_guard_and_costum_reset() {
    let mut pool = Pool::default();

    let a = Animal {
        home: true,
        name: String::from("Meow"),
        age_in_month: 12,
    };

    pool.put(a.clone());

    let animal: DropGuard<Animal> = match pool.get_with_guard() {
        Some(animal) => Reset::reset(animal),
        None => panic!("No animal retrieved."),
    };

    assert_ne!(*animal, a);
    assert_eq!(*animal, Animal::default());
}
