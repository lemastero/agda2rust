use std::rc::Rc;
use std::marker::PhantomData;

pub enum __Impossible {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Nat {
    Zero,
    Succ(Rc<Nat>),
}

use Nat::{Zero, Succ};

pub fn add(n: Rc<Nat>, m: Rc<Nat>) -> Rc<Nat> {
    match (&*n, &*m) {
        (_, Zero) => n.clone(),
        (_, Succ(m)) => Rc::new(Succ(add(n.clone(), m.clone()))),
    }
}

// Erased everything except the A: Set type parameter, which *does* have a useful equivalent
pub enum Eq<A> {
    Refl,
    // Rust doesn't like unused type parameters :(
    __Impossible(__Impossible, PhantomData<A>),
}

use Eq::Refl;

// Set arguments lifted to type parameters, implicit arguments are made explicit
pub fn cong<A, B>(a: Rc<A>, b: Rc<A>, f: Rc<dyn Fn(Rc<A>) -> Rc<B>>, eq: Rc<Eq<A>>) -> Rc<Eq<B>> {
    match (&*a, &*b, &*f, &*eq) {
        (_, _, _, Refl) => Rc::new(Refl),
        _ => unreachable!(),
    }
}

pub fn sym<A>(a: Rc<A>, b: Rc<A>, eq: Rc<Eq<A>>) -> Rc<Eq<A>> {
    match (&*a, &*b, &*eq) {
        (_, _, Refl) => Rc::new(Refl),
        _ => unreachable!(),
    }
}

pub fn trans<A>(a: Rc<A>, b: Rc<A>, c: Rc<A>, eq1: Rc<Eq<A>>, eq2: Rc<Eq<A>>) -> Rc<Eq<A>> {
    match (&*a, &*b, &*c, &*eq1, &*eq2) {
        (_, _, _, Refl, Refl) => Rc::new(Refl),
        _ => unreachable!(),
    }
}

pub fn add_left_id(a: Rc<Nat>) -> Rc<Eq<Nat>> {
    match (&*a, ) {
        (Zero, ) => Rc::new(Refl),
        (Succ(a), ) => cong(add(Rc::new(Zero), a.clone()), a.clone(), Rc::new(|n| Rc::new(Succ(n))), add_left_id(a.clone())),
    }
}

pub fn succ_left_add(a: Rc<Nat>, b: Rc<Nat>) -> Rc<Eq<Nat>> {
    match (&*a, &*b) {
        (_, Zero) => Rc::new(Refl),
        (_, Succ(b)) => cong(add(Rc::new(Succ(a.clone())), b.clone()), Rc::new(Succ(add(a.clone(), b.clone()))), Rc::new(|n| Rc::new(Succ(n))), succ_left_add(a.clone(), b.clone())),
    }
}

pub fn add_comm(a: Rc<Nat>, b: Rc<Nat>) -> Rc<Eq<Nat>> {
    match (&*a, &*b) {
        (_, Zero) => sym(add(b.clone(), a.clone()), add(a.clone(), b.clone()), add_left_id(a.clone())),
        (_, Succ(b)) => trans(
            Rc::new(Succ(add(a.clone(), b.clone()))),
            Rc::new(Succ(add(b.clone(), a.clone()))),
            add(Rc::new(Succ(b.clone())), a.clone()),
            // Eq (Succ (add a b))) (Succ (add b a))
            cong(
                add(a.clone(), b.clone()),
                add(b.clone(), a.clone()),
                Rc::new(|n| Rc::new(Succ(n))),
                // Eq (add a b) (add b a)
                add_comm(a.clone(), b.clone())),
            // Eq (Succ (add b a)) (add (Succ b) a)
            sym(
                add(Rc::new(Succ(b.clone())), a.clone()),
                Rc::new(Succ(add(b.clone(), a.clone()))),
                // Eq (add (Succ b) a) (Succ (add b a))
                succ_left_add(b.clone(), a.clone()))),
    }
}

// To get the compiler to shut up
fn main() {}
