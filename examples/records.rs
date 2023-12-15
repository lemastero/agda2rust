pub struct Pair<A, B> {
    pub fst: ::std::rc::Rc<A>,
    pub snd: ::std::rc::Rc<B>,
    // Can be ommitted, as both `A` and `B` appeared in a field
    pub _phantom: ::std::marker::PhantomData<(A, B)>,
}

pub struct Foo<A> {
    pub foo: ::std::rc::Rc<Pair<A, A>>,
    // Can be ommitted, as `A` appeared in a field
    pub _phantom: ::std::marker::PhantomData<A>,
}

pub fn mk_foo<A>(param0: ::std::rc::Rc<A>) -> ::std::rc::Rc<Foo<A>> {
    match (param0, ) {
        (x, ) => ::std::rc::Rc::new(Foo::<A> {
            foo: ::std::rc::Rc::new(Pair::<A, A> {
                fst: x.clone(),
                snd: x.clone(),
                _phantom: ::std::marker::PhantomData,
            }),
            _phantom: ::std::marker::PhantomData,
        }),
    }
}
