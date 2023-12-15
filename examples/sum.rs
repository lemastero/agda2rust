struct __InternalEmpty;
pub struct __InternalEmptyCase<T>(::std::marker::PhantomData<T>, __InternalEmpty);

pub enum Sum<A, B> {
    Inj1(A),
    Inj2(B),
    // Can be ommitted, as `A` appeared in a variant
    Empty(__InternalEmptyCase<(A, B)>),
}

pub use Sum::{Inj1, Inj2};

pub enum UnusedExample<A, B> {
    OnlyLeft(A),
    // Can not be omitted, as `B` did *not* appear in a variant
    // Could be simplified to __InternalEmptyCase<B>
    Empty(__InternalEmptyCase<(A, B)>),
}

pub use UnusedExample::{OnlyLeft};
