use super::*;

use proptest::prelude::*;
use std::fmt;

/// Provides a `proptest` strategy to try to generate a random value of the specified sort, `S`
pub trait ArbitraryOfSort<S> {
    /// Attempt to generate a random value of the specified sort
    fn of_sort(sort: &S) -> BoxedStrategy<Option<Box<Self>>>;
}

impl<V, O, S> ArbitraryOfSort<S> for Ast<V, O, S>
where
    V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
    O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
    S: 'static + Clone + fmt::Debug + PartialEq,
{
    fn of_sort(sort: &S) -> BoxedStrategy<Option<Box<Self>>> {
        const SIZE: usize = 50;

        Self::arb_node(sort, SIZE)
            .prop_map(|n| n.map(Box::new))
            .boxed()
    }
}

impl<V, O, S> Ast<V, O, S> {
    /// Generate an [`Ast`]  with `size` as an upper-bound depth
    pub fn arb_ast(size: usize) -> impl Strategy<Value = Self>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq + Arbitrary,
    {
        any::<S>()
            .prop_flat_map(move |sort| Self::arb_node(&sort, size))
            .prop_filter_map("Skip when strategy returns None", |n| n)
    }

    /// Generate an [`Ast`]  of the specified `sort` with `size` as an upper-bound depth
    pub fn arb_node(sort: &S, size: usize) -> impl Strategy<Value = Option<Self>>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq,
    {
        if size == 0 {
            Self::arb_variable(sort).boxed()
        } else {
            prop_oneof![Self::arb_variable(sort), Self::arb_operation(sort, size)].boxed()
        }
    }

    fn arb_variable(sort: &S) -> impl Strategy<Value = Option<Self>>
    where
        V: fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: fmt::Debug,
        S: fmt::Debug,
    {
        V::of_sort(&sort).prop_map(|v| Some(Self::from(*(v?))))
    }

    fn arb_operation(sort: &S, size: usize) -> impl Strategy<Value = Option<Self>>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq,
    {
        O::of_sort(&sort).prop_flat_map(move |o| {
            o.clone()
                .map_or_else(
                    || Just(None).boxed(),
                    |o| Self::arb_args(&(o).arity(), size.saturating_sub(1)).boxed(),
                )
                .prop_map(move |args| {
                    Some(
                        o.as_ref()?.clone().apply(&args?).expect(
                            "Generated arguments must be compatible with generated operator",
                        ),
                    )
                })
        })
    }

    fn arb_args(arity: &[S], size: usize) -> impl Strategy<Value = Option<Vec<Self>>>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq,
    {
        let size = size.saturating_sub(1);

        match arity.len() {
            1 => Self::arb_node(&arity[0], size)
                .prop_map(|a0| Some(vec![a0?]))
                .boxed(),
            2 => (
                Self::arb_node(&arity[0], size),
                Self::arb_node(&arity[1], size),
            )
                .prop_map(|(a0, a1)| Some(vec![a0?, a1?]))
                .boxed(),
            _ => Just(None).boxed(),
        }
    }
}
