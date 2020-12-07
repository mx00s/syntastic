//! Abstracting binding trees

// binding
// scope

// placeholder binding: variable
// operator index: parameter

// identifier property: reference to its binding

use crate::errors::{ArgumentSortMismatch, InvalidOperation};
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    convert::{TryFrom, TryInto},
    marker::PhantomData,
};

/// Invalid operation for [Abt]
pub type InvalidOperationError<V, O, S> = InvalidOperation<Valence<S>, Abstractor<V, O, S>>;

/// Leaf node of an abstract binding tree indexed by some sort, `S`
pub trait Variable<S> {
    /// Sort of syntactic element
    fn sort(&self) -> &S;
}

/// TODO
#[derive(Clone, Debug)]
pub struct Valence<S> {
    /// TODO
    pub variables: Vec<S>,
    /// TODO
    pub sort: S,
}

/// Non-leaf node of an abstract binding tree indexed by some sort, `S`
pub trait Operator<S> {
    /// Sort of syntactic element
    fn sort(&self) -> &S;

    /// Expected sorts of the operands and their respective variables
    fn arity(&self) -> Vec<Valence<S>>;

    /// Apply to the expected number and sorts of operands to construct an [Abt]
    ///
    /// # Errors
    ///
    /// Returns an [`InvalidOperation`] if `args` has the wrong number or sorts of arguments (see [`Operator::arity`]).
    fn apply<V>(
        self,
        args: &[Abstractor<V, Self, S>],
    ) -> Result<Abt<V, Self, S>, InvalidOperationError<V, Self, S>>
    where
        V: Clone + Variable<S>,
        Self: Clone + Operator<S> + Sized,
        S: Clone + PartialEq,
    {
        Abt::try_from((self, args))
    }
}

/// TODO
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Abstractor<V, O, S> {
    /// TODO
    pub variables: Vec<V>,
    /// TODO
    pub tree: Abt<V, O, S>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) enum Node<V, O, S> {
    Variable(V, #[serde(skip)] PhantomData<S>),
    Operation(O, Vec<Abstractor<V, O, S>>),
    Let(Vec<(V, Node<V, O, S>)>, Box<Node<V, O, S>>),
}

/// TODO
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Abt<V, O, S>(pub(crate) Node<V, O, S>);

impl<V, O, S> From<V> for Abt<V, O, S> {
    fn from(x: V) -> Self {
        Self(Node::Variable(x, PhantomData))
    }
}

impl<V, O, S> TryFrom<(O, &[Abstractor<V, O, S>])> for Node<V, O, S>
where
    V: Clone + Variable<S>,
    O: Clone + Operator<S>,
    S: Clone + PartialEq,
{
    type Error = InvalidOperationError<V, O, S>;

    fn try_from((operator, args): (O, &[Abstractor<V, O, S>])) -> Result<Self, Self::Error> {
        Self::from_op(operator, args)
    }
}

impl<V, O, S> TryFrom<(O, &[Abstractor<V, O, S>])> for Abt<V, O, S>
where
    V: Clone + Variable<S>,
    O: Clone + Operator<S>,
    S: Clone + PartialEq,
{
    type Error = InvalidOperationError<V, O, S>;

    fn try_from((operator, args): (O, &[Abstractor<V, O, S>])) -> Result<Self, Self::Error> {
        Ok(Self((operator, args).try_into()?))
    }
}

impl<V, O, S> Abstractor<V, O, S> {
    fn sort(&self) -> &S
    where
        V: Variable<S>,
        O: Operator<S>,
    {
        self.tree.sort()
    }
}

impl<V, O, S> Abt<V, O, S> {
    /// Bind variables to abstract binding trees in the scope of the `tree`.
    pub fn bind(bindings: Vec<(V, Self)>, tree: Self) -> Self {
        // TODO: continue allowing empty `bindings`?

        // TODO: check for multiple `bindings` for the same variable

        // TODO: ensure freshness condition on binders is preserved

        Self(Node::Let(
            bindings.into_iter().map(|(v, t)| (v, t.0)).collect(),
            Box::new(tree.0),
        ))
    }

    fn sort(&self) -> &S
    where
        V: Variable<S>,
        O: Operator<S>,
    {
        self.0.sort()
    }
}

impl<V, O, S> Node<V, O, S> {
    fn sort(&self) -> &S
    where
        V: Variable<S>,
        O: Operator<S>,
    {
        match self {
            Self::Variable(x, _) => x.sort(),
            Self::Operation(operator, _) => operator.sort(),
            Self::Let(_, _) => todo!(),
        }
    }

    fn from_op(
        operator: O,
        args: &[Abstractor<V, O, S>],
    ) -> Result<Self, InvalidOperationError<V, O, S>>
    where
        V: Clone + Variable<S>,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        // TODO: address the "freshness condition on binders" refinement of part 2 of the definition on p. 8

        let arity = operator.arity();

        match args.len().cmp(&arity.len()) {
            Ordering::Greater => Err(InvalidOperation::TooManyArguments(args.len() - arity.len())),
            Ordering::Less => Err(InvalidOperation::TooFewArguments(arity.len() - args.len())),
            Ordering::Equal => {
                let bad_args: Vec<ArgumentSortMismatch<Valence<S>, Abstractor<V, O, S>>> = args
                    .iter()
                    .enumerate()
                    .zip(arity.iter())
                    .filter_map(|((index, arg), valence)| {
                        if *arg.sort() == valence.sort
                            && arg
                                .variables
                                .iter()
                                .map(|v| v.sort().clone())
                                .collect::<Vec<S>>()
                                == valence.variables
                        {
                            None
                        } else {
                            Some(ArgumentSortMismatch {
                                index,
                                parameter: (*valence).clone(),
                                argument: (*arg).clone(),
                            })
                        }
                    })
                    .collect();

                if bad_args.is_empty() {
                    Ok(Self::Operation(operator, args.to_vec()))
                } else {
                    Err(InvalidOperation::SortMismatches(bad_args))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]

    use super::*;

    use insta::{assert_debug_snapshot, assert_json_snapshot, assert_ron_snapshot};
    use proptest_derive::Arbitrary;

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct Var<S>(String, S);

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Arbitrary)]
    enum Op {
        Num(usize),
        Plus,
        Times,
    }
    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Arbitrary)]
    enum Sort {
        Num,
        Other,
    }

    impl<S> Var<S> {
        fn new(name: &str, sort: S) -> Self {
            Self(name.to_owned(), sort)
        }
    }

    impl Operator<Sort> for Op {
        fn sort(&self) -> &Sort {
            match self {
                Op::Num(_) | Op::Plus | Op::Times => &Sort::Num,
            }
        }

        fn arity(&self) -> Vec<Valence<Sort>> {
            match self {
                Op::Num(_) => vec![],
                Op::Plus | Op::Times => vec![
                    Valence {
                        variables: vec![Sort::Num],
                        sort: Sort::Num,
                    },
                    Valence {
                        variables: vec![Sort::Num],
                        sort: Sort::Num,
                    },
                ],
            }
        }
    }

    impl<S> Variable<S> for Var<S> {
        fn sort(&self) -> &S {
            &self.1
        }
    }

    // let x be a_1 in a_2
    // let(a_1; x.a_2)   // the x is bound within a_2, but not a_1

    // let x be x * x in x + x

    fn example_abt() -> Abt<Var<Sort>, Op, Sort> {
        // let x be 2 in
        //   let y be 3 in
        //     x + y

        let x = Var::new("x", Sort::Num);
        let y = Var::new("y", Sort::Num);

        Abt::bind(
            vec![(x.clone(), Op::Num(2).apply(&[]).unwrap())],
            Abt::bind(
                vec![(y.clone(), Op::Num(3).apply(&[]).unwrap())],
                Op::Plus
                    .apply(&[
                        Abstractor {
                            variables: vec![x.clone()],
                            tree: Abt::from(x),
                        },
                        Abstractor {
                            variables: vec![y.clone()],
                            tree: Abt::from(y),
                        },
                    ])
                    .unwrap(),
            ),
        )
    }

    #[test]
    fn abt__debug_snapshot() {
        assert_debug_snapshot!(example_abt());
    }

    #[test]
    fn abt__json_snapshot() {
        assert_json_snapshot!(example_abt());
    }

    #[test]
    fn abt__ron_snapshot() {
        assert_ron_snapshot!(example_abt());
    }
}
