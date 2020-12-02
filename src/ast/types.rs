use super::{ArgumentSortMismatch, InvalidOperation, InvalidSubstitution};

use serde::{Deserialize, Serialize};
use std::{cmp::Ordering, convert::TryFrom, marker::PhantomData};

/// Leaf node of an abstract syntax tree indexed by some sort, `S`
pub trait Variable<S> {
    /// Sort of syntactic element
    fn sort(&self) -> &S;
}

/// Non-leaf node of an abstract syntax tree indexed by some sort, `S`
pub trait Operator<S> {
    /// Sort of syntactic element
    fn sort(&self) -> &S;

    /// Expected sorts of [Ast] operands
    fn arity(&self) -> Vec<S>;

    /// Apply to the expected number and sorts of operands to construct an [Ast]
    ///
    /// # Errors
    ///
    /// Returns an [`InvalidOperation`] if `args` has the wrong number or sorts of arguments (see [`Operator::arity`]).
    fn apply<V>(
        self,
        args: &[Ast<V, Self, S>],
    ) -> Result<Ast<V, Self, S>, InvalidOperation<V, Self, S>>
    where
        V: Clone + Variable<S>,
        Self: Clone + Operator<S> + Sized,
        S: Clone + PartialEq,
    {
        Ast::try_from((self, args))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub(crate) enum Node<V, O, S> {
    Variable(V, #[serde(skip)] PhantomData<S>),
    Operation(O, Vec<Node<V, O, S>>),
}

/// Abstract syntax tree of sort, `S`, with leaves ,`V`, and non-leaves, `O`
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Ast<V, O, S>(pub(crate) Node<V, O, S>);

impl<V, O, S> From<V> for Ast<V, O, S>
where
    V: Variable<S>,
{
    fn from(v: V) -> Self {
        Self(Node::Variable(v, PhantomData))
    }
}

impl<V, O, S> TryFrom<(O, &[Self])> for Ast<V, O, S>
where
    V: Clone + Variable<S>,
    O: Clone + Operator<S>,
    S: Clone + PartialEq,
{
    type Error = InvalidOperation<V, O, S>;

    fn try_from((operator, args): (O, &[Self])) -> Result<Self, Self::Error> {
        Ok(Self(Node::from_op(
            operator,
            &args.iter().cloned().map(|a| a.0).collect::<Vec<_>>(),
        )?))
    }
}

impl<V, O, S> Node<V, O, S> {
    pub(crate) fn sort(&self) -> &S
    where
        V: Variable<S>,
        O: Operator<S>,
    {
        match self {
            Self::Variable(v, _phantom) => &v.sort(),
            Self::Operation(o, _) => &o.sort(),
        }
    }

    fn from_op(operator: O, args: &[Self]) -> Result<Self, InvalidOperation<V, O, S>>
    where
        V: Clone + Variable<S>,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        let arity = operator.arity();

        match args.len().cmp(&arity.len()) {
            Ordering::Greater => Err(InvalidOperation::TooManyArguments(args.len() - arity.len())),
            Ordering::Less => Err(InvalidOperation::TooFewArguments(arity.len() - args.len())),
            Ordering::Equal => {
                let bad_args: Vec<_> = args
                    .iter()
                    .enumerate()
                    .zip(arity.iter())
                    .filter_map(|((index, arg), sort)| {
                        if arg.sort() == sort {
                            None
                        } else {
                            Some(ArgumentSortMismatch {
                                index,
                                parameter: (*sort).clone(),
                                argument: Ast((*arg).clone()),
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

    fn substitute(&self, target: Self, x: &V) -> Result<Self, InvalidSubstitution<S>>
    where
        V: Clone + Variable<S> + PartialEq,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        if target.sort() == x.sort() {
            match self {
                Self::Variable(y, _phantom) => {
                    if y == x {
                        Ok(target)
                    } else {
                        Ok((*self).clone())
                    }
                }
                Self::Operation(o, args) => {
                    let mut new_args = Vec::new();
                    for a in args {
                        new_args.push(a.substitute(target.clone(), x)?);
                    }

                    Ok(Self::Operation(o.clone(), new_args))
                }
            }
        } else {
            Err(InvalidSubstitution {
                subject: x.sort().clone(),
                target: target.sort().clone(),
            })
        }
    }
}

impl<V, O, S> Ast<V, O, S> {
    /// Sort of syntactic element
    pub fn sort(&self) -> &S
    where
        V: Variable<S>,
        O: Operator<S>,
    {
        self.0.sort()
    }

    /// Replace each `subject` variable with the `target` [Ast].
    ///
    /// # Errors
    ///
    /// Returns an [`InvalidSubstitution`] error if the sort of the `target` differs from that of the `subject`.
    pub fn substitute(&self, target: Self, subject: &V) -> Result<Self, InvalidSubstitution<S>>
    where
        V: Clone + Variable<S> + PartialEq,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        Ok(Self(self.0.substitute(target.0, subject)?))
    }
}
