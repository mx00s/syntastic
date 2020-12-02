//! Abstract syntax trees
//!
//! [Ast]s are trees with [Variable]s, `V`, as leaves and operations as non-leaves.
//! Each [Ast] and all of its subcomponents are indexed by some syntactic sort, `S`.
//! Each [Operator], `O`, encodes a sequence of sorts and can be composed with [Ast]s of those
//! sorts to construct an operation node.

use serde::{Deserialize, Serialize};
use std::{cmp::Ordering, convert::TryFrom, error::Error, fmt, marker::PhantomData};

#[cfg(test)]
use proptest::prelude::*;
#[cfg(test)]
use std::fmt::Write;

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
    fn apply<V>(
        self,
        args: Vec<Ast<V, Self, S>>,
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
enum Node<V, O, S> {
    Variable(V, #[serde(skip)] PhantomData<S>),
    Operation(O, Vec<Node<V, O, S>>),
}

/// Abstract syntax tree of sort, `S`, with leaves ,`V`, and non-leaves, `O`
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Ast<V, O, S>(Node<V, O, S>);

/// Error matching [Ast] with sort expected by [Operator]
#[derive(Debug, PartialEq)]
pub struct ArgumentSortMismatch<V, O, S> {
    index: usize,
    parameter: S,
    argument: Ast<V, O, S>,
}

/// Error constructing operation from [Operator] and [Ast] arguments
#[derive(Debug, PartialEq)]
pub enum InvalidOperation<V, O, S> {
    /// Operator expects more arguments
    TooFewArguments(usize),
    /// Operator expects fewer arguments
    TooManyArguments(usize),
    /// Operator expects arguments of different sorts
    SortMismatches(Vec<ArgumentSortMismatch<V, O, S>>),
}

/// Error substituting [Ast] in place of [Variable] due to misaligned sorts, `S`
#[derive(Debug, PartialEq)]
pub struct InvalidSubstitution<S> {
    subject: S,
    target: S,
}

impl<V, O, S> Error for ArgumentSortMismatch<V, O, S>
where
    V: fmt::Debug,
    O: fmt::Debug,
    S: fmt::Debug,
{
}

impl<V, O, S> Error for InvalidOperation<V, O, S>
where
    V: fmt::Debug,
    O: fmt::Debug,
    S: fmt::Debug,
{
}

impl<S> Error for InvalidSubstitution<S> where S: fmt::Debug {}

impl<V, O, S> From<V> for Ast<V, O, S>
where
    V: Variable<S>,
{
    fn from(v: V) -> Self {
        Self(Node::Variable(v, PhantomData))
    }
}

impl<V, O, S> TryFrom<(O, Vec<Self>)> for Ast<V, O, S>
where
    V: Clone + Variable<S>,
    O: Clone + Operator<S>,
    S: Clone + PartialEq,
{
    type Error = InvalidOperation<V, O, S>;

    fn try_from((operator, args): (O, Vec<Self>)) -> Result<Self, Self::Error> {
        Ok(Self(Node::from_op(
            operator,
            args.iter().cloned().map(|a| a.0).collect(),
        )?))
    }
}

impl<V, O, S> fmt::Display for ArgumentSortMismatch<V, O, S>
where
    S: fmt::Debug,
    Ast<V, O, S>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Argument at index {} should be of sort {:?}: {:?}",
            self.index, self.parameter, self.argument
        )
    }
}

impl<V, O, S> fmt::Display for InvalidOperation<V, O, S>
where
    S: fmt::Debug,
    Ast<V, O, S>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooFewArguments(n) => write!(f, "Operator requires {} more arguments", n),
            Self::TooManyArguments(n) => write!(f, "Operator requires {} fewer arguments", n),
            Self::SortMismatches(ms) => {
                for m in ms {
                    write!(f, "{}", m)?;
                }
                Ok(())
            }
        }
    }
}

impl<S> fmt::Display for InvalidSubstitution<S>
where
    S: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Cannot substitute {:?} for {:?}",
            self.subject, self.target
        )
    }
}

impl<V, O, S> Node<V, O, S> {
    fn sort(&self) -> &S
    where
        V: Variable<S>,
        O: Operator<S>,
    {
        match self {
            Self::Variable(v, _phantom) => &v.sort(),
            Self::Operation(o, _) => &o.sort(),
        }
    }

    fn from_op(operator: O, args: Vec<Self>) -> Result<Self, InvalidOperation<V, O, S>>
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
                    Ok(Self::Operation(operator, args))
                } else {
                    Err(InvalidOperation::SortMismatches(bad_args))
                }
            }
        }
    }

    fn substitute(&self, target: Self, x: V) -> Result<Self, InvalidSubstitution<S>>
    where
        V: Clone + Variable<S> + PartialEq,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        if target.sort() != x.sort() {
            Err(InvalidSubstitution {
                subject: x.sort().clone(),
                target: target.sort().clone(),
            })
        } else {
            match self {
                Self::Variable(y, _phantom) => {
                    if y == &x {
                        Ok(target)
                    } else {
                        Ok((*self).clone())
                    }
                }
                Self::Operation(o, args) => {
                    let mut new_args = Vec::new();
                    for a in args {
                        new_args.push(a.substitute(target.clone(), x.clone())?);
                    }

                    Ok(Self::Operation(o.clone(), new_args))
                }
            }
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
    pub fn substitute(&self, target: Self, subject: V) -> Result<Self, InvalidSubstitution<S>>
    where
        V: Clone + Variable<S> + PartialEq,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        Ok(Self(self.0.substitute(target.0, subject)?))
    }
}

#[cfg(test)]
pub trait ArbitraryOfSort<S> {
    fn of_sort(sort: &S) -> BoxedStrategy<Option<Box<Self>>>;
}

#[cfg(test)]
impl<V, O, S> ArbitraryOfSort<S> for Ast<V, O, S>
where
    V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
    O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
    S: 'static + Clone + fmt::Debug + PartialEq,
{
    fn of_sort(sort: &S) -> BoxedStrategy<Option<Box<Self>>> {
        const SIZE: usize = 50;

        Self::arb_node(sort.clone(), SIZE)
            .prop_map(|n| n.map(Box::new))
            .boxed()
    }
}

#[cfg(test)]
impl<V, O, S> Node<V, O, S> {
    fn render_sexp(&self) -> String
    where
        V: fmt::Display + Variable<S>,
        O: fmt::Display + Operator<S>,
        S: fmt::Display,
    {
        let mut buf = String::new();
        let write = |b: &mut String, s: &str| b.write_str(s).unwrap();
        match self {
            Node::Variable(v, _phantom) => write(&mut buf, &format!("[{} : {}]", v, v.sort())),
            Node::Operation(o, args) => {
                write(&mut buf, &format!("([{} : ", &o));
                for p in o.arity() {
                    write(&mut buf, &format!("{} -> ", p));
                }
                write(&mut buf, &format!("{}]", self.sort()));

                for a in args {
                    write(&mut buf, &format!(" {}", a.render_sexp()));
                }
                write(&mut buf, ")");
            }
        }
        buf
    }
}

#[cfg(test)]
impl<V, O, S> Ast<V, O, S> {
    fn render_sexp(&self) -> String
    where
        V: fmt::Display + Variable<S>,
        O: fmt::Display + Operator<S>,
        S: fmt::Display,
    {
        self.0.render_sexp()
    }

    pub fn arb_ast(size: usize) -> impl Strategy<Value = Self>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq + Arbitrary,
    {
        any::<S>()
            .prop_flat_map(move |sort| Self::arb_node(sort, size))
            .prop_filter_map("Skip when strategy returns None", |n| n)
    }

    pub fn arb_node(sort: S, size: usize) -> impl Strategy<Value = Option<Self>>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq,
    {
        if size == 0 {
            Self::arb_variable(sort).boxed()
        } else {
            prop_oneof![
                Self::arb_variable(sort.clone()),
                Self::arb_operation(sort, size)
            ]
            .boxed()
        }
    }

    fn arb_variable(sort: S) -> impl Strategy<Value = Option<Self>>
    where
        V: Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: Clone + fmt::Debug + PartialEq,
    {
        V::of_sort(&sort).prop_map(|v| Some(Self::from(*(v?))))
    }

    fn arb_operation(sort: S, size: usize) -> impl Strategy<Value = Option<Self>>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq,
    {
        O::of_sort(&sort).prop_flat_map(move |o| {
            if let Some(o) = o.clone() {
                Self::arb_args((o).arity(), size.saturating_sub(1)).boxed()
            } else {
                Just(None).boxed()
            }
            .prop_map(move |args| {
                Some(
                    o.as_ref()?
                        .clone()
                        .apply(args?)
                        .expect("Generated arguments must be compatible with generated operator"),
                )
            })
        })
    }

    fn arb_args(arity: Vec<S>, size: usize) -> impl Strategy<Value = Option<Vec<Self>>>
    where
        V: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Variable<S>,
        O: 'static + Clone + fmt::Debug + ArbitraryOfSort<S> + Operator<S>,
        S: 'static + Clone + fmt::Debug + PartialEq,
    {
        let size = size.saturating_sub(1);

        match arity.len() {
            1 => Self::arb_node(arity[0].clone(), size)
                .prop_map(|a0| Some(vec![a0?]))
                .boxed(),
            2 => (
                Self::arb_node(arity[0].clone(), size),
                Self::arb_node(arity[1].clone(), size),
            )
                .prop_map(|(a0, a1)| Some(vec![a0?, a1?]))
                .boxed(),
            _ => Just(None).boxed(),
        }
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    use insta::{
        assert_debug_snapshot, assert_json_snapshot, assert_ron_snapshot, assert_snapshot,
    };
    use proptest_derive::Arbitrary;

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    enum Var {
        Num(usize),
        X,
        Y,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Arbitrary)]
    enum Op {
        Plus,
        Times,
    }
    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Arbitrary)]
    enum Sort {
        Num,
        Other,
    }

    impl Variable<Sort> for Var {
        fn sort(&self) -> &Sort {
            match self {
                Var::Num(_) => &Sort::Num,
                Var::X => &Sort::Num,
                Var::Y => &Sort::Other,
            }
        }
    }

    impl Operator<Sort> for Op {
        fn sort(&self) -> &Sort {
            match self {
                Op::Plus => &Sort::Num,
                Op::Times => &Sort::Num,
            }
        }

        fn arity(&self) -> Vec<Sort> {
            match self {
                Op::Plus => vec![Sort::Num, Sort::Num],
                Op::Times => vec![Sort::Num, Sort::Num],
            }
        }
    }

    impl ArbitraryOfSort<Sort> for Var {
        fn of_sort(sort: &Sort) -> BoxedStrategy<Option<Box<Self>>> {
            match sort {
                Sort::Num => prop_oneof![
                    any::<usize>().prop_map(|n| Some(Box::new(Var::Num(n)))),
                    Just(Some(Box::new(Var::X))),
                ]
                .boxed(),
                Sort::Other => Just(Some(Box::new(Var::Y))).boxed(),
            }
        }
    }

    impl ArbitraryOfSort<Sort> for Op {
        fn of_sort(sort: &Sort) -> BoxedStrategy<Option<Box<Self>>> {
            match sort {
                Sort::Num => any::<Op>().prop_map(|o| Some(Box::new(o))).boxed(),
                Sort::Other => Just(None).boxed(),
            }
        }
    }

    impl fmt::Display for Var {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Num(n) => write!(f, "{}", n),
                Self::X => write!(f, "x"),
                Self::Y => write!(f, "y"),
            }
        }
    }

    impl fmt::Display for Op {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Op::Plus => write!(f, "+"),
                Op::Times => write!(f, "×"),
            }
        }
    }

    impl fmt::Display for Sort {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Sort::Num => write!(f, "ℕ"),
                Sort::Other => write!(f, "other"),
            }
        }
    }

    /// 2 + (3 * x)
    fn example_ast() -> Ast<Var, Op, Sort> {
        Op::Plus
            .apply(vec![
                Var::Num(2).into(),
                Op::Times
                    .apply(vec![Var::Num(3).into(), Var::X.into()])
                    .unwrap(),
            ])
            .unwrap()
    }

    #[test]
    fn ast__debug_snapshot() {
        assert_debug_snapshot!(example_ast());
    }

    #[test]
    fn ast__sexp_snapshot() {
        assert_snapshot!(example_ast().render_sexp());
    }

    #[test]
    fn ast__json_snapshot() {
        assert_json_snapshot!(example_ast());
    }

    #[test]
    fn ast__ron_snapshot() {
        assert_ron_snapshot!(example_ast());
    }

    #[test]
    fn ast__valid_operation() {
        let operation = Op::Plus.apply(vec![Var::Num(1).into(), Var::X.into()]);
        assert!(operation.is_ok());
    }

    #[test]
    fn invalid_ast_operation__too_many_args() {
        let operation = Op::Plus
            .apply(vec![Var::Num(1).into(), Var::Num(2).into(), Var::X.into()])
            .unwrap_err();

        assert_eq!(operation, InvalidOperation::TooManyArguments(1));
    }

    #[test]
    fn invalid_ast_operation__too_few_args() {
        let operation = Op::Plus.apply(vec![Var::X.into()]).unwrap_err();

        assert_eq!(operation, InvalidOperation::TooFewArguments(1));
    }

    #[test]
    fn invalid_ast_operation__argument_type() {
        let operation = Op::Plus
            .apply(vec![Var::Num(1).into(), Var::Y.into()])
            .unwrap_err();

        assert_eq!(
            operation,
            InvalidOperation::SortMismatches(vec![ArgumentSortMismatch {
                index: 1,
                parameter: Sort::Num,
                argument: Var::Y.into(),
            }]),
        );
    }

    #[test]
    fn ast__variable_substitution() {
        let actual = example_ast().substitute(Var::Num(4).into(), Var::X);
        let expected = Ok(Op::Plus
            .apply(vec![
                Var::Num(2).into(),
                Op::Times
                    .apply(vec![Var::Num(3).into(), Var::Num(4).into()])
                    .unwrap(),
            ])
            .unwrap());
        assert_eq!(actual, expected);
    }

    #[test]
    fn invalid_ast_substitution__variable_sort_mismatch() {
        let actual = example_ast().substitute(Var::Y.into(), Var::X);
        let expected = Err(InvalidSubstitution {
            subject: Sort::Num,
            target: Sort::Other,
        });
        assert_eq!(actual, expected);
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]
        #[test]
        fn meta__ast_strategy_returns_ast_of_expected_sort(
            ast in Ast::<Var, Op, Sort>::of_sort(&Sort::Num).prop_filter_map("Skip when strategy returns None", |n| n)
        ) {
            prop_assert_eq!(ast.sort(), &Sort::Num);
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]
        #[test]
        fn ast__roundtrips_through_serialization_and_deserialization(
            ast in Ast::<Var, Op, Sort>::arb_ast(50)
        ) {
            let roundtripped: Ast<Var, Op, Sort> =
                serde_json::from_str(&serde_json::to_string(&ast).unwrap()).unwrap();

            prop_assert_eq!(roundtripped, ast);
        }
    }

    // TODO: abstract binding trees (p. 6-10)
}
