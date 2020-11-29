use serde::{Deserialize, Serialize};
use std::{cmp::Ordering, convert::TryFrom, marker::PhantomData};

#[cfg(test)]
use std::fmt::{self, Write};

pub trait Variable<S> {
    fn sort(&self) -> &S;
}

pub trait Operator<S> {
    fn sort(&self) -> &S;
    fn arity(&self) -> Vec<S>;
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum Node<V, O, S> {
    Variable(V, #[serde(skip)] PhantomData<S>),
    Operation(O, Vec<Node<V, O, S>>),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Ast<V, O, S>(Node<V, O, S>);

#[derive(Debug, PartialEq)]
pub struct ArgumentSortMismatch<V, O, S> {
    index: usize,
    parameter: S,
    argument: Ast<V, O, S>,
}

#[derive(Debug, PartialEq)]
pub enum InvalidOperation<V, O, S> {
    TooFewArguments(usize),
    TooManyArguments(usize),
    SortMismatches(Vec<ArgumentSortMismatch<V, O, S>>),
}

#[derive(Debug, PartialEq)]
pub struct InvalidSubstitution<S> {
    subject: S,
    target: S,
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

    #[cfg(test)]
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

impl<V, O, S> Ast<V, O, S> {
    pub fn sort(&self) -> &S
    where
        V: Variable<S>,
        O: Operator<S>,
    {
        self.0.sort()
    }

    pub fn substitute(&self, target: Self, subject: V) -> Result<Self, InvalidSubstitution<S>>
    where
        V: Clone + Variable<S> + PartialEq,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        Ok(Self(self.0.substitute(target.0, subject)?))
    }

    #[cfg(test)]
    fn render_sexp(&self) -> String
    where
        V: fmt::Display + Variable<S>,
        O: fmt::Display + Operator<S>,
        S: fmt::Display,
    {
        self.0.render_sexp()
    }
}

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

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};
    use std::convert::TryInto;

    use insta::{
        assert_debug_snapshot, assert_json_snapshot, assert_ron_snapshot, assert_snapshot,
    };
    use proptest::prelude::*;

    // TODO: extract proptest strategies for `Ast`, leaving those for test types

    // TODO: use proptest-derive on `Ast` and its components, and distinguish between ASTs with valid operation nodes and those with any invalid nodes.

    fn arb_ast(sort: Sort, size: usize) -> impl Strategy<Value = Ast<Var, Op, Sort>> {
        if size == 0 {
            arb_variable(sort.to_owned()).boxed()
        } else {
            prop_oneof![arb_variable(sort.to_owned()), arb_operation(sort, size),].boxed()
        }
    }

    fn arb_variable(sort: Sort) -> BoxedStrategy<Ast<Var, Op, Sort>> {
        arb_var()
            .prop_filter_map("Generated variable must have the expected sort", move |v| {
                if v.sort() == &sort {
                    Some(v.into())
                } else {
                    None
                }
            })
            .boxed()
    }

    prop_compose! {
        fn arb_operation(sort: Sort, size: usize)(
            o in arb_op().prop_filter("Generated operator must have expected sort", move |o| o.sort() == &sort)
        )(
            args in arb_args(o.arity(), size.saturating_sub(1)),
            o in Just(o)
        ) -> Ast<Var, Op, Sort> {
            (o, args)
                .try_into()
                .expect("Generated arguments must be compatible with generated operator")
        }
    }

    fn arb_var() -> impl Strategy<Value = Var> {
        prop_oneof![
            any::<usize>().prop_map(Var::Num),
            Just(Var::X),
            Just(Var::Y)
        ]
    }

    fn arb_op() -> impl Strategy<Value = Op> {
        prop_oneof![Just(Op::Plus), Just(Op::Times),]
    }

    fn arb_args(arity: Vec<Sort>, size: usize) -> BoxedStrategy<Vec<Ast<Var, Op, Sort>>> {
        let size = size.saturating_sub(1);

        match arity.len() {
            1 => arb_ast(arity[0].clone(), size)
                .prop_map(|a0| vec![a0])
                .boxed(),
            2 => (
                arb_ast(arity[0].clone(), size),
                arb_ast(arity[1].clone(), size),
            )
                .prop_map(|(a0, a1)| vec![a0, a1])
                .boxed(),
            _ => unimplemented!(),
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    enum Var {
        Num(usize),
        X,
        Y,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    enum Op {
        Plus,
        Times,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
        (
            Op::Plus,
            vec![
                Var::Num(2).into(),
                (Op::Times, vec![Var::Num(3).into(), Var::X.into()])
                    .try_into()
                    .unwrap(),
            ],
        )
            .try_into()
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
        let operation = Ast::try_from((Op::Plus, vec![Var::Num(1).into(), Var::X.into()]));
        assert!(operation.is_ok());
    }

    #[test]
    fn invalid_ast_operation__too_many_args() {
        let operation = Ast::try_from((
            Op::Plus,
            vec![Var::Num(1).into(), Var::Num(2).into(), Var::X.into()],
        ))
        .unwrap_err();

        assert_eq!(operation, InvalidOperation::TooManyArguments(1));
    }

    #[test]
    fn invalid_ast_operation__too_few_args() {
        let operation = Ast::try_from((Op::Plus, vec![Var::X.into()])).unwrap_err();

        assert_eq!(operation, InvalidOperation::TooFewArguments(1));
    }

    #[test]
    fn invalid_ast_operation__argument_type() {
        let operation =
            Ast::try_from((Op::Plus, vec![Var::Num(1).into(), Var::Y.into()])).unwrap_err();

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
        let expected = Ok((
            Op::Plus,
            vec![
                Var::Num(2).into(),
                (Op::Times, vec![Var::Num(3).into(), Var::Num(4).into()])
                    .try_into()
                    .unwrap(),
            ],
        )
            .try_into()
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
        #[test]
        fn meta__ast_strategy_returns_ast_of_expected_sort(
            ast in arb_ast(Sort::Num, 50)
        ) {
            prop_assert_eq!(ast.sort(), &Sort::Num);
        }
    }

    proptest! {
        #[test]
        fn ast__roundtrips_through_serialization_and_deserialization(
            ast in arb_ast(Sort::Num, 50)
        ) {
            let roundtripped: Ast<Var, Op, Sort> =
                serde_json::from_str(&serde_json::to_string(&ast).unwrap()).unwrap();

            prop_assert_eq!(roundtripped, ast);
        }
    }

    // TODO: abstract binding trees (p. 6-10)
}
