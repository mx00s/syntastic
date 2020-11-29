use serde::{Deserialize, Serialize};
use std::{cmp::Ordering, marker::PhantomData};

#[cfg(test)]
use std::fmt::{self, Write};

pub trait Value<S> {
    fn sort(&self) -> &S;
}

pub trait Variable<S> {
    fn sort(&self) -> &S;
}

pub trait Operator<S> {
    fn sort(&self) -> &S;
    fn arity(&self) -> Vec<S>;
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum Node<T, V, O, S> {
    Value(T, #[serde(skip)] PhantomData<S>),
    Variable(V),
    Operation(O, Vec<Node<T, V, O, S>>),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Ast<T, V, O, S>(Node<T, V, O, S>);

#[derive(Debug, PartialEq)]
pub struct ArgumentSortMismatch<T, V, O, S> {
    index: usize,
    parameter: S,
    argument: Ast<T, V, O, S>,
}

#[derive(Debug, PartialEq)]
pub enum InvalidOperation<T, V, O, S> {
    TooFewArguments(usize),
    TooManyArguments(usize),
    SortMismatches(Vec<ArgumentSortMismatch<T, V, O, S>>),
}

impl<T, V, O, S> Node<T, V, O, S> {
    fn sort(&self) -> &S
    where
        T: Value<S>,
        V: Variable<S>,
        O: Operator<S>,
    {
        match self {
            Self::Value(x, _phantom) => &x.sort(),
            Self::Variable(v) => &v.sort(),
            Self::Operation(o, _) => &o.sort(),
        }
    }

    fn from_op(operator: O, args: Vec<Self>) -> Result<Self, InvalidOperation<T, V, O, S>>
    where
        T: Clone + Value<S>,
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

    #[cfg(test)]
    fn render_sexp(&self) -> String
    where
        T: fmt::Display + Value<S>,
        V: fmt::Display + Variable<S>,
        O: fmt::Display + Operator<S>,
        S: fmt::Display,
    {
        let mut buf = String::new();
        let write = |b: &mut String, s: &str| b.write_str(s).unwrap();
        match self {
            Node::Value(x, _phantom) => write(&mut buf, &format!("[{} : {}]", x, x.sort())),
            Node::Variable(v) => write(&mut buf, &format!("[{} : {}]", v, v.sort())),
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

impl<T, V, O, S> Ast<T, V, O, S> {
    pub fn sort(&self) -> &S
    where
        T: Value<S>,
        V: Variable<S>,
        O: Operator<S>,
    {
        self.0.sort()
    }

    pub fn from_val(x: T) -> Self
    where
        T: Value<S>,
    {
        Self(Node::Value(x, PhantomData))
    }

    pub fn from_var(v: V) -> Self
    where
        V: Variable<S>,
    {
        Self(Node::Variable(v))
    }

    pub fn from_op(operator: O, args: Vec<Self>) -> Result<Self, InvalidOperation<T, V, O, S>>
    where
        T: Clone + Value<S>,
        V: Clone + Variable<S>,
        O: Clone + Operator<S>,
        S: Clone + PartialEq,
    {
        Ok(Self(Node::from_op(
            operator,
            args.iter().cloned().map(|a| a.0).collect(),
        )?))
    }

    #[cfg(test)]
    fn render_sexp(&self) -> String
    where
        T: fmt::Display + Value<S>,
        V: fmt::Display + Variable<S>,
        O: fmt::Display + Operator<S>,
        S: fmt::Display,
    {
        self.0.render_sexp()
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
    use proptest::prelude::*;

    // TODO: extract proptest strategies for `Ast`, leaving those for test types

    fn arb_ast(sort: Sort) -> impl Strategy<Value = Ast<usize, Var, Op, Sort>> {
        prop_oneof![
            if sort == Sort::Num {
                arb_value()
            } else {
                arb_variable(sort.clone())
            },
            arb_variable(sort.clone()),
            arb_operation(sort),
        ]
    }

    fn arb_value() -> BoxedStrategy<Ast<usize, Var, Op, Sort>> {
        any::<usize>().prop_map(Ast::from_val).boxed()
    }

    fn arb_variable(sort: Sort) -> BoxedStrategy<Ast<usize, Var, Op, Sort>> {
        arb_var()
            .prop_filter_map("Generated variable must have the expected sort", move |v| {
                if v.sort() == &sort {
                    Some(Ast::from_var(v))
                } else {
                    None
                }
            })
            .boxed()
    }

    prop_compose! {
        fn arb_operation(sort: Sort)(
            o in arb_op().prop_filter("Generated operator must have expected sort", move |o| o.sort() == &sort)
        )(
            args in arb_args(o.arity()),
            o in Just(o)
        ) -> Ast<usize, Var, Op, Sort> {
            Ast::from_op(o, args)
                .expect("Generated arguments must be compatible with generated operator")
        }
    }

    fn arb_var() -> impl Strategy<Value = Var> {
        prop_oneof![Just(Var::X), Just(Var::Y),]
    }

    fn arb_op() -> impl Strategy<Value = Op> {
        prop_oneof![Just(Op::Plus), Just(Op::Times),]
    }

    fn arb_args(arity: Vec<Sort>) -> BoxedStrategy<Vec<Ast<usize, Var, Op, Sort>>> {
        match arity.len() {
            1 => arb_ast(arity[0].clone()).prop_map(|a0| vec![a0]).boxed(),
            2 => (arb_ast(arity[0].clone()), arb_ast(arity[1].clone()))
                .prop_map(|(a0, a1)| vec![a0, a1])
                .boxed(),
            _ => unimplemented!(),
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    enum Var {
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

    impl Value<Sort> for usize {
        fn sort(&self) -> &Sort {
            &Sort::Num
        }
    }

    impl Variable<Sort> for Var {
        fn sort(&self) -> &Sort {
            match self {
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
            write!(f, "{:?}", self)
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
    fn example_ast() -> Ast<usize, Var, Op, Sort> {
        Ast::from_op(
            Op::Plus,
            vec![
                Ast::from_val(2),
                Ast::from_op(Op::Times, vec![Ast::from_val(3), Ast::from_var(Var::X)]).unwrap(),
            ],
        )
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
    fn invalid_ast_operation__too_many_args() {
        let operation = Ast::from_op(
            Op::Plus,
            vec![Ast::from_val(1), Ast::from_val(2), Ast::from_var(Var::X)],
        )
        .unwrap_err();

        assert_eq!(operation, InvalidOperation::TooManyArguments(1));
    }

    #[test]
    fn invalid_ast_operation__too_few_args() {
        let operation: InvalidOperation<usize, _, _, _> =
            Ast::from_op(Op::Plus, vec![Ast::from_var(Var::X)]).unwrap_err();

        assert_eq!(operation, InvalidOperation::TooFewArguments(1));
    }

    #[test]
    fn invalid_ast_operation__argument_type() {
        let operation =
            Ast::from_op(Op::Plus, vec![Ast::from_val(1), Ast::from_var(Var::Y)]).unwrap_err();

        assert_eq!(
            operation,
            InvalidOperation::SortMismatches(vec![ArgumentSortMismatch {
                index: 1,
                parameter: Sort::Num,
                argument: Ast::from_var(Var::Y),
            }]),
        );
    }

    proptest! {
        #[test]
        fn ast__roundtrips_through_serialization_and_deserialization(
            ast in arb_ast(Sort::Num)
        ) {
            let roundtripped: Ast<usize, Var, Op, Sort> =
                serde_json::from_str(&serde_json::to_string(&ast).unwrap()).unwrap();

            prop_assert_eq!(roundtripped, ast);
        }
    }

    // TODO: substitution (p. 5)

    // TODO: abstract binding trees (p. 6-10)
}
