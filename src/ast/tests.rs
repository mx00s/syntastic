#![allow(non_snake_case)]

use super::*;

use serde::{Deserialize, Serialize};
use std::fmt;

use insta::{assert_debug_snapshot, assert_json_snapshot, assert_ron_snapshot, assert_snapshot};
use proptest_derive::Arbitrary;

#[cfg(feature = "with-proptest")]
use proptest::prelude::*;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum Var {
    X,
    Y,
}

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
            Op::Num(_) | Op::Plus | Op::Times => &Sort::Num,
        }
    }

    fn arity(&self) -> Vec<Sort> {
        match self {
            Op::Num(_) => vec![],
            Op::Plus | Op::Times => vec![Sort::Num, Sort::Num],
        }
    }
}

impl From<usize> for Ast<Var, Op, Sort> {
    fn from(n: usize) -> Self {
        Op::Num(n)
            .apply(&[])
            .expect("Num operator expects no arguments")
    }
}

#[cfg(feature = "with-proptest")]
impl ArbitraryOfSort<Sort> for Var {
    fn of_sort(sort: &Sort) -> BoxedStrategy<Option<Box<Self>>> {
        match sort {
            Sort::Num => Just(Some(Box::new(Var::X))).boxed(),
            Sort::Other => Just(Some(Box::new(Var::Y))).boxed(),
        }
    }
}

#[cfg(feature = "with-proptest")]
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
            Self::X => write!(f, "x"),
            Self::Y => write!(f, "y"),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{}", n),
            Op::Plus => write!(f, "+"),
            #[allow(clippy::non_ascii_literal)]
            Op::Times => write!(f, "×"),
        }
    }
}

impl fmt::Display for Sort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            #[allow(clippy::non_ascii_literal)]
            Sort::Num => write!(f, "ℕ"),
            Sort::Other => write!(f, "other"),
        }
    }
}

/// 2 + (3 * x)
fn example_ast() -> Ast<Var, Op, Sort> {
    // FUTURE IDEAS:
    // * configurable bidirectional parser for the family of concrete syntaxes
    // * crate with an `ast` proc_macro (https://doc.rust-lang.org/book/ch19-06-macros.html#function-like-macros)
    Op::Plus
        .apply(&[
            2.into(),
            Op::Times.apply(&[3.into(), Var::X.into()]).unwrap(),
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
    let operation = Op::Plus.apply(&[1.into(), Var::X.into()]);
    assert!(operation.is_ok());
}

#[test]
fn invalid_ast_operation__too_many_args() {
    let operation = Op::Plus
        .apply(&[1.into(), 2.into(), Var::X.into()])
        .unwrap_err();

    assert_eq!(operation, InvalidOperation::TooManyArguments(1));
}

#[test]
fn invalid_ast_operation__too_few_args() {
    let operation = Op::Plus.apply(&[Var::X.into()]).unwrap_err();

    assert_eq!(operation, InvalidOperation::TooFewArguments(1));
}

#[test]
fn invalid_ast_operation__argument_type() {
    let operation = Op::Plus.apply(&[1.into(), Var::Y.into()]).unwrap_err();

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
    let actual = example_ast().substitute(4.into(), &Var::X);
    let expected = Ok(Op::Plus
        .apply(&[2.into(), Op::Times.apply(&[3.into(), 4.into()]).unwrap()])
        .unwrap());
    assert_eq!(actual, expected);
}

#[test]
fn invalid_ast_substitution__variable_sort_mismatch() {
    let actual = example_ast().substitute(Var::Y.into(), &Var::X);
    let expected = Err(InvalidSubstitution {
        subject: Sort::Num,
        target: Sort::Other,
    });
    assert_eq!(actual, expected);
}

#[cfg(feature = "with-proptest")]
proptest! {
    #![proptest_config(ProptestConfig::with_cases(1000))]
    #[test]
    fn meta__ast_strategy_returns_ast_of_expected_sort(
        ast in Ast::<Var, Op, Sort>::of_sort(&Sort::Num).prop_filter_map("Skip when strategy returns None", |n| n)
    ) {
        prop_assert_eq!(ast.sort(), &Sort::Num);
    }
}

#[cfg(feature = "with-proptest")]
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
