use serde::{Deserialize, Serialize};
use std::{cmp::Ordering, fmt, marker::PhantomData};

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
}

impl<T, V, O, S> fmt::Display for Node<T, V, O, S>
where
    T: fmt::Display + Value<S>,
    V: fmt::Display + Variable<S>,
    O: fmt::Display + Operator<S>,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(x, _phantom) => write!(f, "[{} : {}]", x, x.sort()),
            Self::Variable(v) => write!(f, "[{} : {}]", v, v.sort()),
            Self::Operation(o, args) => {
                write!(f, "([{} : ", &o)?;
                for p in o.arity() {
                    write!(f, "{} -> ", p)?;
                }
                write!(f, "{}]", self.sort())?;

                for a in args {
                    write!(f, " {}", a)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl<T, V, O, S> fmt::Display for Ast<T, V, O, S>
where
    T: fmt::Display + Value<S>,
    V: fmt::Display + Variable<S>,
    O: fmt::Display + Operator<S>,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    use insta::{
        assert_debug_snapshot, assert_display_snapshot, assert_json_snapshot, assert_ron_snapshot,
    };

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
    fn ast__debug() {
        assert_debug_snapshot!(example_ast());
    }

    #[test]
    fn ast__display() {
        assert_display_snapshot!(example_ast());
    }

    #[test]
    fn ast__json() {
        assert_json_snapshot!(example_ast());
    }

    #[test]
    fn ast__ron() {
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

    // TODO: substitution (p. 5)

    // TODO: abstract binding trees (p. 6-10)
}
