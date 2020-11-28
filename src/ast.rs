use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Value<T, S> {
    pub val: T,
    pub sort: S,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable<V, S> {
    pub var: V,
    pub sort: S,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Operator<O, S> {
    pub op: O,
    pub sort: S,
    pub arity: Vec<S>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Operation<T, V, O, S> {
    op: Operator<O, S>,
    args: Vec<Ast<T, V, O, S>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast<T, V, O, S> {
    Val(Value<T, S>),
    Var(Variable<V, S>),
    Op(Operation<T, V, O, S>),
}

impl<T, V, O, S> Ast<T, V, O, S> {
    fn sort(&self) -> &S {
        match self {
            Self::Val(x) => &x.sort,
            Self::Var(v) => &v.sort,
            Self::Op(o) => &o.op.sort,
        }
    }
}

impl<T, S> Value<T, S> {
    pub fn new(val: T, sort: S) -> Self {
        Self { val, sort }
    }
}

impl<V, S> Variable<V, S> {
    pub fn new(var: V, sort: S) -> Self {
        Self { var, sort }
    }
}

impl<O, S> Operator<O, S> {
    pub fn new(op: O, sort: S, arity: Vec<S>) -> Self {
        Self { op, sort, arity }
    }
}

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

impl<T, V, O, S> Operation<T, V, O, S> {
    pub fn new(
        op: Operator<O, S>,
        args: Vec<Ast<T, V, O, S>>,
    ) -> Result<Self, InvalidOperation<T, V, O, S>>
    where
        S: PartialEq + Clone,
        Ast<T, V, O, S>: Clone,
    {
        match args.len().cmp(&op.arity.len()) {
            Ordering::Greater => Err(InvalidOperation::TooManyArguments(
                args.len() - op.arity.len(),
            )),
            Ordering::Less => Err(InvalidOperation::TooFewArguments(
                op.arity.len() - args.len(),
            )),
            Ordering::Equal => {
                let mismatched_sorts: Vec<_> = args
                    .iter()
                    .enumerate()
                    .zip(op.arity.iter())
                    .filter_map(|((index, arg), sort)| {
                        if arg.sort() == sort {
                            None
                        } else {
                            Some(ArgumentSortMismatch {
                                index,
                                parameter: (*sort).clone(),
                                argument: (*arg).clone(),
                            })
                        }
                    })
                    .collect();
                if mismatched_sorts.is_empty() {
                    Ok(Self { op, args })
                } else {
                    Err(InvalidOperation::SortMismatches(mismatched_sorts))
                }
            }
        }
    }
}

impl<T, V, O, S> From<Variable<V, S>> for Ast<T, V, O, S> {
    fn from(var: Variable<V, S>) -> Self {
        Self::Var(var)
    }
}

impl<T, V, O, S> From<Value<T, S>> for Ast<T, V, O, S> {
    fn from(x: Value<T, S>) -> Self {
        Self::Val(x)
    }
}

impl<T, V, O, S> From<Operation<T, V, O, S>> for Ast<T, V, O, S> {
    fn from(o: Operation<T, V, O, S>) -> Self {
        Self::Op(o)
    }
}

impl<T, V, O, S> fmt::Display for Ast<T, V, O, S>
where
    T: fmt::Display,
    V: fmt::Display,
    O: fmt::Display,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Val(x) => write!(f, "{}", x),
            Self::Var(v) => write!(f, "{}", v),
            Self::Op(o) => write!(f, "{}", o),
        }
    }
}

impl<T, S> fmt::Display for Value<T, S>
where
    T: fmt::Display,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{} : {}]", self.val, self.sort)
    }
}

impl<V, S> fmt::Display for Variable<V, S>
where
    V: fmt::Display,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{} : {}]", self.var, self.sort)
    }
}

impl<O, S> fmt::Display for Operator<O, S>
where
    O: fmt::Display,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{} : ", self.op)?;
        for p in &self.arity {
            write!(f, "{} -> ", p)?;
        }
        write!(f, "{}]", self.sort)
    }
}

impl<T, V, O, S> fmt::Display for Operation<T, V, O, S>
where
    T: fmt::Display,
    V: fmt::Display,
    O: fmt::Display,
    S: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}", self.op)?;
        for a in self.args.iter() {
            write!(f, " {}", a)?;
        }
        write!(f, ")")
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_display_snapshot};

    #[derive(Clone, Debug, PartialEq)]
    enum Var {
        X,
    }

    #[derive(Clone, Debug, PartialEq)]
    enum Op {
        Plus,
        Times,
    }

    #[derive(Clone, Debug, PartialEq)]
    enum Sort {
        Num,
        Other,
    }

    impl fmt::Display for Var {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Var::X => write!(f, "x"),
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

    fn example_ast() -> Ast<usize, Var, Op, Sort> {
        // 2 + (3 * x)
        Operation::new(
            Operator::new(Op::Plus, Sort::Num, vec![Sort::Num, Sort::Num]),
            vec![
                Value::new(2, Sort::Num).into(),
                Operation::new(
                    Operator::new(Op::Times, Sort::Num, vec![Sort::Num, Sort::Num]),
                    vec![
                        Value::new(3, Sort::Num).into(),
                        Variable::new(Var::X, Sort::Num).into(),
                    ],
                )
                .unwrap()
                .into(),
            ],
        )
        .unwrap()
        .into()
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
    fn invalid_ast_operation__too_many_args() {
        let operation = Operation::new(
            Operator::new(Op::Plus, Sort::Num, vec![Sort::Num, Sort::Num]),
            vec![
                Value::new(1, Sort::Num).into(),
                Value::new(2, Sort::Num).into(),
                Variable::new(Var::X, Sort::Num).into(),
            ],
        )
        .unwrap_err();

        assert_eq!(operation, InvalidOperation::TooManyArguments(1),);
    }

    #[test]
    fn invalid_ast_operation__too_few_args() {
        let operation = Operation::<usize, _, _, _>::new(
            Operator::new(Op::Plus, Sort::Num, vec![Sort::Num, Sort::Num]),
            vec![Variable::new(Var::X, Sort::Num).into()],
        )
        .unwrap_err();

        assert_eq!(operation, InvalidOperation::TooFewArguments(1),);
    }

    #[test]
    fn invalid_ast_operation__argument_type() {
        let operation = Operation::new(
            Operator::new(Op::Plus, Sort::Num, vec![Sort::Num, Sort::Num]),
            vec![
                Value::new(1, Sort::Num).into(),
                Variable::new(Var::X, Sort::Other).into(),
            ],
        )
        .unwrap_err();

        assert_eq!(
            operation,
            InvalidOperation::SortMismatches(vec![ArgumentSortMismatch {
                index: 1,
                parameter: Sort::Num,
                argument: Variable::new(Var::X, Sort::Other).into(),
            }]),
        );
    }

    // TODO: substitution (p. 5)

    // TODO: abstract binding trees (p. 6-10)
}
