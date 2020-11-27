use std::fmt;

#[derive(Debug)]
pub enum Error<T, V, O, S> {
    InvalidArgumentCount {
        op: Operator<O, S>,
        args: Vec<Ast<T, V, O, S>>,
    },
    InvalidArgumentSort {
        op: Operator<O, S>,
        args: Vec<Ast<T, V, O, S>>,
    },
}

#[derive(Debug)]
pub struct Value<T, S> {
    pub val: T,
    pub sort: S,
}

#[derive(Debug)]
pub struct Variable<V, S> {
    pub var: V,
    pub sort: S,
}

#[derive(Debug)]
pub struct Operator<O, S> {
    pub op: O,
    pub sort: S,
    pub arity: Vec<S>,
}

#[derive(Debug)]
pub struct Operation<T, V, O, S> {
    op: Operator<O, S>,
    args: Vec<Ast<T, V, O, S>>,
}

#[derive(Debug)]
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

impl<T, V, O, S> Operation<T, V, O, S> {
    pub fn new(op: Operator<O, S>, args: Vec<Ast<T, V, O, S>>) -> Result<Self, Error<T, V, O, S>>
    where
        S: PartialEq,
    {
        if args.len() != op.arity.len() {
            Err(Error::InvalidArgumentCount { op, args })
        } else if args
            .iter()
            .zip(op.arity.iter())
            .any(|(arg, sort)| arg.sort() != sort)
        {
            Err(Error::InvalidArgumentSort { op, args })
        } else {
            Ok(Self { op, args })
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
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_display_snapshot};

    #[derive(Debug)]
    enum Var {
        X,
    }

    #[derive(Debug)]
    enum Op {
        Plus,
        Times,
    }

    #[derive(Debug, PartialEq)]
    enum Sort {
        Num,
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
    fn ast_debug() {
        assert_debug_snapshot!(example_ast());
    }

    #[test]
    fn ast_display() {
        assert_display_snapshot!(example_ast());
    }

    // TODO: substitution (p. 5)

    // TODO: abstract binding trees (p. 6-10)
}
