use super::Ast;

use std::{error::Error, fmt};

/// Error matching [Ast] with sort expected by [Operator](`super::Operator`)
#[derive(Debug, PartialEq)]
pub struct ArgumentSortMismatch<V, O, S> {
    /// Operand position
    pub index: usize,
    /// Expected sort
    pub parameter: S,
    /// Actual [Ast] operand
    pub argument: Ast<V, O, S>,
}

/// Error constructing operation from [Operator](`super::Operator`) and [Ast] arguments
#[derive(Debug, PartialEq)]
pub enum InvalidOperation<V, O, S> {
    /// Operator expects more arguments
    TooFewArguments(usize),
    /// Operator expects fewer arguments
    TooManyArguments(usize),
    /// Operator expects arguments of different sorts
    SortMismatches(Vec<ArgumentSortMismatch<V, O, S>>),
}

/// Error substituting [Ast] in place of [Variable](`super::Variable`) due to misaligned sorts, `S`
#[derive(Debug, PartialEq)]
pub struct InvalidSubstitution<S> {
    /// Sort expected by the variable
    pub subject: S,
    /// Sort of the [Ast] to substitute for the variable
    pub target: S,
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
