//! TODO

use std::{error::Error, fmt};

/// Error matching tree with sort expected by operator
#[derive(Debug, PartialEq)]
pub struct ArgumentSortMismatch<S, A> {
    /// Operand position
    pub index: usize,
    /// Expected sort
    pub parameter: S,
    /// Actual operand
    pub argument: A,
}

/// Error constructing operation from operator and arguments
#[derive(Debug, PartialEq)]
pub enum InvalidOperation<S, A> {
    /// Operator expects more arguments
    TooFewArguments(usize),
    /// Operator expects fewer arguments
    TooManyArguments(usize),
    /// Operator expects arguments of different sorts
    SortMismatches(Vec<ArgumentSortMismatch<S, A>>),
}

/// Error substituting tree in place of variable due to misaligned sorts, `S`
#[derive(Debug, PartialEq)]
pub struct InvalidSubstitution<S> {
    /// Sort expected by the variable
    pub subject: S,
    /// Sort of the tree to substitute for the variable
    pub target: S,
}

impl<S, A> Error for ArgumentSortMismatch<S, A>
where
    S: fmt::Debug,
    A: fmt::Debug,
{
}

impl<S, A> Error for InvalidOperation<S, A>
where
    S: fmt::Debug,
    A: fmt::Debug,
{
}

impl<S> Error for InvalidSubstitution<S> where S: fmt::Debug {}

impl<S, A> fmt::Display for ArgumentSortMismatch<S, A>
where
    S: fmt::Debug,
    A: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Argument at index {} should be of sort {:?}: {:?}",
            self.index, self.parameter, self.argument
        )
    }
}

impl<S, A> fmt::Display for InvalidOperation<S, A>
where
    S: fmt::Debug,
    A: fmt::Debug,
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
