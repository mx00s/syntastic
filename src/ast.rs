//! Abstract syntax trees
//!
//! [Ast]s are trees with [Variable]s, `V`, as leaves and operations as non-leaves.
//! Each [Ast] and all of its subcomponents are indexed by some syntactic sort, `S`.
//! Each [Operator], `O`, encodes a sequence of sorts and can be composed with [Ast]s of those
//! sorts to construct an operation node.

#[cfg(test)]
mod tests;

mod types;
pub use self::types::*;

#[cfg(feature = "with-proptest")]
mod with_proptest;
#[cfg(feature = "with-proptest")]
pub use self::with_proptest::*;

#[cfg(test)]
mod bidirectional;
