//! [![Crates.io](https://img.shields.io/crates/v/syntastic.svg)](https://crates.io/crates/syntastic)
//! [![Docs.rs](https://docs.rs/syntastic/badge.svg)](https://docs.rs/syntastic)
//! [![CI](https://github.com/mx00s/syntastic/workflows/Continuous%20Integration/badge.svg)](https://github.com/mx00s/syntastic/actions)
//! [![Coverage Status](https://coveralls.io/repos/github/mx00s/syntastic/badge.svg?branch=main)](https://coveralls.io/github/mx00s/syntastic?branch=main)
//!
//! <!-- `README.md` generated from `src/lib.rs` -->
//!
//! # Thanks
//!
//! * [Robert Harper](https://www.cs.cmu.edu/~rwh/) for formalizing abstract syntax in
//!   [_Practical Foundations for Programming Languages_](https://www.cs.cmu.edu/~rwh/pfpl/).
//! * [Rust GitHub Template](https://rust-github.github.io/) for helping me set up
//!   reasonable defaults for this repository.

#![deny(missing_docs)]
#![deny(clippy::all)]
#![deny(clippy::pedantic)]
#![allow(clippy::unit_arg)] // catches false positives due to single-element enum variants

pub mod ast;

// TODO: abstract binding trees
