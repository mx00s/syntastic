// For now the following `render_sexp` methods aren't public, but there is a snapshot test for them.
//
// Eventually I'd like to generalize this by implementing a generalization which supports generic,
// configurable bidirectional parsers. This is non-trivial because consumers control the rendering of
// the variables and operators they define. In other words, a concrete generic syntax like these sexps
// isn't be compatible with all varieties of `Ast`s. Syntactic could provide a trait for bidirectional
// parsers, and consumers that implement it would be responsible for ensuring there are no ambiguous
// strings. Syntactic could also implement generic roundtrip testing features to help consumers verify
// their end of the contract.

use super::types::{Ast, Node, Operator, Variable};

use std::fmt::{self, Write};

impl<V, O, S> Node<V, O, S> {
    pub(crate) fn render_sexp(&self) -> String
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
    pub(crate) fn render_sexp(&self) -> String
    where
        V: fmt::Display + Variable<S>,
        O: fmt::Display + Operator<S>,
        S: fmt::Display,
    {
        self.0.render_sexp()
    }
}
