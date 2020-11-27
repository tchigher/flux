#![cfg_attr(feature = "strict", deny(warnings, missing_docs))]

//! The `core` crate handles the parsing and semantic analysis of Flux source code.
//! It is made up of five packages. Three of these packages handle the follow work
//! required to analyze Flux code:
//! 
//! - parser
//! - scanner
//! - responsible for forming the abstract syntax tree (AST).
//!
//! Additional crates are responsible for semantic analysis, procuding a Semantic Graph.
//! formatter provide a code formatting tool.
//!
//! Together these crates for the core structures needed for compiling and eventually executing
//! Flux code.
extern crate chrono;
#[macro_use]
extern crate serde_derive;
extern crate serde_aux;

pub mod ast;
pub mod formatter;
pub mod parser;
pub mod scanner;
pub mod semantic;

use std::error;
use std::fmt;

pub use ast::DEFAULT_PACKAGE_NAME;

/// An error that can occur due to problems in AST generation or semantic analysis.
#[derive(Debug, Clone)]
pub struct Error {
    /// Message.
    pub msg: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.msg)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl From<String> for Error {
    fn from(msg: String) -> Self {
        Error { msg }
    }
}

impl From<&str> for Error {
    fn from(msg: &str) -> Self {
        Error {
            msg: String::from(msg),
        }
    }
}

impl From<semantic::nodes::Error> for Error {
    fn from(sn_err: semantic::nodes::Error) -> Self {
        Error {
            msg: sn_err.to_string(),
        }
    }
}

impl From<semantic::check::Error> for Error {
    fn from(err: semantic::check::Error) -> Self {
        Error {
            msg: format!("{}", err),
        }
    }
}
