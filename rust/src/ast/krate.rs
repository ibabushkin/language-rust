use std::rc::Rc;

use syntax::codemap::CodeMap;
use syntax::parse;
use syntax::parse::ParseSess;
use syntax_errors::Handler;
use syntax_errors::emitter::ColorConfig;

/// A Krate is a krate!
///
/// Spreads kancer.
pub struct Krate {
    pub module: Module,
}

/// A Rust Module as parsed from the file.
pub struct Module {
    pub items: Vec<Item>,
}

pub struct Item {

}
