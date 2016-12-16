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

pub struct Module {

}
