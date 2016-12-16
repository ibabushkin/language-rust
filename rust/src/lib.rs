extern crate syntex_syntax as syntax;
extern crate syntex_errors as syntax_errors;

pub mod ast;

use ast::krate;
/*
use std::rc::Rc;

use syntax::codemap::CodeMap;
use syntax::parse;
use syntax::parse::ParseSess;
use syntax_errors::Handler;
use syntax_errors::emitter::ColorConfig;

pub fn test_main() {
    let codemap = Rc::new(CodeMap::new());
    let tty_handler =
        Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(codemap.clone()));
    let parse_session = ParseSess::with_span_handler(tty_handler, codemap.clone());

    let src = "fn foo(x: bool) -> bool { !x }".to_owned();

    let result = parse::parse_crate_from_source_str(String::new(), src, &parse_session);

    println!("parse result: {:?}", result);
}
*/

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
