#[cfg(feature = "parser")]
use reex_ast::ReexError;
pub use reex_vm::matchers;
pub use reex_vm::program;
pub use reex_vm::vm::*;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct Reex<T> {
    program: Arc<Program<T>>,
    state: Option<StateOwned<T>>,
}

#[cfg(feature = "parser")]
impl Reex<char> {
    pub fn new(input: String) -> Result<Reex<char>, ReexError> {
        use reex_ast::{Compiler, ReexBuilder};
        let node = ReexBuilder::parse(input)?;
        let program = Arc::new(Compiler::compile(&node));
        Ok(Reex {
            program,
            state: None,
        })
    }
}

#[cfg(feature = "parser")]
impl<T> Reex<T> {
    pub fn new_typed<I: Iterator<Item = T>, F: Fn(String) -> I>(
        input: String,
        transform: F,
    ) -> Result<Reex<T>, ReexError> {
        use reex_ast::{Compiler, ReexBuilder};
        let node = ReexBuilder::parse(input)?;
        Ok(Reex {
            program: Compiler::compile_typed(&node, transform),
            state: None,
        })
    }
}
