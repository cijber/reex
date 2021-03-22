#[cfg(feature = "parser")]
use reex_ast::ReexError;
use reex_ast::{Compiler, ReexNode};
pub use reex_vm::matchers;
pub use reex_vm::program;
pub use reex_vm::vm::*;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct Reex<T: Debug> {
    program: Arc<Program<T>>,
    runtime: Option<RuntimeOwned<T>>,
}

impl<T: Debug + PartialEq> Reex<T> {
    pub fn find<'a>(&mut self, data: &'a [T]) -> Option<ReexMatchBorrowed<'a, T>> {
        if self.runtime.is_none() {
            self.runtime = Some(self.program.spawn_owned());
        }
        self.runtime
            .as_mut()
            .unwrap()
            .run(data)
            .map(|(start, end)| ReexMatchBorrowed { start, end, data })
    }
}

pub trait ReexMatch<T> {
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn data(&self) -> T;
}

#[derive(Debug)]
pub struct ReexMatchBorrowed<'a, T> {
    start: usize,
    end: usize,
    data: &'a [T],
}

impl<'a, T> ReexMatch<&'a [T]> for ReexMatchBorrowed<'a, T> {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn data(&self) -> &'a [T] {
        &self.data[self.start..self.end]
    }
}

#[derive(Debug)]
pub struct ReexMatchStr<'a> {
    start: usize,
    end: usize,
    data: &'a str,
}

impl<'a> ReexMatch<&'a str> for ReexMatchStr<'a> {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn data(&self) -> &'a str {
        if self.start == self.end {
            return "";
        }

        let mut data = self.data.char_indices().skip(self.start);
        let (first, _) = data.next().unwrap();
        let (end, _) = data
            .skip(self.end - self.start)
            .next()
            .unwrap_or((self.data.len(), ' '));
        &self.data[first..end]
    }
}

impl<T: Debug> Display for ReexMatchBorrowed<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}..{}) ｢{:?}｣", self.start(), self.end(), self.data())
    }
}

impl Display for ReexMatchStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}..{}) ｢{}｣", self.start(), self.end(), self.data())
    }
}

#[cfg(feature = "parser")]
impl Reex<char> {
    pub fn new<T: ToString>(input: T) -> Result<Reex<char>, ReexError> {
        Reex::new_configured(
            input,
            |x| x.chars().collect(),
            |x| {
                reex_ast::custom::populate_compilers(x);
                reex_ast::custom::populate_partial_eq_compilers(x);
                reex_ast::custom::populate_char_compilers(x);
            },
        )
    }

    pub fn from_node(node: &ReexNode) -> Reex<char> {
        Self::from_node_configured(
            node,
            |x| x.chars().collect(),
            |x| {
                reex_ast::custom::populate_compilers(x);
                reex_ast::custom::populate_partial_eq_compilers(x);
                reex_ast::custom::populate_char_compilers(x);
            },
        )
    }
}

impl Reex<char> {
    pub fn find_str<'a>(&mut self, data: &'a str) -> Option<ReexMatchStr<'a>> {
        if self.runtime.is_none() {
            self.runtime = Some(self.program.spawn_owned());
        }

        let chars = data.chars().collect::<Vec<_>>();
        self.runtime
            .as_mut()
            .unwrap()
            .run(chars.as_ref())
            .map(move |(start, end)| ReexMatchStr {
                start,
                end,
                data: &data,
            })
    }
}

#[cfg(feature = "parser")]
impl<T: Debug> Reex<T> {
    pub fn new_typed<S: ToString, F: Fn(&str) -> Vec<T>>(
        input: S,
        transform: F,
    ) -> Result<Reex<T>, ReexError> {
        Self::new_configured(input, transform, |x| {
            reex_ast::custom::populate_compilers(x);
        })
    }

    pub fn new_configured<S: ToString, F: Fn(&str) -> Vec<T>, C: FnOnce(&mut Compiler<T, F>)>(
        input: S,
        transform: F,
        configure: C,
    ) -> Result<Reex<T>, ReexError> {
        use reex_ast::ReexBuilder;
        let mut compiler = Compiler::new(transform);
        configure(&mut compiler);
        let node = ReexBuilder::parse(&input.to_string())?;
        Ok(Reex {
            program: Arc::new(compiler.compile(&node)),
            runtime: None,
        })
    }

    pub fn from_node_configured<F: Fn(&str) -> Vec<T>, C: FnOnce(&mut Compiler<T, F>)>(
        node: &ReexNode,
        transform: F,
        configure: C,
    ) -> Reex<T> {
        use reex_ast::ReexBuilder;
        let mut compiler = Compiler::new(transform);
        configure(&mut compiler);
        Reex {
            program: Arc::new(compiler.compile(&node)),
            runtime: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Reex, ReexMatch};

    #[cfg(feature = "parser")]
    #[test]
    fn test_chain() {
        let mut reex = Reex::new("hello \" \"").unwrap();

        while let Some(matched) = reex.find_str("hello hello hello") {
            println!("{}", matched)
        }
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_empty() {
        let mut reex = Reex::new(":start [  🌱 🌱 🍎  ]? :end").unwrap();

        let matched = reex
            .find_str("")
            .expect("Empty string should return 1 match");
        assert_eq!(0, matched.start());
        assert_eq!(0, matched.end());
        assert_eq!("", matched.data());
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_only_one() {
        let mut reex = Reex::new(r#":lookbehind[ :boundary [ :word | :whitespace* "." :whitespace* ]+ ] :boundary "(" :not[ ")" ]* ")""#).unwrap();

        let matched = reex
            .find_str("static int foo.main(bar baz)")
            .expect("string should return 1 match");
        assert_eq!(19, matched.start());
        assert_eq!(27, matched.end());
        assert_eq!("(bar baz)", matched.data());
        assert!(reex.find_str("static int foo.main(bar baz)").is_none());
    }
}
