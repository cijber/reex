#[cfg(feature = "parser")]
use reex_ast::ReexError;
use reex_ast::{Compiler, ReexNode};
pub use reex_vm::matchers;
pub use reex_vm::program;
pub use reex_vm::vm::*;
pub use reex_vm::ReexString;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::sync::Arc;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone, Debug)]
pub struct Reex<T: Debug + PartialEq> {
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
            .map(|(start, end, selection)| ReexMatchBorrowed {
                start,
                end,
                data,
                selection,
            })
    }
}

pub trait ReexMatchInner<T> {
    fn data_slice(&self, from: usize, to: usize) -> T;
    fn selection_collection(&self) -> &SelectionCollection;
}

pub trait ReexSelectionIndex {
    fn to_selection_index(&self) -> Vec<usize>;
}

impl ReexSelectionIndex for usize {
    fn to_selection_index(&self) -> Vec<usize> {
        vec![*self]
    }
}

impl ReexSelectionIndex for &[usize] {
    fn to_selection_index(&self) -> Vec<usize> {
        self.to_vec()
    }
}

impl ReexSelectionIndex for (usize, usize) {
    fn to_selection_index(&self) -> Vec<usize> {
        vec![self.0, self.1]
    }
}

impl ReexSelectionIndex for (usize, usize, usize) {
    fn to_selection_index(&self) -> Vec<usize> {
        vec![self.0, self.1, self.2]
    }
}

impl ReexSelectionIndex for (usize, usize, usize, usize) {
    fn to_selection_index(&self) -> Vec<usize> {
        vec![self.0, self.1, self.2, self.3]
    }
}

pub trait ReexMatch<T>: ReexMatchInner<T> {
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn data(&self) -> T {
        ReexMatchInner::data_slice(self, self.start(), self.end())
    }

    fn selection<S: ReexSelectionIndex>(&self, index: S) -> Option<ReexSelection<'_, T, Self>>
    where
        Self: Sized,
    {
        self.selections(index).last()
    }

    fn selections<S: ReexSelectionIndex>(
        &self,
        index: S,
    ) -> Box<dyn Iterator<Item = ReexSelection<'_, T, Self>> + '_>
    where
        Self: Sized,
    {
        Box::new(
            self.selection_collection()
                .by_index(&index.to_selection_index())
                .map(move |x| ReexSelection {
                    reex_match: self,
                    full_path: x.full_path().to_vec(),
                    start: x.start(),
                    end: x.end(),
                    _unused: Default::default(),
                }),
        )
    }
}

pub struct ReexSelection<'a, T, O: ReexMatchInner<T>> {
    reex_match: &'a O,
    full_path: Vec<usize>,
    start: usize,
    end: usize,
    _unused: PhantomData<T>,
}

impl<T: Debug, O: ReexMatchInner<T>> Debug for ReexSelection<'_, T, O> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ReexSelection {{ {:?} ({}..{}) {:?} }}",
            self.full_path,
            self.start,
            self.end,
            self.data()
        )
    }
}

impl<'a, T, O: ReexMatchInner<T>> ReexSelection<'a, T, O> {
    pub fn data(&self) -> T {
        self.reex_match.data_slice(self.start, self.end)
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn children(&self) -> impl Iterator<Item = ReexSelection<'_, T, O>> {
        let coll = self.reex_match.selection_collection();
        coll.children(&self.full_path).map(move |x| ReexSelection {
            reex_match: self.reex_match,
            full_path: x.full_path().to_vec(),
            start: x.start(),
            end: x.end(),
            _unused: Default::default(),
        })
    }

    pub fn descendants(&self) -> impl Iterator<Item = ReexSelection<'_, T, O>> {
        let coll = self.reex_match.selection_collection();
        coll.descendants(&self.full_path)
            .map(move |x| ReexSelection {
                reex_match: self.reex_match,
                full_path: x.full_path().to_vec(),
                start: x.start(),
                end: x.end(),
                _unused: Default::default(),
            })
    }

    pub fn get_selection(&self, index: usize) -> Option<ReexSelection<'_, T, O>> {
        self.get_selections(index).last()
    }

    pub fn get_selections(&self, index: usize) -> impl Iterator<Item = ReexSelection<'_, T, O>> {
        self.children()
            .filter(move |x| x.full_path[self.full_path.len()] == index)
    }

    pub fn children_by_name(&self, name: &str) -> impl Iterator<Item = ReexSelection<'_, T, O>> {
        self.reex_match
            .selection_collection()
            .by_name(name)
            .filter(move |x| {
                x.index_path().starts_with(&self.full_path)
                    && x.index_path().len() == self.full_path.len() + 2
            })
            .map(move |x| ReexSelection {
                reex_match: self.reex_match,
                full_path: x.full_path().to_vec(),
                start: x.start(),
                end: x.end(),
                _unused: Default::default(),
            })
    }

    pub fn child_by_name(&self, name: &str) -> Option<ReexSelection<'_, T, O>> {
        self.children_by_name(name).last()
    }

    pub fn descendants_by_name(&self, name: &str) -> impl Iterator<Item = ReexSelection<'_, T, O>> {
        self.reex_match
            .selection_collection()
            .by_name(name)
            .filter(move |x| x.index_path().starts_with(&self.full_path))
            .map(move |x| ReexSelection {
                reex_match: self.reex_match,
                full_path: x.full_path().to_vec(),
                start: x.start(),
                end: x.end(),
                _unused: Default::default(),
            })
    }

    pub fn descendant_by_name(&self, name: &str) -> Option<ReexSelection<'_, T, O>> {
        self.descendants_by_name(name).last()
    }
}

#[derive(Debug)]
pub struct ReexMatchBorrowed<'a, T> {
    start: usize,
    end: usize,
    data: &'a [T],
    selection: SelectionCollection,
}

impl<'a, T> ReexMatchInner<&'a [T]> for ReexMatchBorrowed<'a, T> {
    fn data_slice(&self, from: usize, to: usize) -> &'a [T] {
        &self.data[from..to]
    }

    fn selection_collection(&self) -> &SelectionCollection {
        &self.selection
    }
}

impl<'a, T> ReexMatch<&'a [T]> for ReexMatchBorrowed<'a, T> {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

#[derive(Debug)]
pub struct ReexMatchString {
    start: usize,
    end: usize,
    data: Vec<ReexString>,
    selection: SelectionCollection,
}

impl ReexMatchInner<ReexString> for ReexMatchString {
    fn data_slice(&self, from: usize, to: usize) -> ReexString {
        if from == to {
            return ReexString::empty();
        }

        self.data[from].expand(&self.data[to - 1])
    }

    fn selection_collection(&self) -> &SelectionCollection {
        &self.selection
    }
}

impl ReexMatch<ReexString> for ReexMatchString {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl<T: Debug> Display for ReexMatchBorrowed<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}..{}) ｢{:?}｣", self.start(), self.end(), self.data())
    }
}

impl Display for ReexMatchString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "({}..{}) ｢{}｣", self.start(), self.end(), self.data())?;

        for item in self.selection.iter() {
            writeln!(
                f,
                "{}{} => ({}..{}) ｢{}｣",
                "  ".repeat(item.index_path().len()),
                item.index_path().last().unwrap(),
                item.start(),
                item.end(),
                self.data_slice(item.start(), item.end()),
            )?;
        }

        Ok(())
    }
}

#[cfg(feature = "parser")]
impl Reex<ReexString> {
    pub fn new<T: ToString>(input: T) -> Result<Reex<ReexString>, ReexError> {
        Reex::new_configured(
            input,
            |x| x.graphemes(true).map(ReexString::from).collect::<Vec<_>>(),
            |x| {
                reex_ast::custom::populate_compilers(x);
                reex_ast::custom::populate_reex_string_compilers(x);
            },
        )
    }

    pub fn from_node(node: &ReexNode) -> Reex<ReexString> {
        Self::from_node_configured(
            node,
            |x| x.graphemes(true).map(ReexString::from).collect::<Vec<_>>(),
            |x| {
                reex_ast::custom::populate_compilers(x);
                reex_ast::custom::populate_reex_string_compilers(x);
            },
        )
    }
}

impl Reex<ReexString> {
    pub fn find_str(&mut self, data: &str) -> Option<ReexMatchString> {
        if self.runtime.is_none() {
            self.runtime = Some(self.program.spawn_owned());
        }

        let data = ReexString::from(data).graphemes(true).collect::<Vec<_>>();
        self.runtime
            .as_mut()
            .unwrap()
            .run(data.as_ref())
            .map(move |(start, end, selection)| ReexMatchString {
                start,
                end,
                data,
                selection,
            })
    }
}

#[cfg(feature = "parser")]
impl<T: Debug + PartialEq + 'static> Reex<T> {
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

        assert_eq!(
            "hello ",
            reex.find_str("hello hello hello")
                .expect("Expected 2 matches")
                .data()
                .as_ref()
        );
        assert_eq!(
            "hello ",
            reex.find_str("hello hello hello")
                .expect("Expected 2 matches")
                .data()
                .as_ref()
        );
        assert!(
            reex.find_str("hello hello hello").is_none(),
            "Expected only 3 matches"
        );
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_empty() {
        let mut reex = Reex::new("[  🌱 🌱 🍎  ]?").unwrap();

        let matched = reex
            .find_str("")
            .expect("Empty string should return 1 match");
        assert_eq!(0, matched.start());
        assert_eq!(0, matched.end());
        assert_eq!("", matched.data());

        assert!(
            reex.find_str("").is_none(),
            "Second search on empty string should not return"
        );
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_only_one() {
        let mut reex = Reex::new(r#":lookbehind[ :boundary [ :word | :whitespace* "." :whitespace* ]+ ] :boundary "(" :not[ ")" ]* ")""#).unwrap();
        let input = "static int foo.main(bar baz)";

        let matched = reex
            .find_str("static int foo.main(bar baz)")
            .expect("string should return 1 match");
        assert_eq!(19, matched.start());
        assert_eq!(28, matched.end());
        assert_eq!("(bar baz)", matched.data());
        assert_eq!("(bar baz)", &input[19..28]);
        assert!(reex.find_str("static int foo.main(bar baz)").is_none());
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_grapheme_splitting() {
        // what you see here as ❤️ is actually ♥\u{fe0f}
        // \u{fe0f} forces ♥ into emoji mode, making a red heart
        // however, we don't want to repeat the \u{fe0f} we want to repeat the "whole" emoji
        // this combination of characters is called a "grapheme"
        // this test asserts it's actually allowing the grapheme to repeat
        let mut reex = Reex::new("hello❤️+ :end").unwrap();
        let input = "hello❤️❤️❤️";
        let item = reex.find_str(input).expect("Should find 1 match");
        assert_eq!(input, item.data());

        // now same thing but with,,, lesbians,, yes. these are 4 emoji's joined with ZWJ's
        // but also are technically only 1 grapheme!
        let mut reex = Reex::new("👩‍❤️‍💋‍👩+").unwrap();
        let input = "👩‍❤️‍💋‍👩👩‍❤️‍💋‍👩👩‍❤️‍💋‍👩👩‍❤️‍💋‍👩";
        let item = reex.find_str(input).expect("Should find at least 1 match");
        assert_eq!(input, item.data());

        // and here we have flags, the worst kind of grapheme!!
        let mut reex = Reex::new("🇳🇱+").unwrap();
        let input = "🇳🇱🇳🇱🇳🇱";
        let item = reex.find_str(input).expect("Should find at least 1 match");
        assert_eq!(input, item.data())
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_heavy_black_heart_forced_into_emoji() {
        let mut reex = Reex::new("❤️").unwrap();
        reex.find_str("❤️").expect("Should find itself");
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_repeating_lookahead() {
        let mut reex = Reex::new(":ahead[ ❤️ ]+").unwrap();
        assert_eq!("", reex.find_str("❤️❤️❤️").expect("Should succeed").data());
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_select() {
        let mut reex =
            Reex::new(":select[ :select[ a ]+ ] :select[ b+ ] :ahead[ :select[ c ] ]").unwrap();

        let matched = reex
            .find_str("aaabbbbc")
            .expect("Expected at least 1 match");
        assert_eq!(
            Some("aaa".to_string()),
            matched.selection(0).map(|x| x.data().to_string())
        );
        assert_eq!(3, matched.selection(0).unwrap().children().count());

        let mut reex = Reex::new(":select[ hello :select[ wo :select[ rl d+ ]+ ]+ ]").unwrap();

        let matched = reex
            .find_str("helloworldrldrldddrlddddddworldrldrlddddddd")
            .expect("Expected at least 1 match");
        assert_eq!(
            Some("rlddddddd".to_string()),
            matched.selection((0, 0, 0)).map(|x| x.data().to_string())
        );
        assert_eq!(7, matched.selections((0, 0, 0)).count());
    }
}
