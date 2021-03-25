#[cfg(feature = "parser")]
use reex_ast::ReexError;
use reex_ast::{Compiler, ReexNode};
pub use reex_vm::matchers;
pub use reex_vm::program;
pub use reex_vm::vm::*;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::sync::Arc;

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
pub struct ReexMatchStr<'a> {
    start: usize,
    end: usize,
    data: &'a str,
    selection: SelectionCollection,
}

impl<'a> ReexMatchInner<&'a str> for ReexMatchStr<'a> {
    fn data_slice(&self, from: usize, to: usize) -> &'a str {
        if from == to {
            return "";
        }

        let mut data = self.data.char_indices().skip(from);
        let (first, _) = data.next().unwrap();
        let (end, _) = data
            .skip((to - from) - 1)
            .next()
            .unwrap_or((self.data.len(), ' '));
        &self.data[first..end]
    }

    fn selection_collection(&self) -> &SelectionCollection {
        &self.selection
    }
}

impl<'a, 'b> ReexMatch<&'a str> for ReexMatchStr<'a> {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

fn slice_str_char(input: &str, start: usize, end: usize) -> &str {
    if start == end {
        return "";
    }

    let mut data = input.char_indices().skip(start);
    let (first, _) = data.next().unwrap();
    let (end, _) = data
        .skip((end - start) - 1)
        .next()
        .unwrap_or((input.len(), ' '));
    &input[first..end]
}

impl<T: Debug> Display for ReexMatchBorrowed<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}..{}) ｢{:?}｣", self.start(), self.end(), self.data())
    }
}

impl Display for ReexMatchStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "({}..{}) ｢{}｣", self.start(), self.end(), self.data())?;

        let data = self.data;

        for item in self.selection.iter() {
            writeln!(
                f,
                "{}{} => ({}..{}) ｢{}｣",
                "  ".repeat(item.index_path().len()),
                item.index_path().last().unwrap(),
                item.start(),
                item.end(),
                slice_str_char(data, item.start(), item.end())
            )?;
        }

        Ok(())
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
            .map(move |(start, end, selection)| ReexMatchStr {
                start,
                end,
                data: &data,
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

        while let Some(matched) = reex.find_str("hello hello hello") {
            println!("{}", matched)
        }
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
    fn test_heavy_black_heart_forced_into_emoji() {
        let mut reex = Reex::new("❤️").unwrap();
        reex.find_str("❤️").expect("Should find itself");
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_repeating_lookahead() {
        let mut reex = Reex::new(":ahead[ ❤ ]+").unwrap();
        assert_eq!("", reex.find_str("❤️").expect("Should succeed").data());
    }

    #[cfg(feature = "parser")]
    #[test]
    fn test_select() {
        let mut reex =
            Reex::new(":select[ :select[ a ]+ ] :select[ b+ ] :ahead[ :select[ c ] ]").unwrap();

        println!("{}", reex.find_str("aaabbbbc").unwrap());
    }
}
