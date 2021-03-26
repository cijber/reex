use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::Chars;
use std::sync::Arc;
use unicode_segmentation::GraphemeCursor;

pub mod matchers;
pub mod vm;

#[derive(Clone, Debug)]
enum ReexInnerString {
    Static(&'static str),
    Dynamic(Arc<String>),
}

impl PartialEq for ReexInnerString {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ReexInnerString::Dynamic(s), ReexInnerString::Dynamic(o)) => Arc::ptr_eq(s, o),
            (ReexInnerString::Static(s), ReexInnerString::Static(o)) => s == o,
            _ => false,
        }
    }
}

impl PartialEq<ReexString> for &str {
    fn eq(&self, other: &ReexString) -> bool {
        *self == other.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct ReexString {
    inner: ReexInnerString,
    start: usize,
    end: usize,
}

impl Display for ReexString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_ref(), f)
    }
}

impl PartialEq for ReexString {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

pub struct ReexStringGraphemes {
    value: ReexString,
    offset: usize,
    cursor: GraphemeCursor,
}

impl Iterator for ReexStringGraphemes {
    type Item = ReexString;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(new_offset) = self.cursor.next_boundary(self.value.as_ref(), 0).unwrap() {
            let old_offset = self.offset;
            self.offset = new_offset;
            Some(self.value.slice(old_offset, new_offset))
        } else {
            None
        }
    }
}

impl ReexStringGraphemes {
    pub fn new(value: ReexString, is_extended: bool) -> ReexStringGraphemes {
        ReexStringGraphemes {
            cursor: GraphemeCursor::new(0, value.len(), is_extended),
            value,
            offset: 0,
        }
    }
}

static EMPTY_STR: &str = "";

impl ReexString {
    pub fn expand(&self, other: &ReexString) -> ReexString {
        if self.inner != other.inner {
            panic!("Can't expand ReexString's, they don't have the same parent");
        }

        ReexString {
            inner: self.inner.clone(),
            start: self.start,
            end: other.end,
        }
    }

    pub fn empty() -> ReexString {
        ReexString::from_static(EMPTY_STR)
    }

    pub fn chars(&self) -> Chars<'_> {
        self.as_ref().chars()
    }

    pub fn graphemes(&self, extended: bool) -> ReexStringGraphemes {
        ReexStringGraphemes::new(self.clone(), extended)
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start != self.end
    }

    pub fn slice(&self, start: usize, end: usize) -> ReexString {
        if !self.as_ref().is_char_boundary(start) || !self.as_ref().is_char_boundary(end) {
            panic!("Can only slice on character boundaries");
        }

        if start > end {
            panic!("Start index bigger than end index ({} > {})", start, end)
        }

        if end > (self.end - self.start) {
            panic!("End index bigger than length ({} > {})", end, self.len())
        }

        let end = self.start + end;
        let start = self.start + start;
        ReexString {
            inner: self.inner.clone(),
            start,
            end,
        }
    }
}

impl AsRef<str> for ReexString {
    fn as_ref(&self) -> &str {
        match &self.inner {
            ReexInnerString::Static(inner) => &(*inner)[self.start..self.end],
            ReexInnerString::Dynamic(inner) => &inner.as_ref()[self.start..self.end],
        }
    }
}

impl ReexString {
    pub fn from_static(input: &'static str) -> Self {
        ReexString {
            inner: ReexInnerString::Static(input),
            start: 0,
            end: input.len(),
        }
    }
}

impl From<String> for ReexString {
    fn from(data: String) -> Self {
        ReexString {
            end: data.len(),
            inner: ReexInnerString::Dynamic(Arc::new(data)),
            start: 0,
        }
    }
}

impl From<&str> for ReexString {
    fn from(data: &str) -> Self {
        ReexString::from(data.to_string())
    }
}
