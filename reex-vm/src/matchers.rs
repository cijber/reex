use std::fmt;
use std::fmt::{Debug, Display, Formatter};

pub trait Matcher<T: Debug>: Debug + Display {
    fn matches(&self, item: &T) -> bool;
}

#[derive(Debug)]
pub struct Exact<T: Eq + Debug>(pub(crate) T);

impl<T: Eq + Debug> Display for Exact<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self.0)
    }
}

impl<T: Eq + Debug> Matcher<T> for Exact<T> {
    fn matches(&self, item: &T) -> bool {
        &self.0 == item
    }
}

#[derive(Debug)]
pub struct Multiple<T: Debug>(pub(crate) Vec<Box<dyn Matcher<T>>>);

impl<T: Debug> Matcher<T> for Multiple<T> {
    fn matches(&self, item: &T) -> bool {
        for child in &self.0 {
            if child.matches(item) {
                return true;
            }
        }

        false
    }
}

impl<T: Debug> Display for Multiple<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.0
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug)]
pub struct Whitespace;

impl Display for Whitespace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, ":whitespace")
    }
}

impl Matcher<String> for Whitespace {
    fn matches(&self, item: &String) -> bool {
        item.chars().all(|x| x.is_whitespace())
    }
}

impl Matcher<char> for Whitespace {
    fn matches(&self, item: &char) -> bool {
        item.is_whitespace()
    }
}

impl Matcher<u8> for Whitespace {
    fn matches(&self, item: &u8) -> bool {
        item.is_ascii_whitespace()
    }
}

#[derive(Debug)]
pub struct Word;

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, ":word")
    }
}

impl Matcher<String> for Word {
    fn matches(&self, item: &String) -> bool {
        item.chars().all(|x| x.is_alphanumeric())
    }
}

impl Matcher<char> for Word {
    fn matches(&self, item: &char) -> bool {
        item.is_alphanumeric()
    }
}

impl Matcher<u8> for Word {
    fn matches(&self, item: &u8) -> bool {
        item.is_ascii_alphanumeric()
    }
}

#[derive(Debug)]
pub struct Not<T: Debug>(pub(crate) Box<dyn Matcher<T>>);

impl<T: Debug> Matcher<T> for Not<T> {
    fn matches(&self, item: &T) -> bool {
        !self.0.matches(item)
    }
}

impl<T: Debug> Display for Not<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, ":not( {} )", self.0)
    }
}
