mod compiler;

pub use compiler::custom;
pub use compiler::{compile, compile_typed, Compiler};

use pest::iterators::{Pair, Pairs};
use pest::{error::Error, Parser};
use pest_derive::Parser;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Parser)]
#[grammar = "../pest/reex.pest"] // relative to src
struct ReexParser;

#[derive(Debug)]
pub struct ReexQuantifier {
    start: usize,
    end: usize,
    glue: Option<Box<ReexNode>>,
    trailing_glue: bool,
    min: usize,
    max: usize,
}

impl ReexQuantifier {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn glue(&self) -> Option<&ReexNode> {
        self.glue.as_ref().map(|x| x.as_ref())
    }
}

#[derive(Debug)]
pub struct ReexSet {
    items: Vec<ReexNode>,
}

impl ReexSet {
    pub fn empty() -> ReexSet {
        ReexSet { items: vec![] }
    }

    pub fn items(&self) -> &Vec<ReexNode> {
        &self.items
    }
}

#[derive(Debug)]
pub struct ReexOptions {
    options: Vec<ReexNode>,
}

impl ReexOptions {
    pub fn empty() -> ReexOptions {
        ReexOptions { options: vec![] }
    }

    pub fn options(&self) -> &Vec<ReexNode> {
        &self.options
    }
}

#[derive(Debug)]
pub struct ReexNode {
    start: usize,
    end: usize,
    item: ReexItem,
    quantifier: Option<ReexQuantifier>,
}

impl ReexNode {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn item(&self) -> &ReexItem {
        &self.item
    }

    pub fn quantifier(&self) -> Option<&ReexQuantifier> {
        self.quantifier.as_ref()
    }
}

#[derive(Debug)]
pub struct ReexFlag {
    name: String,
    arguments: Vec<String>,
}

impl ReexFlag {
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct ReexBlock {
    name: String,
    arguments: Vec<String>,
    item: Box<ReexNode>,
}

impl ReexBlock {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn item(&self) -> &ReexNode {
        &self.item
    }
}

#[derive(Debug)]
pub enum ReexItem {
    Set(ReexSet),
    Options(ReexOptions),
    Flag(ReexFlag),
    Block(ReexBlock),
    Literal(ReexLiteral),
}

#[derive(Debug)]
pub struct ReexLiteral {
    value: String,
    quoted: bool,
}

impl ReexLiteral {
    pub fn value(&self) -> &str {
        &self.value
    }
    pub fn quoted(&self) -> bool {
        self.quoted
    }
}

pub struct ReexBuilder;

#[derive(Debug)]
pub enum ReexError {
    Parse(Error<Rule>),
}

impl Display for ReexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ReexError::Parse(err) => err.fmt(f),
        }
    }
}

impl From<Error<Rule>> for ReexError {
    fn from(err: Error<Rule>) -> Self {
        ReexError::Parse(err)
    }
}

impl ReexBuilder {
    pub fn parse(input: &str) -> Result<ReexNode, ReexError> {
        let mut rules: Pairs<Rule> = ReexParser::parse(Rule::reex, &input)?;
        let item = if let Some(item) = rules.next() {
            item
        } else {
            return Ok(ReexNode {
                start: 0,
                end: 0,
                item: ReexItem::Set(ReexSet::empty()),
                quantifier: None,
            });
        };

        let item = Self::build_item(item);
        Ok(item)
    }

    fn build_item(input: Pair<Rule>) -> ReexNode {
        let span = input.as_span();
        let start = span.start();
        let end = span.end();
        let item = match input.as_rule() {
            Rule::reex | Rule::group => ReexItem::Options(Self::build_options(input)),
            Rule::element_list => return Self::build_set(input),
            Rule::block => ReexItem::Block(Self::build_block(input)),
            Rule::flag => ReexItem::Flag(Self::build_flag(input)),
            Rule::item => {
                return Self::build_item(input.into_inner().next().unwrap());
            }
            Rule::quoted_string => ReexItem::Literal(ReexLiteral {
                value: Self::build_string(input),
                quoted: true,
            }),
            Rule::ident => ReexItem::Literal(ReexLiteral {
                value: Self::build_string(input),
                quoted: false,
            }),
            Rule::symbol => match input.as_str() {
                "." => ReexItem::Flag(ReexFlag {
                    name: "any".to_string(),
                    arguments: vec![],
                }),
                "$" => ReexItem::Flag(ReexFlag {
                    name: "end".to_string(),
                    arguments: vec![],
                }),
                "^" => ReexItem::Flag(ReexFlag {
                    name: "start".to_string(),
                    arguments: vec![],
                }),

                _ => panic!("{:?}", input),
            },
            _ => panic!("{:?}", input),
        };

        ReexNode {
            start,
            end,
            item,
            quantifier: None,
        }
    }

    fn build_options(input: Pair<Rule>) -> ReexOptions {
        let mut options = vec![];
        for item in input.into_inner() {
            if item.as_rule() == Rule::EOI {
                continue;
            }

            options.push(Self::build_set(item))
        }

        ReexOptions { options }
    }

    fn build_block(input: Pair<Rule>) -> ReexBlock {
        let mut items = input.into_inner();
        let name = items.next().unwrap().as_str();
        let mut arguments = vec![];
        for item in items {
            if item.as_rule() == Rule::group {
                return ReexBlock {
                    name: name.to_string(),
                    arguments,
                    item: Box::new(Self::build_item(item)),
                };
            } else {
                arguments.push(Self::build_string(item))
            }
        }

        unreachable!();
    }

    fn build_flag(input: Pair<Rule>) -> ReexFlag {
        let mut items = input.into_inner().next().unwrap().into_inner();
        let name = items.next().unwrap().as_str();
        let mut flag = ReexFlag {
            name: name.to_string(),
            arguments: vec![],
        };
        for item in items {
            flag.arguments.push(Self::build_string(item))
        }

        flag
    }

    fn build_string(input: Pair<Rule>) -> String {
        if input.as_rule() == Rule::ident {
            return input.as_str().to_string();
        }

        let text = input.as_str();
        let inner = input.into_inner().next();
        if inner.is_none() {
            return text.to_string();
        }
        let inner = inner.unwrap();
        let inner = if inner.as_rule() != Rule::inner_quoted_string {
            inner.into_inner().next().unwrap()
        } else {
            inner
        };

        let mut escaped = false;
        let mut result = String::new();
        for ch in inner.as_str().chars() {
            if ch == '\\' && !escaped {
                escaped = true;
                continue;
            }

            match (ch, escaped) {
                ('n', true) => result.push('\n'),
                ('t', true) => result.push('\t'),
                ('r', true) => result.push('\r'),
                _ => result.push(ch),
            }

            escaped = false;
        }

        result
    }

    fn build_set(input: Pair<Rule>) -> ReexNode {
        let span = input.as_span();
        let start = span.start();
        let end = span.end();
        let mut items = vec![];
        for item in input.into_inner() {
            if item.as_rule() == Rule::EOI {
                continue;
            }

            items.push(Self::build_node(item))
        }

        if items.len() == 1 {
            return items.pop().unwrap();
        }

        ReexNode {
            start,
            end,
            item: ReexItem::Set(ReexSet { items }),
            quantifier: None,
        }
    }

    fn build_node(input: Pair<Rule>) -> ReexNode {
        let mut item: Option<Pair<Rule>> = None;
        let mut quantifier: Option<Pair<Rule>> = None;
        let mut glue: Option<Pair<Rule>> = None;

        for part in input.into_inner() {
            match part.as_rule() {
                Rule::item => item = Some(part),

                Rule::quantifier => quantifier = Some(part),

                Rule::quantifier_glue => glue = Some(part),

                _ => {}
            }
        }

        let item = item.unwrap();
        let mut child_node = Self::build_item(item);
        if child_node.quantifier.is_none() {
            child_node.quantifier = Self::build_quantifier(quantifier, glue);
            return child_node;
        }

        let quantifier = Self::build_quantifier(quantifier, glue);
        if quantifier.is_none() {
            return child_node;
        }

        ReexNode {
            start: child_node.start,
            end: child_node.end,
            item: ReexItem::Set(ReexSet {
                items: vec![child_node],
            }),
            quantifier,
        }
    }

    fn build_quantifier(
        quantifier: Option<Pair<Rule>>,
        glue_pair: Option<Pair<Rule>>,
    ) -> Option<ReexQuantifier> {
        if quantifier.is_none() && glue_pair.is_none() {
            return None;
        }

        let start = quantifier
            .as_ref()
            .or_else(|| glue_pair.as_ref())
            .unwrap()
            .as_span()
            .start();
        let end = glue_pair
            .as_ref()
            .or_else(|| quantifier.as_ref())
            .unwrap()
            .as_span()
            .end();

        let mut min = 0;
        let mut max = usize::MAX;

        if let Some(range) = quantifier {
            match range.as_str() {
                "+" => min = 1,
                "*" => {}
                "?" => max = 1,
                _ => {
                    let mut items = range.into_inner().next().unwrap().into_inner();
                    let min_str = items.next().unwrap().as_str();
                    let max_str = items.next().unwrap().as_str();
                    if !min_str.is_empty() {
                        min = usize::from_str(min_str).unwrap()
                    }
                    if !max_str.is_empty() {
                        max = usize::from_str(max_str).unwrap()
                    }
                }
            }
        }

        let mut glue = None;
        let mut trailing_glue = false;

        if let Some(glue_pair) = glue_pair {
            let mut inner = glue_pair.into_inner();
            trailing_glue = inner.next().unwrap().as_str() == "%%";
            glue = Some(Box::new(Self::build_item(inner.next().unwrap())));
        }

        Some(ReexQuantifier {
            start,
            end,
            glue,
            trailing_glue,
            min,
            max,
        })
    }
}
