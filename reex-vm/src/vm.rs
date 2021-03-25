use crate::matchers::Matcher;
use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

#[macro_export]
macro_rules! program {
    { $($($in:ident $($var:expr),*)?);+ } => {
        $crate::vm::Program { instructions: vec![$( $( program!(> $in $($var),*) )? ),+] }
    };


    ( > char $a:expr) => {
        $crate::vm::Instruction::Item($a)
    };

    ( > item $a:expr) => {
        $crate::vm::Instruction::Item($a)
    };

    ( > itemclass $a:expr) => {
        $crate::vm::Instruction::ItemClass(Box::new($a))
    };

    ( > split $a:expr, $b:expr) => {
        $crate::vm::Instruction::Split($a, $b)
    };

    ( > inc $a:expr) => {
        $crate::vm::Instruction::Inc($a)
    };

    ( > between $a:expr, $b:expr, $c:expr) => {
        $crate::vm::Instruction::Between($a, $b, $c)
    };

    ( > end) => {
        $crate::vm::Instruction::EndOfString
    };

    ( > match) => {
        $crate::vm::Instruction::Match
    };

    ( > jmp $a:expr) => {
        $crate::vm::Instruction::Jump($a)
    };

    ( > checkpoint $a:expr) => {
        $crate::vm::Instruction::Checkpoint($a)
    };

    ( > rewind $a:expr) => {
        $crate::vm::Instruction::Rewind($a)
    };

    ( > reverse) => {
        $crate::vm::Instruction::Reverse
    };

    ( > forwards) => {
        $crate::vm::Instruction::Forwards
    };

    ( > any) => {
        $crate::vm::Instruction::Any
    };

    ( > peek $a:expr) => {
        $crate::vm::Instruction::Peek(Box::new($a))
    };

    ( > peekbehind $a:expr) => {
        $crate::vm::Instruction::PeekBehind(Box::new($a))
    };

    ( > noop) => {
        $crate::vm::Instruction::PeekBehind(Box::new($a))
    };

    ( > testnotequalsandstoreposition $a:expr) => {
        $crate::vm::Instruction::PeekBehind(Box::new($a))
    };
}

type Address = usize;
type Flag = usize;
type SelectionIdentifier = usize;

#[derive(Debug)]
pub enum Instruction<T> {
    Any,
    Noop,
    Item(T),
    ItemClass(Box<dyn Matcher<T>>),
    Peek(Box<dyn Matcher<T>>),
    PeekBehind(Box<dyn Matcher<T>>),
    TrySplit(Flag, Address),
    StopSplit(Flag),
    Split(Address, Address),
    Inc(Flag),
    Start,
    End,
    Between(Flag, usize, usize),
    Match,
    Jump(Address),
    Checkpoint(Flag),
    Rewind(Flag),
    Reverse,
    Forwards,
    Boundary(Box<dyn Matcher<T>>),
    Invert,
    TestNotEqualsAndStorePosition(Flag),
    StartSelection(SelectionIdentifier),
    EndSelection,
    Marker(&'static str, usize),
}

impl<T: Debug> Instruction<T> {
    pub fn should_advance(&self) -> bool {
        match self {
            Instruction::Any | Instruction::Item(_) | Instruction::ItemClass(_) => true,
            _ => false,
        }
    }
}

impl<T: Debug> Display for Instruction<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            Instruction::Item(i) => write!(f, "item {:?}", i),
            Instruction::Split(a, b) => write!(f, "split {}, {}", a, b),
            Instruction::Inc(a) => write!(f, "inc {}", a),
            Instruction::Between(a, min, max) => write!(f, "between {}, {}, {}", a, min, max),
            Instruction::Match => write!(f, "match"),
            Instruction::Start => write!(f, "start"),
            Instruction::End => write!(f, "end"),
            Instruction::Jump(a) => write!(f, "jmp {}", a),
            Instruction::Checkpoint(a) => write!(f, "checkpoint {}", a),
            Instruction::Rewind(a) => write!(f, "rewind {}", a),
            Instruction::Reverse => write!(f, "reverse"),
            Instruction::Forwards => write!(f, "forwards"),
            Instruction::ItemClass(c) => write!(f, "itemclass {}", c),
            Instruction::Peek(c) => write!(f, "peek {}", c),
            Instruction::PeekBehind(c) => write!(f, "peekbehind {}", c),
            Instruction::Any => write!(f, "any"),
            Instruction::Boundary(c) => write!(f, "boundary {}", c),
            Instruction::Invert => write!(f, "invert"),
            Instruction::TrySplit(fl, ad) => write!(f, "trysplit {}, {}", fl, ad),
            Instruction::StopSplit(fl) => write!(f, "stopsplit {}", fl),
            Instruction::Noop => write!(f, "noop"),
            Instruction::TestNotEqualsAndStorePosition(fl) => {
                write!(f, "testnotequalsandstoreposition {}", fl)
            }
            Instruction::StartSelection(id) => {
                write!(f, "startselection {}", id)
            }
            Instruction::EndSelection => {
                write!(f, "endselection")
            }
            Instruction::Marker(data, n) => {
                write!(f, "marker {:?} {}", data, n)
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct Program<T> {
    pub instructions: Vec<Instruction<T>>,
}

impl<T: Debug> Display for Program<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Program {{")?;
        let mut ind = 0;
        for i in &self.instructions {
            writeln!(f, "{:0>3}   {}", ind, i)?;
            ind += 1;
        }
        writeln!(f, "}}")
    }
}

impl<T: Debug> Program<T> {
    pub fn spawn(&self) -> RuntimeBorrowed<T> {
        RuntimeBorrowed::new(&self)
    }

    pub fn spawn_owned(self: &Arc<Self>) -> RuntimeOwned<T> {
        RuntimeOwned::new(Arc::clone(self))
    }
}

#[derive(Copy, Clone, Debug, Eq, Ord, PartialOrd, PartialEq)]
#[repr(isize)]
pub enum ThreadDirection {
    Forwards = 1,
    Backwards = -1,
}

impl Default for ThreadDirection {
    fn default() -> Self {
        ThreadDirection::Forwards
    }
}

#[derive(Debug, Clone, Default)]
pub struct Selected {
    start: usize,
    end: usize,
    name: Option<String>,
    full_path: Vec<usize>,
    index_path: Vec<usize>,
}

impl Selected {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    pub fn full_path(&self) -> &[usize] {
        &self.full_path
    }

    pub fn index_path(&self) -> &[usize] {
        &self.index_path
    }
}

#[derive(Debug, Clone, Default)]
pub struct SelectionCollection {
    selections: Vec<Selected>,
    by_name: BTreeMap<String, Vec<usize>>,
    by_index: BTreeMap<Vec<usize>, Vec<usize>>,
    by_path: BTreeMap<Vec<usize>, usize>,
}

impl SelectionCollection {
    fn add(&mut self, selection: Selected) {
        let index = self.selections.len();

        if let Some(name) = selection.name.clone() {
            self.by_name.entry(name).or_default().push(index);
        }
        self.by_index
            .entry(selection.index_path.clone())
            .or_default()
            .push(index);
        self.by_path.insert(selection.full_path.clone(), index);
        self.selections.push(selection);
    }

    fn update_end(&mut self, path: &[usize], position: usize) {
        if let Some(item) = self
            .by_path
            .get(path)
            .copied()
            .and_then(|index| self.selections.get_mut(index))
        {
            item.end = max(item.end, position);
            item.start = min(item.start, position);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Selected> {
        self.selections.iter()
    }

    pub fn children(&self, path: &[usize]) -> impl Iterator<Item = &Selected> {
        let path = path.to_owned();
        let mut clone_path = path.clone();
        if clone_path.len() > 0 {
            let id = clone_path.len() - 1;
            clone_path[id] += 1;
        }

        let path_2 = path.clone();

        self.by_path
            .range(path..clone_path)
            .filter_map(move |(child, index)| {
                if (child.len() - 2) > path_2.len() {
                    return None;
                }

                self.selections.get(*index)
            })
    }

    pub fn descendants(&self, path: &[usize]) -> impl Iterator<Item = &Selected> {
        let mut clone_path = path.to_vec();
        if clone_path.len() > 0 {
            let idx = clone_path.len() - 1;
            clone_path[idx] += 1;
        }

        self.by_path
            .range(path.to_vec()..clone_path)
            .filter_map(move |(_, index)| self.selections.get(*index))
    }

    pub fn by_index(&self, index: &'_ [usize]) -> impl Iterator<Item = &Selected> + '_ {
        self.by_index
            .get(index)
            .into_iter()
            .flat_map(|x| x.iter())
            .filter_map(move |x| self.selections.get(*x))
    }

    pub fn by_name<'a>(&'a self, name: &str) -> impl Iterator<Item = &Selected> + 'a {
        self.by_name
            .get(name)
            .into_iter()
            .flat_map(|x| x.iter())
            .filter_map(move |x| self.selections.get(*x))
    }
}

#[derive(Debug, Clone)]
pub struct Thread {
    source_address: usize,
    address: usize,
    position: usize,
    flags: Vec<usize>,
    flag_defined: Vec<bool>,
    try_split: BTreeSet<usize>,
    inverted: bool,
    direction: ThreadDirection,
    selections: SelectionCollection,
    full_path: Vec<usize>,
    index_path: Vec<usize>,
    index_counter: BTreeMap<Vec<usize>, usize>,
}

#[derive(Debug, Clone)]
pub struct Selection {
    start: usize,
    end: usize,
    children: Vec<Vec<Selection>>,
}

impl Selection {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn children(&self) -> &Vec<Vec<Selection>> {
        &self.children
    }
}

impl Default for Thread {
    fn default() -> Self {
        Thread {
            source_address: 0,
            address: 0,
            position: 0,
            flags: vec![],
            flag_defined: vec![],
            try_split: Default::default(),
            inverted: false,
            direction: ThreadDirection::Forwards,
            selections: Default::default(),
            full_path: vec![],
            index_path: vec![],
            index_counter: Default::default(),
        }
    }
}

impl Thread {
    pub fn cont(position: usize) -> Thread {
        Thread {
            position,
            inverted: false,
            ..Default::default()
        }
    }

    pub fn ensure_flag(&mut self, i: usize) {
        if self.flags.len() <= i {
            self.flags.append(&mut vec![0; 1 + (i - self.flags.len())]);
        }

        if self.flag_defined.len() <= i {
            self.flag_defined
                .append(&mut vec![false; 1 + (i - self.flag_defined.len())]);
        }
    }

    fn corrected_position(&mut self) -> usize {
        match self.direction {
            ThreadDirection::Forwards => self.position,
            ThreadDirection::Backwards => max(self.position, 1) - 1,
        }
    }

    pub fn start_selection(&mut self, id: usize) {
        self.index_path.push(id);
        self.full_path.push(id);
        let index = self
            .index_counter
            .get(&self.full_path)
            .copied()
            .unwrap_or(0);
        self.index_counter.insert(self.full_path.clone(), index + 1);
        self.full_path.push(index);
        let selection = Selected {
            start: self.corrected_position(),
            end: self.corrected_position(),
            name: None,
            full_path: self.full_path.clone(),
            index_path: self.index_path.clone(),
        };
        self.selections.add(selection);
    }

    pub fn end_selection(&mut self) {
        let path = self.full_path.clone();
        let pos = self.corrected_position();
        self.selections.update_end(&path, pos);
        self.index_path.pop();
        self.full_path.pop();
        self.full_path.pop();
    }

    pub fn set_flag(&mut self, i: usize, v: usize) {
        self.ensure_flag(i);
        self.flag_defined[i] = true;
        self.flags[i] = v;
    }

    pub fn inc_flag(&mut self, i: usize) {
        self.ensure_flag(i);
        self.flag_defined[i] = true;
        self.flags[i] += 1;
    }

    pub fn get_flag(&mut self, i: usize) -> usize {
        self.ensure_flag(i);
        self.flags[i]
    }

    pub fn get_defined_flag(&mut self, i: usize) -> Option<usize> {
        self.ensure_flag(i);
        if self.flag_defined[i] {
            Some(self.flags[i])
        } else {
            None
        }
    }

    pub fn peek_position_direction<T>(
        &self,
        data: &[T],
        direction: ThreadDirection,
    ) -> Option<usize> {
        match direction {
            ThreadDirection::Forwards => {
                if data.len() <= self.position {
                    None
                } else {
                    Some(self.position)
                }
            }
            ThreadDirection::Backwards => {
                if 0 >= self.position {
                    None
                } else {
                    Some(self.position - 1)
                }
            }
        }
    }

    pub fn peek_position<T>(&self, data: &[T]) -> Option<usize> {
        self.peek_position_direction(data, self.direction)
    }

    pub fn peek_position_behind<T>(&self, data: &[T]) -> Option<usize> {
        self.peek_position_direction(
            data,
            if self.direction == ThreadDirection::Forwards {
                ThreadDirection::Backwards
            } else {
                ThreadDirection::Forwards
            },
        )
    }

    pub fn advance_position(&mut self) {
        match self.direction {
            ThreadDirection::Forwards => self.position += 1,
            ThreadDirection::Backwards => self.position -= 1,
        };
    }
}

#[derive(Default, Debug, Clone)]
pub struct State {
    threads: Vec<Thread>,
    position: usize,
}

#[derive(Debug)]
pub struct RuntimeBorrowed<'a, T: Debug> {
    state: State,
    program: &'a Program<T>,
}

#[derive(Debug, Clone)]
pub struct RuntimeOwned<T: Debug> {
    state: State,
    program: Arc<Program<T>>,
}

impl<T: Debug + PartialEq> Runtime<T> for RuntimeBorrowed<'_, T> {
    fn state(&mut self) -> &mut State {
        &mut self.state
    }

    fn program(&self) -> &Program<T> {
        self.program
    }

    fn run(&mut self, data: &[T]) -> Option<(usize, usize, SelectionCollection)> {
        self.state.run(&self.program, data)
    }
}

impl<T: Debug + PartialEq> Runtime<T> for RuntimeOwned<T> {
    fn state(&mut self) -> &mut State {
        &mut self.state
    }

    fn program(&self) -> &Program<T> {
        self.program.as_ref()
    }

    fn run(&mut self, data: &[T]) -> Option<(usize, usize, SelectionCollection)> {
        self.state.run(&self.program.as_ref(), data)
    }
}

pub trait Runtime<T: Debug + PartialEq> {
    fn state(&mut self) -> &mut State;
    fn program(&self) -> &Program<T>;
    fn run(&mut self, data: &[T]) -> Option<(usize, usize, SelectionCollection)>;
}

impl State {
    fn run<T: Debug + PartialEq>(
        &mut self,
        program: &Program<T>,
        data: &[T],
    ) -> Option<(usize, usize, SelectionCollection)> {
        for i in self.position..=data.len() {
            let mut steps = 0;
            // TODO: allow overlap
            self.threads.clear();
            self.threads.push(Thread::cont(i));
            while let Some(items) = self.step(program, &data) {
                if self.threads.len() > 100 || steps > 1000 {
                    panic!(
                        "{} threads, {} steps:\n{:#?}",
                        self.threads.len(),
                        steps,
                        self.threads
                    );
                }

                if let Some((end, selection)) = items {
                    if i > end {
                        self.position = i + 1;
                    } else if self.position == end {
                        self.position = end + 1;
                    } else {
                        self.position = end;
                    }
                    return Some((i, max(end, i), selection));
                }

                steps += 1usize;
            }
        }

        None
    }

    fn step<T: Debug + PartialEq>(
        &mut self,
        program: &Program<T>,
        data: &[T],
    ) -> Option<Option<(usize, SelectionCollection)>> {
        let mut thread = self.threads.pop()?;
        if thread.address >= program.instructions.len() {
            return Some(None);
        }

        let addr = thread.address;
        // let pos = thread.position;
        let res = self.instruction(program, data, &mut thread);
        // println!(
        //     "{:3>0} {} {:?} {:?}",
        //     addr,
        //     &self.program.instructions[addr],
        //     &data.get(pos),
        //     res
        // );
        match res {
            InstructionResult::Fork(new_thread) => {
                self.threads.push(new_thread);
                self.threads.push(thread);
            }
            InstructionResult::Match => {
                println!("Selected: {:?}", thread.selections);
                return Some(Some((thread.position, thread.selections)));
            }
            InstructionResult::Assertion => {
                if !thread.inverted {
                    if program.instructions[addr].should_advance() {
                        thread.advance_position();

                        for split in &thread.try_split {
                            let mut new_thread = thread.clone();
                            new_thread.address = new_thread.flags[*split];
                            new_thread.try_split.remove(split);
                            self.threads.push(new_thread);
                        }
                    }

                    thread.address += 1;
                    self.threads.push(thread);
                }
            }
            InstructionResult::Continue => {
                thread.address += 1;
                self.threads.push(thread);
            }
            InstructionResult::Jumped => {
                self.threads.push(thread);
            }
            InstructionResult::HardBreak => {}
            InstructionResult::Break => {
                if thread.inverted {
                    if program.instructions[addr].should_advance() {
                        thread.advance_position();

                        for split in &thread.try_split {
                            let mut new_thread = thread.clone();
                            new_thread.address = new_thread.flags[*split];
                            new_thread.try_split.remove(split);
                            self.threads.push(new_thread);
                        }
                    }

                    thread.address += 1;
                    self.threads.push(thread)
                }
            }
        }

        Some(None)
    }

    fn instruction<T: Debug + PartialEq>(
        &mut self,
        program: &Program<T>,
        data: &[T],
        thread: &mut Thread,
    ) -> InstructionResult {
        use InstructionResult::*;
        match &program.instructions[thread.address] {
            Instruction::Item(item) => match thread.peek_position(&data) {
                Some(pos) if &data[pos] == item => Assertion,
                None => HardBreak,
                _ => Break,
            },
            Instruction::Split(first, second) => {
                thread.address = *first;
                let mut fork = thread.clone();
                fork.source_address = thread.address;
                fork.address = *second;
                Fork(fork)
            }
            Instruction::Inc(i) => {
                thread.inc_flag(*i);
                Continue
            }
            Instruction::Between(i, min, max) => {
                let val = thread.get_flag(*i);
                if *min > val || val > *max {
                    return Break;
                }

                Assertion
            }
            Instruction::Start => {
                if 0 != thread.position {
                    Break
                } else {
                    Assertion
                }
            }
            Instruction::End => {
                if data.len() != thread.position {
                    Break
                } else {
                    Assertion
                }
            }
            Instruction::Match => Match,
            Instruction::Jump(addr) => {
                thread.address = *addr;
                Jumped
            }
            Instruction::Checkpoint(flag) => {
                thread.set_flag(*flag, thread.position);
                Continue
            }
            Instruction::Rewind(flag) => {
                thread.position = thread.get_flag(*flag);
                Continue
            }
            Instruction::Reverse => {
                thread.direction = ThreadDirection::Backwards;
                Continue
            }
            Instruction::Forwards => {
                thread.direction = ThreadDirection::Forwards;
                Continue
            }
            Instruction::ItemClass(c) => match thread.peek_position(&data) {
                Some(pos) if c.matches(&data[pos]) => Assertion,
                None => HardBreak,
                _ => Break,
            },
            Instruction::Peek(c) => match thread.peek_position(&data) {
                Some(pos) if c.matches(&data[pos]) => Assertion,
                _ => Break,
            },
            Instruction::PeekBehind(c) => match thread.peek_position_behind(&data) {
                Some(pos) if c.matches(&data[pos]) => Assertion,
                _ => Break,
            },
            Instruction::Any => match thread.peek_position(&data) {
                Some(_) => Assertion,
                _ => HardBreak,
            },
            Instruction::Boundary(c) => {
                match (
                    thread.peek_position_behind(&data),
                    thread.peek_position(&data),
                ) {
                    (Some(a), Some(b)) if c.matches(&data[a]) != c.matches(&data[b]) => Assertion,
                    _ => Break,
                }
            }
            Instruction::Invert => {
                thread.inverted = !thread.inverted;
                Continue
            }
            Instruction::TrySplit(flag, address) => {
                let mut fork = thread.clone();
                fork.address = *address;
                thread.set_flag(*flag, *address);
                thread.address += 1;
                thread.try_split.insert(*flag);
                Fork(fork)
            }
            Instruction::StopSplit(flag) => {
                thread.try_split.remove(flag);
                Continue
            }
            Instruction::Noop => Continue,
            Instruction::TestNotEqualsAndStorePosition(flag) => {
                if let Some(val) = thread.get_defined_flag(*flag) {
                    if val == self.position {
                        return HardBreak;
                    }
                }

                thread.set_flag(*flag, self.position);
                Continue
            }
            Instruction::StartSelection(id) => {
                thread.start_selection(*id);
                Continue
            }
            Instruction::EndSelection => {
                thread.end_selection();
                Continue
            }
            Instruction::Marker(_, _) => Continue,
        }
    }
}

impl<T: Debug> RuntimeBorrowed<'_, T> {
    fn new(program: &Program<T>) -> RuntimeBorrowed<T> {
        RuntimeBorrowed {
            state: State::default(),
            program,
        }
    }
}

impl<T: Debug> RuntimeOwned<T> {
    fn new(program: Arc<Program<T>>) -> RuntimeOwned<T> {
        RuntimeOwned {
            state: State::default(),
            program,
        }
    }
}

#[derive(Debug)]
pub enum InstructionResult {
    Fork(Thread),
    Match,
    Continue,
    Jumped,
    Break,
    Assertion,
    HardBreak,
}
