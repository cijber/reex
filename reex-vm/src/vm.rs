use crate::matchers::Matcher;
use std::collections::BTreeSet;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
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
}

type Address = usize;
type Flag = usize;

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

impl<T: Eq + Debug> Program<T> {
    pub fn spawn(&self) -> State<T> {
        State::new(&self)
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

#[derive(Debug, Clone)]
pub struct Thread {
    source_address: usize,
    address: usize,
    position: usize,
    flags: Vec<usize>,
    try_split: BTreeSet<usize>,
    inverted: bool,
    direction: ThreadDirection,
}

impl Default for Thread {
    fn default() -> Self {
        Thread {
            source_address: 0,
            address: 0,
            position: 0,
            flags: vec![],
            try_split: Default::default(),
            inverted: false,
            direction: ThreadDirection::Forwards,
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
    }

    pub fn set_flag(&mut self, i: usize, v: usize) {
        self.ensure_flag(i);
        self.flags[i] = v;
    }

    pub fn inc_flag(&mut self, i: usize) {
        self.ensure_flag(i);
        self.flags[i] += 1;
    }

    pub fn get_flag(&mut self, i: usize) -> usize {
        self.ensure_flag(i);
        self.flags[i]
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

#[derive(Debug)]
pub struct State<'a, T: Debug> {
    threads: Vec<Thread>,
    program: &'a Program<T>,
}

impl<T: Debug + PartialEq> IsState<T> for State<'_, T> {
    fn program(&self) -> &Program<T> {
        &self.program
    }

    fn threads(&mut self) -> &mut Vec<Thread> {
        &mut self.threads
    }
}

#[derive(Debug)]
pub struct StateOwned<T: Debug> {
    threads: Vec<Thread>,
    program: Arc<Program<T>>,
}

impl<T: Debug + PartialEq> IsState<T> for StateOwned<T> {
    fn program(&self) -> &Program<T> {
        self.program.as_ref()
    }

    fn threads(&mut self) -> &mut Vec<Thread> {
        &mut self.threads
    }
}

pub trait IsState<T: Debug + PartialEq> {
    fn program(&self) -> &Program<T>;
    fn threads(&mut self) -> &mut Vec<Thread>;

    fn run(&mut self, data: &[T]) -> Vec<(usize, usize)> {
        let mut results = vec![];
        for i in 0..data.len() {
            let mut steps = 0;
            self.threads().push(Thread::cont(i));
            while let Some(items) = self.step(&data) {
                if self.threads().len() > 100 || steps > 1000 {
                    panic!(
                        "{} threads, {} steps:\n{:#?}",
                        self.threads().len(),
                        steps,
                        self.threads()
                    );
                }

                if let Some(end) = items {
                    results.push((i, end));
                    return results;
                }

                steps += 1usize;
            }
        }

        results
    }

    fn step(&mut self, data: &[T]) -> Option<Option<usize>> {
        let mut thread = self.threads().pop()?;
        if thread.address >= self.program().instructions.len() {
            return Some(None);
        }

        let addr = thread.address;
        // let pos = thread.position;
        let res = self.instruction(data, &mut thread);
        // println!(
        //     "{:3>0} {} {:?} {:?}",
        //     addr,
        //     &self.program.instructions[addr],
        //     &data.get(pos),
        //     res
        // );
        match res {
            InstructionResult::Fork(new_thread) => {
                self.threads().push(new_thread);
                self.threads().push(thread);
            }
            InstructionResult::Match => return Some(Some(thread.position)),
            InstructionResult::Assertion => {
                if !thread.inverted {
                    if self.program().instructions[addr].should_advance() {
                        thread.advance_position();

                        for split in &thread.try_split {
                            let mut new_thread = thread.clone();
                            new_thread.address = new_thread.flags[*split];
                            new_thread.try_split.remove(split);
                            self.threads().push(new_thread);
                        }
                    }

                    thread.address += 1;
                    self.threads().push(thread);
                }
            }
            InstructionResult::Continue => {
                thread.address += 1;
                self.threads().push(thread);
            }
            InstructionResult::Jumped => {
                self.threads().push(thread);
            }
            InstructionResult::BoundaryBreak => {}
            InstructionResult::Break => {
                if thread.inverted {
                    if self.program().instructions[addr].should_advance() {
                        thread.advance_position();

                        for split in &thread.try_split {
                            let mut new_thread = thread.clone();
                            new_thread.address = new_thread.flags[*split];
                            new_thread.try_split.remove(split);
                            self.threads().push(new_thread);
                        }
                    }

                    thread.address += 1;
                    self.threads().push(thread)
                }
            }
        }

        Some(None)
    }

    fn instruction(&mut self, data: &[T], thread: &mut Thread) -> InstructionResult {
        use InstructionResult::*;
        match &self.program().instructions[thread.address] {
            Instruction::Item(item) => match thread.peek_position(&data) {
                Some(pos) if &data[pos] == item => Assertion,
                None => BoundaryBreak,
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
                None => BoundaryBreak,
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
                _ => BoundaryBreak,
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
        }
    }
}

impl<T: Eq + Debug> State<'_, T> {
    fn new(program: &Program<T>) -> State<T> {
        State {
            threads: vec![],
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
    BoundaryBreak,
}
