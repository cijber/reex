#![allow(dead_code, unused_variables, unused_macros)]

use crate::matchers::Matcher;
use std::fmt::{Debug, Display, Formatter};
use std::{fmt, mem};

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
enum Instruction<T> {
    Any,
    Item(T),
    ItemClass(Box<dyn Matcher<T>>),
    Peek(Box<dyn Matcher<T>>),
    PeekBehind(Box<dyn Matcher<T>>),
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
        }
    }
}

#[derive(Default, Debug)]
struct Program<T> {
    instructions: Vec<Instruction<T>>,
}

impl<T: Debug> Display for Program<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Program {{")?;
        for i in &self.instructions {
            writeln!(f, "  {}", i)?;
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
enum ThreadDirection {
    Forwards = 1,
    Backwards = -1,
}

impl Default for ThreadDirection {
    fn default() -> Self {
        ThreadDirection::Forwards
    }
}

#[derive(Default, Debug, Clone)]
struct Thread {
    address: usize,
    position: usize,
    flags: Vec<usize>,
    direction: ThreadDirection,
}

impl Thread {
    pub fn cont(position: usize) -> Thread {
        Thread {
            position,
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
struct State<'a, T> {
    threads: Vec<Thread>,
    program: &'a Program<T>,
}

impl<T: Eq + Debug> State<'_, T> {
    fn new(program: &Program<T>) -> State<T> {
        State {
            threads: vec![],
            program,
        }
    }

    fn run(&mut self, data: &[T]) -> Vec<(usize, usize)> {
        let mut results = vec![];
        for i in 0..data.len() {
            self.threads.push(Thread::cont(i));
            while let Some(mut items) = self.step(&data) {
                for end in items {
                    results.push((i, end));
                }
            }
        }

        results
    }

    fn step(&mut self, data: &[T]) -> Option<Vec<usize>> {
        let old = mem::replace(&mut self.threads, vec![]);
        let mut matches = vec![];
        let mut i = 0;
        for mut thread in old {
            if thread.address >= self.program.instructions.len() {
                continue;
            }
            i += 1;

            let res = self.instruction(data, &mut thread);
            match res {
                InstructionResult::Fork(new_thread) => {
                    self.threads.push(thread);
                    self.threads.push(new_thread);
                }
                InstructionResult::Match => {
                    matches.push(thread.position);
                }
                InstructionResult::Continue => {
                    thread.address += 1;
                    self.threads.push(thread);
                }
                InstructionResult::Jumped => {
                    self.threads.push(thread);
                }
                InstructionResult::Break => {
                    // println!(
                    //     "Failed on position {} for #{}: {} ",
                    //     thread.position, thread.address, &self.program.instructions[thread.address]
                    // );
                }
            }
        }

        if matches.is_empty() && self.threads.is_empty() {
            return None;
        }

        return Some(matches);
    }

    fn instruction(&mut self, data: &[T], thread: &mut Thread) -> InstructionResult {
        use InstructionResult::*;
        match &self.program.instructions[thread.address] {
            Instruction::Item(item) => match thread.peek_position(&data) {
                Some(pos) if &data[pos] == item => {
                    thread.advance_position();
                    Continue
                }
                _ => Break,
            },
            Instruction::Split(first, second) => {
                thread.address = *first;
                let mut fork = thread.clone();
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

                Continue
            }
            Instruction::Start => {
                if 0 != thread.position {
                    Break
                } else {
                    Continue
                }
            }
            Instruction::End => {
                if data.len() != thread.position {
                    Break
                } else {
                    Continue
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
                Some(pos) if c.matches(&data[pos]) => {
                    thread.advance_position();
                    Continue
                }
                _ => Break,
            },
            Instruction::Peek(c) => match thread.peek_position(&data) {
                Some(pos) if c.matches(&data[pos]) => Continue,
                _ => Break,
            },
            Instruction::PeekBehind(c) => match thread.peek_position_behind(&data) {
                Some(pos) if c.matches(&data[pos]) => Continue,
                _ => Break,
            },
            Instruction::Any => match thread.peek_position(&data) {
                Some(_) => {
                    thread.advance_position();
                    Continue
                }

                _ => Break,
            },
        }
    }
}

#[derive(Debug)]
enum InstructionResult {
    Fork(Thread),
    Match,
    Continue,
    Jumped,
    Break,
}

macro_rules! reex {
    [$($_:tt)*] => {};
}

#[cfg(test)]
mod tests {
    use crate::vm::Program;
    use std::time::{Instant, SystemTime};

    #[test]
    fn test() {
        use crate::matchers::*;

        let prog: Program<char> = program! {
            checkpoint 1;
            reverse;
            split 3, 5;
            itemclass Word;
            split 2, 13;
            split 6, 8;
            itemclass Whitespace;
            split 6, 8;
            item '.';
            jmp 11;
            itemclass Whitespace;
            split 10, 12;
            split 2, 13;
            split 14, 17;
            peek Not(Box::new(Word));
            peekbehind Word;
            jmp 19;
            peek Word;
            peekbehind Not(Box::new(Word));
            rewind 1;
            forwards;
            split 22, 25;
            peek Not(Box::new(Word));
            peekbehind Word;
            jmp 27;
            peek Word;
            peekbehind Not(Box::new(Word));
            char '(';
            split 29, 31;
            itemclass Not(Box::new(Exact(')')));
            split 29, 31;
            char ')';
            match;
        };

        let mut state = prog.spawn();
        let input = r#"
int main(void)
static int foo.main(bar baz)
static main (void)
        "#;
        let start = Instant::now();
        let matches = state.run(&input.chars().collect::<Vec<_>>());
        let end = Instant::now();
        println!("{}", prog);
        println!("Matches ({:?}):", (end - start));
        for (start, end) in matches {
            println!(
                "  ({}..{}) ｢{}｣",
                start,
                end,
                &input
                    .chars()
                    .skip(start)
                    .take(end - start)
                    .collect::<String>(),
            )
        }
    }
}
