use crate::{ReexItem, ReexNode};
use reex_vm::matchers::{Exact, Not, Whitespace, Word};
use reex_vm::vm::{Instruction, Program};

pub struct Compiler<T, I: Iterator<Item = T>, F: Fn(&str) -> I> {
    flag: usize,
    reverse: bool,
    transform: F,
}

pub fn compile(node: &ReexNode) -> Program<char> {
    compile_typed(node, |x| x.chars())
}

pub fn compile_typed<T, I: Iterator<Item = T>, F: Fn(&str) -> I>(
    node: &ReexNode,
    transform: F,
) -> Program<T> {
    Compiler::compile_typed(node, transform)
}

impl<T, I: Iterator<Item = T>, F: Fn(&str) -> I> Compiler<T, I, F> {
    fn new(transform: F) -> Compiler<T, I, F> {
        Compiler {
            flag: 0,
            reverse: false,
            transform,
        }
    }

    pub fn compile_typed(node: &ReexNode, transform: F) -> Program<T> {
        let mut compiler = Compiler::new(transform);
        let mut blocks = compiler.compile_blocks(&node);
        let mut start_index = vec![];
        let mut offset = 0;
        blocks.push(vec![Instruction::Match]);
        for block in &blocks {
            start_index.push(offset);
            offset += block.len();
        }

        Program {
            instructions: blocks
                .into_iter()
                .flat_map(|x| {
                    x.into_iter()
                        .map(|x| match x {
                            Instruction::Jump(a) => Instruction::Jump(start_index[a]),
                            Instruction::Split(a, b) => {
                                Instruction::Split(start_index[a], start_index[b])
                            }
                            instr => instr,
                        })
                        .collect::<Vec<_>>()
                })
                .collect(),
        }
    }

    fn compile_blocks(&mut self, node: &ReexNode) -> Vec<Vec<Instruction<T>>> {
        let mut data = vec![];
        match &node.item {
            ReexItem::Set(item) => {
                if self.reverse {
                    for node in item.items.iter().rev() {
                        Compiler::relocate(self.compile_blocks(node), data.len(), &mut data);
                    }
                } else {
                    for node in &item.items {
                        Compiler::relocate(self.compile_blocks(node), data.len(), &mut data);
                    }
                }
            }
            ReexItem::Options(options) => match options.options.len() {
                1 => data = self.compile_blocks(&options.options[0]),
                0 => {}
                x => {
                    let mut items = options
                        .options
                        .iter()
                        .map(|x| self.compile_blocks(x))
                        .collect::<Vec<_>>();
                    let mut end = items.iter().fold(0, |c, l| c + l.len());

                    items.reverse();

                    end += (x - 1) * 2;

                    while items.len() > 1 {
                        let item = items.pop().unwrap();
                        data.push(vec![Instruction::Split(
                            data.len() + 1,
                            data.len() + 2 + item.len(),
                        )]);
                        Compiler::relocate(item, data.len(), &mut data);
                        data.push(vec![Instruction::Jump(end)]);
                    }

                    Compiler::relocate(items.pop().unwrap(), data.len(), &mut data);
                }
            },
            ReexItem::Flag(flag) => match flag.name.as_str() {
                "s" | "ws" | "whitespace" => {
                    data.push(vec![Instruction::ItemClass(Box::new(Whitespace))])
                }
                "b" | "boundary" => data.push(vec![Instruction::Boundary(Box::new(Word))]),
                "w" | "word" => data.push(vec![Instruction::ItemClass(Box::new(Word))]),
                "end" => data.push(vec![Instruction::End]),
                "start" => data.push(vec![Instruction::Start]),
                "any" => data.push(vec![Instruction::Any]),
                _ => {}
            },
            ReexItem::Block(block) => match block.name.as_str() {
                "lb" | "lookbehind" | "behind" => {
                    let flag = self.get_flag();
                    let old_reverse = self.reverse;
                    let mut start = vec![Instruction::Checkpoint(flag)];
                    if !old_reverse {
                        start.push(Instruction::Reverse);
                    }
                    data.push(start);
                    self.reverse = true;
                    Compiler::relocate(
                        self.compile_blocks(block.item.as_ref()),
                        data.len(),
                        &mut data,
                    );
                    self.reverse = old_reverse;
                    let mut end = vec![Instruction::Rewind(flag)];
                    if !self.reverse {
                        end.push(Instruction::Forwards);
                    }
                    data.push(end);
                }

                "la" | "lookahead" | "ahead" => {
                    self.reverse = true;
                    let flag = self.get_flag();
                    let old_reverse = self.reverse;
                    let mut start = vec![Instruction::Checkpoint(flag)];
                    if old_reverse {
                        start.push(Instruction::Forwards);
                    }
                    data.push(start);
                    self.reverse = false;
                    Compiler::relocate(
                        self.compile_blocks(block.item.as_ref()),
                        data.len(),
                        &mut data,
                    );
                    self.reverse = old_reverse;
                    let mut end = vec![Instruction::Rewind(flag)];
                    if self.reverse {
                        end.push(Instruction::Reverse);
                    }
                    data.push(end);
                }

                "prefix" => {
                    let flag = self.get_flag();
                    let child = self.compile_blocks(block.item.as_ref());
                    data.push(vec![Instruction::TrySplit(flag, child.len() + 2)]);
                    Compiler::relocate(child, data.len(), &mut data);
                    data.push(vec![Instruction::StopSplit(flag)]);
                }

                "not" => {
                    let mut blocks = self.compile_blocks(block.item.as_ref());

                    let skip = if blocks.len() == 1 && blocks[0].len() == 1 {
                        let instruction: Instruction<_> = blocks.pop().unwrap().pop().unwrap();
                        let res = match instruction {
                            Instruction::Item(x) => {
                                Ok(Instruction::ItemClass(Box::new(Not(Box::new(Exact(x))))))
                            }
                            Instruction::ItemClass(x) => {
                                Ok(Instruction::ItemClass(Box::new(Not(x))))
                            }
                            Instruction::Boundary(x) => Ok(Instruction::Boundary(Box::new(Not(x)))),
                            Instruction::Peek(x) => Ok(Instruction::Peek(Box::new(Not(x)))),
                            Instruction::PeekBehind(x) => {
                                Ok(Instruction::PeekBehind(Box::new(Not(x))))
                            }
                            inst => Err(inst),
                        };

                        match res {
                            Ok(instr) => Some(instr),
                            Err(instr) => {
                                blocks.push(vec![instr]);
                                None
                            }
                        }
                    } else {
                        None
                    };

                    if let Some(skip) = skip {
                        data.push(vec![skip]);
                    } else {
                        data.push(vec![Instruction::Invert]);
                        Compiler::relocate(
                            self.compile_blocks(block.item.as_ref()),
                            data.len(),
                            &mut data,
                        );
                        data.push(vec![Instruction::Invert]);
                    }
                }
                _ => {}
            },
            ReexItem::Literal(item) => {
                let iter = self.transform(item);
                let mut items = vec![];

                if self.reverse {
                    for item in iter.rev() {
                        items.push(Instruction::Item(item));
                    }
                } else {
                    for item in iter {
                        items.push(Instruction::Item(item));
                    }
                }

                data.push(items);
            }
        }

        if let Some(quantifier) = &node.quantifier {
            let mut looped_data = vec![];
            let mut data_start = 0;
            if let Some(glue) = quantifier.glue.as_ref() {
                // trailing && reverse
                //   split GLUE, DATA
                //  DATA:
                //   <data>
                //   split GLUE, END
                //  GLUE:
                //   <glue>
                //   jmp DATA
                //  END:
                // trailing
                //  DATA:
                //   <data>
                //   split GLUE, END
                //  GLUE:
                //   <glue>
                //   split DATA, END
                //  END:
                // <normal> && reverse
                //  DATA:
                //   <data>
                //   split GLUE, END
                //  GLUE:
                //   <glue>
                //   jmp DATA
                //  END:
                if quantifier.min == 0 {
                    data_start += 1;
                    looped_data.push(vec![Instruction::Noop]);
                };

                let glue = self.compile_blocks(glue);

                if quantifier.trailing_glue && self.reverse {
                    data_start += 1;
                    let data_end = data.len() + data_start + 1;
                    looped_data.push(vec![Instruction::Split(data_end, data_start)])
                }

                Compiler::relocate(data, data_start, &mut looped_data);
                let end = looped_data.len() + 2 + glue.len();
                looped_data.push(vec![Instruction::Split(looped_data.len() + 1, end)]);

                Compiler::relocate(glue, looped_data.len(), &mut looped_data);
                if quantifier.trailing_glue && !self.reverse {
                    looped_data.push(vec![Instruction::Split(data_start, end)])
                } else {
                    looped_data.push(vec![Instruction::Jump(data_start)])
                }

                if quantifier.min == 0 {
                    looped_data[0] = vec![Instruction::Split(1, end)]
                }
            } else {
                if quantifier.min == 0 {
                    looped_data.push(vec![Instruction::Split(1, data.len() + 2)]);
                    data_start = 1;
                };

                Compiler::relocate(data, looped_data.len(), &mut looped_data);
            }

            if (quantifier.min == 0 || quantifier.min == 1) && quantifier.max == usize::MAX {
                looped_data.push(vec![Instruction::Split(data_start, looped_data.len() + 1)])
            } else {
                let loop_flag = self.get_flag();
                looped_data.push(vec![
                    Instruction::Inc(loop_flag),
                    Instruction::Split(data_start, looped_data.len() + 1),
                ]);
                looped_data.push(vec![Instruction::Between(
                    loop_flag,
                    quantifier.min,
                    quantifier.max,
                )]);
            }

            return looped_data;
        }

        data
    }

    fn get_flag(&mut self) -> usize {
        let flag = self.flag;
        self.flag += 1;
        flag
    }

    fn relocate(node: Vec<Vec<Instruction<T>>>, nr: usize, into: &mut Vec<Vec<Instruction<T>>>) {
        for block in node {
            into.push(
                block
                    .into_iter()
                    .map(|instruction| match instruction {
                        Instruction::Split(addr_a, addr_b) => {
                            Instruction::Split(addr_a + nr, addr_b + nr)
                        }
                        Instruction::Jump(addr) => Instruction::Jump(addr + nr),
                        x => x,
                    })
                    .collect(),
            );
        }
    }
}
