pub mod custom;

use crate::custom::{BlockCompiler, FlagCompiler};
use crate::{ReexBlock, ReexFlag, ReexItem, ReexNode};
use reex_vm::vm::{Instruction, Program};
use std::collections::BTreeMap;
use std::fmt::Debug;

pub struct Compiler<T, F: Fn(&str) -> Vec<T>> {
    flag: usize,
    reverse: bool,
    transform: F,
    blocks: BTreeMap<&'static str, fn(&ReexBlock, &mut Self) -> Vec<Vec<Instruction<T>>>>,
    flags: BTreeMap<&'static str, fn(&ReexFlag, &mut Self) -> Vec<Vec<Instruction<T>>>>,
    counters: BTreeMap<String, usize>,
}

pub fn compile(node: &ReexNode) -> Program<char> {
    compile_typed(node, |x| x.chars().collect::<Vec<_>>())
}

pub fn compile_typed<T: Debug, F: Fn(&str) -> Vec<T>>(node: &ReexNode, transform: F) -> Program<T> {
    Compiler::compile_typed(node, transform)
}

impl<T: Debug, F: Fn(&str) -> Vec<T>> Compiler<T, F> {
    pub fn add_block_compiler<B: BlockCompiler<T>>(&mut self) {
        self.blocks.insert(B::NAME, B::compile);
        for alias in B::ALIASES {
            self.blocks.insert(alias, B::compile);
        }
    }

    pub fn add_flag_compiler<B: FlagCompiler<T>>(&mut self) {
        self.flags.insert(B::NAME, B::compile);
        for alias in B::ALIASES {
            self.flags.insert(alias, B::compile);
        }
    }

    pub fn new(transform: F) -> Compiler<T, F> {
        Compiler {
            flag: 0,
            reverse: false,
            transform,
            blocks: Default::default(),
            flags: Default::default(),
            counters: Default::default(),
        }
    }

    pub fn compile_typed(node: &ReexNode, transform: F) -> Program<T> {
        let mut compiler = Compiler::new(transform);
        compiler.compile(&node)
    }

    pub fn compile(&mut self, node: &ReexNode) -> Program<T> {
        let mut blocks = self.compile_blocks(&node);
        let mut start_index = vec![];
        let mut offset = 0;
        blocks.push(vec![Instruction::Match]);
        for block in &blocks {
            start_index.push(offset);
            offset += block.len();
        }

        // fix selection markers
        let mut c = 0;
        for block in &mut blocks {
            for i in 0..block.len() {
                if let Instruction::Marker("selection_start", _) = block[i] {
                    let id = c;
                    c += 1;
                    block[i] = Instruction::StartSelection(id);
                }

                if let Instruction::Marker("selection_end", _) = block[i] {
                    block[i] = Instruction::EndSelection;
                }
            }
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

    pub fn compile_blocks(&mut self, node: &ReexNode) -> Vec<Vec<Instruction<T>>> {
        let mut data = vec![];
        match &node.item {
            ReexItem::Set(item) => {
                if self.reverse {
                    for node in item.items.iter().rev() {
                        relocate(self.compile_blocks(node), data.len(), &mut data);
                    }
                } else {
                    for node in &item.items {
                        relocate(self.compile_blocks(node), data.len(), &mut data);
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
                        relocate(item, data.len(), &mut data);
                        data.push(vec![Instruction::Jump(end)]);
                    }

                    relocate(items.pop().unwrap(), data.len(), &mut data);
                }
            },
            ReexItem::Flag(flag) => {
                if let Some(flag_compiler) = self.flags.get(flag.name.as_str()).copied() {
                    data = flag_compiler(flag, self);
                }
            }
            ReexItem::Block(block) => {
                if let Some(block_compiler) = self.blocks.get(block.name.as_str()).copied() {
                    data = block_compiler(block, self);
                }
            }
            ReexItem::Literal(item) => {
                let iter = (self.transform)(item);
                let mut items: Vec<_> = iter.into_iter().map(|x| Instruction::Item(x)).collect();

                if self.reverse {
                    items.reverse();
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

                relocate(data, data_start, &mut looped_data);
                let end = looped_data.len() + 2 + glue.len();
                looped_data.push(vec![Instruction::Split(looped_data.len() + 1, end)]);

                relocate(glue, looped_data.len(), &mut looped_data);
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

                relocate(data, looped_data.len(), &mut looped_data);
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

    fn get_counter<S: ToString>(&mut self, name: S) -> usize {
        *self
            .counters
            .entry(name.to_string())
            .and_modify(|x| *x = *x + 1)
            .or_insert(0)
    }
}

pub fn relocate<T>(node: Vec<Vec<Instruction<T>>>, nr: usize, into: &mut Vec<Vec<Instruction<T>>>) {
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
