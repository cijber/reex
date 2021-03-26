use crate::{compiler::relocate, Compiler, ReexBlock, ReexFlag};
use reex_vm::matchers::{Exact, Not, Whitespace, Word};
use reex_vm::vm::Instruction;
use reex_vm::ReexString;
use std::fmt::Debug;

pub trait BlockCompiler<T> {
    const NAME: &'static str;
    const ALIASES: &'static [&'static str] = &[];

    fn compile<F: Fn(&str) -> Vec<T>>(
        block: &ReexBlock,
        compiler: &mut Compiler<T, F>,
    ) -> Vec<Vec<Instruction<T>>>;
}

pub trait FlagCompiler<T> {
    const NAME: &'static str;
    const ALIASES: &'static [&'static str] = &[];

    fn compile<F: Fn(&str) -> Vec<T>>(
        flag: &ReexFlag,
        compiler: &mut Compiler<T, F>,
    ) -> Vec<Vec<Instruction<T>>>;
}

macro_rules! make_alias {
    ([]) => { &[] };
    ([ $($alias:literal),+ ]) => { &[$($alias),*] };
}

macro_rules! define_flags {
    { $($flag:ident: $name:literal $([ $( $alias:literal ),* ])? $( for [$($target:ty),+] )? => $action:expr)* } => {
        $(
            define_flags!{ one $flag: $name [ $( $( $alias ),* )? ]  $( for [$($target),+] )? => $action }
        )+
    };

    { one $flag:ident: $name:literal $( for [$($target:ty),+] )? => $action:expr } => {
        define_flags!{ one $flag: $name [] $( for [$($target),+] )? => $action }
    };

    { one $flag:ident: $name:literal $things:tt for [$($target:ty),+] => $action:expr } => {
        struct $flag;
        $(
           impl FlagCompiler<$target> for $flag {
                const NAME: &'static str = $name;
                const ALIASES: &'static [&'static str] = make_alias!($things);

                fn compile<F: Fn(&str) -> Vec<$target>>(flag: &ReexFlag, compiler: &mut Compiler<$target, F>) -> Vec<Vec<Instruction<$target>>> {
                    $action(flag, compiler)
                }
            }
        )+
    };

    { one $flag:ident: $name:literal $things:tt => $action:expr } => {
        struct $flag;

        impl<T: Debug> FlagCompiler<T> for $flag {
            const NAME: &'static str = $name;
            const ALIASES: &'static [&'static str] = make_alias!($things);

            fn compile<F: Fn(&str) -> Vec<T>>(flag: &ReexFlag, compiler: &mut Compiler<T, F>) -> Vec<Vec<Instruction<T>>> {
                $action(flag, compiler)
            }
        }
    };
}

macro_rules! define_blocks {
    { $($flag:ident $( where $( $lifetime:lifetime )? $(+)? $( $trait:ident )? )?: $name:literal $([ $( $alias:literal ),* ])? $( for [$($target:ty),+] )? => $action:expr)* } => {
        $(
            define_blocks!{ one $flag $( where $( $lifetime )? $( $trait )? )?: $name [ $( $( $alias ),* )? ]  $( for [$($target),+] )? => $action }
        )+
    };

    { one $flag:ident $( where $( $lifetime:lifetime )? $(+)? $trait:ident )?: $name:literal $( for [$($target:ty),+] )? => $action:expr } => {
        define_blocks!{ one $flag $( where $( $lifetime )? $( $trait )? )?: $name [] $( for [$($target),+] )? => $action  }
    };

    { one $flag:ident $( where $( $lifetime:lifetime )? $(+)? $trait:ident )?: $name:literal $things:tt for [$($target:ty),+] => $action:expr } => {
        struct $flag;
        $(
            impl BlockCompiler<$target> for $flag {
                const NAME: &'static str = $name;
                const ALIASES: &'static [&'static str] = make_alias!($things);

                fn compile<F: Fn(&str) -> Vec<T>>(block: &ReexBlock, compiler: &mut Compiler<$target, F>) -> Vec<Vec<Instruction<$target>>> {
                    $action(block, compiler)
                }
            }
        )+
    };

    { one $flag:ident $( where $( $lifetime:lifetime )? $(+)? $ ( $trait:ident )? )?: $name:literal $things:tt => $action:expr } => {
        struct $flag;

        impl<T: $( $( $lifetime + )? )? Debug $( + $( $trait )? )? > BlockCompiler<T> for $flag {
            const NAME: &'static str = $name;
            const ALIASES: &'static [&'static str] = make_alias!($things);

            fn compile<F: Fn(&str) -> Vec<T>>(block: &ReexBlock, compiler: &mut Compiler<T, F>) -> Vec<Vec<Instruction<T>>> {
                $action(block, compiler)
            }
        }
    };
}

define_flags! {
    WhitespaceFlag: "whitespace" ["s", "ws"] for [u8, char, String, ReexString] => |_,_| {
        vec![vec![Instruction::ItemClass(Box::new(Whitespace))]]
    }

    BoundaryFlag: "boundary" ["b"] for [u8, char, ReexString] => |_,c: &mut Compiler<_, _>| {
        vec![vec![
            Instruction::TestNotEqualsAndStorePosition(c.get_flag()),
            Instruction::Boundary(Box::new(Word))
        ]]
    }

    WordFlag: "word" ["w"] for [u8, char, String, ReexString] => |_,_| {
        vec![vec![Instruction::ItemClass(Box::new(Word))]]
    }

    AnyFlag: "any" => |_,_| {
        vec![vec![Instruction::Any]]
    }

    EndFlag: "end" => |_,c: &mut Compiler<_, _>|  {
        vec![vec![
            Instruction::TestNotEqualsAndStorePosition(c.get_flag()),
            Instruction::End
        ]]
    }

    StartFlag: "start" => |_,c: &mut Compiler<_, _>|  {
        vec![vec![
            Instruction::TestNotEqualsAndStorePosition(c.get_flag()),
            Instruction::Start
        ]]
    }
}

define_blocks! {
    SelectBlock: "select" ["sel"] => |block: &ReexBlock, compiler: &mut Compiler<_, _>| {
        let counter = compiler.get_counter("selection");
        let mut data = vec![vec![Instruction::Marker("selection_start", counter)]];
        let mut c = 0;
        let mut inner_data = compiler.compile_blocks(block.item.as_ref());
        for block in &mut inner_data {
            for instrution in block {
                if let Instruction::Marker("selection_start", _) = instrution {
                    let id = c;
                    c += 1;
                    *instrution = Instruction::StartSelection(id);
                }

                if let Instruction::Marker("selection_end", _) = instrution {
                    *instrution = Instruction::EndSelection;
                }
            }
        }

        relocate(inner_data, data.len(), &mut data);

        data.push(vec![Instruction::Marker("selection_end", counter)]);

        data
    }

    LookbehindBlock: "lookbehind" ["behind", "lb"] => |block: &ReexBlock, compiler: &mut Compiler<_, _>| {
        let mut data = vec![];
        let flag = compiler.get_flag();
        let old_reverse = compiler.reverse;
        let mut start = vec![Instruction::TestNotEqualsAndStorePosition(compiler.get_flag()), Instruction::Checkpoint(flag)];
        if !old_reverse {
            start.push(Instruction::Reverse);
        }
        data.push(start);
        compiler.reverse = true;
        relocate(
            compiler.compile_blocks(block.item.as_ref()),
            data.len(),
            &mut data,
        );
        compiler.reverse = old_reverse;
        let mut end = vec![Instruction::Rewind(flag)];
        if !compiler.reverse {
            end.push(Instruction::Forwards);
        }
        data.push(end);
        data
    }

    LookaheadBlock: "lookahead" ["ahead", "la"] => |block: &ReexBlock, compiler: &mut Compiler<_, _>| {
        let mut data = vec![];
        let flag = compiler.get_flag();
        let old_reverse = compiler.reverse;
        let mut start = vec![Instruction::TestNotEqualsAndStorePosition(compiler.get_flag()), Instruction::Checkpoint(flag)];
        if old_reverse {
            start.push(Instruction::Forwards);
        }
        data.push(start);
        compiler.reverse = false;
        relocate(
            compiler.compile_blocks(block.item.as_ref()),
            data.len(),
            &mut data,
        );
        compiler.reverse = old_reverse;
        let mut end = vec![Instruction::Rewind(flag)];
        if compiler.reverse {
            end.push(Instruction::Reverse);
        }
        data.push(end);
        data
    }

    PrefixBlock: "prefix" => |block: &ReexBlock, compiler: &mut Compiler<_, _>| {
        let mut data = vec![];
        let flag = compiler.get_flag();
        let child = compiler.compile_blocks(block.item.as_ref());
        data.push(vec![Instruction::TrySplit(flag, child.len() + 2)]);
        relocate(child, data.len(), &mut data);
        data.push(vec![Instruction::StopSplit(flag)]);
        data
    }

    NotBlock where 'static + PartialEq: "not" => |block: &ReexBlock, compiler: &mut Compiler<_, _>| {
        let mut data = vec![];
        let mut blocks = compiler.compile_blocks(block.item.as_ref());
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
            relocate(
                compiler.compile_blocks(block.item.as_ref()),
                data.len(),
                &mut data,
            );
            data.push(vec![Instruction::Invert]);
        }

        data
    }
}

pub fn populate_compilers<T: Debug + PartialEq + 'static, F: Fn(&str) -> Vec<T>>(
    compiler: &mut Compiler<T, F>,
) {
    compiler.add_block_compiler::<SelectBlock>();
    compiler.add_block_compiler::<LookaheadBlock>();
    compiler.add_block_compiler::<LookbehindBlock>();
    compiler.add_block_compiler::<PrefixBlock>();
    compiler.add_flag_compiler::<AnyFlag>();
    compiler.add_flag_compiler::<StartFlag>();
    compiler.add_flag_compiler::<EndFlag>();
    compiler.add_block_compiler::<NotBlock>();
}

pub fn populate_string_compilers<F: Fn(&str) -> Vec<String>>(compiler: &mut Compiler<String, F>) {
    compiler.add_flag_compiler::<WordFlag>();
    compiler.add_flag_compiler::<WhitespaceFlag>();
}

pub fn populate_u8_compilers<F: Fn(&str) -> Vec<u8>>(compiler: &mut Compiler<u8, F>) {
    compiler.add_flag_compiler::<WordFlag>();
    compiler.add_flag_compiler::<BoundaryFlag>();
    compiler.add_flag_compiler::<WhitespaceFlag>();
}

pub fn populate_reex_string_compilers<F: Fn(&str) -> Vec<ReexString>>(
    compiler: &mut Compiler<ReexString, F>,
) {
    compiler.add_flag_compiler::<WordFlag>();
    compiler.add_flag_compiler::<BoundaryFlag>();
    compiler.add_flag_compiler::<WhitespaceFlag>();
}

pub fn populate_char_compilers<F: Fn(&str) -> Vec<char>>(compiler: &mut Compiler<char, F>) {
    compiler.add_flag_compiler::<WordFlag>();
    compiler.add_flag_compiler::<BoundaryFlag>();
    compiler.add_flag_compiler::<WhitespaceFlag>();
}
