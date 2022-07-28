use std::ops::RangeInclusive;

use anyhow::{bail, Result};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Item {
    Comment(Comment),
    Instruction(Instruction),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Comment {
    pub text: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Instruction {
    Regular(RegularInstr),
    Pseudo(PseudoInstr),
    Import(InstrPath),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RegularInstr {
    pub name: String,
    pub args: Vec<InstrArg>,
    pub bits: InstrBits,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PseudoInstr {
    pub base: InstrPath,
    pub name: String,
    pub args: Vec<InstrArg>,
    pub bits: InstrBits,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InstrArg {
    Dest,       // rd
    Src(usize), // rs*
}

impl InstrArg {
    pub(crate) fn from_str(name: impl AsRef<str>) -> Result<Self> {
        let name = name.as_ref();

        if name.starts_with("rs")
            && name.len() > 2
            && name.chars().skip(2).all(|c| c.is_ascii_digit())
        {
            let begin = name.char_indices().nth(2).unwrap().0;
            Ok(Self::Src(name[begin..].parse()?))
        } else {
            let res = match name {
                "rd" => Self::Dest,
                _ => bail!("unknown instruction argument '{name}'"),
            };
            Ok(res)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InstrBits {
    pub parts: Vec<InstrBitsPart>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum InstrBitsPart {
    Bit(usize, u8),
    Range(RangeInclusive<usize>, u64),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InstrPath {
    pub extension: String,
    pub name: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instr_arg_from_str() {
        assert_eq!(InstrArg::from_str("rs1").unwrap(), InstrArg::Src(1));
        assert_eq!(
            InstrArg::from_str("rs114514").unwrap(),
            InstrArg::Src(114514)
        );
        assert!(InstrArg::from_str("rs").is_err());
        assert!(InstrArg::from_str("rsa").is_err());
    }
}
