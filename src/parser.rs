use std::{fmt::Debug, ops::ControlFlow, str::FromStr};

use anyhow::{anyhow, Result as AnyhowResult};
use chumsky::{prelude::*, stream::Stream};
use num_traits::Num;

use crate::opcodes::*;

type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Keyword(KeywordToken),
    Number(NumberToken),
    Range(NumberToken, NumberToken),
    Operator(Operator),
    Comment(String),
    Ident(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum KeywordToken {
    Import,
    PseudoOp,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum NumberToken {
    Dec(String),
    Hex(String), // prefixed with '0x' (body only)
}

impl NumberToken {
    fn parse<F: Num + FromStr>(&self) -> F
    where
        <F as FromStr>::Err: Debug,
        <F as Num>::FromStrRadixErr: Debug,
    {
        match self {
            Self::Hex(text) => F::from_str_radix(text, 16).unwrap_or_else(|err| {
                panic!("failed to parse hexadecimal number '{self:?}': {err:?}")
            }),
            Self::Dec(text) => text
                .parse()
                .unwrap_or_else(|err| panic!("failed to parse decimal number '{self:?}': {err:?}")),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Operator {
    Assign,      // =
    DoubleColon, // ::
}

#[derive(Clone, Debug)]
enum Error {
    Lexer {
        inner: Vec<Simple<char>>,
        line: usize,
    },
    Parser {
        inner: Vec<Simple<Token>>,
        line: usize,
    },
}

#[cfg(test)]
impl Error {
    #[allow(dead_code)]
    fn lexer_err(self) -> (Vec<Simple<char>>, usize) {
        match self {
            Self::Lexer { inner: err, line } => (err, line),
            Self::Parser { line, .. } => panic!("expected a lexer error in line '{line}'"),
        }
    }

    #[allow(dead_code)]
    fn parser_err(self) -> (Vec<Simple<Token>>, usize) {
        match self {
            Self::Lexer { line, .. } => panic!("expected a parser error in line '{line}'"),
            Self::Parser { inner: err, line } => (err, line),
        }
    }
}

enum Either<A, B> {
    A(A),
    B(B),
}

fn line_lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let zeroes = just('0').repeated().at_least(1).collect();

    let number = just("0x")
        .ignore_then(zeroes.or_not().ignore_then(text::int(16)).or(zeroes))
        .map(NumberToken::Hex)
        .or(zeroes
            .or_not()
            .ignore_then(text::int(10))
            .or(zeroes)
            .map(NumberToken::Dec))
        .labelled("number");

    let range = number
        .then_ignore(just(".."))
        .then(number)
        .map(|(left, right)| Token::Range(left, right))
        .labelled("range");

    let number = number.map(Token::Number);

    let keyword = {
        let prefix = just('$').labelled("prefix");

        let import = prefix
            .ignore_then(just("import"))
            .to(KeywordToken::Import)
            .labelled("import");

        let pseudo_op = prefix
            .ignore_then(just("pseudo_op"))
            .to(KeywordToken::PseudoOp)
            .labelled("pseudo_op");

        import.or(pseudo_op)
    }
    .map(Token::Keyword)
    .labelled("keyword");

    let comment = just('#')
        .ignore_then(any().repeated())
        .map(|c| c.into_iter().collect())
        .map(Token::Comment)
        .labelled("comment");

    let operator = one_of(".:+-*/!=")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(|op| match op.as_str() {
            "=" => Token::Operator(Operator::Assign),
            "::" => Token::Operator(Operator::DoubleColon),
            op => {
                // FIXME:
                // Currently I haven't found a good way to make the lexer return an `Err`, if an
                // `Err` is returned in `try_map` it will be ignored because of the later
                // `repeated`.
                panic!("unexpected operator '{op}'")
            }
        });

    let ctrl = one_of("()[]{};,").map(|c| {
        // FIXME: Same as above
        panic!("unexpected ctrl '{c}'")
    });

    let ident = text::ident()
        .separated_by(just('.'))
        .at_least(1)
        .map(|parts| parts.join("."))
        .or(text::ident())
        .map(Token::Ident)
        .labelled("identifier");

    keyword
        .or(range)
        .or(number)
        .or(ident)
        .or(operator)
        .or(ctrl)
        .or(comment)
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
        .at_least(1)
}

macro_rules! token_number {
    ($t:ty) => {
        select! {
            Token::Number(number) => number.parse::<$t>()
        }
    };
}

fn line_parser() -> impl Parser<Token, Option<Item>, Error = Simple<Token>> {
    let ident = select! {
        Token::Ident(ident) => ident,
    };

    let comment = select! {
        Token::Comment(comment) => Item::Comment(Comment{ text: comment })
    };

    let instr_name = ident.labelled("instruction name");

    let instr_path = ident
        .then_ignore(just(Token::Operator(Operator::DoubleColon)))
        .then(instr_name)
        .map(|(extension, name)| InstrPath { extension, name })
        .labelled("instruction path");

    let range = select! {
        Token::Range(left, right) => {
            let (left, right) = (left.parse::<usize>(), right.parse::<usize>());
            left..=right
        }
    }
    .labelled("range");

    let instr_args_and_bits_parts = {
        let instr_arg = ident
            .map(InstrArg::from_str)
            .unwrapped()
            .labelled("instruction argument");

        let instr_bits = range
            .then_ignore(just(Token::Operator(Operator::Assign)))
            .then(token_number!(u64))
            .map(|(range, value)| InstrBitsPart::Range(range, value))
            .or(token_number!(usize)
                .then_ignore(just(Token::Operator(Operator::Assign)))
                .then(token_number!(u8))
                .map(|(pos, value)| InstrBitsPart::Bit(pos, value)))
            .labelled("instruction bits");

        instr_arg
            .map(Either::A)
            .or(instr_bits.map(Either::B))
            .repeated()
            .try_map(|args_and_parts, span| {
                let res = args_and_parts.into_iter().fold(
                    (vec![], vec![]),
                    |(mut args, mut parts), curr| {
                        match curr {
                            Either::A(arg) => args.push(arg),
                            Either::B(part) => parts.push(part),
                        };
                        (args, parts)
                    },
                );
                if res.1.is_empty() {
                    Err(Simple::custom(span, "expected at least 1 bits part"))
                } else {
                    Ok(res)
                }
            })
    };

    let keyword = {
        let import = just(Token::Keyword(KeywordToken::Import))
            .ignore_then(instr_path.clone())
            .map(Instruction::Import)
            .labelled("import");

        let pseudo_op = just(Token::Keyword(KeywordToken::PseudoOp))
            .ignore_then(instr_path)
            .then(instr_name)
            .then(instr_args_and_bits_parts.clone())
            .map(|((base, name), (args, parts))| {
                Instruction::Pseudo(PseudoInstr {
                    base,
                    name,
                    args,
                    bits: InstrBits { parts },
                })
            })
            .labelled("pseudo_op");

        import
            .or(pseudo_op)
            .map(Item::Instruction)
            .labelled("keyword")
    };

    let instruction = instr_name
        .then(instr_args_and_bits_parts)
        .map(|(name, (args, parts))| {
            Item::Instruction(Instruction::Regular(RegularInstr {
                name,
                args,
                bits: InstrBits { parts },
            }))
        })
        .labelled("instruction");

    keyword
        .or(instruction)
        .or(comment)
        .or_not()
        .then_ignore(end())
}

fn parse_inner(input: impl AsRef<str>) -> Result<Vec<Item>, Error> {
    let res = input
        .as_ref()
        .lines()
        .enumerate()
        .try_fold(vec![], |mut prev, line| {
            let (line, input) = (line.0, line.1.trim());

            if input.is_empty() {
                return ControlFlow::Continue(prev);
            }

            let res = line_lexer()
                .parse(input)
                .map_err(|err| Error::Lexer { inner: err, line })
                .and_then(|tokens| {
                    let len = input.chars().count();
                    line_parser()
                        .parse(Stream::from_iter(len..len + 1, tokens.into_iter()))
                        .map_err(|err| Error::Parser { inner: err, line })
                });

            match res {
                Ok(res) => {
                    if let Some(item) = res {
                        prev.push(item);
                    }
                    ControlFlow::Continue(prev)
                }
                Err(err) => ControlFlow::Break(err),
            }
        });
    match res {
        ControlFlow::Break(err) => Err(err),
        ControlFlow::Continue(res) => Ok(res),
    }
}

pub fn parse(input: impl AsRef<str>) -> AnyhowResult<Vec<Item>> {
    parse_inner(input).map_err(|err| match err {
        Error::Lexer { inner: err, line } => {
            anyhow!("failed to parse by lexer in line '{line}': {err:?}")
        }
        Error::Parser { inner: err, line } => {
            anyhow!("failed to parse by parser in line '{line}': {err:?}")
        }
    })
}

#[cfg(test)]
mod tests {
    use chumsky::error::SimpleReason;

    use super::*;

    #[test]
    fn full_valid() {
        // https://github.com/rust-lang/rustfmt/issues/3135
        #[rustfmt::skip]
        let rustfmt_long_str_bug_workaround =
            "
            # This is a comment
            #

            add     rd rs1 rs2 31..25=0  14..12=0 6..2=0x0C 1..0=3
            $import rv32_zkne::aes32esi
            #This is another comment
            $pseudo_op rv_i::fence fence.tso 31..28=8 27..24=3 23..20=3 rs1 14..12=0 rd 6..2=0x03 1..0=3
            sub     rd rs1 rs2 31..25=32 14..12=0 6..2=0x0C 1..0=3
            $import rv64_zkne::aes64es
            $pseudo_op rv_i::fence pause     31..28=0 27..24=1 23..20=0 19..15=0      14..12=0 11..7=0      6..2=0x03 1..0=3
            ";
        assert_eq!(
            parse(rustfmt_long_str_bug_workaround).unwrap(),
            vec![
                Item::Comment(Comment {
                    text: " This is a comment".to_string()
                }),
                Item::Comment(Comment {
                    text: String::new()
                }),
                Item::Instruction(Instruction::Regular(RegularInstr {
                    name: "add".to_string(),
                    args: vec![InstrArg::Dest, InstrArg::Src(1), InstrArg::Src(2)],
                    bits: InstrBits {
                        parts: vec![
                            InstrBitsPart::Range(31..=25, 0),
                            InstrBitsPart::Range(14..=12, 0),
                            InstrBitsPart::Range(6..=2, 0xC),
                            InstrBitsPart::Range(1..=0, 3),
                        ]
                    }
                })),
                Item::Instruction(Instruction::Import(InstrPath {
                    extension: "rv32_zkne".to_owned(),
                    name: "aes32esi".to_owned()
                })),
                Item::Comment(Comment {
                    text: "This is another comment".to_string()
                }),
                Item::Instruction(Instruction::Pseudo(PseudoInstr {
                    base: InstrPath {
                        extension: "rv_i".to_owned(),
                        name: "fence".to_owned()
                    },
                    name: "fence.tso".to_owned(),
                    args: vec![InstrArg::Src(1), InstrArg::Dest],
                    bits: InstrBits {
                        parts: vec![
                            InstrBitsPart::Range(31..=28, 8),
                            InstrBitsPart::Range(27..=24, 3),
                            InstrBitsPart::Range(23..=20, 3),
                            InstrBitsPart::Range(14..=12, 0),
                            InstrBitsPart::Range(6..=2, 0x3),
                            InstrBitsPart::Range(1..=0, 3),
                        ]
                    }
                })),
                Item::Instruction(Instruction::Regular(RegularInstr {
                    name: "sub".to_string(),
                    args: vec![InstrArg::Dest, InstrArg::Src(1), InstrArg::Src(2)],
                    bits: InstrBits {
                        parts: vec![
                            InstrBitsPart::Range(31..=25, 32),
                            InstrBitsPart::Range(14..=12, 0),
                            InstrBitsPart::Range(6..=2, 0xC),
                            InstrBitsPart::Range(1..=0, 3),
                        ]
                    }
                })),
                Item::Instruction(Instruction::Import(InstrPath {
                    extension: "rv64_zkne".to_owned(),
                    name: "aes64es".to_owned()
                })),
                Item::Instruction(Instruction::Pseudo(PseudoInstr {
                    base: InstrPath {
                        extension: "rv_i".to_owned(),
                        name: "fence".to_owned()
                    },
                    name: "pause".to_owned(),
                    args: vec![],
                    bits: InstrBits {
                        parts: vec![
                            InstrBitsPart::Range(31..=28, 0),
                            InstrBitsPart::Range(27..=24, 1),
                            InstrBitsPart::Range(23..=20, 0),
                            InstrBitsPart::Range(19..=15, 0),
                            InstrBitsPart::Range(14..=12, 0),
                            InstrBitsPart::Range(11..=7, 0),
                            InstrBitsPart::Range(6..=2, 0x3),
                            InstrBitsPart::Range(1..=0, 3),
                        ]
                    }
                })),
            ]
        );
    }

    #[test]
    fn keyword() {
        let err = parse_inner(
            "
            $import a::b
            $this_is_not_a_valid_keyword
            $pseudo_op c::d e rd rs1 1..0=2
            ",
        )
        .unwrap_err()
        .lexer_err();

        assert!(err.0.iter().any(|err| {
            matches!(err.reason(), SimpleReason::Unexpected)
                && *err.found().unwrap() == 't'
                && err.label() == Some("import")
        }));

        assert!(parse_inner(
            "
            $import a::b
            $pseudo_op c::d e rd rs1 1..0=2
            ",
        )
        .is_ok())
    }

    #[test]
    fn import_unexpected_arg() {
        let err = parse_inner("$import a::b c").unwrap_err().parser_err();

        assert!(err.0.iter().any(|err| {
            matches!(err.reason(), SimpleReason::Unexpected)
                && err.expected().collect::<Vec<_>>() == [&None]
                && err.found().unwrap() == &Token::Ident("c".to_string())
        }));

        assert!(parse_inner("$import a::b").is_ok());
    }

    #[test]
    #[should_panic]
    fn invalid_instr_name() {
        let _ = parse_inner("$pseudo_op a::b c . d 1..0=0");
    }

    #[test]
    fn instr_path_missing() {
        {
            let err = parse_inner("$import a::").unwrap_err().parser_err();

            assert!(err.0.iter().any(|err| {
                matches!(err.reason(), SimpleReason::Unexpected)
                    && err.found().is_none()
                    && err.label() == Some("instruction name")
            }));
        }
        {
            let err = parse_inner("$import a").unwrap_err().parser_err();
            assert!(err.0.iter().any(|err| {
                matches!(err.reason(), SimpleReason::Unexpected)
                    && err.expected().filter_map(Option::clone).collect::<Vec<_>>()
                        == vec![Token::Operator(Operator::DoubleColon)]
                    && err.found().is_none()
                    && err.label() == Some("instruction path")
            }));
        }
        {
            let err = parse_inner("$import").unwrap_err().parser_err();
            assert!(err.0.iter().any(|err| {
                matches!(err.reason(), SimpleReason::Unexpected)
                    && err.found().is_none()
                    && err.label() == Some("instruction path")
            }));
        }
    }

    #[test]
    fn instr_missing_name() {
        let err = parse_inner("$pseudo_op a::b 1..0=2")
            .unwrap_err()
            .parser_err();

        assert!(err.0.iter().any(|err| {
            matches!(err.reason(), SimpleReason::Unexpected)
                && matches!(err.found(), Some(Token::Range(_, _)))
                && err.label() == Some("instruction name")
        }));
    }

    #[test]
    fn instr_missing_bits() {
        let err = parse_inner("$pseudo_op a::b c").unwrap_err().parser_err();

        assert!(err.0.iter().any(|err| {
            matches!(err.reason(), SimpleReason::Custom(_))
                && err.found().is_none()
                && err.label() == Some("pseudo_op")
        }));

        assert!(parse_inner("$pseudo_op a::b c 1..0=2").is_ok());
        assert!(parse_inner("$pseudo_op a::b c rd rs1 1..0=2").is_ok())
    }
}
