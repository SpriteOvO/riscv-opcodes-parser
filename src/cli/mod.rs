use std::{collections::BTreeMap, env, path::PathBuf};

use anyhow::Result;
use clap::Parser as CliParser;
use spdlog::prelude::*;

use crate::{parse as opcodes_parse, Fetcher, Reader};

#[derive(Debug)]
pub struct Cli {
    pub fetch: FetchMethod,
    pub filter: FilterMethod,
}

#[derive(Debug)]
pub enum FetchMethod {
    Online,
    Local(Option<PathBuf>),
}

#[derive(Debug)]
pub enum FilterMethod {
    Glob(String),
}

// workaround: https://github.com/clap-rs/clap/issues/2621
mod raw {
    use super::*;

    #[derive(CliParser, Debug)]
    pub struct Cli {
        #[clap(flatten)]
        fetch: FetchMethod,

        #[clap(flatten)]
        filter: FilterMethod,
    }

    impl From<Cli> for super::Cli {
        fn from(internal: Cli) -> Self {
            Self {
                fetch: internal.fetch.into(),
                filter: internal.filter.into(),
            }
        }
    }

    #[derive(CliParser, Debug)]
    #[clap(arg_required_else_help(true))]
    pub struct FetchMethod {
        /// Fetch from the latest commit in the remote official repository
        #[clap(long)]
        pub online: bool,

        /// Use local repository. Default current working directory
        #[clap(long, value_name = "PATH", conflicts_with = "online")]
        pub local: Option<Option<PathBuf>>,
    }

    impl From<FetchMethod> for super::FetchMethod {
        fn from(internal: FetchMethod) -> Self {
            assert!(
                internal.online && internal.local.is_none()
                    || !internal.online && internal.local.is_some()
            );

            if internal.online {
                Self::Online
            } else {
                Self::Local(internal.local.unwrap())
            }
        }
    }

    #[derive(CliParser, Debug)]
    #[clap(arg_required_else_help(true))]
    pub struct FilterMethod {
        /// Filter files by glob. Examples: "rv*", "rv64_*", "unratified/*",
        /// "**/*"
        #[clap(long)]
        pub glob: String,
    }

    impl From<FilterMethod> for super::FilterMethod {
        fn from(internal: FilterMethod) -> Self {
            Self::Glob(internal.glob)
        }
    }
}

pub fn parse() -> Cli {
    raw::Cli::parse().into()
}

pub fn exec(cli: Cli) -> Result<()> {
    let dir = match cli.fetch {
        FetchMethod::Online => {
            let fetcher = Fetcher::new()?;
            (fetcher.opcodes_dir().to_path_buf(), Some(fetcher))
        }
        FetchMethod::Local(dir) => {
            let dir = match dir {
                Some(dir) => dir,
                None => env::current_dir()?,
            };
            (dir, None)
        }
    };

    let reader = Reader::new(dir.0)?;

    let opcodes_files = match cli.filter {
        FilterMethod::Glob(glob) => reader.read_by_glob(glob)?,
    };

    let _opcodes_set = opcodes_files
        .into_iter()
        .inspect(|(file_name, _)| info!("Parsing opcodes file: '{file_name}'"))
        .map(|file| opcodes_parse(file.1).map(|r| (file.0, r)))
        .collect::<Result<BTreeMap<_, _>>>()?;

    todo!()
}
