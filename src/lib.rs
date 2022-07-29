pub mod opcodes;

mod fetcher;
mod parser;
mod reader;

use std::io;

use const_format::formatcp;
pub use fetcher::Fetcher;
pub use parser::parse;
pub use reader::*;
use tempfile::{Builder, TempDir};

fn new_temp_dir() -> io::Result<TempDir> {
    Builder::new()
        .prefix(formatcp!(".{}.", env!("CARGO_PKG_NAME")))
        .tempdir()
}
