use anyhow::Result;
use rvop::cli;
use spdlog::prelude::*;

fn main() {
    setup_logger();

    if let Err(err) = run() {
        error!("{err}");
    }
}

fn setup_logger() {
    if cfg!(debug_assertions) {
        spdlog::default_logger().set_level_filter(LevelFilter::All)
    }
}

fn run() -> Result<()> {
    let cli = cli::parse();
    trace!("cli: {cli:?}");

    cli::exec(cli)
}
