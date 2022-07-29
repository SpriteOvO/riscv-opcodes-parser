use std::path::{Path, PathBuf};

use anyhow::Result as AnyhowResult;
use git2 as git;
use spdlog::prelude::*;
use tempfile::TempDir;

use crate::new_temp_dir;

#[allow(dead_code)]
pub struct Fetcher {
    temp_dir: TempDir,
    repo_dir: PathBuf,
    repo: git::Repository,
}

impl Fetcher {
    const REPO_URL: &'static str = "https://github.com/riscv/riscv-opcodes.git";

    pub fn new() -> AnyhowResult<Self> {
        let temp_dir = new_temp_dir()?;
        let mut repo_dir = temp_dir.path().to_path_buf();
        repo_dir.push("repo");
        trace!(
            "riscv-opcodes repository directory: '{}'",
            repo_dir.display()
        );

        let repo = git::Repository::clone(Self::REPO_URL, &repo_dir)?;

        Ok(Self {
            temp_dir,
            repo_dir,
            repo,
        })
    }

    pub fn opcodes_dir(&self) -> &Path {
        &self.repo_dir
    }
}
