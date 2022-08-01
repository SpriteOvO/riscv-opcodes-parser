use std::path::{Path, PathBuf};

use anyhow::Result as AnyhowResult;
use git::Repository;
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
        let repo_dir = temp_dir.path().join("repo");
        trace!(
            "riscv-opcodes repository directory: '{}'",
            repo_dir.display()
        );

        let repo = Self::fetch(Self::REPO_URL, &repo_dir)?;

        Ok(Self {
            temp_dir,
            repo_dir,
            repo,
        })
    }

    fn fetch(url: &str, dir: impl AsRef<Path>) -> Result<Repository, git::Error> {
        info!("Fetching from remote repository '{url}'");

        let repo = git::Repository::clone(url, dir.as_ref())?;

        let commit_id = repo.head()?.peel_to_commit()?.id();
        info!("Fetch finished. Commit ID: '{commit_id}'");

        Ok(repo)
    }

    pub fn opcodes_dir(&self) -> &Path {
        &self.repo_dir
    }
}
