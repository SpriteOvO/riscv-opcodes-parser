use std::{collections::HashMap, fs, path::PathBuf};

use anyhow::Result as AnyhowResult;
use globset::GlobBuilder;
use walkdir::{DirEntry as WalkDirEntry, Error as WalkDirErr, WalkDir};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum OpcodesFileName {
    Ratified(String),
    Unratified(String),
}

impl OpcodesFileName {
    pub fn name(&self) -> &str {
        match self {
            Self::Ratified(name) => name,
            Self::Unratified(name) => name,
        }
        .as_str()
    }
}

pub struct Reader {
    root: PathBuf,
}

impl Reader {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    pub fn read_by_glob(
        &self,
        glob: impl AsRef<str>,
    ) -> AnyhowResult<HashMap<OpcodesFileName, String>> {
        let glob = GlobBuilder::new(glob.as_ref())
            .literal_separator(true)
            .build()?
            .compile_matcher();

        let matches = self
            .opcodes_files()?
            .into_iter()
            .filter(|f| glob.is_match(f.strip_prefix(&self.root).unwrap()))
            .map(|f| fs::read_to_string(&f).map(|c| (f, c)))
            .collect::<Result<HashMap<_, _>, _>>()?
            .into_iter()
            .map(|(file, contents)| {
                let file = file.strip_prefix(&self.root).unwrap();
                let name_kind = if let Some(parent_dir) = file
                    .parent()
                    .and_then(|p| p.file_name().and_then(|s| s.to_str()))
                {
                    if parent_dir != "unratified" {
                        panic!(
                            "opcodes file under an unknown directory: '{}'",
                            file.display()
                        );
                    }
                    OpcodesFileName::Unratified
                } else {
                    OpcodesFileName::Ratified
                };

                let name = name_kind(
                    file.file_name()
                        .and_then(|s| s.to_str())
                        .unwrap()
                        .to_string(),
                );

                (name, contents)
            })
            .collect();

        Ok(matches)
    }

    pub(crate) fn opcodes_files(&self) -> Result<Vec<PathBuf>, WalkDirErr> {
        fn is_hidden(entry: &WalkDirEntry) -> bool {
            entry
                .file_name()
                .to_str()
                .map(|s| s.starts_with('.'))
                .unwrap_or(false)
        }

        fn is_opcodes_file(entry: &WalkDirEntry) -> bool {
            entry.file_type().is_file()
                && entry
                    .file_name()
                    .to_str()
                    .map(|s| s.starts_with("rv"))
                    .unwrap_or(false)
        }

        let files = WalkDir::new(&self.root)
            .into_iter()
            .filter_entry(|e| !is_hidden(e))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter(is_opcodes_file)
            .map(|e| e.into_path())
            .collect();

        Ok(files)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Fetcher;

    #[test]
    fn expected_files() {
        let fetcher = Fetcher::new().unwrap();
        let reader = Reader::new(&fetcher.opcodes_dir());
        let opcodes_files = reader.opcodes_files().unwrap();

        macro_rules! assert_file {
            (file: $file:literal,contains: $contains:literal) => {{
                println!(" > testing: file = '{}'", $file);
                assert!(fs::read_to_string(fetcher.opcodes_dir().join($file)).unwrap().contains($contains));
                assert_eq!(
                    opcodes_files
                        .iter()
                        .filter(|f| f.file_name().unwrap() == $file)
                        .count(),
                    1
                );
            }};
            (files_iter_excludes: $file:literal) => {
                println!(" > testing: files_iter_excludes = '{}'", $file);
                assert!(!opcodes_files.iter().any(|p| p.join($file).exists()));
            };
            (
                glob: $glob:literal,
                contains: [$($contains:expr),+],
                excludes: [$($excludes:expr),+]
            ) => {
                println!(" > testing: glob = '{}'", $glob);

                let contains = [$($contains),+];
                let excludes = [$($excludes),+];

                let files = reader.read_by_glob($glob).unwrap();

                assert!(files.iter().any(|file| {
                    let file_name = file.0;
                    contains.contains(file_name) && !excludes.contains(file_name)
                }));
            };
        }

        // Doesn't contain directories
        assert_file!(files_iter_excludes: ".git");
        assert_file!(files_iter_excludes: "unratified");

        assert_file!(file: "rv_i", contains: "addi");
        assert_file!(file: "rv_c", contains: "c.addi");
        assert_file!(file: "rv64_i", contains: "addw");

        let ratified_name = |name: &str| OpcodesFileName::Ratified(name.to_string());
        let unratified_name = |name: &str| OpcodesFileName::Unratified(name.to_string());

        fs::write(
            fetcher.opcodes_dir().join("unratified/rv_test_file"),
            "test_text",
        )
        .unwrap();

        assert_file! {
            glob: "rv_*",
            contains: [ratified_name("rv_a"), ratified_name("rv_c"), ratified_name("rv_i")],
            excludes: [ratified_name("rv64_a"), ratified_name("rv64_c"), ratified_name("rv64_i"),
                       ratified_name("rv_test_file"), unratified_name("rv_test_file")]
        };
        assert_file! {
            glob: "rv64_*",
            contains: [ratified_name("rv64_a"), ratified_name("rv64_c"), ratified_name("rv64_i")],
            excludes: [ratified_name("rv_a"), ratified_name("rv_c"), ratified_name("rv_i"),
                       ratified_name("rv_test_file"), unratified_name("rv_test_file")]
        };
        assert_file! {
            glob: "unratified/rv_*",
            contains: [unratified_name("rv_test_file")],
            excludes: [ratified_name("rv64_a"), ratified_name("rv64_c"), ratified_name("rv64_i"),
                       ratified_name("rv_a"), ratified_name("rv_c"), ratified_name("rv_i"),
                       ratified_name("rv_test_file")]
        };
    }
}
