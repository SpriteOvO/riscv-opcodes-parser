use std::{
    cmp::Ordering,
    collections::BTreeMap,
    convert::identity,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{bail, Result as AnyhowResult};
use globset::GlobBuilder;
use walkdir::{DirEntry as WalkDirEntry, Error as WalkDirErr, WalkDir};

#[derive(Clone, Debug, PartialEq, Eq)]
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

impl Display for OpcodesFileName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl Ord for OpcodesFileName {
    fn cmp(&self, other: &Self) -> Ordering {
        const PRIORITIES: [&str; 4] = ["rv_", "rv32_", "rv64_", "rv128_"];

        match (self, other) {
            (Self::Ratified(_), Self::Unratified(_)) => return Ordering::Less,
            (Self::Unratified(_), Self::Ratified(_)) => return Ordering::Greater,
            _ => {}
        }

        let get_priority = |name: &str| {
            PRIORITIES
                .into_iter()
                .enumerate()
                .find(|(_, prefix)| name.starts_with(prefix))
                .map(|(priority, prefix)| (priority, Some(prefix)))
                .unwrap_or((usize::MAX, None))
        };

        let (self_priority, other_priority) =
            (get_priority(self.name()), get_priority(other.name()));

        let ord = self_priority.0.cmp(&other_priority.0);

        if ord != Ordering::Equal {
            ord
        } else {
            match (self_priority.1, other_priority.1) {
                (Some(self_prefix), Some(other_prefix)) => {
                    assert_eq!(self_prefix, other_prefix);

                    self.name()
                        .strip_prefix(self_prefix)
                        .unwrap()
                        .cmp(other.name().strip_prefix(other_prefix).unwrap())
                }
                (None, None) => self.name().cmp(other.name()),
                _ => unreachable!(),
            }
        }
    }
}

impl PartialOrd for OpcodesFileName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct Reader {
    root: PathBuf,
}

impl Reader {
    pub fn new(root: impl Into<PathBuf>) -> AnyhowResult<Self> {
        let root = root.into();

        if !Self::is_opcodes_dir(&root) {
            bail!(
                "'{}' is not a recognizable RISC-V opcodes directory",
                root.display()
            )
        }

        Ok(Self { root })
    }

    fn is_opcodes_dir(path: impl AsRef<Path>) -> bool {
        let path = path.as_ref();

        ["rv_i", "rv32_c", "rv64_a"]
            .iter()
            .map(|name| path.join(name).exists())
            .all(identity)
    }

    pub fn read_by_glob(
        &self,
        glob: impl AsRef<str>,
    ) -> AnyhowResult<BTreeMap<OpcodesFileName, String>> {
        let glob = GlobBuilder::new(glob.as_ref())
            .literal_separator(true)
            .build()?
            .compile_matcher();

        let matches = self
            .opcodes_files()?
            .into_iter()
            .filter(|f| glob.is_match(f.strip_prefix(&self.root).unwrap()))
            .map(|f| fs::read_to_string(&f).map(|c| (f, c)))
            .collect::<Result<BTreeMap<_, _>, _>>()?
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

    fn ratified_name(name: &str) -> OpcodesFileName {
        OpcodesFileName::Ratified(name.to_string())
    }

    fn unratified_name(name: &str) -> OpcodesFileName {
        OpcodesFileName::Unratified(name.to_string())
    }

    #[test]
    fn expected_files() {
        let fetcher = Fetcher::new().unwrap();
        let reader = Reader::new(&fetcher.opcodes_dir()).unwrap();
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

    #[test]
    fn opcodes_file_name_ordering() {
        use ratified_name as ratified;
        use unratified_name as unratified;
        use Ordering::*;

        assert_eq!(ratified("rv32_").cmp(&unratified("rv128_")), Less);
        assert_eq!(ratified("rv128_").cmp(&unratified("rv32_")), Less);
        assert_eq!(unratified("rv32_").cmp(&ratified("rv128_")), Greater);
        assert_eq!(unratified("rv128_").cmp(&ratified("rv32_")), Greater);

        assert_eq!(ratified("rv32_").cmp(&ratified("rv128_")), Less);
        assert_eq!(unratified("rv32_").cmp(&unratified("rv128_")), Less);
        assert_eq!(ratified("rv128_").cmp(&ratified("rv32_")), Greater);
        assert_eq!(unratified("rv128_").cmp(&unratified("rv32_")), Greater);

        assert_eq!(ratified("rv32_").cmp(&ratified("rv32_")), Equal);
        assert_eq!(unratified("rv32_").cmp(&unratified("rv32_")), Equal);

        assert_eq!(ratified("rv32_a").cmp(&ratified("rv32_b")), Less);
        assert_eq!(ratified("rv32_b").cmp(&ratified("rv32_a")), Greater);
        assert_eq!(ratified("rv32_a").cmp(&ratified("rv32_a")), Equal);
    }
}
