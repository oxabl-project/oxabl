//! The r-code oxabl installed beside the sources: `<state>/installed`.
//!
//! One JSON object on each line. A line with a hash and a size is r-code
//! oxabl installed with those bytes. A line with the path alone is an intent:
//! oxabl may write, or has written, that path. A build appends the intents of
//! the units it builds, with one `fsync`, before its first delete or rename,
//! and writes the whole file again at its end. So a path that oxabl wrote is
//! in the file whatever moment a build is stopped at.

use std::collections::BTreeMap;
use std::fs::{self, File, OpenOptions};
use std::io::{self, ErrorKind, Write};
use std::path::{Path, PathBuf};

use serde_json::{Value, json};

/// The name of the file in the state directory.
pub const FILE: &str = "installed";

/// What oxabl knows about one installed path: the hash and the size it
/// installed, or nothing for an intent.
pub type Installed = Option<([u8; 32], u64)>;

/// The manifest of one workspace.
#[derive(Debug, Default)]
pub struct Manifest {
    file: PathBuf,
    /// Root-relative path to what oxabl installed there.
    pub lines: BTreeMap<String, Installed>,
    /// Lines that are not a manifest line, such as the end of an append that
    /// a crash cut. Such a line was never followed by a rename.
    pub unreadable: usize,
}

fn line(path: &str, installed: &Installed) -> String {
    let value = match installed {
        Some((hash, size)) => json!({
            "path": path,
            "size": size,
            "blake3": blake3::Hash::from_bytes(*hash).to_hex().as_str(),
        }),
        None => json!({ "path": path }),
    };
    value.to_string()
}

fn parse(text: &str) -> Option<(String, Installed)> {
    let value: Value = serde_json::from_str(text).ok()?;
    let path = value.get("path")?.as_str()?.to_string();
    let installed = match (value.get("blake3"), value.get("size")) {
        (None, None) => None,
        (Some(hash), Some(size)) => {
            let hash = blake3::Hash::from_hex(hash.as_str()?).ok()?;
            Some((*hash.as_bytes(), size.as_u64()?))
        }
        _ => return None,
    };
    Some((path, installed))
}

/// Calls `fsync` on a directory, so that a name created or renamed in it
/// stays.
fn sync_directory(directory: &Path) -> io::Result<()> {
    File::open(directory)?.sync_all()
}

impl Manifest {
    /// Reads the manifest below `state_dir`. An absent file is an empty
    /// manifest.
    pub fn read(state_dir: &Path) -> io::Result<Manifest> {
        let file = state_dir.join(FILE);
        let text = match fs::read_to_string(&file) {
            Ok(text) => text,
            Err(error) if error.kind() == ErrorKind::NotFound => String::new(),
            Err(error) => return Err(error),
        };
        let mut manifest = Manifest {
            file,
            ..Manifest::default()
        };
        for text in text.lines().filter(|text| !text.is_empty()) {
            match parse(text) {
                Some((path, installed)) => {
                    // An intent appended after a hashed line does not forget
                    // the hash.
                    let known = manifest.lines.entry(path).or_insert(installed);
                    if known.is_none() {
                        *known = installed;
                    }
                }
                None => manifest.unreadable += 1,
            }
        }
        Ok(manifest)
    }

    /// Appends an intent for each path of `paths` that the manifest does not
    /// hold, then calls `fsync` once.
    pub fn intend<'a>(&mut self, paths: impl IntoIterator<Item = &'a str>) -> io::Result<()> {
        let mut text = String::new();
        for path in paths {
            if !self.lines.contains_key(path) {
                self.lines.insert(path.to_string(), None);
                text.push_str(&line(path, &None));
                text.push('\n');
            }
        }
        if text.is_empty() {
            return Ok(());
        }
        let created = !self.file.exists();
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.file)?;
        // A cut line before this append ends here, and the first intent
        // starts on a line of its own.
        file.write_all(b"\n")?;
        file.write_all(text.as_bytes())?;
        file.sync_all()?;
        if created && let Some(directory) = self.file.parent() {
            sync_directory(directory)?;
        }
        Ok(())
    }

    /// Writes `lines` as the whole manifest: a temporary file, `fsync`,
    /// rename. No lines removes the file.
    pub fn replace(&mut self, lines: BTreeMap<String, Installed>) -> io::Result<()> {
        self.lines = lines;
        self.unreadable = 0;
        let directory = self.file.parent().unwrap_or(Path::new("."));
        if self.lines.is_empty() {
            return match fs::remove_file(&self.file) {
                Ok(()) => sync_directory(directory),
                Err(error) if error.kind() == ErrorKind::NotFound => Ok(()),
                Err(error) => Err(error),
            };
        }
        let temporary = self.file.with_file_name(format!("{FILE}.tmp"));
        let mut file = File::create(&temporary)?;
        for (path, installed) in &self.lines {
            file.write_all(line(path, installed).as_bytes())?;
            file.write_all(b"\n")?;
        }
        file.sync_all()?;
        drop(file);
        fs::rename(&temporary, &self.file)?;
        sync_directory(directory)
    }
}
