//! Finding files on a PROPATH that a request defined.

use std::path::{Path, PathBuf};

/// One file found on the PROPATH: the name it goes by in the output, and the
/// path to read it from.
pub struct Found {
    pub shown: String,
    pub real: PathBuf,
}

fn join(entry: &str, relative: &str) -> String {
    match entry {
        "." | "" => relative.to_string(),
        entry => format!("{}/{relative}", entry.trim_end_matches('/')),
    }
}

/// The real directory of a PROPATH entry: `$DLC` resolves against the DLC the
/// fake was given, everything else is a path relative to the working
/// directory or absolute.
fn real(entry: &str, relative: &str, dlc: &Path) -> PathBuf {
    match entry.strip_prefix("$DLC") {
        Some(rest) => dlc.join(rest.trim_start_matches('/')).join(relative),
        None => PathBuf::from(join(entry, relative)),
    }
}

/// The first entry of `propath` that holds the file `relative`.
pub fn find(propath: &[String], relative: &str, dlc: &Path) -> Option<Found> {
    propath.iter().find_map(|entry| {
        let real = real(entry, relative, dlc);
        real.is_file().then(|| Found {
            shown: join(entry, relative),
            real,
        })
    })
}

/// `relative` in the first entry of `propath`, the output directory, when it
/// holds the file.
pub fn in_output(propath: &[String], relative: &str, dlc: &Path) -> Option<Found> {
    let entry = propath.first()?;
    let real = real(entry, relative, dlc);
    real.is_file().then(|| Found {
        shown: join(entry, relative),
        real,
    })
}
