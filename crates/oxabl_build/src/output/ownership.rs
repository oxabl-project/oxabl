//! Output directories belong to oxabl: it marks them and deletes what it did
//! not produce.

use std::path::Path;

use crate::error::BuildError;

/// The marker file of a directory oxabl manages.
pub const MARKER: &str = ".oxabl-output";
/// The lock file of the r-code directory, held for a whole build.
pub const LOCK: &str = ".oxabl-build.lock";

fn io(directory: &str, error: std::io::Error) -> BuildError {
    BuildError::Infrastructure(format!(
        "cannot prepare the output directory {directory}: {error}"
    ))
}

/// Marks each directory of `directories` (root-relative) as managed. A
/// directory is claimed when it is absent or empty; one that holds files and
/// has no marker is a usage error and nothing is changed.
pub fn claim(root: &Path, directories: &[&str]) -> Result<(), BuildError> {
    let mut to_mark = Vec::new();
    for directory in directories {
        let path = root.join(directory);
        if path.join(MARKER).is_file() {
            continue;
        }
        match std::fs::read_dir(&path) {
            Ok(mut entries) => {
                if entries.next().is_some() {
                    return Err(BuildError::Usage(format!(
                        "{directory} is not empty and is not managed by oxabl; empty it or choose another output directory"
                    )));
                }
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(io(directory, error)),
        }
        to_mark.push((*directory, path));
    }
    for (directory, path) in to_mark {
        std::fs::create_dir_all(&path).map_err(|error| io(directory, error))?;
        std::fs::write(path.join(MARKER), b"").map_err(|error| io(directory, error))?;
    }
    Ok(())
}
