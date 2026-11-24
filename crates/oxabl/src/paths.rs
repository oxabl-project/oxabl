//! Where oxabl keeps its data (the install record, the agent skill store) and
//! its update state.

use std::path::PathBuf;

/// The value of environment variable `name` when it is set and not empty.
fn variable(name: &str) -> Option<PathBuf> {
    std::env::var_os(name)
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
}

/// The home directory: `HOME`, or `USERPROFILE` where `HOME` is not set.
pub fn home() -> Option<PathBuf> {
    variable("HOME").or_else(|| variable("USERPROFILE"))
}

fn under_home(xdg: &str, fallback: &str) -> Option<PathBuf> {
    let base = variable(xdg).or_else(|| home().map(|home| home.join(fallback)))?;
    Some(base.join("oxabl"))
}

/// `${XDG_DATA_HOME:-~/.local/share}/oxabl`: the install record and the skill
/// store.
pub fn data_dir() -> Option<PathBuf> {
    under_home("XDG_DATA_HOME", ".local/share")
}

/// `${XDG_STATE_HOME:-~/.local/state}/oxabl`: the update state, lock and log.
#[cfg(feature = "self-update")]
pub fn state_dir() -> Option<PathBuf> {
    under_home("XDG_STATE_HOME", ".local/state")
}
