//! What a step needs from the machine. A missing need fails the step: it never skips it.

use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;

/// A requirement of a step, with the way to meet it.
#[derive(Clone, Copy)]
pub enum Need {
    /// A program on `PATH`.
    Tool {
        name: &'static str,
        hint: &'static str,
    },
    /// A rustup target of the active toolchain.
    RustTarget { name: &'static str },
}

impl Need {
    /// Say what is missing and how to get it, or `Ok` when the need is met.
    pub fn check(self) -> Result<(), String> {
        match self {
            Need::Tool { name, hint } => match find_on_path(name) {
                Some(_) => Ok(()),
                None => Err(format!("missing tool `{name}` on PATH. {hint}")),
            },
            Need::RustTarget { name } => check_rust_target(name),
        }
    }
}

/// The first executable file called `name` in a `PATH` directory.
pub fn find_on_path(name: &str) -> Option<PathBuf> {
    assert!(
        !name.is_empty() && !name.contains('/'),
        "a tool name has no path"
    );
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|directory| directory.join(name))
        .find(|candidate| {
            std::fs::metadata(candidate)
                .map(|metadata| metadata.is_file() && metadata.permissions().mode() & 0o111 != 0)
                .unwrap_or(false)
        })
}

fn check_rust_target(name: &str) -> Result<(), String> {
    let hint = format!("Run `rustup target add {name}`.");
    let output = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .map_err(|error| format!("cannot run `rustc` to find target `{name}`: {error}. {hint}"))?;
    if !output.status.success() {
        return Err(format!(
            "`rustc --print sysroot` failed, so target `{name}` is not established"
        ));
    }
    let sysroot = String::from_utf8_lossy(&output.stdout).trim().to_owned();
    if Path::new(&sysroot).join("lib/rustlib").join(name).is_dir() {
        Ok(())
    } else {
        Err(format!("missing rust target `{name}`. {hint}"))
    }
}
