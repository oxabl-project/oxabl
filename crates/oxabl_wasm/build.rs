//! Emit a build identifier for the browser artifact.
//!
//! `oxabl_wasm` is absent from `release-please-config.json`'s package list and
//! from `.release-please-manifest.json`, so its `CARGO_PKG_VERSION` never moves
//! off `0.1.0`. A crash report carrying only the crate version would therefore
//! identify nothing, and the artifact is **vendored by hand** into the website
//! (`scripts/build-wasm.sh` output copied to `[web] src/wasm/`) with nothing
//! recording which commit produced it. A short git SHA baked in at build time
//! survives that copy and makes two vendored copies distinguishable.

use std::process::Command;

fn main() {
    // Rerun when HEAD moves, so the identifier does not go stale in a warm
    // target dir. Both paths are needed: `HEAD` for a detached checkout, and the
    // ref it points at for a normal branch checkout.
    println!("cargo:rerun-if-changed=../../.git/HEAD");
    println!("cargo:rerun-if-changed=../../.git/refs");
    println!("cargo:rustc-env=OXABL_WASM_BUILD={}", build_identifier());
}

/// The short git SHA, with a `-dirty` marker for uncommitted changes. Falls back
/// to `unknown` outside a checkout — a released tarball built from a source
/// archive has no `.git`, and that is not a build failure.
fn build_identifier() -> String {
    let Some(sha) = git(&["rev-parse", "--short=12", "HEAD"]) else {
        return "unknown".to_string();
    };
    match git(&["status", "--porcelain"]) {
        Some(status) if !status.is_empty() => format!("{sha}-dirty"),
        _ => sha,
    }
}

fn git(args: &[&str]) -> Option<String> {
    let output = Command::new("git").args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    Some(String::from_utf8(output.stdout).ok()?.trim().to_string())
}
