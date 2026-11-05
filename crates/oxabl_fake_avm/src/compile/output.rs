//! The files a compile writes into the stage.

use std::io;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use super::outcome::Outcome;
use super::visit::Context;

fn stem(name: &str) -> &str {
    match name.rsplit_once('.') {
        Some((stem, extension)) if !extension.contains('/') => stem,
        _ => name,
    }
}

fn put(path: &Path, text: &str) -> io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(path, text)
}

fn nanoseconds() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| elapsed.as_nanos())
}

/// The text of an r-code file: the unit, each file it read with its hash, and
/// the time, which makes two compiles of one source differ like real r-code.
fn rcode(name: &str, out: &Outcome) -> String {
    let mut text = format!("oxabl-fake-r 1\nunit {name}\n");
    for read in &out.reads {
        text.push_str(&format!("read {} {}\n", read.shown, read.sha256));
    }
    text.push_str(&format!("compiled-at {}\n", nanoseconds()));
    text
}

fn xref_xml(out: &Outcome) -> String {
    let mut text = String::from("<?xml version=\"1.0\"?>\n<Cross-reference>\n");
    for (index, read) in out.reads.iter().enumerate() {
        text.push_str(&format!(
            "<Source File-num=\"{}\" File-name=\"{}\"/>\n",
            index + 1,
            read.shown
        ));
    }
    text.push_str("</Cross-reference>\n");
    text
}

fn xref_text(out: &Outcome) -> String {
    out.reads
        .iter()
        .map(|read| format!("{}\n", read.shown))
        .collect()
}

/// Where the compiler opens an XREF file: like the real one, it removes a
/// leading `.` from a relative name, so `.s/x` lands in `s/x` and `./.s/x` and
/// absolute names are kept.
fn xref_path(path: PathBuf) -> PathBuf {
    let text = path.to_string_lossy();
    match text.strip_prefix('.') {
        Some(rest) if !rest.starts_with('/') && !text.starts_with("..") => PathBuf::from(rest),
        _ => path,
    }
}

/// Writes the outputs of a compiled unit under the stage.
pub fn write(ctx: &Context<'_>, name: &str, out: &Outcome) -> io::Result<()> {
    put(
        &ctx.stage.join("r").join(format!("{}.r", stem(name))),
        &rcode(name, out),
    )?;
    match ctx.xref {
        "xref-xml" => put(
            &xref_path(ctx.stage.join("xref").join(format!("{name}.xref.xml"))),
            &xref_xml(out),
        )?,
        "xref" => put(
            &xref_path(ctx.stage.join("xref").join(format!("{name}.xref"))),
            &xref_text(out),
        )?,
        _ => {}
    }
    if ctx.debug_list {
        put(
            &ctx.stage.join("dbg").join(format!("{name}.dbg")),
            &out.source,
        )?;
    }
    if ctx.listing {
        put(
            &ctx.stage.join("lst").join(format!("{name}.lst")),
            &out.source,
        )?;
    }
    Ok(())
}

/// The r-code a compile also saves for a class it compiled on the side.
pub fn write_stray(ctx: &Context<'_>, relative: &str) -> io::Result<()> {
    put(
        &ctx.stage.join("r").join(format!("{relative}.r")),
        "oxabl-fake-r 1\nstray\n",
    )
}
