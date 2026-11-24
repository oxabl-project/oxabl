//! The library collection of a unit that reaches encrypted source, at any
//! include depth: it reports one `SOURCE001` and no other finding, whichever
//! client calls. The text is synthetic.

use std::path::PathBuf;

use oxabl_analyze::collect_with_model;
use oxabl_common::{FileId, LintSeverityMap};
use oxabl_schema::Schema;
use oxabl_workspace::InMemoryFileSystem;

fn encrypted() -> String {
    let mut text = String::from("\u{13}Y/%\u{7}\t1I;%");
    for i in 0..400u32 {
        text.push(char::from_u32(0x20 + (i * 37) % 0x5f).unwrap());
        text.push(char::from_u32(1 + (i * 11) % 0x1f).unwrap_or(' '));
    }
    text
}

/// The findings of `source` when `enc.i` holds `include`, as (code, message).
fn findings(source: &str, include: &str, files: &[(&str, &str)]) -> Vec<(&'static str, String)> {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/p/enc.i"), include.to_string());
    for (name, text) in files {
        fs.insert(PathBuf::from(format!("/p/{name}")), (*text).to_string());
    }
    let (_, found) = collect_with_model(
        FileId::new(1),
        source,
        &fs,
        &[PathBuf::from("/p")],
        &Schema::empty(),
        false,
        &LintSeverityMap::default(),
        true,
    );
    found
        .all()
        .map(|c| &c.diagnostic)
        .chain(found.include_origin.iter().map(|c| &c.diagnostic))
        .map(|d| (d.code.0, d.message.clone()))
        .collect()
}

fn codes(found: &[(&'static str, String)]) -> Vec<&'static str> {
    found.iter().map(|(code, _)| *code).collect()
}

/// The unit has findings when `enc.i` is readable and empty, and exactly one
/// `SOURCE001` that names `enc.i` when it is encrypted.
fn assert_withheld(source: &str, files: &[(&str, &str)]) -> Vec<(&'static str, String)> {
    let readable = findings(source, "/* x */\n", files);
    assert!(
        !readable.is_empty(),
        "the unit has findings when the include is readable"
    );
    let found = findings(source, &encrypted(), files);
    assert_eq!(codes(&found), ["SOURCE001"], "{found:?}");
    assert!(found[0].1.contains("'enc.i'"), "{}", found[0].1);
    found
}

#[test]
fn a_direct_encrypted_include_withholds_every_finding() {
    assert_withheld(
        "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\nMESSAGE before-name.\n{enc.i}\nMESSAGE after-name.\n",
        &[],
    );
}

#[test]
fn a_nested_encrypted_include_is_reported_at_the_root_and_withholds_every_finding() {
    let source = "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\n{mid.i}\nMESSAGE typo.\n";
    let found = assert_withheld(source, &[("mid.i", "{enc.i}\n")]);
    assert!(found[0].1.contains("reached through this include"));
    let deeper = assert_withheld(source, &[("mid.i", "{mid2.i}\n"), ("mid2.i", "{enc.i}\n")]);
    assert!(deeper[0].1.contains("reached through this include"));
}

#[test]
fn a_class_whose_member_is_in_an_encrypted_include_reports_no_undefined_symbol() {
    assert_withheld(
        "CLASS cls:\n  METHOD PUBLIC VOID m():\n    x = 1.\n  END METHOD.\n  {enc.i}\nEND CLASS.\n",
        &[],
    );
}

#[test]
fn a_preprocessor_name_that_an_encrypted_include_defines_reports_no_parse_error() {
    assert_withheld(
        "{enc.i}\nDEFINE VARIABLE v AS CHARACTER NO-UNDO.\nv = {&GREETING}.\nMESSAGE v.\n",
        &[],
    );
}

#[test]
fn a_readable_include_with_findings_does_not_survive_an_encrypted_one() {
    assert_withheld(
        "{bad.i}\n{enc.i}\nMESSAGE 1.\n",
        &[("bad.i", "DEFINE VARIABLE unused AS CHARACTER NO-UNDO.\n")],
    );
}

#[test]
fn several_encrypted_includes_are_one_finding() {
    let source = "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\n{enc.i}\n{enc2.i}\n";
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/p/enc.i"), encrypted());
    fs.insert(PathBuf::from("/p/enc2.i"), encrypted());
    let (_, found) = collect_with_model(
        FileId::new(1),
        source,
        &fs,
        &[PathBuf::from("/p")],
        &Schema::empty(),
        false,
        &LintSeverityMap::default(),
        true,
    );
    let found: Vec<_> = found.all().collect();
    assert_eq!(found.len(), 1);
    assert!(found[0].diagnostic.message.contains("and 1 more"));
}
