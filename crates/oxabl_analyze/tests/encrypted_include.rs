//! The library collection of a unit with an encrypted include: the findings
//! that the include can make false are not reported, whichever client calls.
//! The text is synthetic.

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

fn codes(source: &str, with_encrypted: bool) -> Vec<&'static str> {
    let mut fs = InMemoryFileSystem::new();
    let include = if with_encrypted {
        encrypted()
    } else {
        "/* x */\n".to_string()
    };
    fs.insert(PathBuf::from("/p/enc.i"), include);
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
    found.all().map(|c| c.diagnostic.code.0).collect()
}

const SOURCE: &str = "DEFINE VARIABLE v AS CHARACTER NO-UNDO.\n\
                      MESSAGE before-name.\n{enc.i}\nMESSAGE after-name.\n";

#[test]
fn an_encrypted_include_removes_the_findings_it_can_make_false() {
    let plain = codes(SOURCE, false);
    assert!(plain.contains(&"LINT0002"), "{plain:?}");
    assert_eq!(plain.iter().filter(|c| **c == "LINT0001").count(), 2);

    let found = codes(SOURCE, true);
    assert_eq!(
        found.iter().filter(|c| **c == "LINT0001").count(),
        1,
        "only the name used before the include stays: {found:?}"
    );
    assert!(!found.contains(&"LINT0002"), "{found:?}");
    assert!(found.contains(&"SOURCE001"), "{found:?}");
}
