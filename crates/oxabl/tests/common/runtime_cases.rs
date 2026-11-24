//! The cases of the runtime rule, as measured on OpenEdge 12.8 (E1, E1b, E1c
//! and E4 in `build_openedge/runtime.rs`). The fake runtime runs this table in
//! `build_runtime_fake.rs`, and the real AVM runs the same table in
//! `build_openedge/runtime.rs`. When both pass, the fake encodes the rule that
//! the tests of r-code placement rely on.

/// A file of a case, below the workspace. A path below `pkg/` is the class
/// `pkg.<Name>` (see [`class_of`]), any other path a procedure.
#[derive(Clone, Copy, Debug)]
pub enum Made {
    /// A source that prints the text when it runs: a procedure, or for a
    /// `.cls` path a class whose object gives the text.
    Source(&'static str),
    /// The r-code of such a source, compiled elsewhere, with a time older than
    /// the sources of the case.
    Rcode(&'static str),
}

/// How a case starts the program or the class.
#[derive(Clone, Copy, Debug)]
pub enum Start {
    /// `-p <name>`, the name as written.
    Name(&'static str),
    /// `-p <full path of the file>`, for a file below the workspace.
    FullPath(&'static str),
    /// An object of the class made by its name at run time.
    Class(&'static str),
}

#[derive(Clone, Copy, Debug)]
pub struct Case {
    pub what: &'static str,
    pub files: &'static [(&'static str, Made)],
    /// The runtime PROPATH, entries below the workspace.
    pub propath: &'static [&'static str],
    pub start: Start,
    /// What the run prints; `None` when the runtime finds nothing to run.
    pub says: Option<&'static str>,
}

use Made::{Rcode, Source};

const E1: &[(&str, Made)] = &[("a/x.p", Source("a-source")), ("b/x.r", Rcode("b-rcode"))];
const STALE: &[(&str, Made)] = &[("a/x.r", Rcode("old")), ("a/x.p", Source("new"))];
const STALE_AND_OUT: &[(&str, Made)] = &[
    ("a/x.r", Rcode("old")),
    ("a/x.p", Source("new")),
    ("o/x.r", Rcode("out")),
];
const CLASS_STALE: &[(&str, Made)] = &[
    ("a/pkg/Foo.r", Rcode("old")),
    ("a/pkg/Foo.cls", Source("new")),
];

pub const CASES: &[Case] = &[
    Case {
        what: "E1: the source in an earlier entry beats r-code in a later one",
        files: E1,
        propath: &["a", "b"],
        start: Start::Name("x.p"),
        says: Some("a-source"),
    },
    Case {
        what: "E1: r-code in an earlier entry beats the source in a later one",
        files: E1,
        propath: &["b", "a"],
        start: Start::Name("x.p"),
        says: Some("b-rcode"),
    },
    Case {
        what: "E1b: r-code beside a newer source runs",
        files: STALE,
        propath: &["a"],
        start: Start::Name("x.p"),
        says: Some("old"),
    },
    Case {
        what: "E1c: the name without an extension runs the r-code",
        files: STALE,
        propath: &["a"],
        start: Start::Name("x"),
        says: Some("old"),
    },
    Case {
        what: "E1c: the name of the r-code runs it",
        files: STALE,
        propath: &["a"],
        start: Start::Name("x.r"),
        says: Some("old"),
    },
    Case {
        what: "E1c: the full path of the source runs the r-code beside it",
        files: STALE,
        propath: &["a"],
        start: Start::FullPath("a/x.p"),
        says: Some("old"),
    },
    Case {
        what: "E1c: the name runs the r-code of an earlier output entry",
        files: STALE_AND_OUT,
        propath: &["o", "a"],
        start: Start::Name("x.p"),
        says: Some("out"),
    },
    Case {
        what: "E1c: the full path still runs the r-code beside the source",
        files: STALE_AND_OUT,
        propath: &["o", "a"],
        start: Start::FullPath("a/x.p"),
        says: Some("old"),
    },
    Case {
        what: "E1c: the name without an extension and no r-code runs nothing",
        files: &[("a/x.p", Source("new"))],
        propath: &["a"],
        start: Start::Name("x"),
        says: None,
    },
    Case {
        what: "a source alone is compiled on the fly",
        files: &[("a/x.p", Source("new"))],
        propath: &["a"],
        start: Start::Name("x.p"),
        says: Some("new"),
    },
    Case {
        what: "E4: class r-code beside a newer class source runs",
        files: CLASS_STALE,
        propath: &["a"],
        start: Start::Class("pkg.Foo"),
        says: Some("old"),
    },
    Case {
        what: "E4: the class source in an earlier entry beats class r-code in a later one",
        files: &[
            ("a/pkg/Foo.cls", Source("a-source")),
            ("b/pkg/Foo.r", Rcode("b-rcode")),
        ],
        propath: &["a", "b"],
        start: Start::Class("pkg.Foo"),
        says: Some("a-source"),
    },
];

/// The class of a case file below `pkg/`, `pkg/Foo.cls` or `pkg/Foo.r` giving
/// `pkg.Foo`; `None` for a procedure.
pub fn class_of(path: &str) -> Option<String> {
    let (_, name) = path.split_once("pkg/")?;
    let stem = name.rsplit_once('.').map_or(name, |(stem, _)| stem);
    Some(format!("pkg.{stem}"))
}
