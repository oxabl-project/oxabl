//! What a run knows about a name that a named include argument defines, once an
//! include below its own include is read. All fixtures are synthetic ABL.

use std::path::PathBuf;

use oxabl_common::FileId;
use oxabl_preprocessor::{BranchState, PreprocessedFile, Preprocessor};
use oxabl_workspace::InMemoryFileSystem;

const COND: &str = "&IF \"{&x}\" = \"zzz\" &THEN\nA\n&ELSE\nZ\n&ENDIF\n";

fn run(source: &str, files: &[(&str, &str)], includer_unknown: bool) -> PreprocessedFile {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in files {
        fs.insert(PathBuf::from(*path), *text);
    }
    let paths = vec![PathBuf::from("/inc")];
    Preprocessor::new(&fs, &paths)
        .with_branch_tracking(true)
        .with_includer_unknown(includer_unknown)
        .process(FileId::new(1), source)
        .unwrap()
}

fn states(result: &PreprocessedFile) -> Vec<BranchState> {
    result.branches.iter().map(|b| b.state).collect()
}

#[test]
fn a_condition_below_the_include_does_not_read_its_argument() {
    let files = [("/inc/p.i", "{b.i}"), ("/inc/b.i", COND)];
    let result = run("{p.i &x=zzz}", &files, false);
    assert_eq!(&*result.to_text(), "Z\n");
    assert_eq!(
        states(&result),
        [BranchState::NotTaken, BranchState::Taken],
        "the argument is not defined below its include"
    );
}

#[test]
fn a_condition_in_the_include_reads_its_argument() {
    let result = run("{b.i &x=zzz}", &[("/inc/b.i", COND)], false);
    assert_eq!(&*result.to_text(), "A\n");
    assert_eq!(states(&result), [BranchState::Taken, BranchState::NotTaken]);
}

#[test]
fn without_its_includer_a_name_below_the_argument_is_not_known() {
    // The includer of the fragment may define `x` for the include below.
    let files = [("/inc/p.i", "{b.i}"), ("/inc/b.i", COND)];
    let result = run("{p.i &x=zzz}", &files, true);
    assert_eq!(
        states(&result),
        [BranchState::Undecided, BranchState::Undecided]
    );
}

#[test]
fn without_its_includer_the_argument_is_known_in_its_own_include() {
    let result = run("{b.i &x=zzz}", &[("/inc/b.i", COND)], true);
    assert_eq!(states(&result), [BranchState::Taken, BranchState::NotTaken]);
}

#[test]
fn without_its_includer_the_argument_is_known_again_after_the_include_below() {
    // After `b.i` ends, `p.i` reads its own argument again.
    let files = [
        ("/inc/p.i", "{b.i}\n&IF \"{&x}\" = \"zzz\" &THEN\nA\n&ENDIF"),
        ("/inc/b.i", "B\n"),
    ];
    let result = run("{p.i &x=zzz}", &files, true);
    assert_eq!(states(&result), [BranchState::Taken]);
    assert!(result.to_text().ends_with("A\n"), "{:?}", result.to_text());
}

#[test]
fn an_argument_is_read_again_after_a_chain_of_includes_below() {
    let files = [
        ("/inc/p.i", "{m.i &y=1}[{&x}]"),
        ("/inc/m.i", "{b.i}"),
        ("/inc/b.i", "<{&x}{&y}>"),
    ];
    let result = run("{p.i &x=zzz}", &files, false);
    assert_eq!(&*result.to_text(), "<>[zzz]");
}

#[test]
fn a_define_in_an_undecided_region_below_the_argument_keeps_the_name_unknown() {
    let files = [
        ("/inc/p.i", "{b.i}"),
        (
            "/inc/b.i",
            "&IF SEARCH(\"nofile\") <> ? &THEN\n&GLOBAL-DEFINE x yes\n&ENDIF\n{c.i}",
        ),
        (
            "/inc/c.i",
            "&IF DEFINED(x) = 1 &THEN\nA\n&ELSE\nZ\n&ENDIF\n",
        ),
    ];
    let result = run("{p.i &x=zzz}", &files, false);
    assert_eq!(
        states(&result),
        [
            BranchState::Undecided,
            BranchState::Undecided,
            BranchState::Undecided
        ],
        "the global define may have happened"
    );
}

#[test]
fn without_its_includer_a_global_define_below_the_argument_leaves_the_name_unknown_after_it() {
    let files = [
        ("/inc/p.i", "{b.i}\n&IF \"{&x}\" = \"zzz\" &THEN\nA\n&ENDIF"),
        ("/inc/b.i", "&GLOBAL-DEFINE x yes\n"),
    ];
    let result = run("{p.i &x=zzz}", &files, true);
    assert_eq!(states(&result), [BranchState::Undecided]);
}

const SCOPED_COND: &str = "&IF \"{&x}\" = \"yes\" &THEN\nA\n&ELSE\nZ\n&ENDIF\n";
const MAYBE_SCOPED: &str = "&IF SEARCH(\"nofile\") <> ? &THEN\n&SCOPED-DEFINE x yes\n&ENDIF\n";

#[test]
fn a_scoped_define_in_an_undecided_region_below_the_argument_keeps_the_name_unknown() {
    let b = format!("{MAYBE_SCOPED}{{c.i}}");
    let files = [
        ("/inc/p.i", "{b.i}"),
        ("/inc/b.i", b.as_str()),
        ("/inc/c.i", SCOPED_COND),
    ];
    let result = run("{p.i &x=zzz}", &files, false);
    assert_eq!(
        states(&result),
        [
            BranchState::Undecided,
            BranchState::Undecided,
            BranchState::Undecided
        ],
        "the scoped define may have happened"
    );
}

#[test]
fn a_scoped_define_in_an_undecided_region_of_the_include_with_the_argument_keeps_the_name_unknown()
{
    let p = format!("{MAYBE_SCOPED}{{b.i}}");
    let files = [("/inc/p.i", p.as_str()), ("/inc/b.i", SCOPED_COND)];
    let result = run("{p.i &x=zzz}", &files, false);
    assert_eq!(
        states(&result),
        [
            BranchState::Undecided,
            BranchState::Undecided,
            BranchState::Undecided
        ],
        "the scoped define may have happened"
    );
}

#[test]
fn a_decided_scoped_define_below_the_argument_still_decides() {
    let files = [
        ("/inc/p.i", "{b.i}"),
        ("/inc/b.i", "&SCOPED-DEFINE x yes\n{c.i}"),
        ("/inc/c.i", SCOPED_COND),
    ];
    let result = run("{p.i &x=zzz}", &files, false);
    assert_eq!(&*result.to_text(), "A\n");
}
