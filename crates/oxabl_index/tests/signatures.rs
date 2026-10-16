//! Parameter lists through the run index: what `WorkspaceIndex::signature`
//! answers for a program and for an internal procedure, and what it declines.

use std::path::PathBuf;

use oxabl_index::BatchIndex;
use oxabl_semantic::{IndexName, ParamMode, ParamShape, PrimitiveTy, Signature, WorkspaceIndex};
use oxabl_workspace::InMemoryFileSystem;

fn signature(
    files: &[(&str, &str)],
    program: &str,
    procedure: Option<&str>,
) -> Option<std::sync::Arc<Signature>> {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in files {
        fs.insert(PathBuf::from(path), *text);
    }
    let paths = [PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &paths);
    index.signature(
        &IndexName::new(program),
        procedure.map(IndexName::new).as_ref(),
    )
}

const TWO: &str = "\
DEFINE INPUT PARAMETER iOrder AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER cText AS CHARACTER NO-UNDO.
PROCEDURE inner:
    DEFINE INPUT-OUTPUT PARAMETER dTotal AS DECIMAL NO-UNDO.
END PROCEDURE.
";

#[test]
fn a_program_and_its_procedures_have_their_own_lists() {
    let files = [("/src/two.p", TWO)];
    let main = signature(&files, "two.p", None).expect("the main block shows its list");
    assert_eq!(main.params.len(), 2);
    assert_eq!(main.params[0].mode, ParamMode::Input);
    assert_eq!(
        main.params[1].shape,
        ParamShape::Scalar(Some(PrimitiveTy::Character))
    );
    assert!(!main.from_include);
    let inner = signature(&files, "two", Some("INNER")).expect("the procedure has a list");
    assert_eq!(inner.params.len(), 1);
    assert_eq!(inner.params[0].mode, ParamMode::InputOutput);
}

#[test]
fn a_name_the_index_cannot_answer_has_no_list() {
    let files = [("/src/two.p", TWO)];
    assert!(signature(&files, "missing.p", None).is_none());
    assert!(signature(&files, "two.p", Some("nope")).is_none());
    let broken = [("/src/broken.p", "IF THEN THEN ELSE.\n")];
    assert!(signature(&broken, "broken.p", None).is_none());
    let class = [("/src/c.cls", "CLASS c:\nEND CLASS.\n")];
    assert!(signature(&class, "c.cls", None).is_none());
}

#[test]
fn an_include_that_adds_parameters_marks_the_list() {
    let files = [
        (
            "/src/prog.p",
            "{head.i}\nDEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n",
        ),
        (
            "/src/head.i",
            "DEFINE INPUT PARAMETER x AS CHARACTER NO-UNDO.\nDEFINE INPUT PARAMETER y AS CHARACTER NO-UNDO.\n",
        ),
    ];
    let list = signature(&files, "prog.p", None).unwrap();
    assert_eq!(list.params.len(), 3);
    assert!(list.from_include);
}

#[test]
fn an_include_that_adds_none_leaves_the_list_unmarked() {
    let files = [
        (
            "/src/prog.p",
            "{head.i}\nDEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n",
        ),
        ("/src/head.i", "DEFINE VARIABLE v AS INTEGER NO-UNDO.\n"),
    ];
    let list = signature(&files, "prog.p", None).unwrap();
    assert_eq!(list.params.len(), 1);
    assert!(!list.from_include);
}

#[test]
fn an_include_that_does_not_arrive_leaves_the_list_unknown() {
    let files = [(
        "/src/prog.p",
        "{gone.i}\nDEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.\n",
    )];
    assert!(signature(&files, "prog.p", None).is_none());
}

#[test]
fn the_lists_are_read_when_asked_and_not_before() {
    // An index that is asked nothing about parameters reads no file for them:
    // a name with no file answers `None` without a read of any other.
    let files = [("/src/two.p", TWO)];
    assert!(signature(&files, "other.p", None).is_none());
}

#[test]
fn the_analysed_file_is_not_its_own_callee() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/src/two.p"), TWO);
    let paths = [PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &paths);
    let own = PathBuf::from("/src/two.p");
    let view = index.excluding(Some(&own));
    assert!(view.signature(&IndexName::new("two.p"), None).is_none());
    // Another file's view sees it.
    let other = PathBuf::from("/src/caller.p");
    let view = index.excluding(Some(&other));
    assert!(view.signature(&IndexName::new("two.p"), None).is_some());
}

#[test]
fn a_seeded_index_lists_the_procedures_the_workspace_defines() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/src/super.p"),
        "PROCEDURE shared-thing:\nEND PROCEDURE.\nproc abbreviated:\nEND PROCEDURE.\n",
    );
    fs.insert(PathBuf::from("/src/other.p"), "MESSAGE 1.\n");
    let paths = [PathBuf::from("/src")];
    let files = [PathBuf::from("/src/super.p"), PathBuf::from("/src/other.p")];
    let seeded = BatchIndex::new(&fs, &paths).seeded_with(&files);
    assert_eq!(
        seeded.workspace_defines_procedure(&IndexName::new("Shared-Thing")),
        Some(true)
    );
    assert_eq!(
        seeded.workspace_defines_procedure(&IndexName::new("abbreviated")),
        Some(true)
    );
    assert_eq!(
        seeded.workspace_defines_procedure(&IndexName::new("absent")),
        Some(false)
    );
    // An index that holds no file list cannot say.
    let plain = BatchIndex::new(&fs, &paths);
    assert_eq!(
        plain.workspace_defines_procedure(&IndexName::new("x")),
        None
    );
}
