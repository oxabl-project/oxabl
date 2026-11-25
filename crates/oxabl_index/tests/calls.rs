//! The static call graph: what each call of a file calls, resolution over the
//! search path through the run index, and the components of the graph.

use std::path::PathBuf;

use oxabl_index::BatchIndex;
use oxabl_index::calls::{
    CallGraph, CallKind, CallTarget, Callee, FileCalls, ProgramAnswer, UnknownCall, resolve_program,
};
use oxabl_parser::Parser;
use oxabl_workspace::InMemoryFileSystem;

fn calls(source: &str) -> FileCalls {
    let tokens = oxabl_lexer::tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    FileCalls::extract(&program.statements, source, &tokens)
}

fn targets(source: &str) -> Vec<(CallKind, CallTarget)> {
    calls(source)
        .sites
        .into_iter()
        .map(|s| (s.kind, s.target))
        .collect()
}

fn unknown(name: Option<&str>, reason: UnknownCall) -> CallTarget {
    CallTarget::Unknown {
        name: name.map(str::to_string),
        reason,
    }
}

#[test]
fn run_targets_are_internal_external_or_unknown() {
    let src = "\
PROCEDURE inner:
END PROCEDURE.
PROCEDURE viaSuper IN SUPER.
PROCEDURE lib EXTERNAL \"lib.so\":
END PROCEDURE.
RUN inner.
RUN INNER IN THIS-PROCEDURE.
RUN other.p.
RUN viaSuper.
RUN lib.
RUN SUPER.
RUN VALUE(\"x.p\").
RUN far.p ASYNCHRONOUS.
";
    assert_eq!(
        targets(src),
        [
            (CallKind::Run, CallTarget::Procedure("inner".into())),
            (CallKind::Run, CallTarget::Procedure("INNER".into())),
            (CallKind::Run, CallTarget::Program("other.p".into())),
            (CallKind::Run, unknown(Some("viaSuper"), UnknownCall::Super)),
            (CallKind::Run, CallTarget::Library("lib".into())),
            (CallKind::Run, unknown(None, UnknownCall::Super)),
            (CallKind::Run, unknown(None, UnknownCall::Dynamic)),
            (CallKind::Remote, CallTarget::Program("far.p".into())),
        ]
    );
}

#[test]
fn a_handle_set_only_by_one_persistent_run_names_its_program() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE hOther AS HANDLE NO-UNDO.
DEFINE VARIABLE hTwo AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
RUN a.p PERSISTENT SET hTwo.
RUN b.p PERSISTENT SET hTwo.
hOther = SESSION:FIRST-PROCEDURE.
RUN go IN hLib.
RUN go IN hOther.
RUN go IN hTwo.
";
    let t = targets(src);
    assert_eq!(
        t[0],
        (CallKind::Persistent, CallTarget::Program("lib.p".into()))
    );
    assert_eq!(
        t[3].1,
        CallTarget::InProgram {
            program: "lib.p".into(),
            procedure: "go".into()
        }
    );
    assert_eq!(t[4].1, unknown(Some("go"), UnknownCall::Handle));
    assert_eq!(t[5].1, unknown(Some("go"), UnknownCall::Handle));
}

#[test]
fn an_output_argument_or_a_parameter_makes_a_handle_unknown() {
    let src = "\
DEFINE INPUT PARAMETER hIn AS HANDLE NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET h.
RUN getHandle.p (OUTPUT h).
RUN lib.p PERSISTENT SET hIn.
RUN go IN h.
RUN go IN hIn.
";
    let t = targets(src);
    assert_eq!(t[3].1, unknown(Some("go"), UnknownCall::Handle));
    assert_eq!(t[4].1, unknown(Some("go"), UnknownCall::Handle));
}

#[test]
fn function_calls_and_dynamic_calls() {
    let src = "\
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
FUNCTION addOne RETURNS INTEGER (INPUT i AS INTEGER) FORWARD.
FUNCTION far RETURNS INTEGER (INPUT i AS INTEGER) IN h.
FUNCTION upper1 RETURNS INTEGER (INPUT i AS INTEGER) IN SUPER.
x = addOne(1) + far(2) + upper1(3).
x = DYNAMIC-FUNCTION(\"addOne\", 1).
PUBLISH \"changed\".
FUNCTION addOne RETURNS INTEGER (INPUT i AS INTEGER):
    RETURN i + 1.
END FUNCTION.
";
    assert_eq!(
        targets(src),
        [
            (CallKind::Call, CallTarget::Function("addOne".into())),
            (CallKind::Call, unknown(Some("far"), UnknownCall::Handle)),
            (CallKind::Call, unknown(Some("upper1"), UnknownCall::Super)),
            (CallKind::Call, unknown(None, UnknownCall::Dynamic)),
            (CallKind::Publish, unknown(None, UnknownCall::Event)),
        ]
    );
}

#[test]
fn methods_of_handles_are_built_in_and_of_objects_unknown() {
    let src = "\
CLASS demo.Thing:
    DEFINE VARIABLE hq AS HANDLE NO-UNDO.
    DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.
    METHOD PUBLIC VOID Go():
        hq:QUERY-OPEN().
        hq:GET-BUFFER-HANDLE(1):BUFFER-FIELD(\"f\"):BUFFER-VALUE().
        ERROR-STATUS:GET-MESSAGE(1).
        BUFFER Customer:HANDLE:BUFFER-RELEASE().
        o:ToString().
        THIS-OBJECT:Helper().
        Helper().
    END METHOD.
    METHOD PRIVATE VOID Helper():
    END METHOD.
END CLASS.
";
    assert_eq!(
        targets(src),
        [
            (
                CallKind::Call,
                unknown(Some("ToString"), UnknownCall::Method)
            ),
            (CallKind::Call, CallTarget::Method("Helper".into())),
            (CallKind::Call, CallTarget::Method("Helper".into())),
        ]
    );
}

#[test]
fn programs_resolve_over_the_search_path_through_the_run_index() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/src/lib.p"), "MESSAGE 1.");
    fs.insert(PathBuf::from("/src/broken.p"), "DEFINE VARIABLE.");
    let paths = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &paths);
    let ProgramAnswer::Found(id) = resolve_program(&index, "lib.p") else {
        panic!("lib.p is on the path");
    };
    assert_eq!(index.indexed_path(id), Some(PathBuf::from("/src/lib.p")));
    assert_eq!(
        resolve_program(&index, "missing.p"),
        ProgramAnswer::Unknown(UnknownCall::NotFound)
    );
    assert_eq!(
        resolve_program(&index, "broken.p"),
        ProgramAnswer::Unknown(UnknownCall::Unusable)
    );
}

#[test]
fn components_come_callees_first() {
    // 0 -> 1 -> 2 -> 1 (a cycle of 1 and 2), 0 -> 3, 3 -> unknown.
    let mut g = CallGraph::new(4);
    g.add(0, Callee::Node(1));
    g.add(1, Callee::Node(2));
    g.add(2, Callee::Node(1));
    g.add(0, Callee::Node(3));
    g.add(3, Callee::Unknown(UnknownCall::Dynamic));
    g.add(0, Callee::Node(1));
    assert_eq!(g.callees(0), [Callee::Node(1), Callee::Node(3)]);
    let comps = g.components();
    let pos = |n: u32| comps.iter().position(|c| c.contains(&n)).unwrap();
    assert_eq!(comps[pos(1)], [1, 2]);
    assert!(pos(1) < pos(0) && pos(3) < pos(0));
    assert_eq!(comps.len(), 3);
}

#[test]
fn dynamic_calls_are_read_by_kind() {
    let dynamic = |src: &str| calls(src).dynamic;
    assert!(dynamic("PROCEDURE p:\nEND PROCEDURE.\nRUN p.\nRUN a.p.\n").is_empty());

    let run_value = dynamic("RUN VALUE(c).\n");
    assert!(run_value.run_value && !run_value.run_value_in_handle);
    assert!(!run_value.handle_reaches("p"));

    let in_handle = dynamic("RUN VALUE(c) IN h.\n");
    assert!(in_handle.run_value && in_handle.run_value_in_handle);
    assert!(in_handle.handle_reaches("anything"));

    let new = dynamic("o = DYNAMIC-NEW \"demo.Thing\" ().\n");
    assert!(new.dynamic_new && !new.dynamic_invoke && !new.dynamic_function);
    let invoke = dynamic("DYNAMIC-INVOKE(o, \"Go\").\n");
    assert!(invoke.dynamic_invoke && !invoke.dynamic_new);
    let function = dynamic("x = DYNAMIC-FUNCTION(\"f\", 1).\n");
    assert!(function.dynamic_function && !function.dynamic_invoke);

    // A call through a handle names its routine, in any spelling of the case.
    let named = dynamic("RUN DoWork IN h.\nRUN other IN h.\n");
    assert!(named.handle_reaches("dowork") && named.handle_reaches("OTHER"));
    assert!(!named.handle_reaches("third") && !named.unnamed_handle);
    // A call with no name to read can reach any routine.
    let unnamed = dynamic("PROCEDURE p:\n    RUN SUPER.\nEND PROCEDURE.\n");
    assert!(unnamed.unnamed_handle && unnamed.handle_reaches("third"));
    // A routine declared `IN SUPER` is named where it is called.
    let delegated = dynamic("PROCEDURE viaSuper IN SUPER.\nRUN viaSuper.\n");
    assert!(delegated.handle_reaches("viasuper") && !delegated.unnamed_handle);
}

#[test]
fn the_dynamic_calls_of_two_files_join() {
    let mut joined = calls("RUN VALUE(c).\nRUN a IN h.\n").dynamic;
    joined.absorb(&calls("o = DYNAMIC-NEW \"x\" ().\nRUN b IN h.\n").dynamic);
    assert!(joined.run_value && joined.dynamic_new && !joined.dynamic_invoke);
    assert!(joined.handle_reaches("a") && joined.handle_reaches("b"));
    assert!(!joined.handle_reaches("c"));
}
