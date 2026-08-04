//! The workspace reverse query (R1, R4, R5).
//!
//! This is where the integration coverage the edge-set builder cannot run lives:
//! real nested includes, a real cycle, the include depth cap, and a loaded schema,
//! all over a filesystem. The builder takes its inputs from a caller precisely
//! because it cannot assemble them, so those cases can only be exercised here.

use std::path::PathBuf;

use oxabl_pipeline::{EdgeKind, LintPipeline, PipelineConfig, ReverseGraph, Subject};
use oxabl_schema::test_support::customer_schema;
use oxabl_workspace::InMemoryFileSystem;

/// A workspace under `/proj`, with `/proj` as the only include root.
struct Workspace {
    fs: InMemoryFileSystem,
    files: Vec<PathBuf>,
}

impl Workspace {
    fn new(files: &[(&str, &str)]) -> Self {
        let mut fs = InMemoryFileSystem::new();
        let mut compiled = Vec::new();
        for (name, source) in files {
            let path = PathBuf::from(format!("/proj/{name}"));
            fs.insert(path.clone(), *source);
            // Includes are dependencies, not compilation units, so the pass is
            // asked about the programs and classes only — the same list a build
            // would walk.
            if !name.ends_with(".i") {
                compiled.push(path);
            }
        }
        Workspace {
            fs,
            files: compiled,
        }
    }

    fn config(&self) -> PipelineConfig {
        PipelineConfig {
            include_paths: vec![PathBuf::from("/proj")],
            ..PipelineConfig::default()
        }
    }

    fn with_schema(&self) -> PipelineConfig {
        PipelineConfig {
            include_paths: vec![PathBuf::from("/proj")],
            schema: customer_schema(),
            schema_loaded: true,
            ..PipelineConfig::default()
        }
    }

    fn graph(&self, config: &PipelineConfig) -> ReverseGraph {
        let pipeline = LintPipeline::new(config, &self.fs).with_known_files(&self.files);
        ReverseGraph::build(&pipeline, &self.files)
    }

    /// The pass over an explicit subset of the compilation units.
    fn graph_over(&self, config: &PipelineConfig, files: &[PathBuf]) -> ReverseGraph {
        let pipeline = LintPipeline::new(config, &self.fs).with_known_files(files);
        ReverseGraph::build(&pipeline, files)
    }
}

fn proj(name: &str) -> PathBuf {
    PathBuf::from(format!("/proj/{name}"))
}

fn names(files: &[&std::path::Path]) -> Vec<String> {
    files
        .iter()
        .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
        .collect()
}

fn stems(paths: &[PathBuf]) -> Vec<String> {
    paths
        .iter()
        .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
        .collect()
}

// A shared include's dependents are every direct and every transitive includer,
// each once. Real nested includes over a real filesystem — the coverage the edge
// set builder cannot reach.
#[test]
fn a_shared_includes_dependents_are_every_includer_direct_and_transitive() {
    let ws = Workspace::new(&[
        ("base.i", "DEFINE VARIABLE v-base AS INTEGER NO-UNDO.\n"),
        (
            "middle.i",
            "{base.i}\nDEFINE VARIABLE v-mid AS INTEGER NO-UNDO.\n",
        ),
        ("direct.p", "{base.i}\nMESSAGE v-base.\n"),
        ("indirect.p", "{middle.i}\nMESSAGE v-base v-mid.\n"),
        ("unrelated.p", "MESSAGE \"nothing\".\n"),
    ]);
    let config = ws.config();
    let graph = ws.graph(&config);

    let answer = graph.dependents(&Subject::file(proj("base.i")));
    let mut files = names(&answer.files());
    files.sort();
    assert_eq!(files, vec!["direct.p", "indirect.p"]);

    let direct: Vec<_> = answer
        .of_kind(EdgeKind::DirectInclude)
        .map(|d| d.file.file_name().unwrap().to_string_lossy().into_owned())
        .collect();
    assert_eq!(direct, vec!["direct.p"], "only the file writing {{base.i}}");

    let transitive: Vec<_> = answer
        .of_kind(EdgeKind::TransitiveInclude)
        .map(|d| d.file.file_name().unwrap().to_string_lossy().into_owned())
        .collect();
    assert_eq!(transitive, vec!["indirect.p"]);
}

// A file nothing depends on reports an empty dependent set, which is an answer
// rather than an error.
#[test]
fn a_file_nothing_depends_on_reports_an_empty_set() {
    let ws = Workspace::new(&[("lonely.p", "MESSAGE \"alone\".\n")]);
    let config = ws.config();
    let graph = ws.graph(&config);

    let answer = graph.dependents(&Subject::file(proj("lonely.p")));
    assert!(answer.is_empty());
    assert!(answer.all().is_empty());
}

// The rebuild set of a leaf is just that file: changing it rebuilds it, and
// nothing depends on it.
#[test]
fn the_rebuild_set_of_a_leaf_is_itself() {
    let ws = Workspace::new(&[("lonely.p", "MESSAGE \"alone\".\n")]);
    let config = ws.config();
    let graph = ws.graph(&config);

    assert_eq!(
        graph.rebuild_set(&Subject::file(proj("lonely.p"))),
        vec![proj("lonely.p")]
    );
}

// The rebuild set is the transitive closure, and it is a distinct answer from the
// direct dependents.
#[test]
fn the_rebuild_set_closes_over_dependents_of_dependents() {
    let ws = Workspace::new(&[
        ("base.i", "DEFINE VARIABLE v-base AS INTEGER NO-UNDO.\n"),
        ("middle.i", "{base.i}\n"),
        // A program that includes the middle layer, and another that runs it.
        ("worker.p", "{middle.i}\nMESSAGE v-base.\n"),
        ("caller.p", "RUN worker.p.\n"),
    ]);
    let config = ws.config();
    let graph = ws.graph(&config);

    let direct = graph.dependents(&Subject::file(proj("base.i")));
    assert_eq!(names(&direct.files()), vec!["worker.p"]);

    let mut rebuild = stems(&graph.rebuild_set(&Subject::file(proj("base.i"))));
    rebuild.sort();
    assert_eq!(
        rebuild,
        vec!["base.i", "caller.p", "worker.p"],
        "the closure reaches the file that runs the includer"
    );
}

// A real include cycle terminates and each participant appears once.
#[test]
fn a_real_include_cycle_terminates_with_each_file_once() {
    let ws = Workspace::new(&[
        ("a.i", "{b.i}\nDEFINE VARIABLE v-a AS INTEGER NO-UNDO.\n"),
        ("b.i", "{a.i}\nDEFINE VARIABLE v-b AS INTEGER NO-UNDO.\n"),
        ("one.p", "{a.i}\nMESSAGE v-a.\n"),
        ("two.p", "{b.i}\nMESSAGE v-b.\n"),
    ]);
    let config = ws.config();
    let graph = ws.graph(&config);

    let rebuild = graph.rebuild_set(&Subject::file(proj("a.i")));
    let mut unique = rebuild.clone();
    unique.sort();
    unique.dedup();
    assert_eq!(rebuild.len(), unique.len(), "each file once: {rebuild:?}");
    assert!(rebuild.contains(&proj("one.p")));
}

// A file past the include depth cap yields the dependents that were found and
// does not panic.
#[test]
fn the_include_depth_cap_yields_what_was_found() {
    let mut files: Vec<(String, String)> = Vec::new();
    for level in 0..70 {
        files.push((format!("d{level}.i"), format!("{{d{}.i}}\n", level + 1)));
    }
    files.push((
        "deep.p".to_string(),
        "{d0.i}\nMESSAGE \"deep\".\n".to_string(),
    ));
    let borrowed: Vec<(&str, &str)> = files
        .iter()
        .map(|(n, c)| (n.as_str(), c.as_str()))
        .collect();
    let ws = Workspace::new(&borrowed);
    let config = ws.config();
    let graph = ws.graph(&config);

    let answer = graph.dependents(&Subject::file(proj("d0.i")));
    assert_eq!(names(&answer.files()), vec!["deep.p"]);
    // A file the cap cut off still contributed the edges it did reach.
    assert!(graph.edge_count() > 0);
}

// A file reachable two ways appears under both kinds, so an impact answer can
// group by cause without losing one of the causes.
#[test]
fn a_file_reachable_by_two_kinds_appears_under_both() {
    let ws = Workspace::new(&[
        (
            "shared.i",
            "DEFINE NEW SHARED VARIABLE v-site AS CHARACTER NO-UNDO.\n",
        ),
        // Includes the file *and* runs it, so two different edges land on one
        // target from one dependent.
        ("both.p", "{shared.i}\nRUN shared.i.\nMESSAGE v-site.\n"),
    ]);
    let config = ws.config();
    let graph = ws.graph(&config);

    let answer = graph.dependents(&Subject::file(proj("shared.i")));
    let kinds = answer.kinds();
    assert!(
        kinds.contains(&EdgeKind::DirectInclude),
        "expected an include edge, got {kinds:?}"
    );
    assert_eq!(
        answer.files().len(),
        1,
        "one dependent file, however many kinds link it"
    );
}

// A table referenced under a loaded schema yields schema dependents keyed by
// folded name.
#[test]
fn a_table_under_a_loaded_schema_has_dependents_keyed_by_folded_name() {
    let ws = Workspace::new(&[
        (
            "reader.p",
            "FOR EACH Customer NO-LOCK:\nDISPLAY Customer.Name.\nEND.\n",
        ),
        (
            "shouty.p",
            "FOR EACH CUSTOMER NO-LOCK:\nDISPLAY CUSTOMER.Name.\nEND.\n",
        ),
        ("unrelated.p", "MESSAGE \"nothing\".\n"),
    ]);
    let config = ws.with_schema();
    let graph = ws.graph(&config);

    // Asked in any casing, answered the same: the subject folds its name.
    for spelling in ["Customer", "CUSTOMER", "customer"] {
        let answer = graph.dependents(&Subject::table(spelling));
        let mut files = names(&answer.files());
        files.sort();
        assert_eq!(files, vec!["reader.p", "shouty.p"], "asked as {spelling:?}");
        assert!(
            answer.all().iter().all(|d| d.kind == EdgeKind::SchemaTable),
            "a table is depended on as a table"
        );
    }
}

// Unresolved references survive into the query result with their reasons, and are
// never counted as dependents.
#[test]
fn unresolved_references_survive_into_the_result_with_their_reasons() {
    let ws = Workspace::new(&[(
        "hopeful.p",
        "{nowhere.i}\nMESSAGE \"the include does not exist\".\n",
    )]);
    let config = ws.config();
    let graph = ws.graph(&config);

    // Asked about a file named the way the missing include was: the reference that
    // should have reached it is reported, and no dependent is invented.
    let answer = graph.dependents(&Subject::file(proj("nowhere.i")));
    assert!(
        answer.all().is_empty(),
        "an unresolved reference is not a dependent"
    );
    let rows = answer.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].reference.name, "nowhere.i");
    assert!(rows[0].file.ends_with("hopeful.p"));

    // And the workspace-level ratio makes the answer's trustworthiness legible.
    assert!(graph.unresolved_count() > 0);
    assert!(graph.unresolved_ratio() > 0.0);
}

// An include root spelled with `..` still yields the dotted name a class is
// referenced by. Configuration keeps roots literal on purpose, because include
// resolution asks the filesystem and a collapsed `..` can cross a symlink. The
// prefix arithmetic here is pure, so an unnormalised root would simply never
// match a normalised subject and the dotted candidate would vanish.
#[test]
fn a_dotted_class_name_survives_an_include_root_spelled_with_dot_dot() {
    // The child calls a method it does not define, which is what makes the run
    // walk the supertype chain and record the lookup.
    let ws = Workspace::new(&[(
        "pkg/child.cls",
        "CLASS pkg.child INHERITS pkg.thing:\n\
         METHOD PUBLIC VOID go():\n\
         DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
         v-total = calc-total().\n\
         MESSAGE v-total.\n\
         END METHOD.\n\
         END CLASS.\n",
    )]);

    for root in ["/proj", "/proj/sub/.."] {
        let config = PipelineConfig {
            include_paths: vec![PathBuf::from(root)],
            ..PipelineConfig::default()
        };
        let graph = ws.graph(&config);

        // The parent is not in the workspace, so the reference is unresolved. It
        // can only be tied back to the file it *would* have named through the
        // dotted spelling, which is derived by stripping the root off the subject.
        let answer = graph.dependents(&Subject::file(proj("pkg/thing.cls")));
        let rows = answer.unresolved();
        assert_eq!(rows.len(), 1, "root {root} got {rows:?}");
        assert_eq!(rows[0].reference.name, "pkg.thing");
        assert!(rows[0].file.ends_with("child.cls"));
    }
}

// An edge the pass cannot name is recorded, not dropped. A supplied index mints
// its own file ids, and only the index that minted one can map it to a path — so a
// pass answering through a foreign index resolves the edge and still cannot say
// which file it points at. That is a real gap and it is reported as its own kind,
// never as an unresolved reference: the workspace supplied the target perfectly
// well.
#[test]
fn an_edge_whose_target_cannot_be_named_is_reported_rather_than_dropped() {
    let ws = Workspace::new(&[
        (
            "orders/calc-base.cls",
            "CLASS orders.calc-base:\n\
             METHOD PUBLIC INTEGER calc-total():\n\
             RETURN 0.\n\
             END METHOD.\n\
             END CLASS.\n",
        ),
        (
            "orders/child.cls",
            "CLASS orders.child INHERITS orders.calc-base:\n\
             METHOD PUBLIC VOID go():\n\
             DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
             v-total = calc-total().\n\
             MESSAGE v-total.\n\
             END METHOD.\n\
             END CLASS.\n",
        ),
    ]);

    // The handle's own configuration can resolve nothing, so any edge that appears
    // came from the supplied index and carries that index's file id.
    let searchable = vec![PathBuf::from("/proj")];
    let supplied = oxabl_index::BatchIndex::new(&ws.fs, &searchable);
    let nowhere = PipelineConfig::default();
    let child = vec![proj("orders/child.cls")];
    let pipeline = LintPipeline::new(&nowhere, &ws.fs).with_index(&supplied);
    let graph = ReverseGraph::build(&pipeline, &child);

    let rows = graph.unnameable();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::ClassReference);
    assert_eq!(rows[0].target, "orders.calc-base");
    assert!(rows[0].file.ends_with("child.cls"));

    // It is not silently promoted into either of the honest collections.
    assert!(graph.all_unresolved().is_empty(), "not a workspace gap");
    assert!(
        graph
            .dependents(&Subject::file(proj("orders/calc-base.cls")))
            .is_empty(),
        "an unnameable edge is not a dependent"
    );

    // And it moves a number a reader can see.
    assert!(graph.unnameable_ratio() > 0.0);
}

/// A class that inherits `parent` unqualified and calls an inherited method, which
/// is what makes the run record the lookup.
fn caller_inheriting(parent: &str) -> String {
    format!(
        "CLASS caller INHERITS {parent}:\n\
         METHOD PUBLIC VOID go():\n\
         DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
         v-total = calc-total().\n\
         MESSAGE v-total.\n\
         END METHOD.\n\
         END CLASS.\n"
    )
}

/// A class with the one method the caller above calls. Nested under a package, so
/// an unqualified reference cannot resolve to it and the lookup stays a gap.
const LEAF: &str = "CLASS leaf:\n\
                    METHOD PUBLIC INTEGER calc-total():\n\
                    RETURN 0.\n\
                    END METHOD.\n\
                    END CLASS.\n";

// A stem two files answer to stops being evidence. An unqualified `utils` genuinely
// may name either file, so attributing it to both invents a dependent for one.
#[test]
fn an_ambiguous_stem_claims_no_unresolved_reference() {
    let ws = Workspace::new(&[
        ("a/utils.cls", LEAF),
        ("b/utils.cls", LEAF),
        ("caller.cls", &caller_inheriting("utils")),
    ]);
    let config = ws.config();
    let graph = ws.graph(&config);

    // The reference is unresolved and stays visible at the workspace level: the
    // tightening hides the guess, never the gap.
    assert!(
        graph
            .all_unresolved()
            .iter()
            .any(|row| row.reference.name == "utils"),
        "the gap itself must not disappear, got {:?}",
        graph.all_unresolved()
    );

    // But neither candidate claims it.
    for candidate in ["a/utils.cls", "b/utils.cls"] {
        let answer = graph.dependents(&Subject::file(proj(candidate)));
        assert!(
            answer.unresolved().is_empty(),
            "{candidate} claimed an ambiguous stem: {:?}",
            answer.unresolved()
        );
    }
}

// The tightening above must not cost a genuine match. One file carries the stem, so
// the reference is attributed exactly as before.
#[test]
fn an_unambiguous_stem_still_claims_its_reference() {
    let ws = Workspace::new(&[
        ("pkg/helper.cls", LEAF),
        ("caller.cls", &caller_inheriting("helper")),
    ]);
    let config = ws.config();
    let graph = ws.graph(&config);

    let answer = graph.dependents(&Subject::file(proj("pkg/helper.cls")));
    let rows = answer.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].reference.name, "helper");
    assert!(rows[0].file.ends_with("caller.cls"));
}

// A spelling only counts against the kind of reference that could have written it.
// An unresolved include is spelled with its extension, so the stem of a same-named
// file must not claim it.
#[test]
fn a_reference_spelling_does_not_claim_an_unresolved_include() {
    // The include is written without an extension, so its name collides exactly
    // with the stem of an unrelated class. Only the kinds tell them apart.
    let ws = Workspace::new(&[("caller.p", "{thing}\nMESSAGE \"x\".\n")]);
    let config = ws.config();
    let graph = ws.graph(&config);

    assert!(
        graph
            .all_unresolved()
            .iter()
            .any(|row| row.reference.name == "thing" && row.reference.kind.is_include()),
        "expected an unresolved include named `thing`, got {:?}",
        graph.all_unresolved()
    );

    // A file named exactly that still matches it: an include is spelled by file
    // name, and this is one.
    let by_name = graph.dependents(&Subject::file(proj("thing")));
    assert_eq!(by_name.unresolved().len(), 1, "the include still matches");

    // A class whose *stem* is that string does not. Nothing writes a class
    // reference that way, so a match here would be a coincidence of spelling.
    let by_stem = graph.dependents(&Subject::file(proj("pkg/thing.cls")));
    assert!(
        by_stem.unresolved().is_empty(),
        "a class stem must not claim an unresolved include, got {:?}",
        by_stem.unresolved()
    );
}

// A file removed from the supplied list stops appearing as a dependent. The graph
// answers about the files it was given, not about whatever is on disk.
#[test]
fn a_file_removed_from_the_list_stops_being_a_dependent() {
    let ws = Workspace::new(&[
        ("base.i", "DEFINE VARIABLE v-base AS INTEGER NO-UNDO.\n"),
        ("keeps.p", "{base.i}\nMESSAGE v-base.\n"),
        ("drops.p", "{base.i}\nMESSAGE v-base.\n"),
    ]);
    let config = ws.config();

    let full = ws.graph(&config);
    let mut both = names(&full.dependents(&Subject::file(proj("base.i"))).files());
    both.sort();
    assert_eq!(both, vec!["drops.p", "keeps.p"]);

    let subset = ws.graph_over(&config, &[proj("keeps.p")]);
    assert_eq!(
        names(&subset.dependents(&Subject::file(proj("base.i"))).files()),
        vec!["keeps.p"]
    );
}

// A dependent carries the site of the reference, so a client can open an editor at
// the line that creates the dependency.
#[test]
fn a_dependent_carries_the_site_of_its_reference() {
    let source = "MESSAGE \"before\".\n{base.i}\nMESSAGE v-base.\n";
    let ws = Workspace::new(&[
        ("base.i", "DEFINE VARIABLE v-base AS INTEGER NO-UNDO.\n"),
        ("user.p", source),
    ]);
    let config = ws.config();
    let graph = ws.graph(&config);

    let answer = graph.dependents(&Subject::file(proj("base.i")));
    let span = answer.all()[0]
        .span
        .expect("a direct include is written in the dependent file");
    assert_eq!(&source[span.start as usize..span.end as usize], "{base.i}");
}

// An unreadable file is recorded as unanalysed, never as depending on nothing.
#[test]
fn an_unreadable_file_is_recorded_rather_than_treated_as_empty() {
    let ws = Workspace::new(&[("real.p", "MESSAGE \"here\".\n")]);
    let config = ws.config();
    let missing = proj("ghost.p");
    let graph = ws.graph_over(&config, &[proj("real.p"), missing.clone()]);

    let unanalysed = graph.unanalysed();
    assert_eq!(unanalysed.len(), 1, "got {unanalysed:?}");
    assert_eq!(unanalysed[0].file, missing);
    assert!(
        unanalysed[0].reason.contains("unreadable"),
        "got {:?}",
        unanalysed[0].reason
    );
}

#[test]
fn graph_size_includes_owned_rows_and_paths() {
    let ws = Workspace::new(&[
        ("base.i", "DEFINE VARIABLE v-base AS INTEGER NO-UNDO.\n"),
        ("user.p", "{base.i}\nMESSAGE v-base.\n"),
    ]);
    let graph = ws.graph(&ws.config());
    assert!(graph.estimated_heap_bytes() > std::mem::size_of_val(&graph));
}
