//! The raw digest at the start of a build: an equal digest reuses the schema
//! image the last build stored; anything else images the databases again.
//! And the schema checks (V2) as raw passes.

mod common;

use common::built::Built;
use common::cache::entries;
use common::env_with_schema;
use serde_json::Value;
use tempfile::TempDir;

const STORE: &str = ".oxabl/build/schema-images.json";

fn records() -> Vec<Vec<String>> {
    serde_json::from_str(r#"[["db","ledger"],["table","customer"],["field","customer","name"]]"#)
        .unwrap()
}

fn workspace(workers: usize) -> Built {
    let workspace = Built::new();
    workspace.workers(&format!("count = {workers}"));
    workspace.databases(&["ledger"]);
    workspace.set_environment(&env_with_schema(records()));
    workspace.write("src/a.p", "MESSAGE \"a\".\n");
    workspace
}

/// Builds with `--report` and returns the report.
fn report(workspace: &Built) -> Value {
    let path = workspace.path("report.json");
    workspace
        .build_with(&["--report", &path.to_string_lossy()])
        .succeeded();
    serde_json::from_str(&workspace.read("report.json")).unwrap()
}

fn names(report: &Value, which: &str) -> Vec<String> {
    report["schema_images"][which]
        .as_array()
        .unwrap_or_else(|| panic!("no schema_images.{which} in {report}"))
        .iter()
        .map(|name| name.as_str().unwrap().to_string())
        .collect()
}

fn reused(report: &Value) -> bool {
    let (reused, imaged) = (names(report, "reused"), names(report, "imaged"));
    match (reused.as_slice(), imaged.as_slice()) {
        ([ldb], []) if ldb == "ledger" => true,
        ([], [ldb]) if ldb == "ledger" => false,
        _ => panic!("ledger is neither reused nor imaged: {report}"),
    }
}

/// The `SCHEMA` and `RAWDIGEST` requests the fake answered, from its log.
fn requests(workspace: &Built) -> (usize, usize) {
    let lines = workspace.log_lines();
    let count = |prefix: &str| {
        lines
            .iter()
            .filter(|(_, what)| what.starts_with(prefix))
            .count()
    };
    (count("schema ledger"), count("raw ledger"))
}

#[test]
fn an_equal_raw_digest_reuses_the_stored_image_and_images_nothing() {
    let workspace = workspace(1);

    let first = report(&workspace);
    let images = requests(&workspace).0;
    let second = report(&workspace);

    assert!(!reused(&first), "nothing is stored before the first build");
    assert!(reused(&second), "{second}");
    assert_eq!(requests(&workspace).0, images, "the second build imaged");
    assert_eq!(second["environment"], first["environment"]);
    assert_eq!(second["timing"]["setup"]["imaging_ms"], 0);
    assert!(
        second["timing"]["setup"]["raw_digest_ms"].is_u64(),
        "{second}"
    );
}

#[test]
fn a_raw_digest_that_changed_images_again_with_the_same_identity() {
    let workspace = workspace(1);
    let first = report(&workspace);

    workspace.env("FAKE_AVM_RAW_SALT", Some("a field the image leaves out"));
    let second = report(&workspace);
    let third = report(&workspace);

    assert!(!reused(&second));
    assert_eq!(second["environment"], first["environment"]);
    assert!(reused(&third), "the second build stored the new digest");
}

#[test]
fn a_schema_change_images_again_and_changes_the_identity() {
    let workspace = workspace(1);
    let first = report(&workspace);

    let mut changed = records();
    changed[2].push("_label=Name".to_string());
    workspace.set_environment(&env_with_schema(changed));
    let second = report(&workspace);

    assert!(!reused(&second));
    assert_ne!(second["environment"], first["environment"]);
}

#[test]
fn the_digest_does_not_depend_on_the_number_of_workers() {
    let workspace = workspace(1);
    report(&workspace);

    for workers in [2, 3, 8, 9] {
        workspace.workers(&format!("count = {workers}"));
        let next = report(&workspace);
        assert!(reused(&next), "{workers} workers: {next}");
    }
    let raw = workspace.log_lines();
    assert!(
        raw.iter().any(|(_, what)| what == "raw ledger 7/8"),
        "eight workers share the pass"
    );
    assert!(
        !raw.iter()
            .any(|(_, what)| what.starts_with("raw ") && what.ends_with("/9")),
        "a raw pass uses at most eight workers"
    );
}

/// Each broken answer of the raw pass on the second build: the build images
/// again, and does not fail.
#[test]
fn a_broken_raw_pass_images_again() {
    for fault in [
        "missing-piece",
        "extra-piece",
        "bounds",
        "bad-hash",
        "bad-count",
        "error",
        "exit",
    ] {
        for share in ["0", "1"] {
            let workspace = workspace(2);
            report(&workspace);

            workspace.env("FAKE_AVM_RAW_FAULT", Some(fault));
            workspace.env("FAKE_AVM_RAW_FAULT_SHARE", Some(share));
            let second = report(&workspace);

            assert!(!reused(&second), "{fault} in share {share}: {second}");
            assert_eq!(second["summary"]["compiled"], 0, "{fault}");
        }
    }
}

#[test]
fn a_lost_worker_in_the_raw_pass_of_the_only_worker_is_replaced() {
    let workspace = workspace(1);
    report(&workspace);

    workspace.env("FAKE_AVM_RAW_FAULT", Some("exit"));
    let second = report(&workspace);

    assert!(!reused(&second));
    assert!(
        second["notes"].to_string().contains("raw digest"),
        "{second}"
    );
}

/// A stored image whose binding differs in any field is void.
#[test]
fn a_stored_image_of_another_binding_is_void() {
    let workspace = workspace(1);
    report(&workspace);
    let original = workspace.read(STORE);

    for field in [
        "ldb",
        "physical",
        "guid",
        "worker",
        "image_version",
        "form",
        "openedge",
        "raw",
    ] {
        let mut stored: Value = serde_json::from_str(&original).unwrap();
        let value = stored["databases"][0][field].as_str().unwrap().to_string();
        stored["databases"][0][field] = Value::String(format!("{value}x"));
        workspace.write(STORE, &stored.to_string());

        let next = report(&workspace);

        assert!(
            !reused(&next),
            "a stored image with another {field} was used"
        );
    }
}

#[test]
fn a_stored_image_whose_entries_do_not_give_its_identity_is_void() {
    let workspace = workspace(1);
    report(&workspace);
    let mut stored: Value = serde_json::from_str(&workspace.read(STORE)).unwrap();
    stored["databases"][0]["entries"][0]["records"] = Value::from(99);
    workspace.write(STORE, &stored.to_string());

    assert!(!reused(&report(&workspace)));
}

#[test]
fn another_connection_or_openedge_version_images_again() {
    for (name, value) in [("guid", "guid-other"), ("physical", "/elsewhere/ledger")] {
        let workspace = workspace(1);
        report(&workspace);
        let mut env = env_with_schema(records());
        env["databases"][0][name] = Value::String(value.into());
        workspace.set_environment(&env);

        assert!(!reused(&report(&workspace)), "another {name}");
    }
    let workspace = workspace(1);
    report(&workspace);
    let mut env = env_with_schema(records());
    env["env"] = serde_json::json!({ "dlc_version": "OpenEdge Release 12.8.10" });
    workspace.set_environment(&env);

    assert!(!reused(&report(&workspace)), "another OpenEdge version");
}

#[test]
fn the_probe_images_every_database_and_makes_no_raw_pass() {
    let workspace = workspace(1);
    report(&workspace);
    let (images, raw) = requests(&workspace);

    let output = workspace.command(&["--probe", "--json"]).output().unwrap();

    assert_eq!(output.status.code(), Some(0));
    assert_eq!(requests(&workspace), (images + 1, raw));
}

/// A schema change between the raw pass before the imaging and the one after
/// it stores no image: the next build images again.
#[test]
fn a_change_during_the_start_imaging_stores_no_image() {
    let workspace = workspace(1);
    workspace.env("FAKE_AVM_RAW_CHANGE_FROM", Some("2"));
    let first = report(&workspace);
    workspace.env("FAKE_AVM_RAW_CHANGE_FROM", None);

    let second = report(&workspace);

    assert!(!reused(&first));
    assert!(
        !reused(&second),
        "an image the raw passes do not pin was stored"
    );
}

/// Four classes that compile one after the other, each checked on its own.
fn class_chain(workspace: &Built) {
    for n in 1..=4 {
        let parent = match n {
            1 => String::new(),
            _ => format!(" INHERITS pkg.C{}", n - 1),
        };
        workspace.write(
            &format!("src/pkg/C{n}.cls"),
            &format!("CLASS pkg.C{n}{parent}:\nEND CLASS.\n"),
        );
    }
}

fn holds(cache: &TempDir, artifact: &str) -> bool {
    entries(cache.path())
        .iter()
        .any(|entry| entry.join("files").join(artifact).is_file())
}

#[test]
fn a_schema_check_is_a_raw_pass_and_images_nothing() {
    let cache = TempDir::new().unwrap();
    let workspace = workspace(1);
    workspace.reconfigure(
        "include_paths = [\"src\"]",
        "[build.verify]\nimaging_budget = 0",
    );
    workspace.cache_in(cache.path());
    report(&workspace);
    class_chain(&workspace);
    let (images, raw) = requests(&workspace);

    let second = report(&workspace);

    assert!(reused(&second));
    assert_eq!(second["cache"]["stored"], 4, "{second}");
    let (images_now, raw_now) = requests(&workspace);
    assert_eq!(images_now, images, "a schema check imaged");
    assert!(raw_now >= raw + 2, "the start pass and at least one check");
}

#[test]
fn a_raw_digest_that_changes_during_the_build_stores_nothing_from_that_point() {
    let cache = TempDir::new().unwrap();
    let workspace = workspace(1);
    workspace.reconfigure(
        "include_paths = [\"src\"]",
        "[build.verify]\nimaging_budget = 0",
    );
    workspace.cache_in(cache.path());
    report(&workspace);
    class_chain(&workspace);
    let images = requests(&workspace).0;
    // The start pass of the worker is its first raw request and the check
    // after C1 its second; the check after C2 sees the change.
    workspace.env("FAKE_AVM_RAW_CHANGE_FROM", Some("3"));

    let run = workspace.build();

    run.succeeded();
    assert!(holds(&cache, "r/pkg/C1.r"), "released before the change");
    for later in ["C2", "C3", "C4"] {
        assert!(
            !holds(&cache, &format!("r/pkg/{later}.r")),
            "{later} was released after the change"
        );
    }
    assert!(run.stderr.contains("raw digest changed"), "{}", run.stderr);
    assert_eq!(requests(&workspace).0, images, "a schema check imaged");
}
