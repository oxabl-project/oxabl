//! `[build].rcode = "beside-source"`: each unit's r-code at its own path with
//! `.r`, which every runtime finds first. The oracle is the fake runtime with
//! a runtime PROPATH of the source directories, as the project has it; what a
//! test reads on disk only says where a file is.

mod common;

use std::fs;
use std::os::unix::fs::PermissionsExt;

use common::built::{Built, unit};
use common::fake_runtime;

/// A workspace whose r-code goes beside the source.
fn beside() -> Built {
    let workspace = Built::new();
    workspace.beside_source();
    workspace
}

/// The project of the runtime loop: a program, a program with an include and
/// a user of a class, each printing `<what> <version>`.
fn write_loop_sources(workspace: &Built, version: &str) {
    workspace.write(
        "src/hello.p",
        &format!("/* FAKE-SAY program {version} */\n"),
    );
    workspace.write("src/withinc.p", "{msg.i}\n");
    workspace.write("src/msg.i", &format!("/* FAKE-SAY include {version} */\n"));
    workspace.write(
        "src/pkg/Greet.cls",
        &format!("CLASS pkg.Greet:\n/* FAKE-SAY class {version} */\nEND CLASS.\n"),
    );
    workspace.write(
        "src/greet.p",
        "DEFINE VARIABLE o AS pkg.Greet NO-UNDO.\no = NEW pkg.Greet().\n",
    );
}

const PROGRAMS: [&str; 3] = ["hello.p", "withinc.p", "greet.p"];

/// What the three programs print when the project runs them on `propath`.
fn run_loop(workspace: &Built, propath: &[&str]) -> Vec<String> {
    PROGRAMS
        .iter()
        .flat_map(|program| {
            let ran = fake_runtime::run(workspace.root(), propath, program);
            assert_eq!(ran.code, 0, "{program}: {}{}", ran.stdout, ran.stderr);
            ran.said()
        })
        .collect()
}

/// The project's own tool compiles `name` with `COMPILE ... SAVE` beside its
/// source in `src`; returns the SHA-256 of the r-code it made.
fn old_tool(workspace: &Built, name: &str) -> String {
    fake_runtime::compile(workspace.root(), &["src"], name, "src");
    let rcode = format!("src/{}.r", name.rsplit_once('.').unwrap().0);
    workspace.sha256(&rcode)
}

/// The pair of the incident: the same project, the same runtime PROPATH of
/// the source directory alone, the same r-code of the project's own tool
/// beside the sources. In place, the edit runs.
#[test]
fn an_edit_runs_on_a_runtime_that_searches_only_the_sources() {
    let workspace = beside();
    write_loop_sources(&workspace, "v1");
    for name in ["pkg/Greet.cls", "hello.p", "withinc.p", "greet.p"] {
        old_tool(&workspace, name);
    }
    assert_eq!(
        run_loop(&workspace, &["src"]),
        ["program v1", "include v1", "class v1"]
    );
    write_loop_sources(&workspace, "v2");

    workspace.build().succeeded();

    assert_eq!(
        run_loop(&workspace, &["src"]),
        ["program v2", "include v2", "class v2"]
    );
    let ran = fake_runtime::run(workspace.root(), &["src"], "hello.p");
    assert_eq!(
        ran.loaded("hello.p"),
        Some(("src/hello.r".to_string(), "rcode".to_string()))
    );
    assert_eq!(
        ran.read_hash("src/hello.p"),
        Some(workspace.sha256("src/hello.p"))
    );
    assert!(!workspace.exists("build"), "no output directory");
    assert!(!workspace.exists(".oxabl/r"), "no output directory");
}

#[test]
fn the_unit_propath_has_no_output_entry() {
    let workspace = beside();
    workspace.write("src/hello.p", "MESSAGE \"hi\".\n");

    let plan = workspace.plan();

    let propaths = plan["propaths"].as_array().unwrap();
    assert!(!propaths.is_empty(), "{plan}");
    for propath in propaths {
        let text = propath.to_string();
        assert!(!text.contains("$OUT"), "{text}");
        assert!(text.contains("\"src\""), "{text}");
    }
}

#[test]
fn output_and_beside_source_together_are_refused() {
    let workspace = Built::new();
    workspace.rcode("beside-source");
    workspace.write("src/hello.p", "MESSAGE \"hi\".\n");

    for args in [&["--plan"][..], &[][..]] {
        let run = workspace.build_with(args);
        assert_eq!(run.code, 2, "{args:?}: {}", run.stderr);
        assert!(
            run.stderr.contains("[build].output") && run.stderr.contains("[build].rcode"),
            "{}",
            run.stderr
        );
    }
    assert!(!workspace.exists("src/hello.r"));
    assert!(!workspace.exists("build/r"));
}

#[test]
fn the_effective_configuration_shows_where_the_rcode_goes() {
    let workspace = beside();
    let run = workspace.build_with(&["--print-config"]);
    run.succeeded();
    let line = run
        .stdout
        .lines()
        .find(|line| line.starts_with("build.rcode "))
        .unwrap_or_else(|| panic!("{}", run.stdout));
    assert!(line.contains("\"beside-source\""), "{line}");
    assert!(line.ends_with("# oxabl.toml"), "{line}");

    let workspace = Built::new();
    let run = workspace.build_with(&["--print-config"]);
    run.succeeded();
    assert!(
        run.stdout
            .lines()
            .any(|line| line.starts_with("build.rcode ")
                && line.contains("\"directory\"")
                && line.ends_with("# default")),
        "{}",
        run.stdout
    );
}

/// The generation in which each stored analysis was made.
fn analysed_generations(workspace: &Built) -> Vec<(String, i64)> {
    let connection = rusqlite::Connection::open(workspace.path(".oxabl/build/state.db")).unwrap();
    let mut statement = connection
        .prepare(
            "SELECT p.path, a.analysed_gen FROM analyses a JOIN paths p ON p.id = a.path_id ORDER BY p.path",
        )
        .unwrap();
    statement
        .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))
        .unwrap()
        .map(Result::unwrap)
        .collect()
}

/// The directories that a case-blind lookup listed.
fn listed(workspace: &Built) -> Vec<String> {
    let connection = rusqlite::Connection::open(workspace.path(".oxabl/build/state.db")).unwrap();
    let mut statement = connection
        .prepare("SELECT p.path FROM listings l JOIN paths p ON p.id = l.path_id ORDER BY p.path")
        .unwrap();
    statement
        .query_map([], |row| row.get(0))
        .unwrap()
        .map(Result::unwrap)
        .collect()
}

/// The r-code that the first build writes into the source directories starts
/// no analysis and no compile: not through a directory that a case-blind
/// class lookup listed, not through `RUN x.r`, and not through a class whose
/// r-code now sits beside it.
#[test]
fn a_second_build_compiles_and_analyses_nothing() {
    let workspace = Built::with(
        "include_paths = [\"src\"]\ncase_insensitive_lookup = true",
        "",
    );
    workspace.beside_source();
    write_loop_sources(&workspace, "v1");
    workspace.write("src/blind.p", "DEFINE VARIABLE g AS pkg.greet NO-UNDO.\n");
    workspace.write("src/runr.p", "RUN hello.r.\n");
    workspace.build().succeeded();
    for rcode in [
        "hello.r",
        "withinc.r",
        "greet.r",
        "pkg/Greet.r",
        "blind.r",
        "runr.r",
    ] {
        assert!(workspace.exists(&format!("src/{rcode}")), "{rcode}");
    }
    let analysed = analysed_generations(&workspace);
    assert!(!analysed.is_empty());
    assert_eq!(
        listed(&workspace),
        ["src", "src/pkg"],
        "the case-blind class lookup lists both"
    );
    workspace.clear_log();

    workspace.build().succeeded();

    assert_eq!(
        workspace.compiled(),
        Vec::<String>::new(),
        "{}",
        workspace.plan()
    );
    assert_eq!(analysed_generations(&workspace), analysed);
    let plan = workspace.plan();
    assert_eq!(plan["summary"]["rebuild"], 0, "{plan}");
}

/// A directory linked into the sources is one place: a unit reached through
/// both spellings is one unit with one r-code file, so no build sees the
/// other spelling's compile as a change.
#[test]
fn a_linked_directory_is_one_place_for_rcode() {
    let workspace = beside();
    workspace.write("src/a/x.p", "/* FAKE-SAY x */\n");
    std::os::unix::fs::symlink("a", workspace.path("src/b")).unwrap();

    workspace.build().succeeded();
    workspace.clear_log();
    workspace.build().succeeded();

    assert_eq!(workspace.compiled(), Vec::<String>::new());
    let plan = workspace.plan();
    let units: Vec<&str> = plan["units"]
        .as_array()
        .unwrap()
        .iter()
        .map(|unit| unit["path"].as_str().unwrap())
        .collect();
    assert_eq!(units, ["src/a/x.p"]);
    for propath in ["src/a", "src/b"] {
        let ran = fake_runtime::run(workspace.root(), &[propath], "x.p");
        assert_eq!(ran.said(), ["x"]);
        assert_eq!(
            ran.loaded("x.p"),
            Some((format!("{propath}/x.r"), "rcode".to_string()))
        );
    }
}

/// `x.p` and `x.w` in one directory would write one `x.r`: `x.w` is skipped,
/// and the r-code beside it, which is `x.p`'s, stays. Units of one name in
/// two directories each have their own.
#[test]
fn units_collide_only_in_one_directory() {
    let workspace = beside();
    workspace.write("src/x.p", "/* FAKE-SAY p */\n");
    workspace.write("src/x.w", "/* FAKE-SAY w */\n");
    workspace.write("src/one/y.p", "/* FAKE-SAY one */\n");
    workspace.write("src/two/y.p", "/* FAKE-SAY two */\n");

    let plan = workspace.plan();

    assert!(
        unit(&plan, "src/x.w")
            .to_string()
            .contains("output-collision: src/x.p"),
        "{plan}"
    );
    for _ in 0..2 {
        workspace.build().succeeded();
        for (propath, program, says) in [
            ("src", "x.p", "p"),
            ("src/one", "y.p", "one"),
            ("src/two", "y.p", "two"),
        ] {
            let ran = fake_runtime::run(workspace.root(), &[propath], program);
            assert_eq!(ran.said(), [says], "{propath} {program}");
            assert_eq!(
                ran.loaded(program).unwrap().1,
                "rcode",
                "{propath} {program}"
            );
        }
    }
}

/// Two classes of one name below two package roots each compile, and each
/// has its own cross-reference: the output name of a unit is its path.
#[test]
fn classes_of_one_name_in_two_roots_keep_their_own_outputs() {
    let workspace = Built::with("root_relative_include_paths = [\".\"]", "");
    workspace.beside_source();
    workspace.build_keys("[build.outputs]\nxref = \"xref\"");
    for root in ["a", "b"] {
        workspace.write(
            &format!("src/{root}/pkg/Foo.cls"),
            &format!("CLASS pkg.Foo:\n/* FAKE-SAY {root} */\nEND CLASS.\n"),
        );
    }

    workspace.build().succeeded();

    for root in ["a", "b"] {
        let xref = format!("xref/src/{root}/pkg/Foo.cls.xref.xml");
        assert!(workspace.exists(&xref), "{xref}");
        assert!(
            workspace
                .read(&xref)
                .contains(&format!("src/{root}/pkg/Foo.cls")),
            "{xref}"
        );
        let ran = fake_runtime::start(
            workspace.root(),
            &[&format!("src/{root}")],
            common::runtime_cases::Start::Class("pkg.Foo"),
        );
        assert_eq!(ran.said(), [root]);
    }
}

/// A unit that does not compile where it is keeps no r-code of the project's
/// own tool: the runtime runs its source.
#[test]
fn a_skipped_unit_keeps_no_rcode() {
    let workspace = Built::with("include_paths = [\"src/a\", \"src/b\"]", "");
    workspace.beside_source();
    workspace.write("src/a/y.p", "/* FAKE-SAY a */\n");
    workspace.write("src/b/y.p", "/* FAKE-SAY old */\n");
    fake_runtime::compile(workspace.root(), &["src/b"], "y.p", "");
    assert!(workspace.exists("src/b/y.r"));
    workspace.write("src/b/y.p", "/* FAKE-SAY new */\n");

    let plan = workspace.plan();
    assert!(
        unit(&plan, "src/b/y.p")
            .to_string()
            .contains("shadowed-by: src/a/y.p"),
        "{plan}"
    );
    workspace.build().succeeded();

    let ran = fake_runtime::run(workspace.root(), &["src/b"], "y.p");
    assert_eq!(ran.said(), ["new"], "{}", ran.stdout);
    assert_eq!(ran.loaded("y.p").unwrap().1, "compiled-on-the-fly");
}

/// A unit that fails keeps no r-code: the runtime compiles its source and
/// never runs the r-code of the project's own tool.
#[test]
fn a_failed_unit_keeps_no_rcode() {
    let workspace = beside();
    workspace.write("src/bad.p", "/* FAKE-SAY old */\n");
    old_tool(&workspace, "bad.p");
    workspace.write("src/bad.p", "/* FAKE-SAY new */\n/* FAKE-FAIL */\n");

    let run = workspace.build();

    assert_eq!(run.code, 1, "{}", run.stderr);
    // The runtime compiles the source, which fails as the build did.
    let ran = fake_runtime::run(workspace.root(), &["src"], "bad.p");
    assert_ne!(ran.code, 0, "{}", ran.stdout);
    assert!(ran.said().is_empty(), "{}", ran.stdout);
    assert!(!ran.stdout.contains("src/bad.r"), "{}", ran.stdout);
}

/// A user that the analysis does not see use a class compiles before it. It
/// must not bind the class r-code of the project's own tool: the build
/// deletes the r-code of each class it compiles before anything compiles.
#[test]
fn a_user_that_compiles_first_binds_no_old_class_rcode() {
    let workspace = beside();
    workspace.write(
        "src/pkg/Late.cls",
        "CLASS pkg.Late:\n/* FAKE-SAY old */\nEND CLASS.\n",
    );
    let stale = old_tool(&workspace, "pkg/Late.cls");
    workspace.write(
        "src/pkg/Late.cls",
        "CLASS pkg.Late:\n/* FAKE-SAY new */\nEND CLASS.\n",
    );
    // The fake compiler sees the class in the comment; the analysis does not.
    workspace.write("src/a.p", "/* NEW pkg.Late */\n");

    workspace.build().succeeded();

    assert_eq!(
        workspace.compiled(),
        ["a.p", "pkg/Late.cls"],
        "the user compiles first"
    );
    let ran = fake_runtime::run(workspace.root(), &["src"], "a.p");
    assert_eq!(ran.code, 0, "{}{}", ran.stdout, ran.stderr);
    assert_ne!(
        ran.read_hash("src/pkg/Late.r").as_deref(),
        Some(stale.as_str()),
        "{}",
        ran.stdout
    );
    assert_eq!(
        ran.read_hash("src/pkg/Late.cls"),
        Some(workspace.sha256("src/pkg/Late.cls")),
        "{}",
        ran.stdout
    );
}

/// A cycle of two classes whose r-code of the project's own tool was compiled
/// against other versions of both: that r-code fails every compile that binds
/// it (error 12927). The build deletes it first, so the cycle compiles from
/// the sources in two passes and runs the new code.
#[test]
fn a_cycle_with_incompatible_old_rcode_compiles_from_the_sources() {
    let workspace = beside();
    let write = |version: &str| {
        workspace.write(
            "src/pkg/A.cls",
            &format!(
                "CLASS pkg.A:\n/* FAKE-SAY a {version} */\nDEFINE VARIABLE b AS pkg.B NO-UNDO.\nb = NEW pkg.B().\nEND CLASS.\n"
            ),
        );
        workspace.write(
            "src/pkg/B.cls",
            &format!(
                "CLASS pkg.B:\n/* FAKE-SAY b {version} */\nDEFINE VARIABLE a AS pkg.A NO-UNDO.\na = NEW pkg.A().\nEND CLASS.\n"
            ),
        );
    };
    write("v1");
    old_tool(&workspace, "pkg/A.cls");
    old_tool(&workspace, "pkg/B.cls");
    write("v2");

    let run = workspace.build();

    run.succeeded();
    assert!(!run.stderr.contains("12927"), "{}", run.stderr);
    let compiled = workspace.compiled();
    assert_eq!(
        compiled.iter().filter(|name| *name == "pkg/A.cls").count(),
        2,
        "two passes: {compiled:?}"
    );
    assert_eq!(
        compiled.iter().filter(|name| *name == "pkg/B.cls").count(),
        2,
        "two passes: {compiled:?}"
    );
    for (class, says) in [("pkg.A", ["a v2", "b v2"]), ("pkg.B", ["b v2", "a v2"])] {
        let ran = fake_runtime::start(
            workspace.root(),
            &["src"],
            common::runtime_cases::Start::Class(class),
        );
        assert_eq!(ran.said(), says, "{}{}", ran.stdout, ran.stderr);
    }
}

/// A source directory the build cannot write stops the build before any
/// compile, once, with the directory's name.
#[test]
fn a_read_only_source_directory_stops_the_build() {
    let workspace = beside();
    workspace.write("src/ro/x.p", "MESSAGE \"x\".\n");
    workspace.write("src/ro/y.p", "MESSAGE \"y\".\n");
    workspace.write("src/ok.p", "MESSAGE \"ok\".\n");
    let directory = workspace.path("src/ro");
    fs::set_permissions(&directory, fs::Permissions::from_mode(0o555)).unwrap();
    let writable = fs::write(directory.join("probe"), "").is_ok();

    let run = workspace.build();

    fs::set_permissions(&directory, fs::Permissions::from_mode(0o755)).unwrap();
    assert!(
        !writable,
        "this test needs a user that cannot write a read-only directory"
    );
    assert_eq!(run.code, 3, "{}", run.stderr);
    assert_eq!(run.stderr.matches("src/ro").count(), 1, "{}", run.stderr);
    assert_eq!(workspace.compiled(), Vec::<String>::new());
}

/// V3 with no output directory: a compile that names r-code, here the class
/// r-code beside a class in the closure, read a file outside the closure.
#[test]
fn rcode_a_compile_names_is_outside_the_closure() {
    let cache = tempfile::TempDir::new().unwrap();
    let workspace = beside();
    workspace.cache_in(cache.path());
    write_loop_sources(&workspace, "v1");
    workspace.build().succeeded();
    workspace.write(
        "src/reader.p",
        "/* FAKE-READ src/pkg/Greet.r */\nDEFINE VARIABLE o AS pkg.Greet NO-UNDO.\no = NEW pkg.Greet().\n",
    );
    workspace.clear_log();

    let run = workspace.build();

    run.succeeded();
    assert_eq!(workspace.compiled(), ["reader.p"]);
    assert!(
        run.stderr.contains("src/pkg/Greet.r") && run.stderr.contains("not in the closure"),
        "{}",
        run.stderr
    );
}

/// A unit outside the workspace root gets no r-code beside it: oxabl writes
/// nothing outside the root.
#[test]
fn a_unit_outside_the_root_is_skipped() {
    let parent = tempfile::TempDir::new().unwrap();
    let workspace = Built::new_in(parent.path());
    workspace.reconfigure(
        "directories = [\"src\", \"../ext\"]\nroot_relative_include_paths = [\".\"]",
        "",
    );
    workspace.beside_source();
    workspace.write("src/in.p", "/* FAKE-SAY in */\n");
    fs::create_dir_all(parent.path().join("ext")).unwrap();
    fs::write(parent.path().join("ext/out.p"), "/* FAKE-SAY out */\n").unwrap();

    let plan = workspace.plan();
    let outside = plan["units"]
        .as_array()
        .unwrap()
        .iter()
        .find(|unit| unit["path"].as_str().unwrap().ends_with("ext/out.p"))
        .unwrap_or_else(|| panic!("{plan}"));
    assert!(outside.to_string().contains("outside-root"), "{outside}");
    workspace.build().succeeded();

    assert!(!parent.path().join("ext/out.r").exists());
    let ran = fake_runtime::run(workspace.root(), &["src"], "in.p");
    assert_eq!(ran.said(), ["in"]);
}
