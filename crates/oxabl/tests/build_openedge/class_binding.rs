//! How the compiler binds a class name to a file, on a real AVM: r-code or
//! source, which search entry, which session. Each test sets up a class in two
//! versions that differ in one member, so that a user that calls the member
//! compiles (or fails with error 12927) according to the version it bound.

use std::fs;
use std::time::{Duration, SystemTime};

use serde_json::Value;

use crate::common::built::unit;
use crate::common::openedge::Target;
use crate::common::openedge::compile::{Compile, Options, compile_workspace_with};
use crate::setup;

/// ABL that compiles one file and prints `COMPILE <file> OK|ERR` and the
/// number of each message.
const COMP: &str = "PROCEDURE comp:
    DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcInto AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    OS-CREATE-DIR VALUE(pcInto).
    COMPILE VALUE(pcFile) SAVE INTO VALUE(pcInto) NO-ERROR.
    PUT UNFORMATTED \"COMPILE \" pcFile \" \" (IF COMPILER:ERROR OR ERROR-STATUS:ERROR THEN \"ERR\" ELSE \"OK\") SKIP.
    DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
        PUT UNFORMATTED \"  MSG \" ERROR-STATUS:GET-NUMBER(i) \" \" ERROR-STATUS:GET-MESSAGE(i) SKIP.
    END.
END PROCEDURE.
";

/// A `pkg.Foo` whose only difference from the other versions is the method
/// `Is<tag>`.
fn foo(tag: &str) -> String {
    format!(
        "CLASS pkg.Foo:\n    METHOD PUBLIC LOGICAL Is{tag}():\n        RETURN TRUE.\n    END METHOD.\nEND CLASS.\n"
    )
}

/// A procedure that calls `Is<tag>` on a `class`.
fn probe(class: &str, tag: &str) -> String {
    format!("DEFINE VARIABLE o AS {class} NO-UNDO.\no = NEW {class}().\no:Is{tag}().\n")
}

struct Lab {
    target: Target,
    workspace: Compile,
}

impl Lab {
    fn new() -> Lab {
        Lab::with(Options::default())
    }

    fn with(options: Options) -> Lab {
        let target = setup();
        let workspace = compile_workspace_with(&target, options);
        Lab { target, workspace }
    }

    /// Runs `body` in a fresh session whose PROPATH is `propath` (entries
    /// below the workspace), and returns what it printed.
    fn session(&self, propath: &[&str], multi: bool, body: &str) -> String {
        let worker = &self.workspace.worker;
        let entries: Vec<String> = propath.iter().map(|p| format!("{worker}/{p}")).collect();
        let program = format!(
            "{COMP}PROPATH = \"{}\".\nCOMPILER:MULTI-COMPILE = {multi}.\n{body}\nPUT UNFORMATTED \"DONE\" SKIP.\nQUIT.\n",
            entries.join(",")
        );
        let printed = self.workspace.run_program(&self.target, &program);
        assert!(
            printed.contains("DONE"),
            "the session did not finish:\n{printed}"
        );
        printed
    }

    /// The statement that compiles `file` (below the workspace) into `into`.
    fn comp(&self, file: &str, into: &str) -> String {
        let worker = &self.workspace.worker;
        format!("RUN comp(\"{worker}/{file}\", \"{worker}/{into}\").\n")
    }

    /// Writes a class of version `tag` as the file `path`.
    fn class(&self, path: &str, tag: &str) {
        self.workspace.write(path, &foo(tag));
    }

    /// Writes the r-code of a class of version `tag` as the file `path`.
    fn rcode(&self, path: &str, tag: &str) {
        let stage = format!("stage-{tag}");
        self.workspace
            .write(&format!("{stage}/pkg/Foo.cls"), &foo(tag));
        let printed = self.session(
            &[&stage],
            true,
            &self.comp(&format!("{stage}/pkg/Foo.cls"), &format!("{stage}/r")),
        );
        assert!(printed.contains("Foo.cls OK"), "{printed}");
        let to = self.workspace.path(path);
        fs::create_dir_all(to.parent().unwrap()).unwrap();
        fs::copy(self.workspace.path(&format!("{stage}/r/pkg/Foo.r")), to).unwrap();
    }

    /// Sets the modification time of a file to `seconds` after a fixed time.
    fn stamp(&self, relative: &str, seconds: u64) {
        let time = SystemTime::UNIX_EPOCH + Duration::from_secs(1_700_000_000 + seconds);
        fs::File::options()
            .write(true)
            .open(self.workspace.path(relative))
            .unwrap()
            .set_modified(time)
            .unwrap();
    }

    /// The `tags` whose probe on `pkg.Foo` compiles when the compiler resolves
    /// the class through `propath`. Each probe runs in its own fresh session.
    fn binds(&self, propath: &[&str], multi: bool, tags: &[&str]) -> Vec<String> {
        self.binds_class("pkg.Foo", propath, multi, tags)
    }

    fn binds_class(
        &self,
        class: &str,
        propath: &[&str],
        multi: bool,
        tags: &[&str],
    ) -> Vec<String> {
        tags.iter()
            .filter(|tag| {
                self.probe(class, propath, multi, tag)
                    .contains(&format!("Is{tag}.p OK"))
            })
            .map(|tag| tag.to_string())
            .collect()
    }

    /// What a fresh session prints when it compiles the probe of `tag` on `class`.
    fn probe(&self, class: &str, propath: &[&str], multi: bool, tag: &str) -> String {
        let file = format!("probe/{class}-Is{tag}.p");
        self.workspace.write(&file, &probe(class, tag));
        let printed = self.session(propath, multi, &self.comp(&file, "probe-r"));
        eprintln!(
            "EVIDENCE {class} Is{tag} through {propath:?}: {}",
            printed.trim()
        );
        printed
    }

    /// Compiles each of `files` in one session and returns, for each, the files
    /// that the `FileID` log names as opened during that compile, below the
    /// workspace. The compiler's own temporary r-code is left out.
    fn opened(&self, propath: &[&str], multi: bool, files: &[&str]) -> Vec<Vec<String>> {
        let worker = &self.workspace.worker;
        let mut body = String::new();
        for (index, file) in files.iter().enumerate() {
            body.push_str(&format!(
                "LOG-MANAGER:LOGFILE-NAME = \"{worker}/fileid-{index}.log\".\nLOG-MANAGER:LOG-ENTRY-TYPES = \"FileID:2\".\n{}LOG-MANAGER:CLOSE-LOG().\n",
                self.comp(file, "probe-r")
            ));
        }
        let printed = self.session(propath, multi, &body);
        for file in files {
            assert!(printed.contains(&format!("{file} OK")), "{printed}");
        }
        (0..files.len())
            .map(|index| {
                self.workspace
                    .read(&format!("fileid-{index}.log"))
                    .lines()
                    .filter_map(|line| line.split_once("FILEID").map(|(_, rest)| rest.trim()))
                    .filter_map(|rest| rest.strip_prefix("Open "))
                    .filter_map(|rest| rest.rsplit_once(" ID=").map(|(path, _)| path))
                    .filter_map(|path| path.strip_prefix(&format!("{worker}/")))
                    .map(String::from)
                    .collect()
            })
            .collect()
    }
}

const BOTH: [&str; 2] = ["Stale", "Current"];

/// Control: an entry with the source alone binds the source.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn class_source_alone_binds() {
    let lab = Lab::new();
    lab.class("a/pkg/Foo.cls", "Current");
    assert_eq!(lab.binds(&["out", "a"], true, &BOTH), ["Current"]);
}

/// E5b and E3: no r-code of the class in an earlier entry, and one entry holds
/// stale `pkg/Foo.r` beside the current `pkg/Foo.cls`. The compiler binds the
/// r-code, whichever file is newer. For rule U2: stray class r-code in a source
/// entry is bound when no earlier entry (such as `$OUT`) holds the class's own
/// r-code, so U2 cannot be dropped for that case.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn stale_rcode_beside_source_binds_without_out() {
    let lab = Lab::new();
    lab.class("a/pkg/Foo.cls", "Current");
    lab.rcode("a/pkg/Foo.r", "Stale");
    for (source, rcode) in [(100, 0), (0, 100)] {
        lab.stamp("a/pkg/Foo.cls", source);
        lab.stamp("a/pkg/Foo.r", rcode);
        for multi in [true, false] {
            assert_eq!(
                lab.binds(&["out", "a"], multi, &BOTH),
                ["Stale"],
                "source stamp {source}, r-code stamp {rcode}, multi {multi}"
            );
        }
    }
}

/// E5a: `$OUT` holds the r-code of the class and a later entry holds stale
/// r-code beside the current source: the `$OUT` r-code binds. For U2: stray
/// r-code is harmless when `$OUT` holds the class's r-code and comes first.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn out_rcode_beats_stale_rcode_and_source() {
    let lab = Lab::new();
    lab.rcode("out/pkg/Foo.r", "Out");
    lab.class("a/pkg/Foo.cls", "Current");
    lab.rcode("a/pkg/Foo.r", "Stale");
    let tags = ["Out", "Stale", "Current"];
    assert_eq!(lab.binds(&["out", "a"], true, &tags), ["Out"]);
    assert_eq!(lab.binds(&["out", "a"], false, &tags), ["Out"]);
}

/// E5c: an earlier entry with only stale r-code beats a later entry with the
/// source, and an earlier entry with only the source beats a later entry with
/// stale r-code: the first entry that holds either file decides.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn first_entry_with_the_class_decides() {
    let lab = Lab::new();
    lab.rcode("a/pkg/Foo.r", "Stale");
    lab.class("b/pkg/Foo.cls", "Current");
    assert_eq!(lab.binds(&["out", "a", "b"], true, &BOTH), ["Stale"]);
    assert_eq!(lab.binds(&["out", "b", "a"], true, &BOTH), ["Current"]);
}

/// E5d: a name that differs from the file name only in case is not found, for
/// source and for r-code, in the directory or in the file name. For the
/// resolution rule: on Linux the match is exact, so a case-only match never
/// binds; a name that differs in case from the class it finds would bind to
/// another class.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn class_file_name_case_is_exact() {
    let lab = Lab::new();
    lab.class("a/Pkg/foo.cls", "Current");
    lab.class("b/pkg/Foo.cls", "Current");
    lab.rcode("c/Pkg/foo.r", "Stale");
    lab.rcode("d/pkg/Foo.r", "Stale");
    for (class, entry, tag) in [
        ("pkg.Foo", "a", "Current"),
        ("PKG.FOO", "b", "Current"),
        ("pkg.Foo", "c", "Stale"),
        ("PKG.FOO", "d", "Stale"),
    ] {
        let printed = lab.probe(class, &["out", entry], true, tag);
        assert!(
            printed.contains("ERR") && printed.contains("5638"),
            "{class} in {entry}: {printed}"
        );
    }
}

/// E1 and E3: `$OUT` holds the r-code of `pkg.Foo`, a later entry holds a
/// source of the class with another member. In a fresh session the compiler
/// binds the `$OUT` r-code, with `MULTI-COMPILE` on and off, whichever file is
/// newer. For blockers 1 and 2: a user that needs the later source's class
/// fails with error 12927, so one class name must give one file under every
/// PROPATH of the build, and the earlier failure of
/// `per_unit_propath_class_binding` is the session cache only when two classes
/// are bound in one session.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn out_rcode_beats_a_later_source_in_a_fresh_session() {
    let lab = Lab::new();
    lab.rcode("out/pkg/Foo.r", "Out");
    lab.class("b/pkg/Foo.cls", "Current");
    for (source, rcode) in [(100, 0), (0, 100)] {
        lab.stamp("b/pkg/Foo.cls", source);
        lab.stamp("out/pkg/Foo.r", rcode);
        for multi in [true, false] {
            assert_eq!(
                lab.binds(&["out", "b"], multi, &["Out", "Current"]),
                ["Out"],
                "source stamp {source}, r-code stamp {rcode}, multi {multi}"
            );
        }
    }
    let printed = lab.probe("pkg.Foo", &["out", "b"], false, "Current");
    assert!(printed.contains("12927"), "{printed}");
}

fn foo_with_include() -> String {
    "CLASS pkg.Foo:\n    {x.i}\nEND CLASS.\n".to_string()
}

fn include(tag: &str) -> String {
    format!("METHOD PUBLIC LOGICAL Is{tag}():\n    RETURN TRUE.\nEND METHOD.\n")
}

/// E2: the class `pkg.Foo` includes `x.i`; its r-code in `$OUT` was built with
/// a `x.i` that defines `IsV1`, and a later entry holds a `pkg/Foo.cls` and an
/// `x.i` that defines `IsV2`. A plain user and a class that inherits `pkg.Foo`,
/// compiled in a fresh session, bind the r-code: `IsV1` exists, `IsV2` does
/// not, and the `FileID` log names the r-code but neither `pkg/Foo.cls` nor
/// `x.i`. Without the r-code the same compiles read `IsV2` from the source.
/// For blockers 1 and 2: a class unit that is in `$OUT` before its users is
/// read as r-code alone, so its includes need not be uniform across the
/// PROPATHs of its users.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn class_unit_users_read_only_its_rcode() {
    let lab = Lab::new();
    for version in ["v1", "v2"] {
        lab.workspace
            .write(&format!("{version}/pkg/Foo.cls"), &foo_with_include());
        lab.workspace
            .write(&format!("{version}/x.i"), &include(&version.to_uppercase()));
    }
    lab.workspace
        .write("d/D.cls", "CLASS D INHERITS pkg.Foo:\nEND CLASS.\n");
    let printed = lab.session(&["v1"], true, &lab.comp("v1/pkg/Foo.cls", "out"));
    assert!(printed.contains("Foo.cls OK"), "{printed}");
    let tags = ["V1", "V2"];

    assert_eq!(lab.binds(&["out", "v2"], true, &tags), ["V1"]);
    assert_eq!(
        lab.binds_class("D", &["out", "v2", "d"], true, &tags),
        ["V1"]
    );
    lab.workspace.write("probe/D.p", &probe("D", "V1"));
    let reads = lab.opened(&["out", "v2", "d"], true, &["probe/D.p"]);
    assert!(reads[0].contains(&"out/pkg/Foo.r".to_string()), "{reads:?}");
    assert!(reads[0].contains(&"d/D.cls".to_string()), "{reads:?}");
    for source in ["v2/pkg/Foo.cls", "v2/x.i", "v1/pkg/Foo.cls", "v1/x.i"] {
        assert!(
            !reads[0].contains(&source.to_string()),
            "{source}: {reads:?}"
        );
    }

    lab.workspace.remove("v2/pkg/Foo.cls");
    assert_eq!(
        lab.binds_class("D", &["out", "v2", "d"], true, &tags),
        ["V1"]
    );

    lab.workspace.write("v2/pkg/Foo.cls", &foo_with_include());
    assert_eq!(lab.binds_class("D", &["v2", "d"], true, &tags), ["V2"]);
    assert_eq!(lab.binds(&["v2"], true, &tags), ["V2"]);
}

/// E6: the `FileID` log entry type of `LOG-MANAGER` names every file the
/// compiler opens during a `COMPILE`: the unit, a definitions-only include
/// (which `XREF-XML` does not list), a class source with its includes, and a
/// class r-code. But a class that an earlier compile of the same session
/// bound is not opened again, with `MULTI-COMPILE` on or off. For blockers 1, 2
/// and 6: the log is a complete read set of a compile only for the first
/// compile in a fresh session that binds each class.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn fileid_log_names_compiler_reads() {
    let lab = Lab::new();
    lab.workspace.write("v1/pkg/Foo.cls", &foo_with_include());
    lab.workspace.write("v1/x.i", &include("V1"));
    lab.workspace.write("v1/defs.i", "&GLOBAL-DEFINE D 1\n");
    lab.workspace
        .write("v1/pkg/Bar.cls", "CLASS pkg.Bar:\nEND CLASS.\n");
    let user = "{defs.i}\nDEFINE VARIABLE o AS pkg.Foo NO-UNDO.\nDEFINE VARIABLE q AS pkg.Bar NO-UNDO.\no = NEW pkg.Foo().\nq = NEW pkg.Bar().\no:IsV1().\n";
    lab.workspace.write("probe/one.p", user);
    lab.workspace.write("probe/two.p", user);
    let printed = lab.session(&["v1"], true, &lab.comp("v1/pkg/Bar.cls", "out"));
    assert!(printed.contains("Bar.cls OK"), "{printed}");

    for multi in [true, false] {
        let reads = lab.opened(&["out", "v1"], multi, &["probe/one.p", "probe/two.p"]);
        eprintln!("EVIDENCE fileid multi {multi}: {reads:?}");
        for name in [
            "probe/one.p",
            "v1/defs.i",
            "v1/pkg/Foo.cls",
            "v1/x.i",
            "out/pkg/Bar.r",
        ] {
            assert!(
                reads[0].contains(&name.to_string()),
                "multi {multi}, first compile lacks {name}: {reads:?}"
            );
        }
        assert!(
            !reads[0].contains(&"v1/pkg/Bar.cls".to_string()),
            "{reads:?}"
        );
        assert!(reads[1].contains(&"v1/defs.i".to_string()), "{reads:?}");
        for name in ["v1/pkg/Foo.cls", "v1/x.i", "out/pkg/Bar.r"] {
            assert!(
                !reads[1].contains(&name.to_string()),
                "multi {multi}, second compile reads {name}: {reads:?}"
            );
        }
    }
}

/// The statuses of `oxabl build` for the units `names`, and the report.
fn build_statuses(lab: &Lab, names: &[&str]) -> (Vec<String>, Value) {
    let output = lab.workspace.oxabl(&["--json"]).output().unwrap();
    let report: Value = serde_json::from_slice(&output.stdout).unwrap_or_else(|e| {
        panic!(
            "{e}: {}\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
    });
    let statuses = names
        .iter()
        .map(|name| unit(&report, name)["status"].as_str().unwrap().to_string())
        .collect();
    (statuses, report)
}

/// E5a through `oxabl build`: the class is a unit, so its r-code is in `$OUT`
/// before any user compiles; the source directory also holds stale
/// `pkg/Foo.r` beside the current `pkg/Foo.cls`. The user of the current
/// member compiles and the user of the stale member fails with error 12927:
/// the build binds the class it compiled into `$OUT`. For U2: stray class
/// r-code in a source entry is harmless for a class unit, which is compiled into
/// `$OUT` before every user.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn build_class_unit_binds_out_not_stray_rcode() {
    let lab = Lab::with(Options {
        sources: "directories = [\"a\"]\nroot_relative_include_paths = [\".\"]".into(),
        workers: 1,
        ..Options::default()
    });
    lab.class("a/pkg/Foo.cls", "Current");
    lab.rcode("a/pkg/Foo.r", "Stale");
    lab.workspace
        .write("a/ucur.p", &probe("pkg.Foo", "Current"));
    lab.workspace.write("a/uold.p", &probe("pkg.Foo", "Stale"));

    let (statuses, report) = build_statuses(&lab, &["a/pkg/Foo.cls", "a/ucur.p", "a/uold.p"]);

    assert_eq!(statuses, ["ok", "ok", "fail"], "{report}");
    assert!(
        unit(&report, "a/uold.p")["messages"]
            .to_string()
            .contains("12927"),
        "{report}"
    );
    assert!(lab.workspace.exists("build/r/pkg/Foo.r"));
}

/// E5b through `oxabl build`: the class is not a unit (its directory is only an
/// include path), so `$OUT` never holds its r-code, and that directory holds
/// stale `pkg/Foo.r` beside the current `pkg/Foo.cls`. The users bind the
/// stale r-code: the user of the stale member compiles and the user of the
/// current member fails with error 12927. For U2: stray class r-code beside a
/// class that is not a unit changes what compiles, so U2 must hold for it.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn build_class_that_is_not_a_unit_binds_stray_rcode() {
    let lab = Lab::with(Options {
        sources: "directories = [\"a\"]\nroot_relative_include_paths = [\".\"]\ninclude_paths = [\"lib\"]".into(),
        workers: 1,
        ..Options::default()
    });
    lab.class("lib/pkg/Foo.cls", "Current");
    lab.rcode("lib/pkg/Foo.r", "Stale");
    lab.workspace
        .write("a/ucur.p", &probe("pkg.Foo", "Current"));
    lab.workspace.write("a/uold.p", &probe("pkg.Foo", "Stale"));

    let (statuses, report) = build_statuses(&lab, &["a/ucur.p", "a/uold.p"]);

    assert_eq!(statuses, ["fail", "ok"], "{report}");
    assert!(
        unit(&report, "a/ucur.p")["messages"]
            .to_string()
            .contains("12927"),
        "{report}"
    );
    assert!(!lab.workspace.exists("build/r/pkg/Foo.r"));
}

/// The session cache of a class. A compile binds `pkg.Foo` through the first
/// PROPATH; then the PROPATH is set again, to the same value or to an entry
/// with another `pkg/Foo.cls` (or `pkg/Foo.r`), and a user of the other class's
/// member compiles. The same PROPATH keeps the first class (error 12927); a
/// changed PROPATH binds the class again, for source and for r-code, with
/// `MULTI-COMPILE` on and off. For blocker 2: a session does not carry a class
/// across a PROPATH change on this version, so the failure that
/// `per_unit_propath_class_binding` records comes from the `$OUT` r-code of
/// the first class (`out_rcode_beats_a_later_source_in_a_fresh_session`), not
/// from the session. A fresh session per PROPATH id stays the cautious choice:
/// this behaviour is measured, not documented.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn session_binds_a_class_again_after_a_propath_change() {
    let lab = Lab::new();
    lab.class("a/pkg/Foo.cls", "A");
    lab.class("b/pkg/Foo.cls", "B");
    lab.rcode("ar/pkg/Foo.r", "A");
    lab.rcode("br/pkg/Foo.r", "B");
    lab.workspace.write("probe/IsA.p", &probe("pkg.Foo", "A"));
    lab.workspace.write("probe/IsB.p", &probe("pkg.Foo", "B"));
    let worker = lab.workspace.worker.clone();
    for (first, second, rebinds) in [
        ("a", "b", true),
        ("ar", "br", true),
        ("a", "a", false),
        ("ar", "ar", false),
    ] {
        for multi in [true, false] {
            let body = format!(
                "{}PROPATH = \"{worker}/{second}\".\n{}",
                lab.comp("probe/IsA.p", "probe-r"),
                lab.comp("probe/IsB.p", "probe-r")
            );
            let printed = lab.session(&[first], multi, &body);
            let what = format!("{first} then {second}, multi {multi}: {printed}");
            eprintln!("EVIDENCE {what}");
            if rebinds {
                assert!(printed.contains("IsB.p OK"), "{what}");
            } else {
                assert!(printed.contains("IsB.p ERR"), "{what}");
                assert!(printed.contains("12927"), "{what}");
            }
        }
    }
}

/// A class of a cycle: `pkg.<me>` has `New<me>` and calls `New<other>` on a
/// `pkg.<other>`. The stale version has only `Old<me>` and names no class.
fn cycle_class(me: &str, other: &str, stale: bool) -> String {
    match stale {
        true => format!(
            "CLASS pkg.{me}:\n    METHOD PUBLIC LOGICAL Old{me}():\n        RETURN TRUE.\n    END METHOD.\nEND CLASS.\n"
        ),
        false => format!(
            "CLASS pkg.{me}:\n    METHOD PUBLIC LOGICAL New{me}():\n        RETURN TRUE.\n    END METHOD.\n    METHOD PUBLIC LOGICAL Use{other}():\n        DEFINE VARIABLE o AS pkg.{other} NO-UNDO.\n        o = NEW pkg.{other}().\n        RETURN o:New{other}().\n    END METHOD.\nEND CLASS.\n"
        ),
    }
}

impl Lab {
    /// The stale r-code of `pkg.<me>` as the file `path`.
    fn stale_cycle_rcode(&self, me: &str, path: &str) {
        let stage = format!("stale-{me}");
        self.workspace
            .write(&format!("{stage}/pkg/{me}.cls"), &cycle_class(me, "", true));
        let printed = self.session(
            &[&stage],
            true,
            &self.comp(&format!("{stage}/pkg/{me}.cls"), &format!("{stage}/r")),
        );
        assert!(printed.contains(&format!("{me}.cls OK")), "{printed}");
        let to = self.workspace.path(path);
        fs::create_dir_all(to.parent().unwrap()).unwrap();
        fs::copy(self.workspace.path(&format!("{stage}/r/pkg/{me}.r")), to).unwrap();
    }

    /// A cycle `pkg.A` <-> `pkg.B` below `dir`, with stale r-code beside the
    /// sources when `stray`, and stale r-code in `dir/out` when `warm`.
    fn cycle(&self, dir: &str, stray: bool, warm: bool) {
        for (me, other) in [("A", "B"), ("B", "A")] {
            self.workspace.write(
                &format!("{dir}/src/pkg/{me}.cls"),
                &cycle_class(me, other, false),
            );
            if stray {
                self.stale_cycle_rcode(me, &format!("{dir}/src/pkg/{me}.r"));
            }
            if warm {
                self.stale_cycle_rcode(me, &format!("{dir}/out/pkg/{me}.r"));
            }
        }
        fs::create_dir_all(self.workspace.path(&format!("{dir}/out"))).unwrap();
    }
}

/// A cycle: `pkg.A` and `pkg.B` each call a member that only the current
/// version of the other has. Stale r-code of the other class, beside its
/// source (`cold`) or in the output directory (`warm`), is bound first: both
/// compiles fail, also in a second pass in the same session and in a fresh
/// session, because a failed compile writes no r-code. With no r-code first,
/// each compile reads the other class's source. The compiler has no rule of
/// its own for a cycle.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn a_cycle_binds_the_first_rcode_it_finds_for_the_other_class() {
    let lab = Lab::new();
    for (name, stray, warm, ok) in [
        ("cold", true, false, false),
        ("warm", false, true, false),
        ("clean", false, false, true),
    ] {
        lab.cycle(name, stray, warm);
        let a = lab.comp(&format!("{name}/src/pkg/A.cls"), &format!("{name}/stage"));
        let b = lab.comp(&format!("{name}/src/pkg/B.cls"), &format!("{name}/stage"));
        let propath = [format!("{name}/out"), format!("{name}/src")];
        let propath = [propath[0].as_str(), propath[1].as_str()];
        let one = lab.session(&propath, true, &format!("{a}{b}{a}{b}"));
        let fresh = lab.session(&propath, true, &format!("{a}{b}"));
        eprintln!("EVIDENCE cycle {name}: {one} / {fresh}");
        let (status, count) = match ok {
            true => (".cls OK", 6),
            false => (".cls ERR", 6),
        };
        assert_eq!(
            one.matches(status).count() + fresh.matches(status).count(),
            count,
            "{name}: {one}\n{fresh}"
        );
        if !ok {
            assert!(
                one.contains("12927") && fresh.contains("12927"),
                "{name}: {one}"
            );
        }
    }
}

/// The r-code of pass 1 and pass 2 of `A` and `B`.
type Cycle = (Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>);

/// R-code to put in place before a pass, by class.
type Placed<'a> = Vec<(&'a str, &'a [u8])>;

/// The r-code of a cycle member against what the compile bound for the other
/// class, under the r-code oracle (masks from clean compiles of the cycle).
/// Pass 1 compiles both into `$OUT`, pass 2 compiles both again into another
/// directory. With r-code first that an older compile made from a compatible
/// `B` (one more method), beside the source or in `$OUT`, pass 1 gives an
/// `A.r` that differs from a clean compile: a compile depends on the bytes of
/// the r-code it binds. Pass 2 in a fresh session, or in the same session
/// after PROPATH is set to `""` and back, gives clean r-code: it binds the
/// r-code of pass 1, whatever pass 1 bound. In the same session without that
/// reset, also after `MULTI-COMPILE` is turned off and on, the class bound in
/// pass 1 stays bound while its file is unchanged: stray r-code stays, r-code
/// in `$OUT` that pass 1 replaced is read again. For the passes over a job of
/// `oxabl build` that holds a cycle.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn a_second_pass_after_a_propath_reset_binds_only_this_builds_rcode() {
    use crate::common::openedge::mask::Mask;
    let lab = Lab::new();
    let read = |path: &str| fs::read(lab.workspace.path(path)).unwrap();
    // A compatible B' with one more method, as r-code.
    lab.workspace
        .write("bp/src/pkg/A.cls", &cycle_class("A", "B", false));
    lab.workspace.write(
        "bp/src/pkg/B.cls",
        &cycle_class("B", "A", false).replace(
            "END CLASS.",
            "    METHOD PUBLIC LOGICAL Extra():\n        RETURN TRUE.\n    END METHOD.\nEND CLASS.",
        ),
    );
    let printed = lab.session(&["bp/src"], true, &lab.comp("bp/src/pkg/B.cls", "bp/r"));
    assert!(printed.contains("B.cls OK"), "{printed}");
    let bprime = read("bp/r/pkg/B.r");
    // One case in the directory `c`: the sources, then `stray` beside them
    // and `out` in the output directory; pass 1 into `c/out`, then pass 2
    // into `c/p2` in a fresh session (`fresh`) or in the same one after
    // `reset`. Returns pass 1 and pass 2 r-code of A and B.
    let case = |stray: &[(&str, &[u8])], out: &[(&str, &[u8])], fresh: bool, reset: &str| {
        let _ = fs::remove_dir_all(lab.workspace.path("c"));
        for (me, other) in [("A", "B"), ("B", "A")] {
            lab.workspace.write(
                &format!("c/src/pkg/{me}.cls"),
                &cycle_class(me, other, false),
            );
        }
        fs::create_dir_all(lab.workspace.path("c/out/pkg")).unwrap();
        for (me, bytes) in stray {
            fs::write(lab.workspace.path(&format!("c/src/pkg/{me}.r")), bytes).unwrap();
        }
        for (me, bytes) in out {
            fs::write(lab.workspace.path(&format!("c/out/pkg/{me}.r")), bytes).unwrap();
        }
        std::thread::sleep(Duration::from_millis(1200));
        let both = |into: &str| {
            format!(
                "{}{}",
                lab.comp("c/src/pkg/A.cls", into),
                lab.comp("c/src/pkg/B.cls", into)
            )
        };
        let propath = ["c/out", "c/src"];
        let printed = match fresh {
            true => {
                let one = lab.session(&propath, true, &both("c/out"));
                one + &lab.session(&propath, true, &both("c/p2"))
            }
            false => lab.session(
                &propath,
                true,
                &format!("{}{reset}{}", both("c/out"), both("c/p2")),
            ),
        };
        assert_eq!(printed.matches(".cls OK").count(), 4, "{printed}");
        (
            read("c/out/pkg/A.r"),
            read("c/out/pkg/B.r"),
            read("c/p2/pkg/A.r"),
            read("c/p2/pkg/B.r"),
        )
    };
    let samples: Vec<_> = (0..4).map(|_| case(&[], &[], true, "")).collect();
    let mask = |pick: &dyn Fn(&Cycle) -> Vec<u8>| {
        let first = pick(&samples[0]);
        let others: Vec<Vec<u8>> = samples[1..].iter().map(pick).collect();
        let refs: Vec<&[u8]> = others.iter().map(Vec::as_slice).collect();
        (first.clone(), Mask::from_samples(&first, &refs))
    };
    let (a1, ma1) = mask(&|s| s.0.clone());
    let (b1, mb1) = mask(&|s| s.1.clone());
    let (a2, ma2) = mask(&|s| s.2.clone());
    let (b2, mb2) = mask(&|s| s.3.clone());
    assert!(ma1.equivalent(&a1, &a2).is_ok() && mb1.equivalent(&b1, &b2).is_ok());
    eprintln!(
        "EVIDENCE clean: pass 2 vs pass 1 A {:?} B {:?}; mask sizes {} {}",
        ma1.equivalent(&a1, &a2),
        mb1.equivalent(&b1, &b2),
        ma1.len(),
        ma2.len()
    );
    let multi = "COMPILER:MULTI-COMPILE = FALSE.\nCOMPILER:MULTI-COMPILE = TRUE.\n";
    let propath = "DEFINE VARIABLE cKeep AS CHARACTER NO-UNDO.\ncKeep = PROPATH.\nPROPATH = \"\".\nPROPATH = cKeep.\n";
    let cases: [(&str, Placed, Placed); 3] = [
        ("current stray", vec![("A", &a1), ("B", &b1)], vec![]),
        (
            "compatible stray B",
            vec![("A", &a1), ("B", &bprime)],
            vec![],
        ),
        ("compatible B in out", vec![], vec![("B", &bprime)]),
    ];
    for (name, stray, out) in &cases {
        for (session, fresh, reset) in [
            ("fresh", true, ""),
            ("same", false, ""),
            ("same+reset", false, multi),
            ("same+propath", false, propath),
        ] {
            let got = case(stray, out, fresh, reset);
            let current = matches!(*name, "current stray");
            let stays = *name == "compatible stray B" && matches!(session, "same" | "same+reset");
            assert_eq!(
                ma1.equivalent(&a1, &got.0).is_ok(),
                current,
                "{name} {session}: pass 1 A"
            );
            assert!(
                mb1.equivalent(&b1, &got.1).is_ok(),
                "{name} {session}: pass 1 B"
            );
            assert_eq!(
                ma2.equivalent(&a2, &got.2).is_ok(),
                !stays,
                "{name} {session}: pass 2 A"
            );
            assert!(
                mb2.equivalent(&b2, &got.3).is_ok(),
                "{name} {session}: pass 2 B"
            );
            eprintln!(
                "EVIDENCE {name} / pass 2 {session}: pass 1 A {:?} B {:?}; pass 2 A {:?} B {:?}",
                ma1.equivalent(&a1, &got.0),
                mb1.equivalent(&b1, &got.1),
                ma2.equivalent(&a2, &got.2),
                mb2.equivalent(&b2, &got.3)
            );
        }
    }
}

/// `oxabl build` over a cycle of class units with r-code beside both sources
/// that an older compile made: `A.r` current, `B.r` from a compatible `B` with
/// one more method. The job compiles in passes, so every r-code is equivalent
/// to a clean build's, and every unit is keyed.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn build_cycle_with_stray_rcode_gives_clean_rcode() {
    use crate::common::openedge::mask::Masks;
    let write = |lab: &Lab| {
        lab.workspace
            .write("src/pkg/A.cls", &cycle_class("A", "B", false));
        lab.workspace
            .write("src/pkg/B.cls", &cycle_class("B", "A", false));
        lab.workspace.write(
            "src/user.p",
            "DEFINE VARIABLE o AS pkg.A NO-UNDO.\no = NEW pkg.A().\nMESSAGE o:UseB().\n",
        );
    };
    let clean = Lab::new();
    write(&clean);
    let masks = Masks::record(&clean.workspace, 4);

    let lab = Lab::new();
    write(&lab);
    lab.workspace
        .write("bp/pkg/A.cls", &cycle_class("A", "B", false));
    lab.workspace.write(
        "bp/pkg/B.cls",
        &cycle_class("B", "A", false).replace(
            "END CLASS.",
            "    METHOD PUBLIC LOGICAL Extra():\n        RETURN TRUE.\n    END METHOD.\nEND CLASS.",
        ),
    );
    let printed = lab.session(&["bp"], true, &lab.comp("bp/pkg/B.cls", "bp-r"));
    assert!(printed.contains("B.cls OK"), "{printed}");
    fs::copy(
        lab.workspace.path("bp-r/pkg/B.r"),
        lab.workspace.path("src/pkg/B.r"),
    )
    .unwrap();
    fs::write(lab.workspace.path("src/pkg/A.r"), &masks.first["pkg/A.r"]).unwrap();

    let plan = lab.workspace.plan();
    for path in ["src/pkg/A.cls", "src/pkg/B.cls", "src/user.p"] {
        assert!(unit(&plan, path)["key"].is_string(), "{path}: {plan}");
    }
    let (statuses, report) =
        build_statuses(&lab, &["src/pkg/A.cls", "src/pkg/B.cls", "src/user.p"]);
    assert_eq!(statuses, ["ok", "ok", "ok"], "{report}");
    fs::remove_file(lab.workspace.path("src/pkg/A.r")).unwrap();
    fs::remove_file(lab.workspace.path("src/pkg/B.r")).unwrap();
    masks.assert_equivalent(&lab.workspace.rcode(), "stray r-code beside a cycle");
}

/// A class `wsobj` with no package on the PROPATH, with a static method
/// `OnlyStatic`, and a variable `wsobj` of type `Progress.Lang.Object`.
/// `wsobj:OnlyStatic()` compiles only when the name is the class, and
/// `wsobj:ToString()` only when it is the variable. With the variable
/// defined before the use, the name is the variable, also inside an internal
/// procedure; with no variable it is the class. For the analysis of branches
/// the preprocessor did not take: `x:member` there is no class reference
/// when a main-block variable `x` comes before it.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn a_variable_hides_a_class_of_the_same_name() {
    let lab = Lab::new();
    lab.workspace.write(
        "c/wsobj.cls",
        "CLASS wsobj:\n    METHOD PUBLIC STATIC LOGICAL OnlyStatic():\n        RETURN TRUE.\n    END METHOD.\nEND CLASS.\n",
    );
    let define = "DEFINE VARIABLE wsobj AS Progress.Lang.Object NO-UNDO.\n";
    let compiles = |name: &str, text: &str| {
        let file = format!("v/{name}.p");
        lab.workspace.write(&file, text);
        let printed = lab.session(&["c"], true, &lab.comp(&file, "v-r"));
        eprintln!("EVIDENCE {name}: {}", printed.trim().replace('\n', " | "));
        printed.contains(&format!("{name}.p OK"))
    };
    assert!(compiles("class_static", "wsobj:OnlyStatic().\n"));
    assert!(!compiles("class_instance", "wsobj:ToString().\n"));
    assert!(!compiles(
        "var_static",
        &format!("{define}wsobj:OnlyStatic().\n")
    ));
    assert!(compiles(
        "var_instance",
        &format!("{define}wsobj:ToString().\n")
    ));
    assert!(compiles(
        "var_in_procedure",
        &format!("{define}PROCEDURE p:\n    wsobj:ToString().\nEND PROCEDURE.\n")
    ));
    assert!(!compiles(
        "var_static_in_procedure",
        &format!("{define}PROCEDURE p:\n    wsobj:OnlyStatic().\nEND PROCEDURE.\n")
    ));
}
