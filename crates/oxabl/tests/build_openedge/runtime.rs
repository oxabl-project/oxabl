//! How the runtime and the compiler treat r-code beside the source, on a real
//! AVM, and whether a program that a project runs shows an edit after
//! `oxabl build`.
//!
//! The runtime PROPATH of every run here comes from the environment that the
//! program starts in, as a project configures it: the `PROPATH` variable, set
//! on the worker command. No program run here sets `PROPATH`, and no run takes
//! its PROPATH from oxabl's output directory, except where a test names that
//! layout as the project's own. The compiles that make r-code for a setup
//! start the same way.

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::process::{Command, Stdio};
use std::time::{Duration, SystemTime};

use crate::class_binding::cycle_class;
use crate::common::openedge::Target;
use crate::common::openedge::compile::{Compile, Options, compile_workspace_with};
use crate::common::openedge::mask::Mask;
use crate::common::openedge::worker::expand;
use crate::setup;

/// ABL that compiles a file, `SAVE` alone when `pcInto` is empty, and prints
/// `COMPILE <file> OK|ERR` with the number of each message.
const COMP: &str = "PROCEDURE comp:
    DEFINE INPUT PARAMETER pcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcInto AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    IF pcInto = \"\" THEN
        COMPILE VALUE(pcFile) SAVE NO-ERROR.
    ELSE DO:
        OS-CREATE-DIR VALUE(pcInto).
        COMPILE VALUE(pcFile) SAVE INTO VALUE(pcInto) NO-ERROR.
    END.
    PUT UNFORMATTED \"COMPILE \" pcFile \" \" (IF COMPILER:ERROR OR ERROR-STATUS:ERROR THEN \"ERR\" ELSE \"OK\") SKIP.
    DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
        PUT UNFORMATTED \"  MSG \" ERROR-STATUS:GET-NUMBER(i) \" \" ERROR-STATUS:GET-MESSAGE(i) SKIP.
    END.
END PROCEDURE.
";

/// A procedure that prints `SAYS <text>`.
fn says(text: &str) -> String {
    format!("PUT UNFORMATTED \"SAYS {text}\" SKIP.\n")
}

/// The `SAYS` texts in what a run printed.
fn said(printed: &str) -> Vec<String> {
    printed
        .lines()
        .filter_map(|line| line.strip_prefix("SAYS "))
        .map(String::from)
        .collect()
}

struct Runtime {
    target: Target,
    workspace: Compile,
}

impl Runtime {
    fn new() -> Runtime {
        Runtime::with(Options::default())
    }

    fn with(options: Options) -> Runtime {
        let target = setup();
        let workspace = compile_workspace_with(&target, options);
        Runtime { target, workspace }
    }

    /// The worker-side path of `relative`.
    fn worker(&self, relative: &str) -> String {
        format!("{}/{relative}", self.workspace.worker)
    }

    /// Starts `program` (the value of `-p`) through the worker command, with
    /// `PROPATH` in its environment set to `propath` (entries below the
    /// workspace), and returns what it printed. The variable is set by `env`
    /// in front of the AVM executable of the command.
    fn start(&self, propath: &[&str], program: &str) -> String {
        let entries: Vec<String> = propath.iter().map(|entry| self.worker(entry)).collect();
        let mut argv = expand(&self.target, program, &self.workspace.worker);
        let avm = argv
            .iter()
            .position(|arg| {
                let name = arg.rsplit('/').next().unwrap_or(arg);
                matches!(name, "_progres" | "pro" | "mpro" | "bpro")
            })
            .unwrap_or_else(|| {
                panic!("the worker command names no AVM executable, so the runtime PROPATH cannot be set: {argv:?}")
            });
        argv.splice(
            avm..avm,
            ["env".to_string(), format!("PROPATH={}", entries.join(":"))],
        );
        let output = Command::new(&argv[0])
            .args(&argv[1..])
            .current_dir(&self.workspace.host)
            .stdin(Stdio::null())
            .output()
            .expect("the worker command runs");
        let printed = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        eprintln!("EVIDENCE -p {program} on {propath:?}: {}", printed.trim());
        printed
    }

    /// Writes `body` as a program outside the PROPATH, starts it by its full
    /// path and returns what it printed. The program ends with `DONE`.
    fn session(&self, propath: &[&str], body: &str) -> String {
        let name = format!("check/s{}.p", crate::common::openedge::unique_name("x"));
        self.workspace.write(
            &name,
            &format!("{COMP}{body}\nPUT UNFORMATTED \"DONE\" SKIP.\nQUIT.\n"),
        );
        let printed = self.start(propath, &self.worker(&name));
        assert!(
            printed.contains("DONE"),
            "the session did not finish:\n{printed}"
        );
        printed
    }

    /// The statement that compiles `file` into `into`, below the workspace;
    /// an empty `into` compiles with `SAVE` alone. A `file` that is not a file
    /// below the workspace is a name that the compiler finds on the PROPATH.
    fn comp(&self, file: &str, into: &str) -> String {
        let file = match self.workspace.exists(file) {
            true => self.worker(file),
            false => file.to_string(),
        };
        let into = match into.is_empty() {
            true => String::new(),
            false => self.worker(into),
        };
        format!("RUN comp(\"{file}\", \"{into}\").\n")
    }

    /// Compiles `file` in a fresh session and asserts that it compiled.
    fn compile(&self, propath: &[&str], file: &str, into: &str) {
        let printed = self.session(propath, &self.comp(file, into));
        assert!(printed.contains(" OK"), "{file}: {printed}");
    }

    /// Writes the r-code of the procedure `source` as the file `path`, compiled
    /// from a stage of its own.
    fn rcode_of(&self, path: &str, source: &str) {
        let stem = path.rsplit('/').next().unwrap().trim_end_matches(".r");
        let stage = crate::common::openedge::unique_name("stage");
        self.workspace.write(&format!("{stage}/{stem}.p"), source);
        self.compile(
            &[&stage],
            &format!("{stage}/{stem}.p"),
            &format!("{stage}/r"),
        );
        self.place(&format!("{stage}/r/{stem}.r"), path);
    }

    /// Copies the file `from` to `to`, below the workspace.
    fn place(&self, from: &str, to: &str) {
        let to = self.workspace.path(to);
        fs::create_dir_all(to.parent().unwrap()).unwrap();
        fs::copy(self.workspace.path(from), to).unwrap();
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

    fn inode(&self, relative: &str) -> u64 {
        fs::metadata(self.workspace.path(relative)).unwrap().ino()
    }

    /// The r-code oracle for `file` compiled on `propath` into `into`: four
    /// compiles 1.2 s apart, each with no r-code at `rcode` before it. Returns
    /// the first r-code and the mask.
    fn masked(&self, propath: &[&str], file: &str, into: &str, rcode: &str) -> (Vec<u8>, Mask) {
        let mut samples = Vec::new();
        for index in 0..4 {
            if index > 0 {
                std::thread::sleep(Duration::from_millis(1200));
            }
            let _ = fs::remove_file(self.workspace.path(rcode));
            self.compile(propath, file, into);
            samples.push(self.workspace.bytes(rcode));
        }
        let others: Vec<&[u8]> = samples[1..].iter().map(Vec::as_slice).collect();
        let mask = Mask::from_samples(&samples[0], &others);
        (samples[0].clone(), mask)
    }

    /// Compiles `file` into `into` in a fresh session with the `FileID` log on,
    /// asserts that it compiled, and returns the files below the workspace
    /// that the log names as opened.
    fn opened(&self, propath: &[&str], file: &str, into: &str) -> Vec<String> {
        let log = crate::common::openedge::unique_name("fileid");
        let body = format!(
            "LOG-MANAGER:LOGFILE-NAME = \"{}\".\nLOG-MANAGER:LOG-ENTRY-TYPES = \"FileID:2\".\n{}LOG-MANAGER:CLOSE-LOG().\n",
            self.worker(&format!("{log}.log")),
            self.comp(file, into)
        );
        let printed = self.session(propath, &body);
        assert!(printed.contains(" OK"), "{file}: {printed}");
        let root = format!("{}/", self.workspace.worker);
        let opened: Vec<String> = self
            .workspace
            .read(&format!("{log}.log"))
            .lines()
            .filter_map(|line| line.split_once("FILEID").map(|(_, rest)| rest.trim()))
            .filter_map(|rest| rest.strip_prefix("Open "))
            .filter_map(|rest| rest.rsplit_once(" ID=").map(|(path, _)| path))
            .filter_map(|path| path.strip_prefix(&root))
            .map(String::from)
            .collect();
        eprintln!("EVIDENCE FileID {file}: {opened:?}");
        opened
    }
}

/// E1: `a/x.p` and, in a later entry, `b/x.r` from another source, and no
/// `b/x.p`. `RUN x.p` runs `a/x.p`: the runtime searches entry by entry, the
/// r-code and then the source in each entry, as the compiler does for a class.
/// So a separate output directory must come before the source entries on the
/// runtime PROPATH, and r-code beside the source always wins in its entry.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn the_runtime_searches_entry_by_entry() {
    let runtime = Runtime::new();
    runtime.workspace.write("a/x.p", &says("a-source"));
    runtime.rcode_of("b/x.r", &says("b-rcode"));

    let printed = runtime.session(&["a", "b"], "RUN x.p.");
    assert_eq!(said(&printed), ["a-source"], "{printed}");
    let printed = runtime.session(&["b", "a"], "RUN x.p.");
    assert_eq!(said(&printed), ["b-rcode"], "{printed}");
}

/// The setup of E1b and E1c: `a/x.r` from an old source, and `a/x.p` edited
/// after it, with a newer time.
fn stale_beside_source() -> Runtime {
    let runtime = Runtime::new();
    runtime.rcode_of("a/x.r", &says("old"));
    runtime.workspace.write("a/x.p", &says("new"));
    runtime.stamp("a/x.r", 0);
    runtime.stamp("a/x.p", 100);
    runtime
}

/// E1b: `a/x.r` from an old source beside a newer, edited `a/x.p`. The runtime
/// runs the old r-code: it does not compare times. This is how a build that
/// the runtime does not search goes unseen.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn rcode_beside_a_newer_source_runs() {
    let runtime = stale_beside_source();
    let printed = runtime.session(&["a"], "RUN x.p.");
    assert_eq!(said(&printed), ["old"], "{printed}");
}

/// E1c: with the setup of E1b, every name of the program runs the old r-code:
/// `RUN x`, `RUN x.r`, `RUN` of the full path of `a/x.p`, and a start with
/// `-p` and the full path or the name. With an output entry `o` before `a`
/// that holds `o/x.r`, the name runs `o/x.r`, but the full path of `a/x.p`
/// still runs `a/x.r`. So a project that starts programs by the path of the
/// source can only use r-code beside the source; a separate directory serves
/// only programs started by name.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn every_name_of_a_program_runs_the_rcode_beside_it() {
    let runtime = stale_beside_source();
    let full = runtime.worker("a/x.p");
    for body in [
        "RUN x.".to_string(),
        "RUN x.r.".to_string(),
        format!("RUN VALUE(\"{full}\")."),
    ] {
        let printed = runtime.session(&["a"], &body);
        assert_eq!(said(&printed), ["old"], "{body}: {printed}");
    }
    for program in [full.as_str(), "x.p"] {
        let printed = runtime.start(&["a"], program);
        assert_eq!(said(&printed), ["old"], "-p {program}: {printed}");
    }

    runtime.rcode_of("o/x.r", &says("out"));
    for (program, text) in [(full.as_str(), "old"), ("x.p", "out")] {
        let printed = runtime.start(&["o", "a"], program);
        assert_eq!(said(&printed), [text], "-p {program} on o, a: {printed}");
    }
    let printed = runtime.session(&["o", "a"], &format!("RUN VALUE(\"{full}\")."));
    assert_eq!(said(&printed), ["old"], "{printed}");
}

/// E3: `COMPILE ... SAVE` and `COMPILE ... SAVE INTO` over an existing `.r`
/// write a new file: the inode changes and a hard link to the old file keeps
/// the old bytes.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn compile_save_replaces_the_rcode_file() {
    let runtime = Runtime::new();
    runtime.workspace.write("a/x.p", &says("one"));
    runtime.compile(&["a"], "a/x.p", "");
    for (step, into) in [("save", ""), ("save-into", "a")] {
        let before = runtime.inode("a/x.r");
        let old = runtime.workspace.bytes("a/x.r");
        let link = format!("keep-{step}.r");
        fs::hard_link(
            runtime.workspace.path("a/x.r"),
            runtime.workspace.path(&link),
        )
        .unwrap();
        runtime
            .workspace
            .write("a/x.p", &says(&format!("{step}-edit")));
        runtime.compile(&["a"], "a/x.p", into);
        let after = runtime.inode("a/x.r");
        eprintln!("EVIDENCE {step}: inode {before} -> {after}");
        assert_ne!(before, after, "{step}: the compile wrote into the old file");
        assert_eq!(runtime.workspace.bytes(&link), old, "{step}");
        assert_ne!(runtime.workspace.bytes("a/x.r"), old, "{step}");
    }
}

/// A `pkg.Foo` whose `ToString` gives `text`.
fn foo_says(text: &str) -> String {
    format!(
        "CLASS pkg.Foo:\n    METHOD PUBLIC OVERRIDE CHARACTER ToString():\n        RETURN \"{text}\".\n    END METHOD.\nEND CLASS.\n"
    )
}

/// E4: `a/pkg/Foo.cls` and, beside it, an older `a/pkg/Foo.r` from another
/// source. A run that makes the object by name at run time (`DYNAMIC-NEW`)
/// and a run that names the class in its source both get the old class: the
/// runtime takes the r-code in the entry, as the compiler does. So r-code
/// beside the source serves classes too.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn class_rcode_beside_a_newer_source_runs() {
    let runtime = Runtime::new();
    runtime
        .workspace
        .write("stage/pkg/Foo.cls", &foo_says("old"));
    runtime.compile(&["stage"], "stage/pkg/Foo.cls", "stage/r");
    runtime.place("stage/r/pkg/Foo.r", "a/pkg/Foo.r");
    runtime.workspace.write("a/pkg/Foo.cls", &foo_says("new"));
    runtime.stamp("a/pkg/Foo.r", 0);
    runtime.stamp("a/pkg/Foo.cls", 100);

    let dynamic = "DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.\no = DYNAMIC-NEW \"pkg.Foo\" ().\nPUT UNFORMATTED \"SAYS \" o:ToString() SKIP.";
    let printed = runtime.session(&["a"], dynamic);
    assert_eq!(said(&printed), ["old"], "{printed}");
    runtime.workspace.write(
        "a/user.p",
        "DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\nPUT UNFORMATTED \"SAYS \" o:ToString() SKIP.\n",
    );
    let printed = runtime.session(&["a"], "RUN user.p.");
    assert_eq!(said(&printed), ["old"], "{printed}");
}

/// E7: `u.p` runs `v.p`, and files of random bytes sit at `u.r` and `v.r`
/// beside the sources. The compile of `u.p` by name opens `u.r`, the r-code
/// that the PROPATH search finds first for that name, but not `v.r`, and its
/// r-code is equivalent to a compile without those files under the r-code
/// oracle. So the r-code of a `RUN` target is no compile input, and r-code at
/// the unit's own place is opened but changes nothing.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn the_compiler_opens_no_procedure_rcode() {
    let runtime = Runtime::new();
    runtime.workspace.write("src/u.p", "RUN v.p.\n");
    runtime.workspace.write("src/v.p", &says("v"));
    let (clean, mask) = runtime.masked(&["src"], "u.p", "out", "out/u.r");
    let opened = runtime.opened(&["src"], "u.p", "out");
    assert!(
        !opened.iter().any(|path| path.ends_with(".r")),
        "{opened:?}"
    );

    let noise: Vec<u8> = (0..4096u32)
        .map(|i| (i.wrapping_mul(2_654_435_761) >> 13) as u8)
        .collect();
    fs::write(runtime.workspace.path("src/u.r"), &noise).unwrap();
    fs::write(runtime.workspace.path("src/v.r"), &noise).unwrap();
    std::thread::sleep(Duration::from_millis(1200));
    fs::remove_file(runtime.workspace.path("out/u.r")).unwrap();
    let opened = runtime.opened(&["src"], "u.p", "out");

    assert!(opened.contains(&"src/u.p".to_string()), "{opened:?}");
    assert!(opened.contains(&"src/u.r".to_string()), "{opened:?}");
    assert!(!opened.contains(&"src/v.r".to_string()), "{opened:?}");
    let noisy = runtime.workspace.bytes("out/u.r");
    assert!(
        mask.equivalent(&clean, &noisy).is_ok(),
        "{:?}",
        mask.equivalent(&clean, &noisy)
    );
}

/// E9: `COMPILE x.p SAVE` by name, with stale r-code of another version at
/// `a/x.r`, where the compile writes. The compile opens the stale file, as in
/// E7, succeeds, and gives r-code equivalent to a compile without it; the
/// program then runs the new text.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn stale_rcode_beside_a_procedure_changes_no_compile() {
    let runtime = Runtime::new();
    runtime.workspace.write("a/x.p", &says("new"));
    let (clean, mask) = runtime.masked(&["a"], "x.p", "", "a/x.r");
    runtime.rcode_of("a/x.r", &says("old"));
    let stale = runtime.workspace.bytes("a/x.r");
    assert!(
        mask.equivalent(&clean, &stale).is_err(),
        "the stale r-code is another program"
    );
    std::thread::sleep(Duration::from_millis(1200));

    let opened = runtime.opened(&["a"], "x.p", "");

    assert!(opened.contains(&"a/x.r".to_string()), "{opened:?}");
    let compiled = runtime.workspace.bytes("a/x.r");
    assert!(
        mask.equivalent(&clean, &compiled).is_ok(),
        "{:?}",
        mask.equivalent(&clean, &compiled)
    );
    let printed = runtime.session(&["a"], "RUN x.p.");
    assert_eq!(said(&printed), ["new"], "{printed}");
}

/// A `pkg.Foo` whose only difference from the other versions is the method
/// `Is<tag>`.
fn foo(tag: &str) -> String {
    format!(
        "CLASS pkg.Foo:\n    METHOD PUBLIC LOGICAL Is{tag}():\n        RETURN TRUE.\n    END METHOD.\nEND CLASS.\n"
    )
}

/// A procedure that calls `Is<tag>` on a `pkg.Foo`.
fn user(tag: &str) -> String {
    format!("DEFINE VARIABLE o AS pkg.Foo NO-UNDO.\no = NEW pkg.Foo().\no:Is{tag}().\n")
}

impl Runtime {
    /// Whether `file` compiles on `propath` in a fresh session, and the files
    /// below the workspace that the compile opened.
    fn user_compiles(&self, propath: &[&str], file: &str) -> (bool, Vec<String>) {
        let log = crate::common::openedge::unique_name("fileid");
        let body = format!(
            "LOG-MANAGER:LOGFILE-NAME = \"{}\".\nLOG-MANAGER:LOG-ENTRY-TYPES = \"FileID:2\".\n{}LOG-MANAGER:CLOSE-LOG().\n",
            self.worker(&format!("{log}.log")),
            self.comp(file, "user-r")
        );
        let printed = self.session(propath, &body);
        let root = format!("{}/", self.workspace.worker);
        let opened = self
            .workspace
            .read(&format!("{log}.log"))
            .lines()
            .filter_map(|line| line.split_once("FILEID").map(|(_, rest)| rest.trim()))
            .filter_map(|rest| rest.strip_prefix("Open "))
            .filter_map(|rest| rest.rsplit_once(" ID=").map(|(path, _)| path))
            .filter_map(|path| path.strip_prefix(&root))
            .map(String::from)
            .collect();
        (printed.contains(" OK"), opened)
    }

    /// The users of `pkg.Foo` in `a/` and `b/`, compiled on the PROPATH of a
    /// build in place with the per-directory layout (the unit's own directory,
    /// then `a`): which versions compile, and that each bound `a/pkg/Foo.r`.
    fn users_bind(&self, tags: &[&str]) -> Vec<String> {
        let mut bound = Vec::new();
        for dir in ["a", "b"] {
            for tag in tags {
                let file = format!("{dir}/u{tag}.p");
                self.workspace.write(&file, &user(tag));
                let propath: &[&str] = match dir {
                    "a" => &["a"],
                    _ => &["b", "a"],
                };
                let (ok, opened) = self.user_compiles(propath, &file);
                assert!(
                    opened.contains(&"a/pkg/Foo.r".to_string())
                        && !opened.contains(&"a/pkg/Foo.cls".to_string()),
                    "{file} did not bind the r-code beside the class: {opened:?}"
                );
                if ok {
                    bound.push(format!("{dir}:{tag}"));
                }
            }
        }
        bound
    }
}

/// E8, one class: with no output directory on the compile PROPATH (only the
/// source entries), `a/pkg/Foo.cls` compiled with `SAVE INTO a` puts
/// `a/pkg/Foo.r` beside the source, and the users in `a/` and in `b/` bind it
/// (r-code first in the first entry that holds the class). With a stale
/// `a/pkg/Foo.r` there before, the class compiles over it, and the users then
/// bind the new r-code. So in place without a separate directory, a class
/// unit that compiles before its users is what they bind, as `$OUT` is today.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn class_users_bind_the_rcode_compiled_beside_the_class() {
    let runtime = Runtime::new();
    runtime.workspace.write("a/pkg/Foo.cls", &foo("Current"));
    runtime.compile(&["a"], "pkg/Foo.cls", "a");
    assert!(runtime.workspace.exists("a/pkg/Foo.r"));
    assert_eq!(
        runtime.users_bind(&["Current", "Stale"]),
        ["a:Current", "b:Current"]
    );

    let stale = Runtime::new();
    stale.workspace.write("stage/pkg/Foo.cls", &foo("Stale"));
    stale.compile(&["stage"], "stage/pkg/Foo.cls", "stage/r");
    stale.place("stage/r/pkg/Foo.r", "a/pkg/Foo.r");
    stale.workspace.write("a/pkg/Foo.cls", &foo("Current"));
    assert_eq!(
        stale.users_bind(&["Current", "Stale"]),
        ["a:Stale", "b:Stale"],
        "before the class compiles, its users bind the stale r-code"
    );
    stale.compile(&["a"], "pkg/Foo.cls", "a");
    assert_eq!(
        stale.users_bind(&["Current", "Stale"]),
        ["a:Current", "b:Current"]
    );
}

/// What each pass of a cycle printed, and the final `A.r` and `B.r`.
type Passes = (String, String, Vec<u8>, Vec<u8>);

impl Runtime {
    /// Writes the cycle `pkg.A` <-> `pkg.B` below `c/src`, with the r-code
    /// `placed` beside the sources, and compiles it in place (`SAVE INTO
    /// c/src`, PROPATH `c/src` alone) in two passes, each in a fresh session.
    /// Returns the compile results of each pass and the final `A.r`, `B.r`.
    fn cycle_in_place(&self, placed: &[(&str, &[u8])]) -> Passes {
        let _ = fs::remove_dir_all(self.workspace.path("c"));
        for (me, other) in [("A", "B"), ("B", "A")] {
            self.workspace.write(
                &format!("c/src/pkg/{me}.cls"),
                &cycle_class(me, other, false),
            );
        }
        for (me, bytes) in placed {
            fs::write(self.workspace.path(&format!("c/src/pkg/{me}.r")), bytes).unwrap();
        }
        std::thread::sleep(Duration::from_millis(1200));
        let pass = format!(
            "{}{}",
            self.comp("pkg/A.cls", "c/src"),
            self.comp("pkg/B.cls", "c/src")
        );
        let one = self.session(&["c/src"], &pass);
        let two = self.session(&["c/src"], &pass);
        let read = |me: &str| {
            fs::read(self.workspace.path(&format!("c/src/pkg/{me}.r"))).unwrap_or_default()
        };
        (one, two, read("A"), read("B"))
    }
}

/// E8, a cycle: `pkg.A` and `pkg.B` call each other, compiled in place with
/// only the source entry on the PROPATH, two passes in fresh sessions. With
/// stale r-code beside both from a compatible older version (`A.r` current,
/// `B.r` with one more method), the r-code after pass 2 is equivalent to a
/// clean build's under the r-code oracle. With stale r-code of versions that
/// lack the members the other calls, every compile of both passes fails with
/// error 12927 and the stale files stay: a failed compile writes no r-code,
/// so in place a cycle never recovers by itself from such r-code.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn a_cycle_in_place_after_two_passes() {
    let runtime = Runtime::new();
    let samples: Vec<_> = (0..4).map(|_| runtime.cycle_in_place(&[])).collect();
    for (one, two, _, _) in &samples {
        assert_eq!(one.matches(".cls OK").count(), 2, "{one}");
        assert_eq!(two.matches(".cls OK").count(), 2, "{two}");
    }
    let mask = |pick: &dyn Fn(&Passes) -> Vec<u8>| {
        let first = pick(&samples[0]);
        let others: Vec<Vec<u8>> = samples[1..].iter().map(pick).collect();
        let refs: Vec<&[u8]> = others.iter().map(Vec::as_slice).collect();
        (first.clone(), Mask::from_samples(&first, &refs))
    };
    let (a, mask_a) = mask(&|s| s.2.clone());
    let (b, mask_b) = mask(&|s| s.3.clone());

    runtime.workspace.write(
        "bp/pkg/B.cls",
        &cycle_class("B", "A", false).replace(
            "END CLASS.",
            "    METHOD PUBLIC LOGICAL Extra():\n        RETURN TRUE.\n    END METHOD.\nEND CLASS.",
        ),
    );
    runtime
        .workspace
        .write("bp/pkg/A.cls", &cycle_class("A", "B", false));
    runtime.compile(&["bp"], "bp/pkg/B.cls", "bp-r");
    let bprime = runtime.workspace.bytes("bp-r/pkg/B.r");
    let (one, two, got_a, got_b) = runtime.cycle_in_place(&[("A", &a), ("B", &bprime)]);
    eprintln!("EVIDENCE compatible: {one} / {two}");
    assert_eq!(two.matches(".cls OK").count(), 2, "{two}");
    assert!(
        mask_a.equivalent(&a, &got_a).is_ok(),
        "{:?}",
        mask_a.equivalent(&a, &got_a)
    );
    assert!(
        mask_b.equivalent(&b, &got_b).is_ok(),
        "{:?}",
        mask_b.equivalent(&b, &got_b)
    );

    let mut stale = Vec::new();
    for me in ["A", "B"] {
        runtime
            .workspace
            .write(&format!("old/pkg/{me}.cls"), &cycle_class(me, "", true));
        runtime.compile(&["old"], &format!("old/pkg/{me}.cls"), "old-r");
        stale.push(runtime.workspace.bytes(&format!("old-r/pkg/{me}.r")));
    }
    let (one, two, got_a, got_b) = runtime.cycle_in_place(&[("A", &stale[0]), ("B", &stale[1])]);
    eprintln!("EVIDENCE incompatible: {one} / {two}");
    for pass in [&one, &two] {
        assert_eq!(pass.matches(".cls ERR").count(), 2, "{pass}");
        assert!(pass.contains("12927"), "{pass}");
    }
    assert_eq!(got_a, stale[0]);
    assert_eq!(got_b, stale[1]);
}

/// The project of the runtime loop: a program, a program with an include and
/// a user of a class, each printing `<what> <version>`.
fn write_loop_sources(workspace: &Compile, version: &str) {
    workspace.write("src/hello.p", &says(&format!("program {version}")));
    workspace.write("src/withinc.p", "{msg.i}\n");
    workspace.write("src/msg.i", &says(&format!("include {version}")));
    workspace.write(
        "src/pkg/Greet.cls",
        &format!(
            "CLASS pkg.Greet:\n    METHOD PUBLIC CHARACTER Text():\n        RETURN \"class {version}\".\n    END METHOD.\nEND CLASS.\n"
        ),
    );
    workspace.write(
        "src/greet.p",
        "DEFINE VARIABLE o AS pkg.Greet NO-UNDO.\no = NEW pkg.Greet().\nPUT UNFORMATTED \"SAYS \" o:Text() SKIP.\n",
    );
}

/// What the three programs print when the project runs them on `propath`.
fn run_loop(runtime: &Runtime, propath: &[&str]) -> Vec<String> {
    ["hello.p", "withinc.p", "greet.p"]
        .iter()
        .flat_map(|program| said(&runtime.session(propath, &format!("RUN {program}."))))
        .collect()
}

/// The project at version 1, compiled by the project's own tool with
/// `COMPILE ... SAVE` beside the sources, then edited to version 2 and built
/// by `oxabl build` into `build/r`.
fn edited_and_built() -> Runtime {
    let runtime = Runtime::new();
    write_loop_sources(&runtime.workspace, "v1");
    runtime.compile(&["src"], "pkg/Greet.cls", "src");
    for program in ["hello.p", "withinc.p", "greet.p"] {
        runtime.compile(&["src"], program, "");
    }
    for rcode in [
        "src/hello.r",
        "src/withinc.r",
        "src/greet.r",
        "src/pkg/Greet.r",
    ] {
        assert!(runtime.workspace.exists(rcode), "{rcode}");
    }
    assert_eq!(
        run_loop(&runtime, &["src"]),
        ["program v1", "include v1", "class v1"]
    );
    std::thread::sleep(Duration::from_millis(1200));
    write_loop_sources(&runtime.workspace, "v2");
    let output = runtime.workspace.build(&["--json"], 0);
    let report: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(report["summary"]["failed"], 0, "{report}");
    for rcode in ["hello.r", "withinc.r", "greet.r", "pkg/Greet.r"] {
        assert!(
            runtime.workspace.exists(&format!("build/r/{rcode}")),
            "{rcode}"
        );
    }
    runtime
}

/// The incident, pinned: the runtime PROPATH holds the source directory only,
/// as the project has it, and r-code of the project's own tool sits beside the
/// sources. After an edit and `oxabl build`, the program, the include and the
/// class all still print the old text: the runtime never sees `build/r`
/// (E1b). This test asserts the old text on purpose.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn the_runtime_ignores_an_output_directory_it_does_not_search() {
    let runtime = edited_and_built();
    assert_eq!(
        run_loop(&runtime, &["src"]),
        ["program v1", "include v1", "class v1"]
    );
}

/// The same project with one output entry before each source entry on the
/// runtime PROPATH (`build/r`, then `src`): after the edit and `oxabl build`,
/// the program, the include and the class print the new text.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn an_edit_of_a_program_shows_in_the_runtime() {
    let runtime = edited_and_built();
    assert_eq!(
        run_loop(&runtime, &["build/r", "src"]),
        ["program v2", "include v2", "class v2"]
    );
}
