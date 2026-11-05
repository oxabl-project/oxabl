//! What r-code depends on, compared with the r-code oracle: two files are
//! equivalent when they have the same size and differ only where two compiles
//! of one unit in one root differ.

use crate::common::built::unit;
use crate::common::openedge::mask::Masks;
use std::sync::OnceLock;

use crate::common::openedge::Target;
use crate::common::openedge::compile::compile_workspace;
use crate::compile::run_from_rcode;
use crate::setup;

const SAMPLES: usize = 8;

/// The masks of the fixture, recorded once in a root of their own.
fn fixture_masks(target: &Target) -> &'static Masks {
    static MASKS: OnceLock<Masks> = OnceLock::new();
    MASKS.get_or_init(|| {
        let workspace = compile_workspace(target);
        workspace.copy_fixture();
        Masks::record(&workspace, SAMPLES)
    })
}

/// The fixture copied to two roots gives equivalent r-code for every unit.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn portable_rcode() {
    let target = setup();
    let masks = fixture_masks(&target);
    masks.assert_small();

    let second = compile_workspace(&target);
    second.copy_fixture();
    second.build(&["--json"], 0);

    masks.assert_equivalent(&second.rcode(), "another root");
    let tampered = {
        let mut bytes = second.rcode()["main.r"].clone();
        let at = masks.masks["main.r"].unmasked_offset();
        bytes[at] ^= 0xff;
        bytes
    };
    assert!(
        masks.masks["main.r"]
            .equivalent(&masks.first["main.r"], &tampered)
            .is_err(),
        "the oracle accepts a changed byte"
    );
}

/// With and without `XREF`, in either form, the r-code is equivalent.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn xref_does_not_change_rcode() {
    let target = setup();
    let masks = fixture_masks(&target);
    let mut workspace = compile_workspace(&target);
    workspace.copy_fixture();
    workspace.build(&["--json"], 0);
    masks.assert_equivalent(&workspace.rcode(), "XML XREF");

    workspace.set_outputs("xref = \"build/xref\"\nxref_format = \"text\"");
    workspace.drop_rcode();
    workspace.build(&["--json"], 0);
    masks.assert_equivalent(&workspace.rcode(), "text XREF");
    assert!(
        workspace.exists("build/xref/main.p.xref"),
        "no text XREF written"
    );

    workspace.without_xref_output();
    workspace.drop_rcode();
    workspace.build(&["--json"], 0);
    masks.assert_equivalent(&workspace.rcode(), "no XREF");
}

/// Editing the target of a `RUN` leaves the caller out of the rebuild set, and
/// its r-code stays equivalent when it compiles again.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn run_target_not_read() {
    let target = setup();
    let masks = fixture_masks(&target);
    let workspace = compile_workspace(&target);
    workspace.copy_fixture();
    workspace.build(&["--json"], 0);

    workspace.write(
        "src/helper.p",
        "DEFINE INPUT PARAMETER who AS CHARACTER NO-UNDO.\nDEFINE VARIABLE extra AS INTEGER NO-UNDO.\nextra = LENGTH(who).\nPUT UNFORMATTED \"changed helper \" who \" \" extra SKIP.\n",
    );
    let plan = workspace.build(&["--plan", "--json", "--all"], 0);
    let plan: serde_json::Value = serde_json::from_slice(&plan.stdout).unwrap();
    assert_eq!(unit(&plan, "src/helper.p")["action"], "rebuild");
    assert_eq!(unit(&plan, "src/main.p")["action"], "up-to-date");

    workspace.remove("build/r/main.r");
    workspace.build(&["--json"], 0);

    let rcode = workspace.rcode();
    masks.masks["main.r"]
        .equivalent(&masks.first["main.r"], &rcode["main.r"])
        .unwrap_or_else(|why| panic!("main.r changed with its RUN target: {why}"));
    assert_ne!(
        masks.first["helper.r"].len(),
        rcode["helper.r"].len(),
        "the edit did not reach helper.r"
    );
    let printed = workspace.run_program(&target, &run_from_rcode("main.p"));
    assert!(printed.contains("changed helper hello 5"), "{printed}");
}

const BASE: &str = "CLASS t.B:
    METHOD PUBLIC CHARACTER Base():
        RETURN \"b1\".
    END METHOD.
END CLASS.
";

const DERIVED: &str = "CLASS t.A INHERITS t.B:
    METHOD PUBLIC CHARACTER Name():
        RETURN \"A:\" + Base().
    END METHOD.
END CLASS.
";

const USER: &str = "DEFINE VARIABLE a AS t.A NO-UNDO.
a = NEW t.A().
PUT UNFORMATTED a:Name() SKIP.
";

/// The base class with another body and one more method, so its public
/// signature changes too.
fn changed_base() -> String {
    BASE.replace("b1", "b2").replace(
        "END CLASS.",
        "    METHOD PUBLIC CHARACTER More():\n        RETURN \"more\".\n    END METHOD.\nEND CLASS.",
    )
}

/// The r-code of a class and of its user, from two builds, still run together:
/// a build that compiles only the changed base class, signature included,
/// leaves the old derived class and the old user valid.
#[test]
#[ignore = "needs a real AVM; see the module docs"]
fn mixed_provenance() {
    let target = setup();
    let workspace = compile_workspace(&target);
    workspace.write("src/t/B.cls", BASE);
    workspace.write("src/t/A.cls", DERIVED);
    workspace.write("src/prog.p", USER);
    let report = workspace.build_report();
    assert_eq!(report["summary"]["compiled"], 3, "{report}");
    let printed = workspace.run_program(&target, &run_from_rcode("prog.p"));
    assert!(printed.contains("A:b1"), "{printed}");
    let old_derived = workspace.bytes("build/r/t/A.r");
    let old_user = workspace.bytes("build/r/prog.r");

    workspace.write("src/t/B.cls", &changed_base());
    workspace.build(&["--json"], 0);
    std::fs::write(workspace.path("build/r/t/A.r"), old_derived).unwrap();
    std::fs::write(workspace.path("build/r/prog.r"), old_user).unwrap();

    let printed = workspace.run_program(&target, &run_from_rcode("prog.p"));
    assert!(
        printed.contains("A:b2") && printed.contains("RAN"),
        "the old derived class and user do not run with the new base class: {printed}"
    );
}
