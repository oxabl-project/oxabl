//! `USING` imports, `NEW`, and member access across files: what a
//! package-qualified type name reaches once the workspace index can answer for
//! it, and what it must not.
//!
//! Same harness shape as `cross_file_inheritance.rs`, and for the same reason:
//! these run the real [`BatchIndex`] over an in-memory filesystem rather than a
//! hand-written stub, so what they pin is the resolution every client gets.
//!
//! Three properties recur, because they are the ones a later change is most
//! likely to break:
//!
//! 1. **With no index attached nothing changes.** Every scenario is run twice
//!    and the no-index run must produce today's answer — `External` where an
//!    entry exists at all, and *no entry* where none exists today. That is the
//!    R11 firewall.
//! 2. **The local scope tree wins.** A class declared in the file under analysis
//!    shadows a workspace class reachable under the same name.
//! 3. **No new finding, ever.** Where a test says "produces no finding" it also
//!    asserts a sibling shape where the finding genuinely does fire, so the
//!    assertion cannot pass by the rule being inert.

mod support;

use oxabl_ast::{NodeId, RunTarget, Statement, StatementKind};
use oxabl_lexer::oxabl_atom::OxablAtom;
use oxabl_semantic::{
    NamespaceId, PrimitiveTy, Resolution, ResolvedType, SymbolKind, UnresolvedReason,
};

use support::*;

// ---------------------------------------------------------------------------
// Fixtures — synthetic ABL only
// ---------------------------------------------------------------------------

/// The workspace class every scenario imports: one public property, one public
/// method with a declared return type, one protected method, one private method.
const CACHE: &str = r#"CLASS myapp.cache:
    DEFINE PUBLIC PROPERTY entry-count AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC CHARACTER fetch-label():
        RETURN "".
    END METHOD.
    METHOD PROTECTED INTEGER shared-slot():
        RETURN 0.
    END METHOD.
    METHOD PRIVATE INTEGER hidden-slot():
        RETURN 0.
    END METHOD.
END CLASS."#;

/// Path the batch index searches for [`CACHE`] under, given `/src` on the paths:
/// a qualified name maps to a relative path by replacing dots with separators.
const CACHE_PATH: &str = "/src/myapp/cache.cls";

/// The workspace as every scenario sees it.
const WORKSPACE: [(&str, &str); 1] = [(CACHE_PATH, CACHE)];

/// The `(node id, name span)` of the file's sole top-level `USING`.
fn sole_using(stmts: &[Statement]) -> (NodeId, oxabl_ast::Span) {
    let mut hits: Vec<(NodeId, oxabl_ast::Span)> = stmts
        .iter()
        .filter_map(|s| match &s.kind {
            StatementKind::Using { id, name_span, .. } => Some((*id, *name_span)),
            _ => None,
        })
        .collect();
    assert_eq!(hits.len(), 1, "fixture must carry exactly one USING");
    hits.pop().expect("checked above")
}

// ---------------------------------------------------------------------------
// The USING statement itself
// ---------------------------------------------------------------------------

#[test]
fn a_using_naming_a_class_that_exists_resolves_on_the_statement() {
    let source = "USING myapp.cache.\nDEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n";

    let (stmts, sem) = with_index(source, &WORKSPACE);
    let (using_id, _) = sole_using(&stmts);
    let sym = match sem.references.get(using_id) {
        Some(Resolution::Resolved(sym)) => *sym,
        other => panic!("the USING statement must carry a resolved reference, got {other:?}"),
    };
    assert_eq!(sem.symbols.get(sym).name, OxablAtom::from("myapp.cache"));
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Class);
    assert_synthesized(&sem, sym, NamespaceId::Types);

    // An import is not a *use*: nothing bumps a count on the imported class, so
    // a future unused-import rule can still tell the two apart.
    assert_eq!(sem.symbols.get(sym).read_count, 0);

    // The firewall. Today a `USING` produces no reference entry at all, and an
    // `External` placeholder would add a row to the analyze envelope of every
    // file that imports anything, on a run where nothing was looked at.
    let (stmts, plain) = without_index(source);
    let (using_id, _) = sole_using(&stmts);
    assert_eq!(plain.references.get(using_id), None);
    assert!(symbols_named(&plain, "myapp.cache").is_empty());
}

#[test]
fn a_using_naming_a_class_no_file_declares_records_not_found_over_the_qualified_name() {
    let source = "USING myapp.absent-thing.\nDEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n";

    let (stmts, sem) = with_index(source, &WORKSPACE);
    let (using_id, name_span) = sole_using(&stmts);
    assert_eq!(
        sem.references.get(using_id),
        Some(&Resolution::Unresolved {
            name: OxablAtom::from("myapp.absent-thing"),
            // `NotFoundInWorkspace`, not `External`: an index was attached, it
            // looked on the configured paths, and no file declares this class.
            reason: UnresolvedReason::NotFoundInWorkspace,
        })
    );
    // The span is the load-bearing half — a later diagnostic about an import
    // nothing declares has to point at the qualified name, not at the statement.
    assert_eq!(
        &source[name_span.start as usize..name_span.end as usize],
        "myapp.absent-thing"
    );
    assert!(
        lint0001_with_index(source, &WORKSPACE).is_empty(),
        "an unresolvable import is not an undefined symbol"
    );

    let (stmts, plain) = without_index(source);
    let (using_id, _) = sole_using(&stmts);
    assert_eq!(plain.references.get(using_id), None);
}

#[test]
fn a_wildcard_using_names_no_single_class_so_it_records_nothing() {
    // It is not a miss and it is not a resolution: `USING myapp.*` has no single
    // target for a reference to point at. Its whole effect is to widen the
    // prefixes a later unqualified name is tried against, which the `NEW` test
    // below observes.
    let source = "USING myapp.*.\nDEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n";
    let (stmts, sem) = with_index(source, &WORKSPACE);
    let (using_id, _) = sole_using(&stmts);
    assert_eq!(sem.references.get(using_id), None);
}

// ---------------------------------------------------------------------------
// NEW
// ---------------------------------------------------------------------------

#[test]
fn new_after_an_explicit_using_resolves_to_that_class() {
    let source = "USING myapp.cache.\n\
                  DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
                  v-cache = NEW cache().\n";

    let (_stmts, sem) = with_index(source, &WORKSPACE);
    // One symbol, not two: the `USING` and the `NEW` share it, which is what
    // makes class identity comparable at all.
    let sym = sole_resolved(&sem, "myapp.cache");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Class);
    assert_synthesized(&sem, sym, NamespaceId::Types);

    let (_stmts, plain) = without_index(source);
    assert_eq!(
        unresolved_reasons(&plain, "cache"),
        vec![UnresolvedReason::External],
        "today's answer for `NEW` of a USING-imported class"
    );
    assert!(symbols_named(&plain, "myapp.cache").is_empty());
}

#[test]
fn new_fully_qualified_resolves_with_no_using_present() {
    let source = "DEFINE VARIABLE v-cache AS CLASS myapp.cache NO-UNDO.\n\
                  v-cache = NEW myapp.cache().\n";

    let (_stmts, sem) = with_index(source, &WORKSPACE);
    let sym = sole_resolved(&sem, "myapp.cache");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Class);

    let (_stmts, plain) = without_index(source);
    assert_eq!(
        unresolved_reasons(&plain, "myapp.cache"),
        vec![UnresolvedReason::External]
    );
}

#[test]
fn new_after_a_wildcard_using_resolves_through_the_wildcard() {
    let source = "USING myapp.*.\n\
                  DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
                  v-cache = NEW cache().\n";

    let (_stmts, sem) = with_index(source, &WORKSPACE);
    let sym = sole_resolved(&sem, "myapp.cache");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Class);

    let (_stmts, plain) = without_index(source);
    assert_eq!(
        unresolved_reasons(&plain, "cache"),
        vec![UnresolvedReason::External]
    );
}

#[test]
fn new_of_a_class_no_file_declares_records_not_found() {
    let source = "USING myapp.*.\nv-x = NEW absent-thing().\n";
    let (_stmts, sem) = with_index(source, &WORKSPACE);
    assert!(
        unresolved_reasons(&sem, "absent-thing").contains(&UnresolvedReason::NotFoundInWorkspace),
        "a `NEW` operand is unambiguously a type name, so a miss is a fact about \
         the workspace rather than a shrug"
    );
    // And still no finding: every rule skip-lists the cross-file reasons.
    let findings = lint0001_with_index(source, &WORKSPACE);
    assert_eq!(
        findings.len(),
        1,
        "exactly one finding, and it is for the undeclared `v-x` — not for the \
         class name: {findings:?}"
    );
}

#[test]
fn a_locally_declared_class_wins_over_a_workspace_class_of_the_same_name() {
    // Both are reachable: the wildcard import puts `myapp.cache` within reach
    // under the bare name `cache`, and the file declares its own `cache`.
    let source = "USING myapp.*.\n\
                  CLASS cache:\n\
                      METHOD PUBLIC VOID build():\n\
                          DEFINE VARIABLE v-own AS CLASS cache NO-UNDO.\n\
                          v-own = NEW cache().\n\
                      END METHOD.\n\
                  END CLASS.\n";

    let (_stmts, sem) = with_index(source, &WORKSPACE);
    let sym = sole_resolved(&sem, "cache");
    assert_ne!(
        sem.symbols.get(sym).declaration,
        NodeId::DUMMY,
        "the local declaration wins, so the resolved symbol is a real one"
    );
    assert!(
        symbols_named(&sem, "myapp.cache").is_empty(),
        "the workspace class must not even be synthesized: the scope tree \
         answered first, so the index was never asked"
    );
}

// ---------------------------------------------------------------------------
// Member access through a resolved class type
// ---------------------------------------------------------------------------

/// A `USING`-imported instance, its declaration, and the one statement under
/// test. Shared so each member scenario differs only in its last line.
fn instance_source(last_line: &str) -> String {
    format!(
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
         DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n\
         {last_line}\n"
    )
}

#[test]
fn a_method_call_on_a_using_imported_instance_resolves_with_its_return_type() {
    let source = instance_source("v-label = v-cache:fetch-label().");

    let (_stmts, sem) = with_index(&source, &WORKSPACE);
    let sym = sole_resolved(&sem, "fetch-label");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Function);
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Character)),
        "the resolved symbol carries the class's declared return type"
    );
    assert_synthesized(&sem, sym, NamespaceId::Functions);

    // The firewall: today a method name gets no reference entry at all.
    let (_stmts, plain) = without_index(&source);
    assert!(resolved_to(&plain, "fetch-label").is_empty());
    assert!(symbols_named(&plain, "fetch-label").is_empty());
}

#[test]
fn a_property_read_on_a_using_imported_instance_resolves_and_types() {
    let source = instance_source("v-count = v-cache:entry-count.");

    let (_stmts, sem) = with_index(&source, &WORKSPACE);
    let sym = sole_resolved(&sem, "entry-count");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Property);
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Integer))
    );
    assert_synthesized(&sem, sym, NamespaceId::Values);

    let (_stmts, plain) = without_index(&source);
    assert!(symbols_named(&plain, "entry-count").is_empty());
}

#[test]
fn a_method_call_through_a_using_imported_type_name_resolves() {
    // The receiver-softening path: `cache` is not a local symbol, so today it is
    // rewritten to `External` and the call goes nowhere. With an index it is
    // tried as a type name *first*, and only softened on a miss.
    let source = instance_source("v-label = cache:fetch-label().");

    let (_stmts, sem) = with_index(&source, &WORKSPACE);
    let class = sole_resolved(&sem, "myapp.cache");
    assert_eq!(sem.symbols.get(class).kind, SymbolKind::Class);
    let member = sole_resolved(&sem, "fetch-label");
    assert_eq!(
        sem.symbols.get(member).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Character))
    );

    let (_stmts, plain) = without_index(&source);
    assert_eq!(
        unresolved_reasons(&plain, "cache"),
        vec![UnresolvedReason::External],
        "today's answer: an unresolved receiver is softened, never reported"
    );
}

#[test]
fn a_local_variable_receiver_is_never_shadowed_by_a_workspace_class() {
    // The ordering rule, from the other side: a variable *named* like the
    // imported class must keep resolving to itself.
    let source = "USING myapp.cache.\n\
                  DEFINE VARIABLE cache AS CHARACTER NO-UNDO.\n\
                  DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.\n\
                  v-label = cache.\n";

    let (_stmts, sem) = with_index(source, &WORKSPACE);
    let sym = sole_resolved(&sem, "cache");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Variable);
    assert_ne!(sem.symbols.get(sym).declaration, NodeId::DUMMY);
}

#[test]
fn a_method_the_class_does_not_declare_resolves_as_not_found_and_produces_no_finding() {
    let source = instance_source("v-label = v-cache:absent-method().");

    let (_stmts, sem) = with_index(&source, &WORKSPACE);
    assert!(resolved_to(&sem, "absent-method").is_empty());
    assert!(
        symbols_named(&sem, "absent-method").is_empty(),
        "nothing the class does not offer is synthesized"
    );
    assert_eq!(
        unresolved_reasons(&sem, "absent-method"),
        vec![UnresolvedReason::NotFoundInWorkspace],
        "the class *was* consulted, so this is not `NotInScope` — which is the \
         one reason LINT0001 renders"
    );
    assert!(
        lint0001_with_index(&source, &WORKSPACE).is_empty(),
        "a member the class does not declare is not an undefined symbol"
    );

    // The baseline that proves the rule is not simply inert on this fixture: the
    // same missing name called *unqualified* is a genuine undefined symbol, and
    // fires.
    let baseline = instance_source("v-label = absent-method().");
    let fires = lint0001_with_index(&baseline, &WORKSPACE);
    assert_eq!(
        fires.len(),
        1,
        "an unqualified call to a name nothing declares still fires: {fires:?}"
    );
}

#[test]
fn a_protected_or_private_member_is_not_reachable_from_outside_the_class() {
    // The accessibility split this pass owns rather than the index: a subclass
    // inherits `PROTECTED`, but a caller holding an instance does not see it.
    for member in ["shared-slot", "hidden-slot"] {
        let source = instance_source(&format!("v-count = v-cache:{member}()."));
        let (_stmts, sem) = with_index(&source, &WORKSPACE);
        assert!(
            resolved_to(&sem, member).is_empty(),
            "`{member}` is not part of the class's public surface"
        );
        assert!(
            symbols_named(&sem, member).is_empty(),
            "and nothing is synthesized for it, so a later access-check rule \
             sees a violation rather than a resolved reference"
        );
        assert_eq!(
            unresolved_reasons(&sem, member),
            vec![UnresolvedReason::NotFoundInWorkspace]
        );
        assert!(lint0001_with_index(&source, &WORKSPACE).is_empty());
    }
}

#[test]
fn a_member_of_a_new_expression_resolves_without_an_intervening_variable() {
    let source = "USING myapp.cache.\n\
                  DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.\n\
                  v-label = NEW cache():fetch-label().\n";

    let (_stmts, sem) = with_index(source, &WORKSPACE);
    let sym = sole_resolved(&sem, "fetch-label");
    assert_eq!(
        sem.symbols.get(sym).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Character))
    );
}

#[test]
fn twenty_references_to_one_member_synthesize_it_once() {
    // The memo, asserted through the symbol table rather than through a call
    // counter: the walk is memoized per class and the member symbol per name, so
    // twenty call sites cost one of each.
    let mut source = String::from(
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
         DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.\n",
    );
    for _ in 0..20 {
        source.push_str("v-label = v-cache:fetch-label().\n");
    }
    let (_stmts, sem) = with_index(&source, &WORKSPACE);
    assert_eq!(symbols_named(&sem, "fetch-label").len(), 1);
    assert_eq!(symbols_named(&sem, "myapp.cache").len(), 1);
}

// ---------------------------------------------------------------------------
// The type lattice must not learn about a cross-file class in this unit
// ---------------------------------------------------------------------------

#[test]
fn resolving_a_cross_file_class_produces_no_type_mismatch_finding() {
    // The R11 firewall, on the three shapes that would break it. Each one is a
    // genuine mistake that `type-mismatch-assignment` *would* report the moment
    // a cross-file class became a real `ResolvedType::Class`: `assignable`
    // compares class symbols by identity and knows nothing about inheritance.
    // Until assignability is widened deliberately, a workspace class stays at
    // the lattice bottom — which is exactly where a class-typed declaration
    // whose class lives in another file already sits today.
    let shapes = [
        // A `NEW` of a workspace class assigned to a primitive.
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n\
         v-count = NEW cache().\n",
        // A primitive assigned to a workspace-class-typed variable.
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
         v-cache = 5.\n",
        // A workspace-class-typed variable assigned from a method return.
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
         DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n\
         v-count = v-cache:fetch-label().\n",
    ];
    for source in shapes {
        assert_eq!(
            lint0004_with_index(source, &WORKSPACE),
            lint0004_without_index(source),
            "attaching an index must not change what LINT0004 says about:\n{source}"
        );
        assert!(
            lint0004_with_index(source, &WORKSPACE).is_empty(),
            "and today it says nothing about any of these:\n{source}"
        );
    }
}

#[test]
fn a_locally_declared_class_still_types_as_itself() {
    // The baseline that proves the assertion above is not passing because
    // LINT0004 is inert on assignments involving classes: with the class
    // declared *in this file* the same shape fires, index or no index.
    let source = "CLASS cache:\n\
                  END CLASS.\n\
                  DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n\
                  v-count = NEW cache().\n";
    assert_eq!(
        lint0004_without_index(source).len(),
        1,
        "a local class produces a real `Class` type, so the mismatch is reported"
    );
    assert_eq!(
        lint0004_with_index(source, &WORKSPACE).len(),
        1,
        "and attaching an index does not change that"
    );
}

#[test]
fn attaching_an_index_adds_no_diagnostic_to_any_scenario() {
    // The phase's central property, swept over every `USING` / `NEW` / member-access
    // fixture in this file at once and across **all six** lint rules plus the
    // semantic pass. `resolving_a_cross_file_class_produces_no_type_mismatch_finding`
    // above covers three class-typed shapes through LINT0004 only; this covers every
    // shape through every rule, which is what makes a rule nobody thought about the
    // sweep's problem rather than a reviewer's.
    //
    // The mismatched fixtures come first, and they are the reason the sweep is not
    // vacuous: a *member*'s declared type reaching the lattice is a separate route
    // from a *class*'s, and every member fixture below happened to type-match its
    // assignment (`v-label AS CHARACTER = v-cache:fetch-label()`), so none of them
    // could observe it.
    let mismatched = [
        // A `CHARACTER`-returning method on an imported instance, into an INTEGER.
        instance_source("v-count = v-cache:fetch-label()."),
        // The same, through the type name used as a static receiver.
        instance_source("v-count = cache:fetch-label()."),
        // The same, off a `NEW` with no intervening variable.
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n\
         v-count = NEW cache():fetch-label().\n"
            .to_string(),
        // An INTEGER property read into a CHARACTER, and written from one — both
        // directions, since the rule reads the target's type from one place and the
        // value's from another.
        instance_source("v-label = v-cache:entry-count."),
        instance_source("v-cache:entry-count = v-label."),
    ];

    let scenarios = [
        // Every fixture the tests above use, type-matched ones included, so a change
        // that shifts one of them shows up here too.
        "USING myapp.cache.\nDEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n".to_string(),
        "USING myapp.absent-thing.\nDEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n".to_string(),
        "USING myapp.*.\nDEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n".to_string(),
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
         v-cache = NEW cache().\n"
            .to_string(),
        "DEFINE VARIABLE v-cache AS CLASS myapp.cache NO-UNDO.\n\
         v-cache = NEW myapp.cache().\n"
            .to_string(),
        "USING myapp.*.\n\
         DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
         v-cache = NEW cache().\n"
            .to_string(),
        "USING myapp.*.\nv-x = NEW absent-thing().\n".to_string(),
        "USING myapp.*.\n\
         CLASS cache:\n\
             METHOD PUBLIC VOID build():\n\
                 DEFINE VARIABLE v-own AS CLASS cache NO-UNDO.\n\
                 v-own = NEW cache().\n\
             END METHOD.\n\
         END CLASS.\n"
            .to_string(),
        instance_source("v-label = v-cache:fetch-label()."),
        instance_source("v-count = v-cache:entry-count."),
        instance_source("v-label = cache:fetch-label()."),
        instance_source("v-label = v-cache:absent-method()."),
        instance_source("v-label = absent-method()."),
        // A `PROTECTED` and a `PRIVATE` member reached from outside the class.
        instance_source("v-count = v-cache:shared-slot()."),
        instance_source("v-count = v-cache:hidden-slot()."),
        "USING myapp.cache.\n\
         DEFINE VARIABLE cache AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.\n\
         v-label = cache.\n"
            .to_string(),
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-label AS CHARACTER NO-UNDO.\n\
         v-label = NEW cache():fetch-label().\n"
            .to_string(),
        // The three class-typed shapes, and a locally declared class.
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n\
         v-count = NEW cache().\n"
            .to_string(),
        "USING myapp.cache.\n\
         DEFINE VARIABLE v-cache AS CLASS cache NO-UNDO.\n\
         v-cache = 5.\n"
            .to_string(),
        "CLASS cache:\n\
         END CLASS.\n\
         DEFINE VARIABLE v-count AS INTEGER NO-UNDO.\n\
         v-count = NEW cache().\n"
            .to_string(),
        "DEFINE VARIABLE c-name AS CHARACTER NO-UNDO.\nRUN VALUE(c-name).\n".to_string(),
    ];

    for source in mismatched.iter().chain(&scenarios) {
        assert_index_adds_no_diagnostic(source, &WORKSPACE);
    }
}

#[test]
fn a_mismatch_through_a_colon_qualified_member_stays_silent_at_the_call_site() {
    // Every ingredient is present and cross-file — the member resolves, its
    // declared `CHARACTER` is on the symbol, the target is a genuinely
    // incompatible `INTEGER` — and LINT0004 still says nothing. The reason is no
    // longer that the type is parked off the symbol; it is on `data_type` now.
    // It is that `check.rs` types every `MethodCall` as `Unknown` without
    // consulting the symbol it resolved to, so the *expression*, not the symbol,
    // is what the rule cannot judge.
    //
    // That boundary is deliberate and scoped out of this phase: `:`-qualified
    // access is the ordinary OO-ABL spelling and therefore the larger half of the
    // population, which deserves its own evidence rather than being folded into
    // this one. Pinned here so the day it opens, this test is what says so.
    let source = instance_source("v-count = v-cache:fetch-label().");
    let (_stmts, sem) = with_index(&source, &WORKSPACE);

    let member = sole_resolved(&sem, "fetch-label");
    assert_eq!(
        sem.symbols.get(member).data_type.as_ref(),
        Some(&ResolvedType::Primitive(PrimitiveTy::Character)),
        "the imported member carries its declared type, like any other symbol"
    );
    assert_eq!(
        sem.symbols.get(sole_symbol(&sem, "v-count")).data_type,
        Some(ResolvedType::Primitive(PrimitiveTy::Integer)),
        "and the target is a genuinely incompatible INTEGER"
    );
    assert!(lint0004_with_index(&source, &WORKSPACE).is_empty());
}

// ---------------------------------------------------------------------------
// Untouched by this unit
// ---------------------------------------------------------------------------

#[test]
fn run_value_still_produces_the_dynamic_arm() {
    // AE2, restated as a non-regression: a run-time-computed target is not a
    // name this unit resolves, and the expression inside it is walked exactly as
    // before — with an index attached and without.
    let source = "DEFINE VARIABLE c-name AS CHARACTER NO-UNDO.\nRUN VALUE(c-name).\n";

    for (label, (stmts, sem)) in [
        ("with an index", with_index(source, &WORKSPACE)),
        ("without one", without_index(source)),
    ] {
        let dynamic = stmts.iter().any(|s| {
            matches!(
                &s.kind,
                StatementKind::Run {
                    target: RunTarget::Dynamic(_),
                    ..
                }
            )
        });
        assert!(dynamic, "{label}: RUN VALUE must stay the dynamic arm");
        let sym = sole_resolved(&sem, "c-name");
        assert_eq!(
            sem.symbols.get(sym).kind,
            SymbolKind::Variable,
            "{label}: the computed name's expression resolves normally"
        );
        assert!(
            unresolved_reasons(&sem, "c-name").is_empty(),
            "{label}: and carries no unresolved entry"
        );
    }
}
