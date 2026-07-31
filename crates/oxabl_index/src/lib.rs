//! Fact extraction and the batch workspace index.
//!
//! This crate is the *implementation* side of the seam
//! [`oxabl_semantic::index`] defines: one routine, [`index_file`], that turns a
//! referenced file into the facts the index answers with, and one plain in-run
//! cache over it, [`BatchIndex`], that dedups shared dependencies.
//!
//! # Why the extraction lives here and not in a client
//!
//! Both caches — this crate's `BatchIndex` and the language server's
//! salsa-backed one — call [`index_file`]. Only the memoization differs. That is
//! what makes R7 ("one shared question set, no client carve-outs, so every
//! client resolves identically and differs only in what files exist and how
//! answers are cached") structural rather than documented: there is no second
//! copy of the projection to drift.
//!
//! # What is not done here
//!
//! No panic guard. Every query is *total* in its answers — found, not-found,
//! unknowable — but totality is not licence to swallow unwinding: salsa's
//! `Cancelled` travels as a panic payload in this workspace, so a guard around a
//! lookup would turn a cancelled recompute into `NotFound` and freeze a buffer
//! on stale results. Cancellation propagates, exactly as it does through the
//! deliberately unguarded `LintPipeline::expand`/`collect`.
//!
//! No include expansion. A referenced file is indexed as written; a declaration
//! that only exists after an `{include}` splice is invisible to the index. That
//! is the conservative direction — a missing fact yields a missing link, which
//! by the R11 firewall produces no finding.

mod batch;
mod facts;
mod search;

pub use batch::{BatchIndex, ExcludingFile};
pub use facts::{ClassFacts, FileFacts};

use std::sync::Arc;

use oxabl_ast::{DataType, Statement, StatementKind};
use oxabl_common::FileId;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{
    AnalysisContext, ClassDescriptor, ClassKind, IndexName, IndexedFileId, MemberDescriptor,
    MemberType, ResolvedType, ScopeId, SymbolFlags, SymbolKind, declare_pass,
};

/// The [`FileId`] the declare pass is run under while indexing.
///
/// Every diagnostic the pass produces is discarded — a referenced file's errors
/// belong to *that* file's own analysis, not to the file that referenced it — so
/// this id never reaches a rendered span. It exists because
/// [`AnalysisContext`] requires one.
const INDEXED_FILE: FileId = FileId::new(1);

/// Extract the facts `source` contributes to the index, under the identity
/// `file`.
///
/// Tokenizes, parses, and runs the **declare pass**, then projects the result
/// down to [`FileFacts`] and drops everything else.
///
/// A parse that recovered any error yields [`FileFacts::unparseable`] rather
/// than partial facts. Error recovery resynchronizes on period boundaries, so a
/// broken statement can leave a class body missing members or a member carrying
/// the wrong type — and a *wrong* fact is poison in a way a missing one is not:
/// it mis-attributes symbols across the program graph, while a missing fact just
/// leaves a name unresolved and silent.
///
/// Not guarded against panics, on purpose — see the module docs.
pub fn index_file(file: IndexedFileId, source: &str) -> FileFacts {
    let tokens = oxabl_lexer::tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    if !program.is_ok() {
        return FileFacts::unparseable(file);
    }

    // The declare pass is the same machinery a client runs over the file it is
    // analyzing, which is what keeps an indexed declaration and a local one from
    // being recognized by two different sets of rules. It is also why the
    // routine-level facts below read the *symbol table* rather than the
    // statement list: a `FUNCTION` prototype and its later definition merge into
    // one symbol there (#69), and a name the pass refused to declare twice
    // appears once.
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(INDEXED_FILE, source, &schema);
    let (_scopes, symbols, _diagnostics, _schema_revision) =
        declare_pass(&program.statements, &ctx);

    let mut procedures = Vec::new();
    let mut functions = Vec::new();
    let mut shared_definitions = Vec::new();
    for (_id, symbol) in symbols.iter() {
        // `IndexName::new` folds, and the atom is already folded, so this is an
        // idempotent second pass over ASCII bytes plus an intern-table hit.
        if symbol.kind == SymbolKind::Procedure {
            procedures.push(IndexName::new(&symbol.name));
        } else if symbol.kind == SymbolKind::Function && symbol.declared_in == ScopeId::ROOT {
            // A method carries `SymbolKind::Function` too, but is declared in
            // its class's scope and is reported as a member instead.
            functions.push(IndexName::new(&symbol.name));
        }
        if symbol
            .flags
            .intersects(SymbolFlags::NEW_SHARED | SymbolFlags::NEW_GLOBAL_SHARED)
        {
            shared_definitions.push(IndexName::new(&symbol.name));
        }
    }

    FileFacts {
        file,
        parsed: true,
        classes: declared_classes(file, &program.statements),
        procedures,
        functions,
        shared_definitions,
    }
}

/// Project the class and interface declarations of `statements`.
///
/// Read from the AST rather than from the declare pass's symbols, because the
/// two facts the seam needs are precisely the two the pass discards: it keeps no
/// record of a class header (`INHERITS` / `IMPLEMENTS` are not symbols), and it
/// lowers a class-typed declaration to
/// [`ResolvedType::Unknown`](oxabl_semantic::ResolvedType::Unknown) by design,
/// dropping the type *name* that [`MemberType::Named`] exists to carry. The AST
/// spelling — an unresolved name, as the declaring file wrote it — is exactly
/// what a cross-file answer must carry anyway, since resolving it is the
/// consumer's job in the consumer's own symbol space.
///
/// File level only: a class nested inside a preprocessor `&IF` branch is not
/// indexed, which is the conservative direction (a missing fact, not a wrong
/// one).
fn declared_classes(file: IndexedFileId, statements: &[Statement]) -> Vec<ClassFacts> {
    let mut classes = Vec::new();
    for stmt in statements {
        if let StatementKind::Class {
            name,
            inherits,
            implements,
            body,
            ..
        } = &stmt.kind
        {
            classes.push(ClassFacts {
                descriptor: Arc::new(ClassDescriptor {
                    name: IndexName::new(&name.name),
                    file,
                    kind: ClassKind::Class,
                    inherits: inherits.as_ref().map(|id| IndexName::new(&id.name)),
                    implements: implements
                        .iter()
                        .map(|id| IndexName::new(&id.name))
                        .collect(),
                }),
                members: declared_members(body),
            });
        } else if let StatementKind::Interface {
            name,
            inherits,
            body,
        } = &stmt.kind
        {
            classes.push(ClassFacts {
                descriptor: Arc::new(ClassDescriptor {
                    name: IndexName::new(&name.name),
                    file,
                    kind: ClassKind::Interface,
                    // An interface may extend *several* interfaces, so its
                    // `INHERITS` list cannot fit `inherits: Option<_>`. The list
                    // goes to `implements`, which is already "the other
                    // supertypes, as a set" — no information is lost and the
                    // consumer's walk is unchanged, whereas picking a first
                    // parent and discarding the rest would silently drop
                    // members.
                    inherits: None,
                    implements: inherits.iter().map(|id| IndexName::new(&id.name)).collect(),
                }),
                members: declared_members(body),
            });
        }
    }
    classes
}

/// Project the members a class or interface body declares itself.
///
/// Methods and properties only. A class-level `DEFINE VARIABLE` is reachable as
/// a data member in ABL, but no planned consumer asks for one, so indexing it
/// would be a fact nothing reads.
fn declared_members(body: &[Statement]) -> Arc<[MemberDescriptor]> {
    let mut members = Vec::new();
    for stmt in body {
        if let StatementKind::Method {
            access,
            is_static,
            return_type,
            name,
            ..
        } = &stmt.kind
        {
            members.push(MemberDescriptor {
                name: IndexName::new(&name.name),
                // The same kind the declare pass gives a locally declared
                // method, so a synthesized member is indistinguishable from a
                // local one downstream.
                kind: SymbolKind::Function,
                // `None` is `VOID`: the declaration names no type at all, which
                // is a different fact from naming one the index cannot carry.
                ty: return_type
                    .as_ref()
                    .map_or(MemberType::Untyped, member_type),
                access: *access,
                is_static: *is_static,
            });
        } else if let StatementKind::Property {
            access,
            is_static,
            name,
            data_type,
            ..
        } = &stmt.kind
        {
            members.push(MemberDescriptor {
                name: IndexName::new(&name.name),
                kind: SymbolKind::Property,
                ty: member_type(data_type),
                access: *access,
                is_static: *is_static,
            });
        }
    }
    members.into()
}

/// Lower a declared [`DataType`] into a form that survives the file boundary.
///
/// A class-typed declaration becomes [`MemberType::Named`] — the name as
/// written, for the consumer to resolve in its own symbol space. Every other
/// spelling goes through the declare pass's own
/// [`ResolvedType::from_data_type`], so an indexed member's type and a local
/// declaration's type are computed by one conversion rather than two that could
/// disagree.
fn member_type(declared: &DataType) -> MemberType {
    if let DataType::Class(name) = declared {
        return MemberType::Named(IndexName::new(name));
    }
    // `from_data_type` yields only primitives, arrays over them, and `Unknown`
    // (its answer for a class type and for an unresolved `&IF` type position),
    // all of which are portable — so the fallback is unreachable today and is
    // here only so a future non-portable lowering degrades to "carries no type"
    // instead of dropping the member.
    MemberType::portable(ResolvedType::from_data_type(declared)).unwrap_or(MemberType::Untyped)
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::AccessModifier;
    use oxabl_semantic::{PortableType, PrimitiveTy};

    const FILE: IndexedFileId = IndexedFileId::new(7);

    fn portable(ty: ResolvedType) -> MemberType {
        MemberType::Portable(PortableType::new(ty).expect("fixture type is portable"))
    }

    #[test]
    fn class_facts_carry_header_and_public_members() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc INHERITS orders.calc-base IMPLEMENTS orders.i-calc, orders.i-audit:
                   DEFINE PUBLIC PROPERTY label AS CHARACTER NO-UNDO GET. SET.
                   METHOD PUBLIC INTEGER calc-total():
                       RETURN 0.
                   END METHOD.
                   METHOD PUBLIC VOID reset():
                   END METHOD.
               END CLASS."#,
        );

        assert!(facts.parsed);
        assert_eq!(facts.classes.len(), 1);
        let class = facts
            .class(&IndexName::new("Orders.Total-Calc"))
            .expect("the class is indexed under its folded qualified name");
        assert_eq!(class.descriptor.file, FILE);
        assert_eq!(class.descriptor.kind, ClassKind::Class);
        assert_eq!(
            class.descriptor.inherits,
            Some(IndexName::new("orders.calc-base"))
        );
        assert_eq!(
            class.descriptor.implements,
            vec![
                IndexName::new("orders.i-calc"),
                IndexName::new("orders.i-audit")
            ]
        );

        let members = &class.members;
        assert_eq!(members.len(), 3, "one property and two methods");
        assert_eq!(members[0].name, IndexName::new("label"));
        assert_eq!(members[0].kind, SymbolKind::Property);
        assert_eq!(
            members[0].ty,
            portable(ResolvedType::Primitive(PrimitiveTy::Character))
        );
        assert_eq!(members[1].name, IndexName::new("calc-total"));
        assert_eq!(members[1].kind, SymbolKind::Function);
        assert_eq!(
            members[1].ty,
            portable(ResolvedType::Primitive(PrimitiveTy::Integer))
        );
        assert_eq!(members[2].name, IndexName::new("reset"));
        assert_eq!(
            members[2].ty,
            MemberType::Untyped,
            "VOID names no type at all"
        );
        assert!(members.iter().all(|m| m.access == AccessModifier::Public));
        assert!(members.iter().all(|m| !m.is_static));
    }

    #[test]
    fn a_class_typed_member_carries_the_name_not_a_dangling_id() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   DEFINE PUBLIC PROPERTY owner AS CLASS orders.calc-base NO-UNDO GET.
               END CLASS."#,
        );
        let class = facts.class(&IndexName::new("orders.total-calc")).unwrap();
        assert_eq!(
            class.members[0].ty,
            MemberType::Named(IndexName::new("orders.calc-base")),
        );
    }

    #[test]
    fn non_public_members_are_indexed_but_not_inherited() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   METHOD PRIVATE VOID audit():
                   END METHOD.
                   METHOD PROTECTED STATIC VOID reset():
                   END METHOD.
               END CLASS."#,
        );
        let class = facts.class(&IndexName::new("orders.total-calc")).unwrap();
        assert_eq!(class.members[0].access, AccessModifier::Private);
        assert!(!class.members[0].inherited_by_subclass());
        assert_eq!(class.members[1].access, AccessModifier::Protected);
        assert!(class.members[1].is_static);
        assert!(class.members[1].inherited_by_subclass());
    }

    #[test]
    fn an_interface_is_indexed_with_its_supertypes_as_implements() {
        let facts = index_file(
            FILE,
            "INTERFACE orders.i-calc INHERITS orders.i-audit: END INTERFACE.",
        );
        let iface = facts.class(&IndexName::new("orders.i-calc")).unwrap();
        assert_eq!(iface.descriptor.kind, ClassKind::Interface);
        assert_eq!(iface.descriptor.inherits, None);
        assert_eq!(
            iface.descriptor.implements,
            vec![IndexName::new("orders.i-audit")]
        );
    }

    #[test]
    fn internal_procedures_and_functions_are_indexed() {
        let facts = index_file(
            FILE,
            r#"PROCEDURE post-order:
                   MESSAGE "posted".
               END PROCEDURE.

               FUNCTION calc-tax RETURNS DECIMAL (INPUT p-amount AS DECIMAL):
                   RETURN p-amount * 0.1.
               END FUNCTION."#,
        );
        assert!(facts.parsed);
        assert_eq!(facts.procedures, vec![IndexName::new("post-order")]);
        assert_eq!(facts.functions, vec![IndexName::new("calc-tax")]);
    }

    #[test]
    fn a_method_is_not_reported_as_a_file_level_function() {
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   METHOD PUBLIC VOID reset():
                   END METHOD.
               END CLASS."#,
        );
        assert!(
            facts.functions.is_empty(),
            "a method is reached through its class, not as a file-level function"
        );
    }

    #[test]
    fn only_new_shared_definitions_are_producers() {
        let facts = index_file(
            FILE,
            r#"DEFINE NEW SHARED VARIABLE v-order-count AS INTEGER NO-UNDO.
               DEFINE NEW GLOBAL SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.
               DEFINE SHARED VARIABLE v-consumed AS INTEGER NO-UNDO."#,
        );
        assert!(facts.parsed);
        assert!(facts.defines_shared(&IndexName::new("v-order-count")));
        assert!(facts.defines_shared(&IndexName::new("V-SITE-CODE")));
        assert!(
            !facts.defines_shared(&IndexName::new("v-consumed")),
            "a plain DEFINE SHARED is a consumer, not a producer"
        );
        assert_eq!(facts.shared_definitions.len(), 2);
    }

    #[test]
    fn a_broken_file_is_distinguishable_from_one_that_declares_nothing() {
        let broken = index_file(FILE, "DEFINE VARIABLE .");
        assert!(!broken.parsed, "a recovered parse error is not usable");
        assert!(broken.classes.is_empty());
        assert!(broken.procedures.is_empty());
        assert!(broken.functions.is_empty());
        assert!(broken.shared_definitions.is_empty());

        let quiet = index_file(FILE, "MESSAGE \"nothing declared here\".");
        assert!(quiet.parsed, "a file may parse and declare nothing");
        assert!(quiet.classes.is_empty());
        assert!(quiet.procedures.is_empty());
        assert!(quiet.functions.is_empty());
        assert!(quiet.shared_definitions.is_empty());
    }

    #[test]
    fn a_broken_class_body_yields_no_partial_class() {
        // The whole-file conservatism: recovery could leave the member list
        // short or mistyped, and a wrong member is worse than no class.
        let facts = index_file(
            FILE,
            r#"CLASS orders.total-calc:
                   DEFINE PUBLIC PROPERTY AS CHARACTER NO-UNDO GET.
               END CLASS."#,
        );
        assert!(!facts.parsed);
        assert!(facts.class(&IndexName::new("orders.total-calc")).is_none());
    }
}
