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
//!
//! One exception, for a file whose text as written does not parse or names a
//! class with a `{&name}` reference: the file's *own* preprocessor directives are
//! applied, with no file read, and the result is used when no include is left
//! unexpanded. Facts then depend on the one file's text and on the configured
//! predefined names (`{&OPSYS}` and friends), which is what both caches need.

mod batch;
mod deps;
mod facts;
mod producers;
// Public because a second cache lives outside this crate: the language server's
// salsa-backed index does its own memoization but must use the *same*
// name-to-path policy — two spellings tried in order, exactly one match, `.i`
// never a root, no escaping the configured paths. A private module would force
// that policy to be written twice, which is precisely the drift R7 exists to
// prevent.
pub mod search;

pub use batch::{BatchIndex, ExcludingFile};
pub use deps::{
    BranchKind, Completeness, ConditionalBranch, ConditionalInclude, DegradedReason,
    DependencyEdge, DependencyEdges, DirectIncludeInput, EdgeInputs, EdgeKind, EdgeTarget,
    IncludeOriginInput, NestedUnresolvedIncludeInput, UnreadableIncludeInput,
    UnresolvedIncludeInput, UnresolvedReference, build_edge_set,
};
pub use facts::{ClassFacts, FileFacts};
pub use producers::{SharedProducers, definitions_in};

use std::sync::Arc;

use oxabl_ast::{AccessModifier, DataType, Statement, StatementKind};
use oxabl_common::FileId;
use oxabl_lexer::{Kind, Token};
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_schema::Schema;
use oxabl_semantic::{
    AnalysisContext, ClassDescriptor, ClassKind, ImportContext, IndexName, IndexedFileId,
    MemberDescriptor, MemberType, ResolvedType, ScopeId, SymbolFlags, SymbolKind, declare_pass,
};
use oxabl_workspace::{InMemoryFileSystem, PreprocessorConfig};

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
/// `predefined` answers the AVM's predefined names (`{&OPSYS}` and friends) when
/// the fallback applies the file's own directives, so a directive-chosen
/// declaration follows the configured environment.
///
/// Not guarded against panics, on purpose — see the module docs.
pub fn index_file(file: IndexedFileId, source: &str, predefined: &PreprocessorConfig) -> FileFacts {
    let facts = index_parsed(file, source);
    if facts.parsed && !names_a_reference(&facts) {
        return facts;
    }
    match expand_in_file(source, predefined).map(|expanded| index_parsed(file, &expanded)) {
        Some(expanded) if expanded.parsed => expanded,
        _ => facts,
    }
}

/// Whether a class header in `facts` is spelled with an unexpanded `{&name}`
/// reference, which the parser accepts as an ordinary name.
fn names_a_reference(facts: &FileFacts) -> bool {
    facts.classes.iter().any(|class| {
        let descriptor = &class.descriptor;
        descriptor.name.as_str().contains('{')
            || descriptor
                .inherits
                .iter()
                .chain(&descriptor.implements)
                .any(|name| name.as_str().contains('{'))
    })
}

/// `source` with its own preprocessor directives applied and every `{include}`
/// left unresolved.
///
/// The fallback for a file whose raw text does not parse because the
/// declaration is built from `&GLOBAL-DEFINE` names (`CLASS {&fullName}:`).
/// No file is read, so the result depends on `source` alone, which is what keeps
/// a memoizing cache's invalidation honest. `None` when the directives alone do
/// not settle the text: a class whose header or body comes from an `{include}`
/// stays unusable.
fn expand_in_file(source: &str, predefined: &PreprocessorConfig) -> Option<String> {
    let fs = InMemoryFileSystem::new();
    let processed = Preprocessor::new(&fs, &[])
        .with_predefined(predefined)
        .process(INDEXED_FILE, source)
        .ok()?;
    // Any include left unexpanded could have defined a name a later `&IF` reads,
    // so the branches taken, and the facts read off them, would be a guess.
    let guessed = !processed.diagnostics.is_empty()
        || !processed.unresolved_includes.is_empty()
        || !processed.unreadable_includes.is_empty();
    (!guessed).then(|| processed.to_text().to_string())
}

/// The facts of `source` as written, or the unparseable record if it parses
/// with recovered errors.
fn index_parsed(file: IndexedFileId, source: &str) -> FileFacts {
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

    let mut imports = ImportContext::from_statements(&program.statements);
    if include_before_header(&tokens, &program.statements) {
        imports = imports.with_unseen_imports();
    }

    FileFacts {
        file,
        parsed: true,
        classes: declared_classes(file, &program.statements, &Arc::new(imports))
            .into_iter()
            .chain(declared_enums(file, &tokens, source))
            .collect(),
        procedures,
        functions,
        shared_definitions,
    }
}

/// Whether an `{include}` stands ahead of the first class or interface header
/// statement, where it could bring in `USING` statements the parse does not see.
///
/// The header is found in the statements, not the tokens: `USING Progress.Lang.Class.`
/// holds a `CLASS` token that is no header. With no header statement at file
/// level, an include anywhere counts.
fn include_before_header(tokens: &[Token], statements: &[Statement]) -> bool {
    let header = statements
        .iter()
        .find(|stmt| {
            matches!(
                stmt.kind,
                StatementKind::Class { .. } | StatementKind::Interface { .. }
            )
        })
        .map_or(usize::MAX, |stmt| stmt.span.start as usize);
    tokens
        .iter()
        .take_while(|t| t.start < header)
        .any(|t| t.kind == Kind::IncludeReference)
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
///
/// A class that names no supertype carries no imports, so an edit to the file's
/// `USING` list leaves its descriptor, and every answer keyed on it, unchanged.
fn declared_classes(
    file: IndexedFileId,
    statements: &[Statement],
    imports: &Arc<ImportContext>,
) -> Vec<ClassFacts> {
    let imports_for = |has_supertype: bool| {
        if has_supertype {
            Arc::clone(imports)
        } else {
            Arc::default()
        }
    };
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
                    imports: imports_for(inherits.is_some() || !implements.is_empty()),
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
                    imports: imports_for(!inherits.is_empty()),
                }),
                members: declared_members(body),
            });
        }
    }
    classes
}

/// Project the `ENUM` declarations at file level.
///
/// The parser skips an enum body without keeping a node, so this reads the token
/// stream: an `ENUM` at the start of a statement is a declaration, and a
/// `DEFINE ENUM` inside it is a member list. A member list that does not follow
/// the grammar yields no members rather than a guessed one.
fn declared_enums(file: IndexedFileId, tokens: &[Token], source: &str) -> Vec<ClassFacts> {
    let text = |t: &Token| &source[t.start..t.end];
    let is_word = |t: &Token| text(t).starts_with(|c: char| c.is_alphabetic() || c == '_');
    let mut enums = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let previous = tokens[..i].iter().rev().find(|t| t.kind != Kind::Comment);
        let at_statement_start = previous.is_none_or(|t| t.kind == Kind::Period);
        if !(at_statement_start && tokens[i].kind == Kind::KwEnum) {
            i += 1;
            continue;
        }
        let Some(first) = tokens.get(i + 1).filter(|t| is_word(t)) else {
            i += 1;
            continue;
        };
        let mut name = text(first).to_string();
        i += 2;
        while let (
            Some(Token {
                kind: Kind::Period, ..
            }),
            Some(next),
        ) = (tokens.get(i), tokens.get(i + 1).filter(|t| is_word(t)))
        {
            name.push('.');
            name.push_str(text(next));
            i += 2;
        }
        let mut members = Vec::new();
        let mut well_formed = true;
        while i < tokens.len() && !(tokens[i].kind == Kind::End) {
            if tokens[i].kind == Kind::Define
                && tokens.get(i + 1).is_some_and(|t| t.kind == Kind::KwEnum)
            {
                let (names, next) = enum_members(&tokens[i + 2..], source);
                well_formed &= names.is_some();
                members.extend(names.unwrap_or_default());
                i += 2 + next;
            } else {
                i += 1;
            }
        }
        enums.push(ClassFacts {
            descriptor: Arc::new(ClassDescriptor {
                name: IndexName::new(&name),
                file,
                kind: ClassKind::Enum,
                inherits: None,
                implements: Vec::new(),
                imports: Arc::default(),
            }),
            members: if well_formed {
                members.into()
            } else {
                Arc::from([])
            },
        });
    }
    enums
}

/// The member names of one `DEFINE ENUM` list, and how many tokens it spans.
///
/// `None` when the list is not `name [= value] [, ...]` with `|`-joined values.
fn enum_members(tokens: &[Token], source: &str) -> (Option<Vec<MemberDescriptor>>, usize) {
    let text = |t: &Token| &source[t.start..t.end];
    let mut members = Vec::new();
    let mut i = 0;
    let mut ok = true;
    while let Some(token) = tokens.get(i) {
        match token.kind {
            Kind::Period => {
                i += 1;
                break;
            }
            Kind::Comma => i += 1,
            _ if text(token).starts_with(|c: char| c.is_alphabetic() || c == '_') => {
                members.push(MemberDescriptor {
                    name: IndexName::new(text(token)),
                    kind: SymbolKind::Property,
                    ty: MemberType::Untyped,
                    access: AccessModifier::Public,
                    is_static: true,
                });
                i += 1;
                if tokens.get(i).is_some_and(|t| t.kind == Kind::Equals) {
                    i += 1;
                    if tokens.get(i).is_some_and(|t| t.kind == Kind::Minus) {
                        i += 1;
                    }
                    i += 1;
                    while tokens.get(i).is_some_and(|t| text(t) == "|") {
                        i += 2;
                    }
                }
            }
            _ => {
                ok = false;
                i += 1;
            }
        }
    }
    (ok.then_some(members), i)
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
    if let Some(name) = declared.class_name() {
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

    fn index_file(file: IndexedFileId, source: &str) -> FileFacts {
        super::index_file(file, source, &PreprocessorConfig::default())
    }

    fn portable(ty: ResolvedType) -> MemberType {
        MemberType::Portable(PortableType::new(ty).expect("fixture type is portable"))
    }

    #[test]
    fn an_enum_is_indexed_as_a_class_with_its_members() {
        let facts = index_file(
            FILE,
            "/* header */\nENUM orders.Shade FLAGS:\n    \
             DEFINE ENUM light = 1, dark = 2\n        both = light | dark\n        none.\n\
             END ENUM.\n",
        );

        assert!(facts.parsed);
        let class = facts
            .class(&IndexName::new("orders.shade"))
            .expect("the enum is indexed under its qualified name");
        assert_eq!(class.descriptor.kind, ClassKind::Enum);
        let names: Vec<_> = class.members.iter().map(|m| m.name.as_str()).collect();
        assert_eq!(names, ["light", "dark", "both", "none"]);
        assert!(class.members.iter().all(|m| m.is_static));
    }

    #[test]
    fn a_define_enum_that_is_not_an_enum_declaration_indexes_nothing() {
        let facts = index_file(
            FILE,
            "DEFINE VARIABLE enum AS INTEGER NO-UNDO.\nenum = 1.\n",
        );
        assert!(facts.classes.is_empty());
    }

    #[test]
    fn a_class_named_by_its_own_directives_is_indexed_under_the_expanded_name() {
        let facts = index_file(
            FILE,
            "&GLOBAL-DEFINE fullName orders.built\nCLASS {&fullName}:\nEND CLASS.\n",
        );
        assert!(facts.class(&IndexName::new("orders.built")).is_some());
    }

    #[test]
    fn a_class_whose_directives_depend_on_an_include_is_not_guessed_at() {
        let facts = index_file(
            FILE,
            "{names.i}\n&IF DEFINED(legacy) = 0 &THEN\nCLASS orders.built:\n&ELSE\n\
             CLASS orders.built INHERITS orders.old:\n&ENDIF\nEND CLASS.\n",
        );
        assert!(facts.class(&IndexName::new("orders.built")).is_none());
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

    fn imports_of(facts: &FileFacts, class: &str) -> Arc<ImportContext> {
        Arc::clone(
            &facts
                .class(&IndexName::new(class))
                .expect("the class is indexed")
                .descriptor
                .imports,
        )
    }

    #[test]
    fn a_class_with_a_parent_carries_the_imports_it_names_it_under() {
        let wildcard = index_file(
            FILE,
            "USING rules.*.\nCLASS orders.mid INHERITS base:\nEND CLASS.\n",
        );
        let explicit = index_file(
            FILE,
            "USING rules.base.\nCLASS orders.mid INHERITS base:\nEND CLASS.\n",
        );
        let bare = index_file(FILE, "CLASS orders.mid INHERITS base:\nEND CLASS.\n");

        let (wildcard, explicit, bare) = (
            imports_of(&wildcard, "orders.mid"),
            imports_of(&explicit, "orders.mid"),
            imports_of(&bare, "orders.mid"),
        );
        assert!(!wildcard.is_empty() && !explicit.is_empty() && bare.is_empty());
        assert_ne!(wildcard, explicit, "the two import forms differ");
        assert_ne!(wildcard, bare);
    }

    #[test]
    fn an_interface_carries_the_imports_of_the_interfaces_it_extends() {
        let facts = index_file(
            FILE,
            "USING rules.*.\nINTERFACE orders.i-mid INHERITS i-base:\nEND INTERFACE.\n",
        );
        assert!(!imports_of(&facts, "orders.i-mid").is_empty());
    }

    #[test]
    fn a_class_with_no_parent_carries_no_imports() {
        // An edit to the `USING` list of a class nothing walks past leaves its
        // descriptor, and so every answer keyed on it, unchanged.
        let facts = index_file(FILE, "USING rules.*.\nCLASS orders.leaf:\nEND CLASS.\n");
        assert_eq!(*imports_of(&facts, "orders.leaf"), ImportContext::default());
    }

    #[test]
    fn imports_that_differ_only_in_case_are_different_facts() {
        // A cache keyed on equality must not serve the paths one spelling derives
        // for the other.
        let upper = index_file(
            FILE,
            "USING Rules.Base.\nCLASS orders.mid INHERITS Base:\nEND CLASS.\n",
        );
        let lower = index_file(
            FILE,
            "USING rules.base.\nCLASS orders.mid INHERITS Base:\nEND CLASS.\n",
        );
        assert_ne!(
            imports_of(&upper, "orders.mid"),
            imports_of(&lower, "orders.mid")
        );
    }

    #[test]
    fn an_include_ahead_of_the_header_may_hold_imports_the_index_does_not_read() {
        let hidden = index_file(
            FILE,
            "{usings.i}\nCLASS orders.mid INHERITS base:\nEND CLASS.\n",
        );
        assert!(imports_of(&hidden, "orders.mid").has_unseen_imports());

        let after_header = index_file(
            FILE,
            "CLASS orders.mid INHERITS base:\n    METHOD PUBLIC VOID m():\n        {body.i}\n    \
             END METHOD.\nEND CLASS.\n",
        );
        assert!(!imports_of(&after_header, "orders.mid").has_unseen_imports());
    }
}
