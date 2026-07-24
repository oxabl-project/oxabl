//! JSON dump for the [`Semantic`] model plus lint-diagnostics envelope.
//!
//! `oxabl_analyze` lives outside `oxabl_semantic` so the semantic crate
//! stays free of any `serde_json` dependency — formatter, LSP, and future
//! workspace consumers shouldn't transitively pull a JSON encoder just to
//! reach the side-table model.
//!
//! The dump uses **per-section versioning** so breaking changes to any one
//! section (scopes, symbols, references, types, diagnostics) bump only
//! that section's version, not the whole envelope.
//!
//! `symbols` section v2: the symbol table now includes schema-derived
//! entries — synthesized `field` symbols for schema-validated field
//! references and default-`buffer` symbols for bare table names (both
//! marked with `declaration == NodeId::DUMMY`, i.e. `u32::MAX`).
//!
//! ```text
//! {
//!   "envelope": 1,
//!   "sections": {
//!     "scopes": 1,
//!     "symbols": 2,
//!     "types": 1,
//!     "references": 1,
//!     "diagnostics": 1
//!   },
//!   "schema_revision": 0,
//!   "scopes": [ ... ],
//!   "symbols": [ ... ],
//!   "references": [ ... ],
//!   "types": [ ... ],
//!   "diagnostics": [ ... ]
//! }
//! ```

mod collect;

pub use collect::{
    CollectedDiagnostic, CollectedDiagnostics, DiagnosticSource, ExpandedFile, collect_diagnostics,
    collect_from_expanded, collect_with_model, expand_source, is_loud,
};

use oxabl_ast::{NodeId, Statement};
use oxabl_common::{Diagnostic, Severity};
use oxabl_semantic::{
    AnalysisContext, NamespaceId, Resolution, ResolvedType, ScopeId, ScopeKind, Semantic, SymbolId,
    SymbolKind, UnresolvedReason,
};
use serde::Serialize;
use serde_json::{Map, Value, json};

/// Current envelope version. Bump only on **breaking structural** changes to
/// the outermost JSON object (e.g. moving a section out into its own file).
pub const ENVELOPE_VERSION: u32 = 1;

/// Produce a stable, versioned JSON document describing a file's semantic
/// analysis. Includes lint diagnostics from [`oxabl_lint::lint_file`].
pub fn dump_json(
    program: &[Statement],
    sem: &Semantic,
    ctx: &AnalysisContext,
    include_lint: bool,
) -> Value {
    let lint_diags: Vec<Diagnostic> = if include_lint {
        oxabl_lint::lint_file(program, sem, ctx)
    } else {
        Vec::new()
    };

    let mut sections = Map::new();
    sections.insert("scopes".into(), json!(1));
    // v2: symbols include schema-derived synthetic entries (declaration =
    // NodeId::DUMMY) when a schema is loaded.
    sections.insert("symbols".into(), json!(2));
    sections.insert("types".into(), json!(1));
    sections.insert("references".into(), json!(1));
    sections.insert("diagnostics".into(), json!(1));

    json!({
        "envelope": ENVELOPE_VERSION,
        "sections": Value::Object(sections),
        "schema_revision": sem.schema_revision.raw(),
        "scopes": scopes_json(sem),
        "symbols": symbols_json(sem),
        "references": references_json(sem),
        "types": types_json(sem),
        "diagnostics": diagnostics_json(sem, &lint_diags),
    })
}

/// Produce the versioned JSON document, sourcing the `diagnostics` section
/// from a pre-computed [`CollectedDiagnostics`] instead of re-running the lint
/// pass internally. This is the path the CLI `analyze` command uses so its
/// diagnostic set is byte-for-byte the collector's (R7): the non-diagnostic
/// sections (scopes, symbols, references, types) still come from `sem`, but the
/// `diagnostics` array is built from the collector's parse / semantic / lint
/// entries. Preprocessor diagnostics are intentionally excluded here — the CLI
/// surfaces them through its separate `preproc_diagnostics` channel.
pub fn dump_json_with_diagnostics(sem: &Semantic, collected: &CollectedDiagnostics) -> Value {
    let mut sections = Map::new();
    sections.insert("scopes".into(), json!(1));
    sections.insert("symbols".into(), json!(2));
    sections.insert("types".into(), json!(1));
    sections.insert("references".into(), json!(1));
    sections.insert("diagnostics".into(), json!(1));

    json!({
        "envelope": ENVELOPE_VERSION,
        "sections": Value::Object(sections),
        "schema_revision": sem.schema_revision.raw(),
        "scopes": scopes_json(sem),
        "symbols": symbols_json(sem),
        "references": references_json(sem),
        "types": types_json(sem),
        "diagnostics": collected_diagnostics_json(collected),
    })
}

/// Human-oriented text rendering. Compact, not stable across versions — if
/// you need stability, dump to JSON. Used for interactive `oxabl analyze`
/// runs without `--format json`.
pub fn dump_text(program: &[Statement], sem: &Semantic, ctx: &AnalysisContext) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    write_scopes_and_symbols(&mut out, sem);

    let diags = oxabl_lint::lint_file(program, sem, ctx);
    writeln!(
        out,
        "\n=== Diagnostics ({} semantic + {} lint) ===",
        sem.diagnostics.len(),
        diags.len()
    )
    .ok();
    for d in sem.diagnostics.iter().chain(diags.iter()) {
        writeln!(out, "  [{}] {:?} {}", d.code.0, d.severity, d.message).ok();
    }

    out
}

/// Text rendering that sources its diagnostics from the shared collector (the
/// CLI `analyze --format text` path). Scopes and symbols come from `sem`; the
/// diagnostics list is the collector's parse / semantic / lint entries
/// (preprocessor diagnostics are surfaced on their own CLI channel).
pub fn dump_text_with_diagnostics(sem: &Semantic, collected: &CollectedDiagnostics) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    write_scopes_and_symbols(&mut out, sem);

    let diags: Vec<&Diagnostic> = collected
        .all()
        .filter(|c| c.source != DiagnosticSource::Preproc)
        .map(|c| &c.diagnostic)
        .collect();
    writeln!(out, "\n=== Diagnostics ({}) ===", diags.len()).ok();
    for d in diags {
        writeln!(out, "  [{}] {:?} {}", d.code.0, d.severity, d.message).ok();
    }

    out
}

fn write_scopes_and_symbols(out: &mut String, sem: &Semantic) {
    use std::fmt::Write;

    writeln!(out, "=== Scopes ({}) ===", sem.scope_tree.len()).ok();
    for (id, s) in sem.scope_tree.iter() {
        writeln!(
            out,
            "  [{:>2}] {:<14} parent={:<3} owner={:>4} bindings={}",
            id.raw(),
            format!("{:?}", s.kind),
            match s.parent {
                Some(p) => p.raw().to_string(),
                None => "—".into(),
            },
            s.owner_node.as_u32(),
            s.bindings.iter().map(|b| b.len()).sum::<usize>(),
        )
        .ok();
    }

    writeln!(out, "\n=== Symbols ({}) ===", sem.symbols.len()).ok();
    for (id, sym) in sem.symbols.iter() {
        writeln!(
            out,
            "  [{:>3}] {:<10} {:<14} scope={:<3} reads={} writes={} ty={}",
            id.raw(),
            format!("{:?}", sym.kind),
            sym.name.as_ref(),
            sym.declared_in.raw(),
            sym.read_count,
            sym.write_count,
            sym.data_type
                .as_ref()
                .map(render_type)
                .unwrap_or_else(|| "—".into())
        )
        .ok();
    }
}

// ---------------------------------------------------------------------------
// Section builders
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct ScopeRow {
    id: u32,
    kind: &'static str,
    parent: Option<u32>,
    owner_node: u32,
    bindings: Vec<BindingRow>,
}

#[derive(Serialize)]
struct BindingRow {
    namespace: &'static str,
    name: String,
    symbol: u32,
}

fn scopes_json(sem: &Semantic) -> Value {
    let rows: Vec<ScopeRow> = sem
        .scope_tree
        .iter()
        .map(|(id, s)| ScopeRow {
            id: id.raw(),
            kind: scope_kind_str(s.kind),
            parent: s.parent.map(ScopeId::raw),
            owner_node: s.owner_node.as_u32(),
            bindings: s
                .bindings
                .iter()
                .enumerate()
                .flat_map(|(ns_idx, map)| {
                    let ns = namespace_from_index(ns_idx);
                    map.iter().map(move |(name, sym)| BindingRow {
                        namespace: namespace_str(ns),
                        name: name.as_ref().to_string(),
                        symbol: sym.raw(),
                    })
                })
                .collect(),
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

#[derive(Serialize)]
struct SymbolRow {
    id: u32,
    name: String,
    namespace: &'static str,
    kind: &'static str,
    declared_in: u32,
    declaration: u32,
    read_count: u32,
    write_count: u32,
    flags: Vec<&'static str>,
    data_type: Option<String>,
}

fn symbols_json(sem: &Semantic) -> Value {
    let rows: Vec<SymbolRow> = sem
        .symbols
        .iter()
        .map(|(id, sym)| SymbolRow {
            id: id.raw(),
            name: sym.name.as_ref().to_string(),
            namespace: namespace_str(sym.namespace),
            kind: symbol_kind_str(sym.kind),
            declared_in: sym.declared_in.raw(),
            declaration: sym.declaration.as_u32(),
            read_count: sym.read_count,
            write_count: sym.write_count,
            flags: symbol_flags_list(sym.flags),
            data_type: sym.data_type.as_ref().map(render_type),
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

#[derive(Serialize)]
struct ReferenceRow {
    node: u32,
    resolution: ResolutionKind,
    symbol: Option<u32>,
    name: Option<String>,
    reason: Option<&'static str>,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum ResolutionKind {
    Resolved,
    Unresolved,
}

fn references_json(sem: &Semantic) -> Value {
    let rows: Vec<ReferenceRow> = sem
        .references
        .iter()
        .map(|(nid, r)| match r {
            Resolution::Resolved(sym) => ReferenceRow {
                node: nid.as_u32(),
                resolution: ResolutionKind::Resolved,
                symbol: Some(sym.raw()),
                name: None,
                reason: None,
            },
            Resolution::Unresolved { name, reason } => ReferenceRow {
                node: nid.as_u32(),
                resolution: ResolutionKind::Unresolved,
                symbol: None,
                name: Some(name.as_ref().to_string()),
                reason: Some(unresolved_reason_str(*reason)),
            },
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

#[derive(Serialize)]
struct TypeRow {
    node: u32,
    r#type: String,
}

fn types_json(sem: &Semantic) -> Value {
    let rows: Vec<TypeRow> = sem
        .types
        .iter()
        .map(|(nid, t)| TypeRow {
            node: nid.as_u32(),
            r#type: render_type(t),
        })
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

#[derive(Serialize)]
struct DiagnosticRow {
    code: String,
    severity: &'static str,
    message: String,
    span: SpanRow,
    source: &'static str,
}

#[derive(Serialize)]
struct SpanRow {
    file: u32,
    start: u32,
    end: u32,
}

fn diagnostics_json(sem: &Semantic, lint_diags: &[Diagnostic]) -> Value {
    let mut rows: Vec<DiagnosticRow> = sem
        .diagnostics
        .iter()
        .map(|d| diag_row(d, "semantic"))
        .collect();
    rows.extend(lint_diags.iter().map(|d| diag_row(d, "lint")));
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

/// Build the `diagnostics` section from the shared collector's output.
///
/// Only the parse / semantic / lint stages feed the envelope section (in that
/// pipeline order); preprocessor diagnostics are surfaced through the CLI's
/// separate `preproc_diagnostics` channel and are skipped here. Spans are
/// already resolved to root-buffer coordinates by the collector.
fn collected_diagnostics_json(collected: &CollectedDiagnostics) -> Value {
    let rows: Vec<DiagnosticRow> = collected
        .all()
        .filter(|c| c.source != DiagnosticSource::Preproc)
        .map(|c| diag_row(&c.diagnostic, c.source.as_str()))
        .collect();
    serde_json::to_value(rows).unwrap_or(Value::Null)
}

fn diag_row(d: &Diagnostic, source: &'static str) -> DiagnosticRow {
    DiagnosticRow {
        code: d.code.0.to_string(),
        severity: severity_str(d.severity),
        message: d.message.clone(),
        span: SpanRow {
            file: d.span.file.raw(),
            start: d.span.span.start,
            end: d.span.span.end,
        },
        source,
    }
}

// ---------------------------------------------------------------------------
// Enum stringification
// ---------------------------------------------------------------------------

fn scope_kind_str(k: ScopeKind) -> &'static str {
    match k {
        ScopeKind::File => "file",
        ScopeKind::Procedure => "procedure",
        ScopeKind::Function => "function",
        ScopeKind::Class => "class",
        ScopeKind::Interface => "interface",
        ScopeKind::Method => "method",
        ScopeKind::PropertyGet => "property_get",
        ScopeKind::PropertySet => "property_set",
        ScopeKind::Constructor => "constructor",
        ScopeKind::Destructor => "destructor",
        ScopeKind::Block => "block",
        ScopeKind::Catch => "catch",
        ScopeKind::Finally => "finally",
        ScopeKind::Trigger => "trigger",
        ScopeKind::Frame => "frame",
        ScopeKind::TempTable => "temp_table",
        ScopeKind::TriggerProcedure => "trigger_procedure",
    }
}

fn namespace_str(ns: NamespaceId) -> &'static str {
    match ns {
        NamespaceId::Values => "values",
        NamespaceId::Buffers => "buffers",
        NamespaceId::Tables => "tables",
        NamespaceId::Types => "types",
        NamespaceId::Procedures => "procedures",
        NamespaceId::Functions => "functions",
        NamespaceId::Streams => "streams",
        NamespaceId::Frames => "frames",
        NamespaceId::Events => "events",
        NamespaceId::WidgetHandles => "widget_handles",
    }
}

fn namespace_from_index(i: usize) -> NamespaceId {
    NamespaceId::ALL[i]
}

fn symbol_kind_str(k: SymbolKind) -> &'static str {
    match k {
        SymbolKind::Variable => "variable",
        SymbolKind::Parameter => "parameter",
        SymbolKind::Property => "property",
        SymbolKind::Field => "field",
        SymbolKind::TempTable => "temp_table",
        SymbolKind::Buffer => "buffer",
        SymbolKind::Stream => "stream",
        SymbolKind::Frame => "frame",
        SymbolKind::Event => "event",
        SymbolKind::Procedure => "procedure",
        SymbolKind::Function => "function",
        SymbolKind::Class => "class",
        SymbolKind::Interface => "interface",
        SymbolKind::BuiltIn => "builtin",
        SymbolKind::Dataset => "dataset",
        SymbolKind::DataSource => "data_source",
    }
}

fn symbol_flags_list(f: oxabl_semantic::SymbolFlags) -> Vec<&'static str> {
    use oxabl_semantic::SymbolFlags as F;
    let mut out = Vec::new();
    if f.contains(F::NO_UNDO) {
        out.push("no_undo");
    }
    if f.contains(F::STATIC) {
        out.push("static");
    }
    if f.contains(F::ABSTRACT) {
        out.push("abstract");
    }
    if f.contains(F::FINAL) {
        out.push("final");
    }
    if f.contains(F::OVERRIDE) {
        out.push("override");
    }
    if f.contains(F::PARAM_INPUT) {
        out.push("param_input");
    }
    if f.contains(F::PARAM_OUTPUT) {
        out.push("param_output");
    }
    if f.contains(F::PARAM_INPUT_OUT) {
        out.push("param_input_output");
    }
    if f.contains(F::PARAM_RETURN) {
        out.push("param_return");
    }
    if f.contains(F::SHARED) {
        out.push("shared");
    }
    if f.contains(F::NEW_SHARED) {
        out.push("new_shared");
    }
    if f.contains(F::NEW_GLOBAL_SHARED) {
        out.push("new_global_shared");
    }
    if f.contains(F::PUBLIC) {
        out.push("public");
    }
    if f.contains(F::PRIVATE) {
        out.push("private");
    }
    if f.contains(F::PROTECTED) {
        out.push("protected");
    }
    if f.contains(F::PACKAGE_PRIVATE) {
        out.push("package_private");
    }
    if f.contains(F::PROTOTYPE) {
        out.push("prototype");
    }
    out
}

fn unresolved_reason_str(r: UnresolvedReason) -> &'static str {
    match r {
        UnresolvedReason::NotInScope => "not_in_scope",
        UnresolvedReason::External => "external",
        UnresolvedReason::NoSchema => "no_schema",
    }
}

fn severity_str(s: Severity) -> &'static str {
    match s {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
        Severity::Hint => "hint",
    }
}

fn render_type(t: &ResolvedType) -> String {
    match t {
        ResolvedType::Primitive(p) => format!("{p:?}").to_lowercase(),
        ResolvedType::Class(sid) => format!("class#{}", sid.raw()),
        ResolvedType::Buffer(sid) => format!("buffer#{}", sid.raw()),
        ResolvedType::Table(rev, tid) => format!("table#{}/{}", rev.raw(), tid.raw()),
        ResolvedType::Array { element, extent } => {
            let ext = extent
                .map(|n| n.to_string())
                .unwrap_or_else(|| "dyn".into());
            format!("array[{ext}] {}", render_type(element))
        }
        ResolvedType::Unknown => "unknown".into(),
        ResolvedType::Error => "error".into(),
    }
}

// Keep SymbolId / NodeId imports live for section types that embed them.
const _: fn() = || {
    let _: SymbolId = SymbolId::new(0);
    let _: NodeId = NodeId::from_u32(0);
};

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_ast::{DataType, Identifier, Span, Statement, StatementKind, TypeSource};
    use oxabl_common::FileId;
    use oxabl_schema::Schema;
    use oxabl_semantic::analyze_file;

    fn ident(n: &str) -> Identifier {
        Identifier {
            span: Span {
                start: 0,
                end: n.len() as u32,
            },
            name: n.into(),
        }
    }

    fn var_decl(n: &str, ty: DataType) -> Statement {
        Statement::new(StatementKind::VariableDeclaration {
            name: ident(n),
            type_source: TypeSource::Explicit(ty),
            initial_value: None,
            no_undo: false,
            extent: None,
            is_new_shared: false,
            is_shared: false,
            is_new_global_shared: false,
        })
    }

    fn run_dump(stmts: Vec<Statement>) -> Value {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let sem = analyze_file(&stmts, &ctx);
        dump_json(&stmts, &sem, &ctx, true)
    }

    #[test]
    fn envelope_has_expected_sections() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        assert_eq!(v["envelope"], 1);
        assert!(v["sections"]["scopes"].is_number());
        assert!(v["scopes"].is_array());
        assert!(v["symbols"].is_array());
        assert!(v["references"].is_array());
        assert!(v["types"].is_array());
        assert!(v["diagnostics"].is_array());
    }

    #[test]
    fn schema_revision_included() {
        let v = run_dump(vec![]);
        assert!(v["schema_revision"].is_number());
    }

    #[test]
    fn builtins_appear_in_symbols() {
        let v = run_dump(vec![]);
        let symbols = v["symbols"].as_array().unwrap();
        let names: Vec<&str> = symbols
            .iter()
            .map(|s| s.get("name").and_then(Value::as_str).unwrap())
            .collect();
        for expected in ["session", "error-status", "self", "super", "this-object"] {
            assert!(names.contains(&expected), "{expected} not in {names:?}");
        }
    }

    #[test]
    fn user_variable_symbol_serialized_with_kind() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        let x = v["symbols"]
            .as_array()
            .unwrap()
            .iter()
            .find(|s| s["name"] == "x")
            .unwrap();
        assert_eq!(x["kind"], "variable");
        assert_eq!(x["namespace"], "values");
        assert_eq!(x["data_type"], "integer");
    }

    #[test]
    fn dump_is_valid_json_string() {
        let v = run_dump(vec![var_decl("x", DataType::Integer)]);
        let s = serde_json::to_string(&v).unwrap();
        // Round-trip parses cleanly.
        let back: Value = serde_json::from_str(&s).unwrap();
        assert_eq!(back["envelope"], 1);
    }

    #[test]
    fn diagnostics_entry_tagged_with_source() {
        let v = run_dump(vec![var_decl("unused", DataType::Integer)]);
        let diags = v["diagnostics"].as_array().unwrap();
        // Should include LINT0002 unused-variable from the lint pass.
        let lint_sources: Vec<&str> = diags
            .iter()
            .filter_map(|d| d.get("source").and_then(Value::as_str))
            .collect();
        assert!(lint_sources.contains(&"lint"));
    }

    #[test]
    fn dump_text_contains_scopes_and_symbols_headers() {
        let schema = Schema::empty();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        let stmts = vec![var_decl("x", DataType::Integer)];
        let sem = analyze_file(&stmts, &ctx);
        let text = dump_text(&stmts, &sem, &ctx);
        assert!(text.contains("=== Scopes"));
        assert!(text.contains("=== Symbols"));
        assert!(text.contains("=== Diagnostics"));
    }

    #[test]
    fn duplicate_decl_surfaces_in_diagnostics() {
        let v = run_dump(vec![
            var_decl("dup", DataType::Integer),
            var_decl("dup", DataType::Integer),
        ]);
        let diags = v["diagnostics"].as_array().unwrap();
        assert!(diags.iter().any(|d| d["code"] == "SEM0001"));
    }

    // ---- Schema-derived symbols (symbols section v2) ----------------------

    use oxabl_schema::test_support::customer_schema as test_schema;

    #[test]
    fn symbols_section_v2_includes_schema_derived_symbols() {
        use oxabl_ast::{BufferTarget, Expression, ExpressionKind, XmlSerializeOptions};

        let schema = test_schema();
        let ctx = AnalysisContext::new(FileId::UNKNOWN, "", &schema);
        // `Customer.Name` with no DEFINE BUFFER: the qualifier binds a
        // synthesized default buffer and the field a synthesized Field
        // symbol — both must appear in the dump.
        let fa = Expression::with_id(
            NodeId::from_u32(2),
            oxabl_ast::Span::DUMMY,
            ExpressionKind::FieldAccess {
                qualifier: Box::new(Expression::with_id(
                    NodeId::from_u32(3),
                    oxabl_ast::Span::DUMMY,
                    ExpressionKind::Identifier(ident("Customer")),
                )),
                field: ident("Name"),
            },
        );
        let stmts = vec![
            Statement::with_id(
                NodeId::from_u32(4),
                oxabl_ast::Span::DUMMY,
                StatementKind::DefineBuffer {
                    name: ident("bCust"),
                    target: BufferTarget::Table(ident("Customer")),
                    preselect: false,
                    label: None,
                    xml_options: XmlSerializeOptions::default(),
                    is_new_shared: false,
                    is_shared: false,
                    is_new_global_shared: false,
                },
            ),
            Statement::with_id(
                NodeId::from_u32(5),
                oxabl_ast::Span::DUMMY,
                StatementKind::ExpressionStatement(fa),
            ),
        ];
        let sem = analyze_file(&stmts, &ctx);
        let v = dump_json(&stmts, &sem, &ctx, true);

        assert_eq!(v["sections"]["symbols"], 2);
        let symbols = v["symbols"].as_array().unwrap();
        // Synthesized default buffer for `Customer` (kind buffer,
        // declaration = NodeId::DUMMY = u32::MAX).
        let synth_buffer = symbols
            .iter()
            .find(|s| s["name"] == "customer" && s["kind"] == "buffer")
            .expect("synthesized default buffer in dump");
        assert_eq!(synth_buffer["declaration"], u32::MAX);
        // Synthesized field symbol for `Name`, typed from the schema.
        let synth_field = symbols
            .iter()
            .find(|s| s["name"] == "name" && s["kind"] == "field")
            .expect("synthesized field in dump");
        assert_eq!(synth_field["declaration"], u32::MAX);
        assert_eq!(synth_field["data_type"], "character");
    }
}
