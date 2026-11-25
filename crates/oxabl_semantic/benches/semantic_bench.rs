//! Per-pass benchmarks for `oxabl_semantic`.
//!
//! Splits the three passes (declare, resolve, check) plus the end-to-end
//! `analyze_file` wrapper into separate criterion groups so regressions in
//! any single pass are caught without aggregate numbers hiding them. See
//! plan §"Bench granularity" — the split is deliberate.

use criterion::{
    BenchmarkGroup, Criterion, Throughput, black_box, criterion_group, criterion_main,
};
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use std::sync::Arc;

use oxabl_ast::AccessModifier;
use oxabl_semantic::{
    AnalysisContext, ClassDescriptor, ClassKind, IndexAnswer, IndexName, IndexRevision,
    IndexedFileId, MemberDescriptor, MemberParam, MemberParamShape, MemberType, ParamMode,
    PrimitiveTy, ResolvedType, SymbolFlags, SymbolKind, WorkspaceIndex, analyze_file, check_pass,
    declare_pass, resolve_pass,
};

const TINY: &str = r#"
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE y AS CHARACTER NO-UNDO.
ASSIGN x = 42 y = "hello".
MESSAGE x y.
"#;

const MEDIUM: &str = r#"
DEFINE VARIABLE total AS DECIMAL NO-UNDO.
DEFINE VARIABLE count AS INTEGER NO-UNDO.

PROCEDURE sum-range:
    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER b AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER result AS INTEGER NO-UNDO.
    ASSIGN result = a + b.
END PROCEDURE.

FUNCTION avg RETURNS DECIMAL (INPUT t AS DECIMAL, INPUT c AS INTEGER):
    RETURN t / c.
END FUNCTION.

DO count = 1 TO 10:
    ASSIGN total = total + count.
END.

MESSAGE total count.
PUT UNFORMATTED total " " count SKIP.
"#;

/// Recognized-but-unmodelled statement forms (`StatementKind::Skipped`). The
/// resolve pass pays one scope-chain lookup per harvested token here, which is
/// the expensive half of the #128 lexical harvest — the parser only pays a
/// filter over tokens it was already walking. Nothing else in this file
/// exercises the `Skipped` arm at all.
const SKIPPED: &str = r#"
DEFINE VARIABLE lv-total AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-count AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-city AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-flag AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-key AS CHARACTER NO-UNDO.

UNDERLINE lv-total.
UNDERLINE lv-name lv-city.
FORM lv-total FORMAT ">>>,>>9" WITH FRAME f-detail.
EXPORT lv-name lv-city lv-total.
EXPORT DELIMITER "," lv-line lv-flag.
ENABLE lv-name lv-city WITH FRAME f-detail.
DISABLE lv-name WITH FRAME f-detail.
UPDATE lv-name lv-city WITH FRAME f-detail.
SET lv-name lv-city WITH FRAME f-detail.
NEXT-PROMPT lv-key WITH FRAME f-prompt.
APPLY "CHOOSE" TO btn-ok IN FRAME f-detail.
WAIT-FOR "CHOOSE" OF btn-ok.
GET-KEY-VALUE SECTION "app" KEY "path" VALUE lv-key.
IMPORT DELIMITER "," lv-name lv-city lv-total.
GET-KEY-VALUE SECTION "app" KEY "last" VALUE lv-name.
NEXT-PROMPT lv-name WITH FRAME f-detail.
STATUS INPUT lv-name.
UNDERLINE lv-total lv-count lv-name lv-city.
EXPORT lv-name lv-city lv-total lv-count lv-line lv-flag lv-key.
ENABLE lv-name lv-city WITH FRAME f-detail.
UPDATE lv-name lv-city WITH FRAME f-detail.
"#;

const BUILTIN_CLASSES: &str = r#"
USING OpenEdge.Core.*.
DEFINE VARIABLE text AS String NO-UNDO.
DEFINE VARIABLE error-object AS Progress.Lang.AppError NO-UNDO.
text = NEW String().
error-object = NEW Progress.Lang.AppError().
MESSAGE Progress.Lang.Class:GetClass("OpenEdge.Core.String").
MESSAGE OpenEdge.Net.URI:Decode("example").
"#;

/// Members of built-in types read from the static catalog: a `CATCH`
/// variable, `JsonObject` calls with overloads, a static call, an enum
/// member, and a class of the file that inherits `Progress.Lang.AppError`.
/// No index.
const BUILTIN_MEMBERS: &str = r#"
CLASS bench.OrderError INHERITS Progress.Lang.AppError:
    METHOD PUBLIC CHARACTER Describe():
        RETURN GetMessage(1) + STRING(NumMessages).
    END METHOD.
END CLASS.
DEFINE VARIABLE oJson AS Progress.Json.ObjectModel.JsonObject NO-UNDO.
DEFINE VARIABLE oErr AS bench.OrderError NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE lc AS LONGCHAR NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.
oJson = NEW Progress.Json.ObjectModel.JsonObject().
oJson:Add("a", "b").
oJson:Add("n", 1).
c = oJson:GetCharacter("a").
lc = oJson:GetLongchar("a", "UTF-8").
l = oJson:Has("a").
l = OpenEdge.Core.String:IsQuoted(lc).
MESSAGE Progress.ApplicationServer.AdapterTypes:SOAP:GetValue().
c = oErr:Describe() + oErr:ToString().
DO ON ERROR UNDO, LEAVE:
    c = "x".
    CATCH e AS Progress.Lang.Error:
        c = e:GetMessage(1).
        MESSAGE e:GetMessageNum(1) e:NumMessages.
    END CATCH.
END.
MESSAGE c lc l.
"#;

/// A class of the analysed file: properties, overload sets, and a method body
/// of `THIS-OBJECT` chains, static members and calls. No index.
const CLASS_MEMBERS: &str = r#"
CLASS bench.Node:
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Size AS INTEGER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Tags AS CHARACTER EXTENT 3 NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY Next AS bench.Node NO-UNDO GET. SET.
    DEFINE PUBLIC STATIC PROPERTY Count AS INTEGER NO-UNDO GET. SET.
    METHOD PUBLIC INTEGER Score(INPUT p AS INTEGER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC CHARACTER Score(INPUT p AS CHARACTER):
        RETURN p.
    END METHOD.
    METHOD PUBLIC DECIMAL Score(INPUT p AS INTEGER, INPUT q AS INTEGER):
        RETURN p + q.
    END METHOD.
    METHOD PUBLIC bench.Node Link():
        RETURN Next.
    END METHOD.
    METHOD PUBLIC STATIC bench.Node Make(INPUT n AS CHARACTER):
        RETURN NEW bench.Node().
    END METHOD.
    METHOD PUBLIC VOID Walk():
        DEFINE VARIABLE c AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE VARIABLE d AS DECIMAL NO-UNDO.
        c = THIS-OBJECT:Name.
        i = THIS-OBJECT:Size.
        c = THIS-OBJECT:Next:Name.
        c = THIS-OBJECT:Link():Link():Name.
        i = THIS-OBJECT:Score(1).
        c = THIS-OBJECT:Score("a").
        d = THIS-OBJECT:Score(1, 2).
        i = Score(i).
        c = Score(c).
        i = bench.Node:Count.
        c = bench.Node:Make("x"):Name.
        c = THIS-OBJECT:Tags[1].
        c = CAST(THIS-OBJECT, bench.Node):Name.
        MESSAGE c i d.
    END METHOD.
END CLASS.
"#;

/// A program over two workspace classes, through [`BenchIndex`].
const CLASS_MEMBERS_INDEXED: &str = r#"
DEFINE VARIABLE oP AS bench.Person NO-UNDO.
DEFINE VARIABLE oE AS bench.Employee NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE d AS DECIMAL NO-UNDO.
c = oP:Name.
i = oP:Age.
c = oP:Spouse:Name.
c = oP:Spouse:Spouse:Greet().
i = oP:Score(1).
c = oP:Score("a").
c = oE:Name.
d = oE:Salary.
c = oE:Greet().
i = oE:Score(i).
c = oE:Spouse:Name.
i = bench.Person:Count.
c = NEW bench.Employee():Greet().
MESSAGE c i d.
"#;

/// A stub of the environment: answers the two classes of
/// [`CLASS_MEMBERS_INDEXED`] from fixed descriptors.
struct BenchIndex;

fn bench_member(
    name: &str,
    kind: SymbolKind,
    ty: MemberType,
    params: Option<Vec<MemberParam>>,
) -> MemberDescriptor {
    MemberDescriptor {
        name: IndexName::new(name),
        kind,
        ty,
        access: AccessModifier::Public,
        is_static: false,
        record: None,
        params: params.map(Into::into),
    }
}

fn prim(ty: PrimitiveTy) -> MemberType {
    MemberType::portable(ResolvedType::Primitive(ty)).expect("a primitive is portable")
}

fn input(ty: PrimitiveTy) -> MemberParam {
    MemberParam {
        mode: ParamMode::Input,
        shape: MemberParamShape::Value(prim(ty)),
    }
}

impl WorkspaceIndex for BenchIndex {
    fn class(&self, name: &IndexName) -> IndexAnswer<Arc<ClassDescriptor>> {
        let (file, inherits) = match name.as_str() {
            "bench.person" => (1, None),
            "bench.employee" => (2, Some(IndexName::new("bench.Person"))),
            _ => return IndexAnswer::NotFound,
        };
        IndexAnswer::Found(Arc::new(ClassDescriptor {
            name: name.clone(),
            file: IndexedFileId::new(file),
            kind: ClassKind::Class,
            inherits,
            implements: Vec::new(),
            imports: Arc::default(),
            members_complete: true,
            is_abstract: false,
        }))
    }

    fn class_members(&self, class: &IndexName) -> IndexAnswer<Arc<[MemberDescriptor]>> {
        use PrimitiveTy::*;
        let members = match class.as_str() {
            "bench.person" => vec![
                bench_member("name", SymbolKind::Property, prim(Character), None),
                bench_member("age", SymbolKind::Property, prim(Integer), None),
                bench_member(
                    "spouse",
                    SymbolKind::Property,
                    MemberType::Named(IndexName::new("bench.Person")),
                    None,
                ),
                MemberDescriptor {
                    is_static: true,
                    ..bench_member("count", SymbolKind::Property, prim(Integer), None)
                },
                bench_member("greet", SymbolKind::Function, prim(Character), Some(vec![])),
                bench_member(
                    "score",
                    SymbolKind::Function,
                    prim(Integer),
                    Some(vec![input(Integer)]),
                ),
                bench_member(
                    "score",
                    SymbolKind::Function,
                    prim(Character),
                    Some(vec![input(Character)]),
                ),
            ],
            "bench.employee" => vec![
                bench_member("salary", SymbolKind::Property, prim(Decimal), None),
                bench_member("greet", SymbolKind::Function, prim(Character), Some(vec![])),
            ],
            _ => return IndexAnswer::NotFound,
        };
        IndexAnswer::Found(members.into())
    }

    fn program(&self, _target: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }

    fn shared_producer(&self, _name: &IndexName) -> IndexAnswer<IndexedFileId> {
        IndexAnswer::NotFound
    }

    fn revision(&self) -> IndexRevision {
        IndexRevision::new(1)
    }
}

fn parse_program(source: &str) -> Vec<oxabl_ast::Statement> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_statements().expect("parse fixture")
}

fn bench_fixture(
    group: &mut BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    source: &str,
) {
    bench_fixture_with(group, name, source, None);
}

fn bench_fixture_with(
    group: &mut BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    source: &str,
    index: Option<&dyn WorkspaceIndex>,
) {
    let program = parse_program(source);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::new(1), source, &schema);
    let ctx = match index {
        Some(index) => ctx.with_index(index),
        None => ctx,
    };
    if index.is_some() {
        // The fixture measures the surface walk only if the stub answers.
        let sem = analyze_file(&program, &ctx);
        assert!(
            sem.symbols
                .iter()
                .any(|(_, s)| s.flags.contains(SymbolFlags::INHERITED)),
            "{name}: no member came from the index"
        );
    }

    group.throughput(Throughput::Bytes(source.len() as u64));

    // Declare pass in isolation.
    group.bench_function(format!("{name}/declare"), |b| {
        b.iter(|| {
            let (tree, symbols, diags, rev) = declare_pass(black_box(&program), black_box(&ctx));
            black_box((tree, symbols, diags, rev));
        })
    });

    // Resolve pass — needs declare output. Pre-run declare once per sample
    // so the measurement captures resolve in isolation.
    group.bench_function(format!("{name}/resolve"), |b| {
        b.iter_batched(
            || declare_pass(&program, &ctx),
            |(tree, mut symbols, _diags, rev)| {
                let (refs, types, rd) = resolve_pass(
                    black_box(&program),
                    black_box(&ctx),
                    &tree,
                    &mut symbols,
                    rev,
                );
                black_box((tree, symbols, refs, types, rd));
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // Check pass — needs declare + resolve output.
    group.bench_function(format!("{name}/check"), |b| {
        b.iter_batched(
            || {
                let (tree, mut symbols, _d, rev) = declare_pass(&program, &ctx);
                let (refs, types, _rd) = resolve_pass(&program, &ctx, &tree, &mut symbols, rev);
                (tree, symbols, refs, types)
            },
            |(tree, symbols, refs, mut types)| {
                let diags = check_pass(
                    black_box(&program),
                    black_box(&ctx),
                    &tree,
                    &symbols,
                    &refs,
                    &mut types,
                );
                black_box((tree, symbols, refs, types, diags));
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // End-to-end `analyze_file` — the user-visible entry point.
    group.bench_function(format!("{name}/analyze_file"), |b| {
        b.iter(|| {
            let sem = analyze_file(black_box(&program), black_box(&ctx));
            black_box(sem);
        })
    });
}

fn semantic_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("semantic");
    bench_fixture(&mut group, "tiny", TINY);
    bench_fixture(&mut group, "medium", MEDIUM);
    bench_fixture(&mut group, "skipped", SKIPPED);
    bench_fixture(&mut group, "builtin_classes", BUILTIN_CLASSES);
    bench_fixture(&mut group, "class_members", CLASS_MEMBERS);
    bench_fixture(&mut group, "builtin_members", BUILTIN_MEMBERS);
    bench_fixture_with(
        &mut group,
        "class_members_indexed",
        CLASS_MEMBERS_INDEXED,
        Some(&BenchIndex),
    );
    group.finish();

    // Schema-loaded resolve bench: exercises the field-lookup hot path
    // (`Schema::get_by_id` + `Table::get_field` + synth-cache probe per
    // field access), which the `Schema::empty()` fixtures above never hit.
    let mut schema_group = c.benchmark_group("schema_resolve");
    bench_schema_heavy(&mut schema_group);
    schema_group.finish();
}

/// `.df` text for the bench schema: two tables of ten fields each.
fn bench_df() -> String {
    let mut df = String::from("ADD TABLE \"Customer\"\nADD TABLE \"Item\"\n");
    for (table, prefix) in [("Customer", "c"), ("Item", "i")] {
        for n in 0..10 {
            let ty = if n % 2 == 0 { "integer" } else { "character" };
            df.push_str(&format!(
                "ADD FIELD \"{prefix}{n}\" OF \"{table}\" AS {ty}\n"
            ));
        }
    }
    df
}

/// Field-access-heavy program: nested `FOR EACH` loops referencing many
/// `Customer.Field` / `Item.Field` pairs (bare table qualifiers — the
/// synthesized-default-buffer path) plus a `FIND`.
fn schema_heavy_source() -> String {
    let mut src = String::from("DEFINE VARIABLE t AS DECIMAL NO-UNDO.\n");
    src.push_str("FOR EACH Customer:\n");
    src.push_str("    FOR EACH Item:\n");
    for n in 0..10 {
        src.push_str(&format!("        t = t + Customer.c{n} + Item.i{n}.\n"));
    }
    src.push_str("    END.\nEND.\n");
    src.push_str("FIND FIRST Customer.\n");
    for n in 0..10 {
        src.push_str(&format!("t = t + Customer.c{n}.\n"));
    }
    src
}

fn bench_schema_heavy(group: &mut BenchmarkGroup<'_, criterion::measurement::WallTime>) {
    let source = schema_heavy_source();
    let program = parse_program(&source);

    let schema = oxabl_schema::test_support::schema_from_df(&bench_df());
    let ctx = AnalysisContext::new(FileId::new(1), &source, &schema);

    group.throughput(Throughput::Bytes(source.len() as u64));

    group.bench_function("schema_heavy/declare", |b| {
        b.iter(|| {
            let (tree, symbols, diags, rev) = declare_pass(black_box(&program), black_box(&ctx));
            black_box((tree, symbols, diags, rev));
        })
    });

    group.bench_function("schema_heavy/resolve", |b| {
        b.iter_batched(
            || declare_pass(&program, &ctx),
            |(tree, mut symbols, _diags, rev)| {
                let (refs, types, rd) = resolve_pass(
                    black_box(&program),
                    black_box(&ctx),
                    &tree,
                    &mut symbols,
                    rev,
                );
                black_box((tree, symbols, refs, types, rd));
            },
            criterion::BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, semantic_benchmarks);
criterion_main!(benches);
