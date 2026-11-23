#![no_main]

use std::path::{Path, PathBuf};

use libfuzzer_sys::fuzz_target;
use lsp_types::PositionEncodingKind;
use oxabl_lsp::position::byte_to_position;
use oxabl_pipeline::{
    LintPipeline, PipelineConfig, SchemaDatabase, Target, TextPosition, TextRange, schema_database,
    schema_table,
};
use oxabl_schema::SchemaLoader;
use oxabl_workspace::{FileSystem, InMemoryFileSystem};
use ropey::Rope;

// Go to definition and document links (docs/design/navigation.md) on a fuzzed
// file, a fuzzed `.df` and a fuzzed include, in a small workspace: they never
// panic, every target is a file of the workspace, and every span is inside the
// file it names and on character boundaries. The schema browser's tree of the
// fuzzed `.df` (docs/design/schema-browser.md) holds every table and field the
// loader read from it, with every span inside the file.
//
// Input: `source \0 schema \0 include`, each part optional.
fuzz_target!(|data: &[u8]| {
    oxabl_fuzz::on_analysis_stack(data, check);
});

/// The most cursor positions tried per file.
const MAX_OFFSETS: usize = 2048;

fn check(data: &[u8]) {
    let text = String::from_utf8_lossy(data);
    let mut parts = text.splitn(3, '\0');
    let source = parts.next().unwrap_or_default();
    let schema = parts.next().unwrap_or_default();
    let include = parts.next().unwrap_or_default();

    let root = PathBuf::from("/w/fuzz.p");
    let include_path = PathBuf::from("/w/inc.i");
    let df = PathBuf::from("/w/db/x.df");
    let mut fs = InMemoryFileSystem::new();
    fs.insert(root.clone(), source);
    fs.insert(include_path.clone(), include);
    fs.insert(df.clone(), schema);
    fs.insert(PathBuf::from("/w/p.p"), "MESSAGE 1.\n");
    fs.insert(PathBuf::from("/w/two.p"), "MESSAGE 2.\n");
    fs.insert(PathBuf::from("/w/lib/two.p"), "MESSAGE 3.\n");
    fs.insert(
        PathBuf::from("/w/app/C.cls"),
        "CLASS app.C INHERITS app.B:\n  METHOD PUBLIC VOID M():\n  END METHOD.\nEND CLASS.\n",
    );
    fs.insert(
        PathBuf::from("/w/app/B.cls"),
        "CLASS app.B:\n  METHOD PUBLIC VOID N():\n  END METHOD.\n  METHOD PUBLIC VOID N(INPUT i AS INTEGER):\n  END METHOD.\nEND CLASS.\n",
    );

    let (loaded, _) = SchemaLoader::load_files(std::slice::from_ref(&df), &fs);
    check_schema_view(&df, schema, &loaded);
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/w"), PathBuf::from("/w/lib")],
        schema: loaded,
        schema_loaded: true,
        ..PipelineConfig::default()
    };

    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file(&root);
    let expansion = handle.expand(source);
    let none = |_| None;
    let Some(navigation) = handle.navigation(&expansion, &none) else {
        return;
    };
    for (file, text) in [(&root, source), (&include_path, include)] {
        for offset in offsets(text) {
            for target in navigation.definitions(file, offset) {
                check_target(&fs, &target, file, offset);
            }
        }
    }
    for link in navigation.links() {
        let (start, end) = (link.span.start as usize, link.span.end as usize);
        assert!(
            start <= end && end <= source.len(),
            "a link outside the file: {link:?}"
        );
        assert!(
            source.is_char_boundary(start) && source.is_char_boundary(end),
            "a link off a character boundary: {link:?}"
        );
        assert!(
            fs.exists(&link.target),
            "a link to a file that is not there: {link:?}"
        );
    }
}

/// The browser's tree of one `.df` against the loader's schema of the same
/// file: the same tables and fields, both ways, also for a file whose tables
/// an earlier file declares; every span inside the file; every node placed
/// with the language server's `byte_to_position`, which must agree with an
/// independent count of lines and UTF-16 units; and the tree serializes.
fn check_schema_view(path: &Path, text: &str, loaded: &oxabl_schema::Schema) {
    let mut view = schema_database(path, text.to_string(), &|_| false);
    for span in view.spans() {
        let (start, end) = (span.start as usize, span.end as usize);
        assert!(
            start <= end && end <= text.len(),
            "a schema span outside the file: {span:?}"
        );
        assert!(
            text.is_char_boundary(start) && text.is_char_boundary(end),
            "a schema span off a character boundary: {span:?}"
        );
    }
    assert_eq!(
        tree_tables(&view),
        loader_tables(loaded),
        "the tree and the loader disagree"
    );
    check_extended(path, text);
    for (_, table) in loaded.tables() {
        let hover = schema_table(table);
        assert_eq!(hover.fields.len(), table.fields.len());
    }

    // The language server's own placement, against an independent count.
    let rope = Rope::from_str(text);
    let place = |offset: u32| {
        let at = byte_to_position(&rope, offset as usize, &PositionEncodingKind::UTF16);
        let at = TextPosition {
            line: at.line,
            character: at.character,
        };
        assert_eq!(
            at,
            position(text, offset as usize),
            "the server places byte {offset} elsewhere"
        );
        at
    };
    view.place(|span| TextRange {
        start: place(span.start),
        end: place(span.end),
    });
    for table in &view.tables {
        let range = table.range.expect("a placed table");
        assert!(
            (range.start.line, range.start.character) <= (range.end.line, range.end.character),
            "a range that ends before it starts: {range:?}"
        );
    }
    let json = serde_json::to_value(&view).expect("the tree serializes");
    assert_eq!(
        json["tables"].as_array().map(Vec::len),
        Some(view.tables.len())
    );
}

/// Folded table names with their sorted folded field names.
type Tables = Vec<(String, Vec<String>)>;

fn tree_tables(view: &SchemaDatabase) -> Tables {
    let mut tables: Tables = view
        .tables
        .iter()
        .map(|t| {
            let mut fields: Vec<String> = t.fields.iter().map(|f| fold(&f.name)).collect();
            fields.sort();
            (fold(&t.name), fields)
        })
        .collect();
    tables.sort();
    tables
}

fn loader_tables(schema: &oxabl_schema::Schema) -> Tables {
    let mut tables: Tables = schema
        .tables()
        .map(|(_, t)| {
            let mut fields: Vec<String> = t.fields.iter().map(|f| fold(&f.display_name)).collect();
            fields.sort();
            (fold(&t.display_name), fields)
        })
        .collect();
    tables.sort();
    tables
}

fn fold(name: &str) -> String {
    name.to_ascii_lowercase()
}

/// With every table declared by an earlier file, the tree holds what the
/// loader holds after a file that declares the tables this one extends.
fn check_extended(path: &Path, text: &str) {
    let view = schema_database(path, text.to_string(), &|_| true);
    if view.problems.iter().any(|p| p.severity == "error") {
        return;
    }
    // Every table the file names, from the parse rather than from the tree.
    let parsed = oxabl_schema::parse_df(text, oxabl_common::FileId::new(1));
    let named = parsed
        .fields
        .iter()
        .map(|f| &f.table_display)
        .chain(parsed.indexes.iter().map(|i| &i.table_display));
    let stub: String = named
        .map(|name| format!("ADD TABLE \"{}\"\n", name.replace('"', "\"\"")))
        .collect();
    let earlier = PathBuf::from("/w/db/earlier.df");
    let mut fs = InMemoryFileSystem::new();
    fs.insert(earlier.clone(), stub);
    fs.insert(path.to_path_buf(), text);
    let (loaded, _) = SchemaLoader::load_files(&[earlier, path.to_path_buf()], &fs);
    assert_eq!(
        tree_tables(&view),
        loader_tables(&loaded),
        "an extended tree and the loader disagree"
    );
}

/// The line and UTF-16 column of `offset`, with `\n`, `\r\n` and a lone `\r`
/// as line breaks, as the language server counts them.
fn position(text: &str, offset: usize) -> TextPosition {
    let before = &text[..offset];
    let mut line = 0;
    let mut start = 0;
    let bytes = text.as_bytes();
    for (at, byte) in bytes[..offset].iter().enumerate() {
        let lone_cr = *byte == b'\r' && bytes.get(at + 1) != Some(&b'\n');
        if *byte == b'\n' || lone_cr {
            line += 1;
            start = at + 1;
        }
    }
    TextPosition {
        line,
        character: before[start..].encode_utf16().count() as u32,
    }
}

/// Every offset of `text`, or an even spread of them in a long one.
fn offsets(text: &str) -> impl Iterator<Item = u32> {
    let step = (text.len() / MAX_OFFSETS).max(1);
    (0..=text.len()).step_by(step).map(|at| at as u32)
}

fn check_target(fs: &InMemoryFileSystem, target: &Target, file: &Path, offset: u32) {
    let Ok(text) = fs.read(&target.path) else {
        panic!("{file:?}@{offset}: a target that is not a file: {target:?}");
    };
    if let Some(span) = target.span {
        let (start, end) = (span.start as usize, span.end as usize);
        assert!(
            start <= end && end <= text.len(),
            "{file:?}@{offset}: a span outside its file: {target:?}"
        );
        assert!(
            text.is_char_boundary(start) && text.is_char_boundary(end),
            "{file:?}@{offset}: a span off a character boundary: {target:?}"
        );
    }
}
