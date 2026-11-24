#![no_main]

use std::path::{Path, PathBuf};

use libfuzzer_sys::fuzz_target;
use oxabl_pipeline::{LintPipeline, PipelineConfig, Target};
use oxabl_schema::SchemaLoader;
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

// Go to definition, document highlights and document links
// (docs/design/navigation.md) on a fuzzed file, a fuzzed `.df` and a fuzzed
// include, in a small workspace: they never panic, every target is a file of
// the workspace, and every span is inside the file it names and on character
// boundaries.
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
            for highlight in navigation.highlights(file, offset) {
                let (start, end) = (highlight.span.start as usize, highlight.span.end as usize);
                assert!(
                    start < end && end <= text.len(),
                    "{file:?}@{offset}: a highlight outside its file: {highlight:?}"
                );
                assert!(
                    text.is_char_boundary(start) && text.is_char_boundary(end),
                    "{file:?}@{offset}: a highlight off a character boundary: {highlight:?}"
                );
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
