//! The paths and preprocessor names of code completion through the public
//! engine: a directory that cannot be listed, a directory that does not
//! exist, and the scope of the names after `{&`.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_pipeline::completion::{
    CompletionBase, CompletionList, CompletionRequest, FreshListing, complete,
};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_style::KeywordCase;
use oxabl_workspace::{DirEntry, FileSystem, InMemoryFileSystem};

/// The files of `inner`, with no directory listing.
struct CannotList(InMemoryFileSystem);

impl FileSystem for CannotList {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        self.0.read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.0.exists(path)
    }

    fn list_dir(&self, _dir: &Path) -> Option<Vec<DirEntry>> {
        None
    }
}

fn files(entries: &[(&str, &str)]) -> InMemoryFileSystem {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in entries {
        fs.insert(PathBuf::from(path), *text);
    }
    fs
}

/// The answer at the `|` of `marked`, the text of `/ws/<name>`, with a base
/// of that text.
fn ask(fs: &dyn FileSystem, name: &str, marked: &str) -> Option<CompletionList> {
    let at = marked.find('|').expect("a cursor");
    let text = marked.replacen('|', "", 1);
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/ws"), PathBuf::from("/ws/lib")],
        ..PipelineConfig::default()
    };
    let path = PathBuf::from("/ws").join(name);
    let run = LintPipeline::new(&config, fs).with_define_record(true);
    let handle = run.with_file(&path);
    let expansion = handle.expand(&text);
    let model = Arc::new(
        handle
            .navigation_model(&expansion)
            .expect("it preprocesses"),
    );
    let none = |_| None;
    let navigation = handle
        .navigation_with(&expansion, model, &none)
        .expect("it preprocesses");
    let base = CompletionBase {
        navigation: &navigation,
        text: &text,
        fragment: false,
        stale: false,
        tokens: None,
        classes: None,
    };
    let request = CompletionRequest {
        text: &text,
        offset: at as u32,
        path: &path,
        keyword_case: KeywordCase::Uppercase,
        snippets: false,
        trigger: None,
        config: &config,
        listing: &FreshListing(fs),
    };
    complete(&request, Some(&base))
}

fn labels(list: &CompletionList) -> Vec<&str> {
    let mut labels: Vec<&str> = list.items.iter().map(|item| item.label.as_str()).collect();
    labels.sort_unstable();
    labels
}

const PROGRAM: &str = "RUN |\nPROCEDURE localProc:\nEND PROCEDURE.\n";

#[test]
fn a_directory_that_cannot_be_listed_makes_the_list_incomplete() {
    let fs = CannotList(files(&[("/ws/top.p", "MESSAGE 1.")]));
    let list = ask(&fs, "main.p", PROGRAM).expect("a list");
    assert_eq!(labels(&list), ["localProc"]);
    assert!(list.incomplete, "an unlisted directory is not an empty one");
}

#[test]
fn a_directory_that_does_not_exist_is_empty() {
    let fs = files(&[
        ("/ws/main.p", PROGRAM),
        ("/ws/util/helper.p", "MESSAGE 1."),
        ("/ws/util/notes.txt", "notes"),
        ("/ws/lib/libprog.p", "MESSAGE 1."),
    ]);
    let list = ask(&fs, "main.p", "RUN util/|").expect("a list");
    assert_eq!(labels(&list), ["util/helper.p"]);
    assert!(
        !list.incomplete,
        "`/ws/lib/util` does not exist, and `/ws/util` lists"
    );

    let list = ask(&fs, "main.p", PROGRAM).expect("a list");
    assert_eq!(
        labels(&list),
        ["lib/", "libprog.p", "localProc", "main.p", "util/"]
    );
}

#[test]
fn a_scoped_name_ends_with_its_file_and_an_undefined_name_is_gone() {
    let fs = files(&[(
        "/ws/defs.i",
        "&SCOPED-DEFINE sInside 1\n&GLOBAL-DEFINE gInside 2\n&GLOBAL-DEFINE gGone 3\n",
    )]);
    let list = ask(
        &fs,
        "main.p",
        "&SCOPED-DEFINE sOwn a\n{defs.i}\n&UNDEFINE gGone\nDISPLAY {&|}.\n&GLOBAL-DEFINE gLater 4\n",
    )
    .expect("a list");
    let found = labels(&list);
    for name in ["sOwn", "gInside", "OPSYS"] {
        assert!(found.contains(&name), "{name} in {found:?}");
    }
    for name in ["sInside", "gGone", "gLater"] {
        assert!(!found.contains(&name), "{name} in {found:?}");
    }
    let detail = |label: &str| {
        list.items
            .iter()
            .find(|item| item.label == label)
            .and_then(|item| item.detail.clone())
    };
    assert_eq!(detail("gInside").as_deref(), Some("2"));
}
