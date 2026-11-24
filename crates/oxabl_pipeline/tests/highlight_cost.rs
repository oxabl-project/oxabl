//! What a highlight reads from the file system. A unit with many `RUN p IN h`
//! must not cost a read per call site: a variable reads nothing, and a name that
//! the search must answer reads each file once.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

const CALLS: usize = 300;

struct CountingFs {
    inner: InMemoryFileSystem,
    operations: AtomicUsize,
}

impl FileSystem for CountingFs {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        self.operations.fetch_add(1, Ordering::SeqCst);
        self.inner.read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.operations.fetch_add(1, Ordering::SeqCst);
        self.inner.exists(path)
    }
}

fn workspace() -> (CountingFs, String) {
    let mut library = String::new();
    let mut main = String::from(
        "DEFINE VARIABLE h AS HANDLE NO-UNDO.\nDEFINE VARIABLE zz AS INTEGER NO-UNDO.\nRUN lib.p PERSISTENT SET h.\n",
    );
    for n in 0..CALLS {
        library += &format!("PROCEDURE p{n}:\nEND PROCEDURE.\n");
        main += &format!("RUN p{n} IN h.\n");
    }
    main += "RUN p7 IN h.\nzz = 1.\nzz = zz + 1.\n";
    let mut inner = InMemoryFileSystem::new();
    inner.insert(PathBuf::from("/w/lib.p"), library.as_str());
    inner.insert(PathBuf::from("/w/main.p"), main.as_str());
    (
        CountingFs {
            inner,
            operations: AtomicUsize::new(0),
        },
        main,
    )
}

/// The file system operations of one `highlights` call with the cursor one
/// character into `needle`, and the number of highlights it gave.
fn highlight_cost(needle: &str) -> (usize, usize) {
    let (fs, main) = workspace();
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/w")],
        ..PipelineConfig::default()
    };
    let run = LintPipeline::new(&config, &fs);
    let root = PathBuf::from("/w/main.p");
    let handle = run.with_file(&root);
    let expansion = handle.expand(&main);
    let none = |_| None;
    let navigation = handle
        .navigation(&expansion, &none)
        .expect("the unit preprocesses");
    let at = main.find(needle).expect("the needle is in the unit") + 1;
    let before = fs.operations.load(Ordering::SeqCst);
    let found = navigation.highlights(&root, at as u32).len();
    (fs.operations.load(Ordering::SeqCst) - before, found)
}

#[test]
fn a_variable_reads_no_file_whatever_the_calls_in_the_unit() {
    let (operations, found) = highlight_cost("zz = 1");
    assert_eq!(found, 4);
    assert_eq!(operations, 0, "a variable needs no file");
}

#[test]
fn a_call_through_a_handle_reads_each_file_once() {
    // Two calls of `p7`; the declaration of the procedure is in `lib.p`.
    let (operations, found) = highlight_cost("p7 IN h");
    assert_eq!(found, 2, "the two calls");
    assert!(
        operations <= 1,
        "{operations} file operations for {CALLS} calls: the cost must not follow the calls"
    );
}
