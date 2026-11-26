//! A schema loaded through a `SchemaCache` is the schema that a load without
//! one gives, diagnostics included, and a damaged entry is a miss.

use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::PathBuf;
use std::sync::Mutex;

use oxabl_common::Diagnostic;
use oxabl_schema::{Schema, SchemaCache, SchemaLoader, SchemaSource, fold_atom};
use oxabl_workspace::InMemoryFileSystem;

const SP2K: &str = include_str!("../fixtures/sp2k.df");

/// Redeclares a table of sp2k (`SCHEMA0010`, with a label) and holds a
/// sequence and an unknown type.
const CONFLICT: &str = r#"
ADD TABLE "Customer"
  AREA "Other"
ADD FIELD "CustNum" OF "Customer" AS character
  FORMAT "x(8)"
  VIEW-AS "a ""b"" c"
ADD FIELD "Odd" OF "Customer" AS Weird-Type
  EXTENT 3
  DECIMALS 2
  MANDATORY
  CASE-SENSITIVE
  INITIAL "?"
ADD INDEX "odd" ON "Customer"
  AREA "Idx"
  UNIQUE
  INDEX-FIELD "Odd" DESCENDING ABBREVIATED
ADD SEQUENCE "Seq"
  INITIAL ?
  INCREMENT -2
  MAX-VAL 9
  CYCLE-ON-LIMIT yes
"#;

#[derive(Default)]
struct MemoryCache {
    entries: Mutex<HashMap<String, Vec<u8>>>,
    keys: Mutex<usize>,
    fetches: Mutex<usize>,
    stores: Mutex<usize>,
}

fn key_of(files: &[SchemaSource<'_>]) -> String {
    let mut hasher = DefaultHasher::new();
    for file in files {
        file.path.hash(&mut hasher);
        file.text.hash(&mut hasher);
    }
    hasher.finish().to_string()
}

impl SchemaCache for MemoryCache {
    fn key(&self, files: &[SchemaSource<'_>]) -> String {
        *self.keys.lock().unwrap() += 1;
        key_of(files)
    }

    fn fetch(&self, key: &str) -> Option<Vec<u8>> {
        *self.fetches.lock().unwrap() += 1;
        self.entries.lock().unwrap().get(key).cloned()
    }

    fn store(&self, key: &str, entry: &[u8]) {
        *self.stores.lock().unwrap() += 1;
        self.entries
            .lock()
            .unwrap()
            .insert(key.to_string(), entry.to_vec());
    }
}

/// Answers every `fetch` with the same bytes.
struct Fixed(Vec<u8>);

impl SchemaCache for Fixed {
    fn key(&self, _: &[SchemaSource<'_>]) -> String {
        String::new()
    }

    fn fetch(&self, _: &str) -> Option<Vec<u8>> {
        Some(self.0.clone())
    }

    fn store(&self, _: &str, _: &[u8]) {}
}

fn fs(files: &[(&str, &str)]) -> (InMemoryFileSystem, Vec<PathBuf>) {
    let mut fs = InMemoryFileSystem::new();
    let mut paths = Vec::new();
    for (path, text) in files {
        fs.insert(PathBuf::from(path), text.to_string());
        paths.push(PathBuf::from(path));
    }
    (fs, paths)
}

/// Everything a reader of the schema can see of it.
fn describe(schema: &Schema) -> String {
    let mut out = format!(
        "revision {} metaschema {} tables {} sequences {}\n",
        schema.revision().raw(),
        schema.metaschema_enabled(),
        schema.len(),
        schema.sequence_count()
    );
    for (id, table) in schema.tables() {
        out += &format!("{id:?} {table:?} {:?}\n", schema.table_source(id));
        // Lookups by name, by abbreviation, and through the catalog.
        let name = table.name.to_string();
        for end in 1..=name.len() {
            out += &format!("{:?}\n", schema.resolve_table(&fold_atom(&name[..end])));
        }
        assert_eq!(schema.table_id(&table.name), Some(id));
    }
    for sequence in schema.sequences() {
        out += &format!("{sequence:?}\n");
    }
    out += &format!("{:?}\n", schema.table("_file").map(|t| t.fields.len()));
    out
}

type Load = (Schema, Vec<Diagnostic>);

fn files() -> (InMemoryFileSystem, Vec<PathBuf>) {
    fs(&[
        ("/a.df", SP2K),
        ("/b.df", SP2K),
        ("/c.df", CONFLICT),
        (
            "/d.df",
            "ADD TABLE \"Lone\"\nADD FIELD \"x\" OF \"Lone\" AS integer\n\
             ADD FIELD \"x\" OF \"Lone\" AS integer\nADD FIELD \"y\" OF \"Lone\" AS integer\n\
             ADD FIELD \"y\" OF \"Lone\" AS character\n",
        ),
    ])
}

fn assert_same(left: &Load, right: &Load) {
    assert_eq!(describe(&left.0), describe(&right.0));
    assert_eq!(left.1, right.1);
}

#[test]
fn a_cached_load_gives_the_load_it_cached() {
    let (fs, paths) = files();
    let plain = SchemaLoader::load_files(&paths, &fs);
    assert!(
        plain.1.iter().any(|d| d.code.0 == "SCHEMA0010")
            && plain.1.iter().any(|d| d.code.0 == "SCHEMA0011")
            && plain.1.iter().any(|d| d.code.0 == "SCHEMA0012")
            && plain.1.iter().any(|d| !d.labels.is_empty()),
        "the inputs must load with each kind of diagnostic: {:#?}",
        plain.1
    );

    let cache = MemoryCache::default();
    let cold = SchemaLoader::load_files_cached(&paths, &fs, Some(&cache));
    assert_eq!(
        (
            *cache.fetches.lock().unwrap(),
            *cache.stores.lock().unwrap()
        ),
        (1, 1)
    );
    assert_eq!(*cache.keys.lock().unwrap(), 1, "one key for a miss");
    assert_same(&plain, &cold);

    let warm = SchemaLoader::load_files_cached(&paths, &fs, Some(&cache));
    assert_eq!(
        (
            *cache.fetches.lock().unwrap(),
            *cache.stores.lock().unwrap()
        ),
        (2, 1),
        "the second load must come from the entry, and store nothing"
    );
    assert_same(&plain, &warm);
}

#[test]
fn a_hit_is_not_parsed_again() {
    // The cache holds the schema of one text under the key of another: a load
    // that parsed the files would not give it.
    let (fs_a, paths) = fs(&[("/db.df", "ADD TABLE \"Alpha\"\n")]);
    let (fs_b, _) = fs(&[("/db.df", "ADD TABLE \"Beta\"\n")]);
    let cache = MemoryCache::default();
    SchemaLoader::load_files_cached(&paths, &fs_a, Some(&cache));
    let entry = cache.entries.lock().unwrap().values().next().cloned();
    let (schema, _) =
        SchemaLoader::load_files_cached(&paths, &fs_b, Some(&Fixed(entry.expect("an entry"))));
    assert!(schema.table("Alpha").is_some() && schema.table("Beta").is_none());
}

#[test]
fn changed_text_is_a_miss() {
    let cache = MemoryCache::default();
    let (fs_a, paths) = fs(&[("/db.df", "ADD TABLE \"Alpha\"\n")]);
    let (fs_b, _) = fs(&[("/db.df", "ADD TABLE \"Beta\"\n")]);
    SchemaLoader::load_files_cached(&paths, &fs_a, Some(&cache));
    let (schema, _) = SchemaLoader::load_files_cached(&paths, &fs_b, Some(&cache));
    assert!(schema.table("Beta").is_some() && schema.table("Alpha").is_none());
    assert_eq!(*cache.stores.lock().unwrap(), 2);
}

#[test]
fn a_file_that_cannot_be_read_skips_the_cache() {
    let (fs, mut paths) = fs(&[("/a.df", SP2K)]);
    paths.push(PathBuf::from("/missing.df"));
    let cache = MemoryCache::default();
    let cached = SchemaLoader::load_files_cached(&paths, &fs, Some(&cache));
    assert_same(&SchemaLoader::load_files(&paths, &fs), &cached);
    assert!(cached.1.iter().any(|d| d.message.contains("cannot read")));
    assert_eq!(
        (
            *cache.fetches.lock().unwrap(),
            *cache.stores.lock().unwrap()
        ),
        (0, 0)
    );
    assert_eq!(*cache.keys.lock().unwrap(), 0);
}

#[test]
fn a_damaged_entry_is_a_miss() {
    let (fs, paths) = files();
    let plain = SchemaLoader::load_files(&paths, &fs);
    let cache = MemoryCache::default();
    SchemaLoader::load_files_cached(&paths, &fs, Some(&cache));
    let entry = cache
        .entries
        .lock()
        .unwrap()
        .values()
        .next()
        .cloned()
        .unwrap();
    let load = |bytes: Vec<u8>| SchemaLoader::load_files_cached(&paths, &fs, Some(&Fixed(bytes)));

    // Every short prefix, and a spread of longer ones.
    let step = entry.len() / 97 + 1;
    for end in (0..entry.len()).filter(|end| *end < 80 || end % step == 0) {
        assert_same(&plain, &load(entry[..end].to_vec()));
    }
    let mut longer = entry.clone();
    longer.push(0);
    assert_same(&plain, &load(longer));
    let mut other_version = entry.clone();
    other_version[8] ^= 0xff;
    assert_same(&plain, &load(other_version));
    assert_same(&plain, &load(b"not an entry at all".to_vec()));

    // A flipped byte may still be a schema, but never a crash.
    for at in (0..entry.len()).step_by(step) {
        let mut flipped = entry.clone();
        flipped[at] ^= 0x5a;
        let _ = load(flipped);
    }
}

#[test]
fn an_entry_of_other_paths_is_a_miss() {
    let (fs_one, one) = fs(&[("/one.df", "ADD TABLE \"Alpha\"\n")]);
    let (fs_two, two) = fs(&[("/two.df", "ADD TABLE \"Alpha\"\n")]);
    let cache = MemoryCache::default();
    SchemaLoader::load_files_cached(&one, &fs_one, Some(&cache));
    let entry = cache.entries.lock().unwrap().values().next().cloned();
    let cached = SchemaLoader::load_files_cached(&two, &fs_two, Some(&Fixed(entry.unwrap())));
    assert_same(&SchemaLoader::load_files(&two, &fs_two), &cached);
}
