//! `cargo xtask ci --changed`: the jobs and the tests that the changed paths of a branch select.
//!
//! The selection is a pure function of the changed paths, the workspace manifests and the file
//! names of `crates/oxabl/tests/`. A path that no rule names selects every job: an unknown
//! path is never a pass. The rules are in `docs` terms in `AGENTS.md`, "Run CI locally".
//!
//! - A path of a crate selects the crate and its direct reverse dependencies, without the
//!   umbrella crate `oxabl`.
//! - The umbrella crate holds most of the test time, so its test files belong to areas
//!   (`AREAS`). A crate selects an area when the area's anchor crates depend on it.
//! - `Cargo.lock`, the root `Cargo.toml`, `.github/`, `xtask/` and the toolchain files select
//!   every job. Documents select the format check only.

use crate::jobs::JOB_NAMES;
use std::collections::BTreeSet;
use std::path::Path;
use std::process::Command;

/// The jobs of the lint tier: what `--lint-only` keeps.
pub const LINT_JOBS: [&str; 3] = ["check", "fmt", "clippy"];
/// The jobs that a local run leaves to CI.
pub const HEAVY_JOBS: [&str; 2] = ["musl-binary", "vscode-e2e"];

const UMBRELLA: &str = "oxabl";
const TESTS_DIRECTORY: &str = "crates/oxabl/tests";
const CRATES_DIRECTORY: &str = "crates/";
const FUZZ_CRATE: &str = "oxabl-fuzz";
const PATHS_MAX: usize = 100_000;

/// Crates whose reverse dependency, or the crate itself, a job builds on its own.
const NODE_JOBS: [(&str, &str); 3] = [
    ("oxabl_wasm", "wasm"),
    ("oxabl_build_remote", "remote-cache"),
    (FUZZ_CRATE, "fuzz-targets"),
];

/// Crates that derive the schema of `oxabl.toml`, and the crates that feed its rule list.
const SCHEMA_CRATES: [&str; 5] = [
    UMBRELLA,
    "oxabl_workspace",
    "oxabl_style",
    "oxabl_lint",
    "oxabl_common",
];

const GRAMMAR_CRATE: &str = "oxabl_codegen";
const GENERATED_FILES: [&str; 2] = [
    "crates/oxabl_pipeline/src/keyword_docs.rs",
    "crates/oxabl_pipeline/src/completion/snippets.rs",
];

/// A group of umbrella test files that one set of crates can change.
pub struct Area {
    pub name: &'static str,
    /// File stems and directory names. `*` stands at the start or at the end, or not at all.
    /// A pattern that starts with `!` takes a name out of the area.
    pub patterns: &'static [&'static str],
    /// Jobs that run the area's feature-gated tests, and the jobs the area needs.
    pub jobs: &'static [&'static str],
}

pub const AREAS: [Area; 4] = [
    Area {
        name: "build",
        patterns: &["build_*"],
        jobs: &["remote-cache"],
    },
    Area {
        name: "lsp",
        patterns: &[
            "*_lsp",
            "daemon_*",
            "connection_queue_daemon",
            "lsp_*",
            "lsp",
        ],
        jobs: &[],
    },
    Area {
        name: "cli",
        patterns: &[
            "*_cli",
            "!build_*",
            "check_*",
            "skill_*",
            "class_members_ws",
            "analyze_collector",
            "create_triggers",
            "deep_nesting",
            "exit_codes",
            "format_stability",
            "large_inputs",
            "preprocessor_limits",
            "public_api",
            "schema_cache",
            "substitution_includes",
            "undefined_macro_expansion",
            "unjudged_symbols_reporting",
            "include_path_config",
            "vscode_snippets",
        ],
        jobs: &[],
    },
    Area {
        name: "release",
        patterns: &["self_update*", "install_script"],
        jobs: &["self-update", "install-script"],
    },
];

/// Entries of the umbrella's test directory that every area uses.
pub const SHARED: [&str; 3] = ["common", "fixtures", "scripts"];

/// Scripts and agent folders that no job reads.
const INERT_PREFIXES: [&str; 11] = [
    "scripts/bench/",
    "scripts/soak/",
    "scripts/gen-",
    "scripts/corpus-ab-gate",
    "scripts/lint-ab-diff",
    "scripts/extract-statements",
    "scripts/seeding-inventory",
    "scripts/publish-workspace",
    "scripts/dump-metaschema",
    ".claude/",
    ".grok/",
];
const INERT_FILES: [&str; 5] = [
    "LICENSE",
    ".gitignore",
    ".gitattributes",
    "release-please-config.json",
    ".release-please-manifest.json",
];
const EVERY_JOB_FILES: [&str; 9] = [
    "Cargo.lock",
    "Cargo.toml",
    "mise.toml",
    "rust-toolchain",
    "rust-toolchain.toml",
    "rustfmt.toml",
    ".rustfmt.toml",
    "clippy.toml",
    ".clippy.toml",
];
const EVERY_JOB_DIRECTORIES: [&str; 3] = [".github/", "xtask/", ".cargo/"];

/// A crate of the workspace, or the fuzz crate that sits outside it.
#[derive(Debug, Clone)]
pub struct Crate {
    pub name: String,
    pub directory: String,
    /// Workspace crates in `[dependencies]` and `[build-dependencies]`.
    pub dependencies: BTreeSet<String>,
    /// Workspace crates in `[dev-dependencies]`.
    pub dev_dependencies: BTreeSet<String>,
}

#[derive(Debug, Clone)]
pub struct Workspace {
    pub crates: Vec<Crate>,
}

struct Manifest {
    name: String,
    dependencies: BTreeSet<String>,
    dev_dependencies: BTreeSet<String>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Section {
    Package,
    Dependencies,
    DevDependencies,
    Other,
}

enum Header {
    Table(Section),
    /// `[dependencies.name]`: one dependency with its own table.
    Dependency(Section, String),
}

fn header_of(line: &str) -> Header {
    let inner = line.trim().trim_matches(|c| c == '[' || c == ']').trim();
    if inner == "package" {
        return Header::Table(Section::Package);
    }
    for (prefix, section) in [
        ("dependencies.", Section::Dependencies),
        ("build-dependencies.", Section::Dependencies),
        ("dev-dependencies.", Section::DevDependencies),
    ] {
        if let Some(name) = inner.strip_prefix(prefix) {
            return Header::Dependency(section, name.to_owned());
        }
    }
    if inner.ends_with("dev-dependencies") {
        Header::Table(Section::DevDependencies)
    } else if inner.ends_with("dependencies") && !inner.starts_with("workspace.") {
        Header::Table(Section::Dependencies)
    } else {
        Header::Table(Section::Other)
    }
}

fn quoted_value(line: &str) -> Option<&str> {
    let (_, value) = line.split_once('=')?;
    value.trim().strip_prefix('"')?.split('"').next()
}

/// Read the package name and the dependency names of one manifest. A renamed dependency
/// (`package = "..."`) is not followed: no workspace manifest renames one.
fn parse_manifest(text: &str) -> Result<Manifest, String> {
    let mut manifest = Manifest {
        name: String::new(),
        dependencies: BTreeSet::new(),
        dev_dependencies: BTreeSet::new(),
    };
    let mut section = Section::Other;
    for line in text.lines() {
        if line.starts_with('[') {
            section = match header_of(line) {
                Header::Table(next) => next,
                Header::Dependency(kind, name) => {
                    if kind == Section::DevDependencies {
                        manifest.dev_dependencies.insert(name);
                    } else {
                        manifest.dependencies.insert(name);
                    }
                    Section::Other
                }
            };
            continue;
        }
        if line.starts_with(|c: char| c.is_whitespace() || c == '#' || c == ']' || c == '"') {
            continue;
        }
        let Some((key, _)) = line.split_once('=') else {
            continue;
        };
        let key = key.trim();
        match section {
            Section::Package if key == "name" => {
                manifest.name = quoted_value(line).unwrap_or_default().to_owned();
            }
            Section::Dependencies => {
                manifest.dependencies.insert(key.to_owned());
            }
            Section::DevDependencies => {
                manifest.dev_dependencies.insert(key.to_owned());
            }
            _ => {}
        }
    }
    if manifest.name.is_empty() {
        return Err("a manifest has no package name".into());
    }
    Ok(manifest)
}

/// The quoted strings of the `members = [...]` list of a workspace manifest.
fn members_of(text: &str) -> Vec<String> {
    let mut members = Vec::new();
    let mut inside = false;
    for line in text.lines() {
        if line.trim_start().starts_with("members") && line.contains('[') {
            inside = true;
        }
        if inside {
            members.extend(line.split('"').skip(1).step_by(2).map(str::to_owned));
            if line.contains(']') {
                break;
            }
        }
    }
    members
}

impl Workspace {
    /// Build the graph from the root manifest, the manifest of each member and the fuzz manifest.
    pub fn from_manifests(
        root_manifest: &str,
        read: &dyn Fn(&str) -> Result<String, String>,
    ) -> Result<Workspace, String> {
        let mut parsed: Vec<(String, Manifest)> = Vec::new();
        let mut directories = members_of(root_manifest);
        if directories.is_empty() {
            return Err("the root manifest lists no workspace member".into());
        }
        directories.push("fuzz".into());
        for directory in directories {
            let manifest = parse_manifest(&read(&format!("{directory}/Cargo.toml"))?)
                .map_err(|error| format!("{directory}: {error}"))?;
            parsed.push((directory, manifest));
        }
        let names: BTreeSet<String> = parsed.iter().map(|(_, m)| m.name.clone()).collect();
        let crates = parsed
            .into_iter()
            .map(|(directory, manifest)| Crate {
                name: if directory == "fuzz" {
                    FUZZ_CRATE.to_owned()
                } else {
                    manifest.name
                },
                directory,
                dependencies: manifest
                    .dependencies
                    .intersection(&names)
                    .cloned()
                    .collect(),
                dev_dependencies: manifest
                    .dev_dependencies
                    .intersection(&names)
                    .cloned()
                    .collect(),
            })
            .collect();
        Ok(Workspace { crates })
    }

    pub fn load(root: &Path) -> Result<Workspace, String> {
        let read = |relative: &str| {
            std::fs::read_to_string(root.join(relative))
                .map_err(|error| format!("cannot read {relative}: {error}"))
        };
        Workspace::from_manifests(&read("Cargo.toml")?, &read)
    }

    fn by_name(&self, name: &str) -> Option<&Crate> {
        self.crates.iter().find(|c| c.name == name)
    }

    fn by_directory(&self, directory: &str) -> Option<&Crate> {
        self.crates.iter().find(|c| c.directory == directory)
    }

    /// The crates that name `name` as a dependency, dev-dependencies included.
    pub fn reverse_dependencies(&self, name: &str) -> BTreeSet<String> {
        self.crates
            .iter()
            .filter(|c| c.dependencies.contains(name) || c.dev_dependencies.contains(name))
            .map(|c| c.name.clone())
            .collect()
    }

    /// The anchors and everything they depend on, through `[dependencies]` only.
    fn closure(&self, anchors: &BTreeSet<String>) -> BTreeSet<String> {
        let mut seen = anchors.clone();
        let mut pending: Vec<String> = anchors.iter().cloned().collect();
        while let Some(name) = pending.pop() {
            let Some(krate) = self.by_name(&name) else {
                continue;
            };
            for dependency in &krate.dependencies {
                if seen.insert(dependency.clone()) {
                    pending.push(dependency.clone());
                }
            }
        }
        assert!(
            seen.len() <= self.crates.len(),
            "a closure stays in the workspace"
        );
        seen
    }

    /// The crates whose change can change a test of the area.
    fn area_crates(&self, area: &Area) -> BTreeSet<String> {
        let anchors: BTreeSet<String> = match area.name {
            "build" => ["oxabl_build", "oxabl_fake_avm"].map(str::to_owned).into(),
            "lsp" => ["oxabl_lsp", "oxabl_daemon"].map(str::to_owned).into(),
            // The command line: the umbrella's own dependencies that no other area owns.
            "cli" => {
                let own = self.by_name(UMBRELLA).map(|c| c.dependencies.clone());
                let others = [
                    "oxabl_lsp",
                    "oxabl_daemon",
                    "oxabl_build",
                    "oxabl_build_remote",
                ];
                own.unwrap_or_default()
                    .into_iter()
                    .filter(|name| !others.contains(&name.as_str()))
                    .collect()
            }
            _ => BTreeSet::new(),
        };
        self.closure(&anchors)
    }
}

/// Does `name` match `pattern`? One `*` at the start or at the end, or none.
pub fn matches(pattern: &str, name: &str) -> bool {
    assert!(
        pattern.matches('*').count() <= 1,
        "one star at most: {pattern}"
    );
    if let Some(suffix) = pattern.strip_prefix('*') {
        name.ends_with(suffix)
    } else if let Some(prefix) = pattern.strip_suffix('*') {
        name.starts_with(prefix)
    } else {
        assert!(!pattern.contains('*'), "a star stands at an end: {pattern}");
        name == pattern
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Home {
    Area(usize),
    Shared,
}

fn in_area(area: &Area, entry: &str) -> bool {
    let excluded = area
        .patterns
        .iter()
        .filter_map(|p| p.strip_prefix('!'))
        .any(|p| matches(p, entry));
    !excluded
        && area
            .patterns
            .iter()
            .filter(|p| !p.starts_with('!'))
            .any(|p| matches(p, entry))
}

/// The homes of an entry of the umbrella's test directory. A good entry has exactly one.
pub fn homes_of(entry: &str) -> Vec<Home> {
    let mut homes: Vec<Home> = AREAS
        .iter()
        .enumerate()
        .filter(|(_, area)| in_area(area, entry))
        .map(|(index, _)| Home::Area(index))
        .collect();
    if SHARED.contains(&entry) {
        homes.push(Home::Shared);
    }
    homes
}

/// The files and directories of `crates/oxabl/tests/`, and the tests that need a feature.
#[derive(Debug, Clone, Default)]
pub struct UmbrellaTests {
    /// File stems of the top-level test targets.
    pub files: BTreeSet<String>,
    /// Names of the directories.
    pub directories: BTreeSet<String>,
    /// Entries that are neither a Rust file nor a directory.
    pub strays: BTreeSet<String>,
    /// Tests that `required-features` keeps out of a plain `cargo test`.
    pub gated: BTreeSet<String>,
}

fn gated_tests(manifest: &str) -> BTreeSet<String> {
    let mut gated = BTreeSet::new();
    let mut name = String::new();
    let mut in_test = false;
    let mut needs_feature = false;
    let flush = |name: &mut String, needs: &mut bool, gated: &mut BTreeSet<String>| {
        if *needs && !name.is_empty() {
            gated.insert(std::mem::take(name));
        }
        name.clear();
        *needs = false;
    };
    for line in manifest.lines() {
        if line.starts_with('[') {
            flush(&mut name, &mut needs_feature, &mut gated);
            in_test = line.trim() == "[[test]]";
        } else if in_test && line.starts_with("name") {
            name = quoted_value(line).unwrap_or_default().to_owned();
        } else if in_test && line.starts_with("required-features") {
            needs_feature = true;
        }
    }
    flush(&mut name, &mut needs_feature, &mut gated);
    gated
}

impl UmbrellaTests {
    pub fn load(root: &Path) -> Result<UmbrellaTests, String> {
        let directory = root.join(TESTS_DIRECTORY);
        let listing = std::fs::read_dir(&directory)
            .map_err(|error| format!("cannot read {}: {error}", directory.display()))?;
        let mut tests = UmbrellaTests::default();
        for entry in listing {
            let entry = entry.map_err(|error| format!("cannot list the tests: {error}"))?;
            let name = entry.file_name().to_string_lossy().into_owned();
            let kind = entry
                .file_type()
                .map_err(|error| format!("cannot stat {name}: {error}"))?;
            if kind.is_dir() {
                tests.directories.insert(name);
            } else if let Some(stem) = name.strip_suffix(".rs") {
                tests.files.insert(stem.to_owned());
            } else {
                tests.strays.insert(name);
            }
        }
        let manifest = std::fs::read_to_string(root.join("crates/oxabl/Cargo.toml"))
            .map_err(|error| format!("cannot read the umbrella manifest: {error}"))?;
        tests.gated = gated_tests(&manifest);
        Ok(tests)
    }

    /// The test targets of an area that a plain `cargo test` builds.
    fn in_area(&self, area: usize) -> BTreeSet<String> {
        self.files
            .iter()
            .filter(|stem| homes_of(stem) == [Home::Area(area)] && !self.gated.contains(*stem))
            .cloned()
            .collect()
    }
}

/// What `cargo test` runs for a selection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tests {
    Nothing,
    /// `cargo test --workspace`.
    Full,
    Scoped {
        /// Crates for `cargo test -p`, without the umbrella.
        packages: BTreeSet<String>,
        umbrella: Umbrella,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Umbrella {
    None,
    /// These test targets of the umbrella crate.
    Targets(BTreeSet<String>),
    /// The umbrella's source changed: all of its tests.
    All,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Selection {
    pub jobs: BTreeSet<&'static str>,
    pub tests: Tests,
}

impl Selection {
    /// Keep only the lint jobs: the release pull request takes the fast tier.
    pub fn lint_only(mut self) -> Selection {
        self.jobs.retain(|job| LINT_JOBS.contains(job));
        self.tests = Tests::Nothing;
        self
    }
}

#[derive(Default)]
struct Accumulator {
    any: bool,
    every_job: bool,
    rust: bool,
    jobs: BTreeSet<&'static str>,
    crates: BTreeSet<String>,
    /// Areas that a crate selects: every test of the area runs.
    crate_areas: BTreeSet<usize>,
    umbrella_all: bool,
    umbrella_targets: BTreeSet<String>,
}

fn static_job(name: &str) -> &'static str {
    match JOB_NAMES.iter().find(|job| **job == name) {
        Some(job) => job,
        None => panic!("`{name}` is not a job"),
    }
}

impl Accumulator {
    fn job(&mut self, name: &str) {
        self.jobs.insert(static_job(name));
    }

    fn area_jobs(&mut self, index: usize) {
        for job in AREAS[index].jobs {
            self.job(job);
        }
    }

    fn every_area(&mut self) {
        for index in 0..AREAS.len() {
            self.area_jobs(index);
            self.crate_areas.insert(index);
        }
    }

    fn krate(&mut self, workspace: &Workspace, name: &str) {
        self.rust = true;
        self.crates.insert(name.to_owned());
        for dependent in workspace.reverse_dependencies(name) {
            self.crates.insert(dependent);
        }
        for (index, area) in AREAS.iter().enumerate() {
            if name == UMBRELLA || workspace.area_crates(area).contains(name) {
                self.area_jobs(index);
                self.crate_areas.insert(index);
            }
        }
        if SCHEMA_CRATES.contains(&name) {
            self.job("schema-drift");
        }
        if name == GRAMMAR_CRATE {
            self.job("grammar-drift");
        }
    }

    /// A test of the umbrella crate, or a file it reads, changed.
    fn umbrella_test(&mut self, tests: &UmbrellaTests, entry: &str) {
        self.rust = true;
        let homes = homes_of(entry);
        if homes.is_empty() {
            self.every_job = true;
            return;
        }
        for home in homes {
            match home {
                Home::Shared => self.every_area(),
                Home::Area(index) => {
                    self.area_jobs(index);
                    if tests.files.contains(entry) || !tests.directories.contains(entry) {
                        self.umbrella_targets.insert(entry.to_owned());
                    } else {
                        let all = tests.in_area(index);
                        self.umbrella_targets.extend(all);
                    }
                }
            }
        }
    }

    /// A path of the umbrella crate: its tests by file, anything else as a whole.
    fn umbrella_path(&mut self, workspace: &Workspace, tests: &UmbrellaTests, rest: &str) {
        if let Some(entry) = rest.strip_prefix("tests/") {
            let first = entry.split('/').next().unwrap_or(entry);
            let stem = first.strip_suffix(".rs").unwrap_or(first);
            self.umbrella_test(tests, stem);
        } else {
            self.krate(workspace, UMBRELLA);
            self.umbrella_all = true;
        }
    }

    fn crate_path(&mut self, workspace: &Workspace, tests: &UmbrellaTests, path: &str) {
        let Some(rest) = path.strip_prefix(CRATES_DIRECTORY) else {
            self.every_job = true;
            return;
        };
        let (directory, inside) = rest.split_once('/').unwrap_or((rest, ""));
        let Some(krate) = workspace.by_directory(&format!("{CRATES_DIRECTORY}{directory}")) else {
            self.every_job = true;
            return;
        };
        if inside == "CHANGELOG.md" {
            return;
        }
        if krate.name == UMBRELLA {
            self.umbrella_path(workspace, tests, inside);
        } else {
            self.krate(workspace, &krate.name);
        }
        if GENERATED_FILES.contains(&path) {
            self.job("grammar-drift");
        }
    }

    fn vscode_path(&mut self, tests: &UmbrellaTests, rest: &str) {
        self.job("vscode-client");
        self.job("vscode-e2e");
        if rest.starts_with("syntaxes/") || rest.starts_with("snippets/") {
            self.job("grammar-drift");
        }
        if rest.starts_with("schemas/") {
            self.job("schema-drift");
        }
        if rest.starts_with("snippets/") || rest == "test/keyword-case.json" {
            self.umbrella_test(tests, "vscode_snippets");
        }
        if rest == "package.json" {
            self.umbrella_test(tests, "semantic_tokens_lsp");
        }
    }

    fn script_path(&mut self, tests: &UmbrellaTests, path: &str) {
        let name = path.strip_prefix("scripts/").unwrap_or(path);
        if INERT_PREFIXES.iter().any(|p| path.starts_with(p)) {
            return;
        }
        match name {
            "install.sh" => self.job("install-script"),
            "remote-cache-binaries.sh" => self.job("remote-cache"),
            "plan-parity.sh" => self.umbrella_test(tests, "build_plan_parity"),
            "build-wasm.sh" => self.job("wasm"),
            _ if name.starts_with("parity/") => self.umbrella_test(tests, "build_plan_parity"),
            _ if name.contains("musl") => self.job("musl-binary"),
            _ => self.every_job = true,
        }
    }

    fn apply(&mut self, workspace: &Workspace, tests: &UmbrellaTests, path: &str) {
        self.any = true;
        if EVERY_JOB_FILES.contains(&path)
            || EVERY_JOB_DIRECTORIES.iter().any(|d| path.starts_with(d))
        {
            self.every_job = true;
        } else if path.starts_with(CRATES_DIRECTORY) {
            self.crate_path(workspace, tests, path);
        } else if let Some(rest) = path.strip_prefix("clients/vscode/") {
            self.vscode_path(tests, rest);
        } else if path.starts_with("scripts/") {
            self.script_path(tests, path);
        } else if path.starts_with("fuzz/") {
            self.rust = true;
            self.job("fuzz-targets");
        } else if path.starts_with("resources/") {
            self.resource_path(workspace);
        } else if path.starts_with("docs/") || (path.ends_with(".md") && !path.contains('/')) {
            self.document_path(workspace, tests, path);
        } else if INERT_FILES.contains(&path) || INERT_PREFIXES.iter().any(|p| path.starts_with(p))
        {
            // The format check still runs for the change.
        } else {
            self.every_job = true;
        }
    }

    /// Files of `resources/` feed the code generator, the parser tests and the grammar tests.
    fn resource_path(&mut self, workspace: &Workspace) {
        for name in [GRAMMAR_CRATE, "oxabl_parser", "oxabl_bench"] {
            self.krate(workspace, name);
        }
        self.job("grammar-drift");
        self.job("vscode-client");
    }

    /// A document selects the format check only, unless a test reads it.
    fn document_path(&mut self, workspace: &Workspace, tests: &UmbrellaTests, path: &str) {
        match path {
            "docs/build-wrapper.md" => self.umbrella_test(tests, "build_wrapper"),
            "docs/overview.md" => self.krate(workspace, "oxabl_lint"),
            _ => {}
        }
    }

    fn finish(mut self, workspace: &Workspace, tests: &UmbrellaTests) -> Selection {
        if self.every_job {
            return Selection {
                jobs: JOB_NAMES.iter().copied().collect(),
                tests: Tests::Full,
            };
        }
        if self.any {
            self.job("fmt");
        }
        if self.rust {
            self.job("check");
            self.job("clippy");
        }
        let packages: BTreeSet<String> = self
            .crates
            .iter()
            .filter(|name| {
                name.as_str() != UMBRELLA
                    && workspace
                        .by_name(name)
                        .is_some_and(|c| c.directory.starts_with(CRATES_DIRECTORY))
            })
            .cloned()
            .collect();
        let umbrella = if self.umbrella_all {
            Umbrella::All
        } else {
            let mut targets = self.umbrella_targets.clone();
            for area in &self.crate_areas {
                targets.extend(tests.in_area(*area));
            }
            targets.retain(|t| tests.files.contains(t) && !tests.gated.contains(t));
            if targets.is_empty() {
                Umbrella::None
            } else {
                Umbrella::Targets(targets)
            }
        };
        let nothing = packages.is_empty() && umbrella == Umbrella::None;
        if !nothing {
            self.job("test");
        }
        for (node, job) in NODE_JOBS {
            if self.crates.contains(node) {
                self.job(job);
            }
        }
        let tests = if nothing {
            Tests::Nothing
        } else {
            Tests::Scoped { packages, umbrella }
        };
        Selection {
            jobs: self.jobs,
            tests,
        }
    }
}

/// The jobs and the tests that these changed paths select.
pub fn select(workspace: &Workspace, tests: &UmbrellaTests, paths: &[String]) -> Selection {
    assert!(paths.len() <= PATHS_MAX, "too many changed paths");
    let mut accumulator = Accumulator::default();
    for path in paths {
        accumulator.apply(workspace, tests, path);
    }
    accumulator.finish(workspace, tests)
}

/// The paths that the branch changed since its merge base with `base`, with the paths of
/// uncommitted and untracked files. Renames count as a delete and an add.
pub fn changed_paths(root: &Path, base: &str) -> Result<Vec<String>, String> {
    let git = |args: &[&str]| -> Result<String, String> {
        let output = Command::new("git")
            .args(args)
            .current_dir(root)
            .output()
            .map_err(|error| format!("cannot run git: {error}"))?;
        if !output.status.success() {
            return Err(format!(
                "git {} failed: {}",
                args.join(" "),
                String::from_utf8_lossy(&output.stderr).trim()
            ));
        }
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    };
    let merge_base = git(&["merge-base", base, "HEAD"])?;
    let merge_base = merge_base.trim();
    if merge_base.is_empty() {
        return Err(format!("no merge base of {base} and HEAD"));
    }
    let diff = git(&["diff", "--name-only", "--no-renames", merge_base])?;
    let untracked = git(&["ls-files", "--others", "--exclude-standard"])?;
    let paths: BTreeSet<String> = diff
        .lines()
        .chain(untracked.lines())
        .filter(|line| !line.is_empty())
        .map(str::to_owned)
        .collect();
    Ok(paths.into_iter().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jobs;
    use std::path::PathBuf;

    fn root() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..")
    }

    fn setup() -> (Workspace, UmbrellaTests) {
        let workspace = Workspace::load(&root()).expect("the workspace loads");
        let tests = UmbrellaTests::load(&root()).expect("the umbrella tests load");
        (workspace, tests)
    }

    fn select_paths(paths: &[&str]) -> Selection {
        let (workspace, tests) = setup();
        let paths: Vec<String> = paths.iter().map(|p| (*p).to_owned()).collect();
        select(&workspace, &tests, &paths)
    }

    fn selected(selection: &Selection) -> Vec<&'static str> {
        selection.jobs.iter().copied().collect()
    }

    fn sorted(names: &[&'static str]) -> Vec<&'static str> {
        let mut names = names.to_vec();
        names.sort_unstable();
        names
    }

    fn packages(selection: &Selection) -> Vec<String> {
        match &selection.tests {
            Tests::Scoped { packages, .. } => packages.iter().cloned().collect(),
            _ => Vec::new(),
        }
    }

    fn umbrella_targets(selection: &Selection) -> BTreeSet<String> {
        match &selection.tests {
            Tests::Scoped {
                umbrella: Umbrella::Targets(targets),
                ..
            } => targets.clone(),
            _ => BTreeSet::new(),
        }
    }

    fn files_of(area: &str, tests: &UmbrellaTests) -> BTreeSet<String> {
        let index = AREAS.iter().position(|a| a.name == area).expect("an area");
        tests.in_area(index)
    }

    #[test]
    fn a_daemon_change_selects_the_daemon_its_dependents_and_the_lsp_tests() {
        let (_, tests) = setup();
        let selection = select_paths(&["crates/oxabl_daemon/src/lib.rs"]);
        assert_eq!(
            selected(&selection),
            sorted(&["check", "clippy", "fmt", "test"])
        );
        assert_eq!(packages(&selection), ["oxabl_daemon", "oxabl_lsp"]);
        assert_eq!(umbrella_targets(&selection), files_of("lsp", &tests));
        assert!(umbrella_targets(&selection).contains("daemon_exit_lsp"));
        assert!(!umbrella_targets(&selection).contains("build_compile_basic"));
    }

    #[test]
    fn a_document_selects_the_format_check_only() {
        let selection = select_paths(&["docs/design/completion.md", "CLAUDE.md", "README.md"]);
        assert_eq!(selected(&selection), ["fmt"]);
        assert_eq!(selection.tests, Tests::Nothing);
    }

    #[test]
    fn a_document_that_a_test_reads_selects_that_test() {
        let selection = select_paths(&["docs/build-wrapper.md"]);
        assert!(umbrella_targets(&selection).contains("build_wrapper"));
        let selection = select_paths(&["docs/overview.md"]);
        assert!(packages(&selection).contains(&"oxabl_lint".to_owned()));
    }

    #[test]
    fn a_vscode_change_selects_the_client_jobs() {
        let selection = select_paths(&["clients/vscode/src/extension.ts"]);
        assert_eq!(
            selected(&selection),
            sorted(&["fmt", "vscode-client", "vscode-e2e"])
        );
        assert_eq!(selection.tests, Tests::Nothing);
    }

    #[test]
    fn a_snippet_change_also_selects_the_drift_job_and_its_test() {
        let selection = select_paths(&["clients/vscode/snippets/abl.json"]);
        assert!(selection.jobs.contains("grammar-drift"));
        assert_eq!(
            umbrella_targets(&selection),
            BTreeSet::from(["vscode_snippets".to_owned()])
        );
    }

    #[test]
    fn a_lock_file_or_a_workflow_or_xtask_selects_every_job() {
        for path in [
            "Cargo.lock",
            "Cargo.toml",
            ".github/workflows/ci.yml",
            "xtask/src/jobs.rs",
        ] {
            let selection = select_paths(&[path]);
            assert_eq!(selection.jobs.len(), jobs::JOB_NAMES.len(), "{path}");
            assert_eq!(selection.tests, Tests::Full, "{path}");
        }
    }

    #[test]
    fn a_path_that_no_rule_names_selects_every_job() {
        for path in [
            "weird.txt",
            "crates/unknown_crate/src/lib.rs",
            "scripts/unlisted.sh",
        ] {
            let selection = select_paths(&[path]);
            assert_eq!(selection.tests, Tests::Full, "{path}");
        }
    }

    #[test]
    fn one_umbrella_test_file_selects_that_file_only() {
        let selection = select_paths(&["crates/oxabl/tests/format_cli.rs"]);
        assert_eq!(
            umbrella_targets(&selection),
            BTreeSet::from(["format_cli".to_owned()])
        );
        assert!(packages(&selection).is_empty());
        assert_eq!(
            selected(&selection),
            sorted(&["check", "clippy", "fmt", "test"])
        );
    }

    #[test]
    fn a_test_that_needs_a_feature_runs_in_its_job() {
        let selection = select_paths(&["crates/oxabl/tests/self_update.rs"]);
        assert!(selection.jobs.contains("self-update"));
        assert!(umbrella_targets(&selection).is_empty());
        let selection = select_paths(&["crates/oxabl/tests/build_remote_cache.rs"]);
        assert!(selection.jobs.contains("remote-cache"));
        assert!(umbrella_targets(&selection).is_empty());
    }

    #[test]
    fn the_shared_test_helpers_select_every_area() {
        let (_, tests) = setup();
        let selection = select_paths(&["crates/oxabl/tests/common/mod.rs"]);
        let everything: BTreeSet<String> = (0..AREAS.len())
            .flat_map(|index| tests.in_area(index))
            .collect();
        assert_eq!(umbrella_targets(&selection), everything);
    }

    #[test]
    fn a_source_change_of_the_umbrella_runs_all_its_tests() {
        let selection = select_paths(&["crates/oxabl/src/main.rs"]);
        let Tests::Scoped { umbrella, .. } = &selection.tests else {
            panic!("a scoped selection");
        };
        assert_eq!(*umbrella, Umbrella::All);
        for job in [
            "self-update",
            "install-script",
            "remote-cache",
            "schema-drift",
            "wasm",
        ] {
            assert!(selection.jobs.contains(job), "{job}");
        }
    }

    #[test]
    fn a_low_crate_selects_every_area_and_its_direct_dependents() {
        let (_, tests) = setup();
        let selection = select_paths(&["crates/oxabl_lexer/src/lib.rs"]);
        let packages = packages(&selection);
        assert!(packages.contains(&"oxabl_lexer".to_owned()));
        assert!(packages.contains(&"oxabl_parser".to_owned()));
        assert!(!packages.contains(&"oxabl".to_owned()));
        assert!(!packages.contains(&"oxabl_daemon".to_owned()));
        for area in ["build", "lsp", "cli"] {
            assert!(umbrella_targets(&selection).is_superset(&files_of(area, &tests)));
        }
        assert!(umbrella_targets(&selection).is_disjoint(&files_of("release", &tests)));
    }

    #[test]
    fn a_crate_that_a_job_builds_alone_selects_that_job() {
        assert!(
            select_paths(&["crates/oxabl_build_remote/src/lib.rs"])
                .jobs
                .contains("remote-cache")
        );
        assert!(
            select_paths(&["crates/oxabl_wasm/src/lib.rs"])
                .jobs
                .contains("wasm")
        );
        assert!(
            select_paths(&["fuzz/fuzz_targets/lex.rs"])
                .jobs
                .contains("fuzz-targets")
        );
        assert!(
            select_paths(&["crates/oxabl_codegen/src/main.rs"])
                .jobs
                .contains("grammar-drift")
        );
        assert!(
            select_paths(&["scripts/install.sh"])
                .jobs
                .contains("install-script")
        );
        assert!(
            select_paths(&["scripts/build-musl.sh"])
                .jobs
                .contains("musl-binary")
        );
        // The build crate's dependents include the remote cache.
        assert!(
            select_paths(&["crates/oxabl_build/src/lib.rs"])
                .jobs
                .contains("remote-cache")
        );
    }

    #[test]
    fn a_change_log_selects_no_test() {
        let selection = select_paths(&["crates/oxabl_lexer/CHANGELOG.md"]);
        assert_eq!(selected(&selection), ["fmt"]);
    }

    #[test]
    fn the_lint_tier_keeps_only_the_lint_jobs() {
        let selection = select_paths(&["Cargo.lock"]).lint_only();
        assert_eq!(selected(&selection), sorted(&LINT_JOBS));
        assert_eq!(selection.tests, Tests::Nothing);
    }

    #[test]
    fn no_changed_path_selects_nothing() {
        let selection = select_paths(&[]);
        assert!(selection.jobs.is_empty());
        assert_eq!(selection.tests, Tests::Nothing);
    }

    #[test]
    fn every_entry_of_the_umbrella_tests_has_exactly_one_home() {
        let (_, tests) = setup();
        assert!(tests.strays.is_empty(), "stray entries: {:?}", tests.strays);
        for entry in tests.files.iter().chain(&tests.directories) {
            let homes = homes_of(entry);
            assert_eq!(
                homes.len(),
                1,
                "crates/oxabl/tests/{entry} needs exactly one area in xtask/src/changed.rs, has {homes:?}"
            );
        }
    }

    #[test]
    fn the_home_check_sees_a_missing_and_a_double_home() {
        assert!(homes_of("brand_new_test").is_empty());
        // A name that two areas claim.
        assert_eq!(homes_of("build_cache_lsp").len(), 2);
        // An exclusion keeps a build test out of the command-line area.
        assert_eq!(homes_of("build_probe_cli"), [Home::Area(0)]);
    }

    #[test]
    fn every_area_owns_a_file_and_every_gated_test_exists() {
        let (_, tests) = setup();
        for (index, area) in AREAS.iter().enumerate() {
            assert!(
                !tests.in_area(index).is_empty(),
                "area {} owns no test",
                area.name
            );
        }
        assert!(tests.gated.len() >= 6, "gated: {:?}", tests.gated);
        for name in &tests.gated {
            assert!(
                tests.files.contains(name),
                "{name} is gated and has no file"
            );
        }
    }

    #[test]
    fn the_job_names_of_the_rules_are_jobs() {
        for job in LINT_JOBS.iter().chain(&HEAVY_JOBS) {
            assert!(jobs::JOB_NAMES.contains(job), "{job}");
        }
        for (_, job) in NODE_JOBS {
            assert!(jobs::JOB_NAMES.contains(&job), "{job}");
        }
        for area in &AREAS {
            for job in area.jobs {
                assert!(jobs::JOB_NAMES.contains(job), "{job}");
            }
        }
    }

    #[test]
    fn the_workspace_graph_holds_every_member_and_the_fuzz_crate() {
        let (workspace, _) = setup();
        for name in [UMBRELLA, "oxabl_lexer", "oxabl_wasm", FUZZ_CRATE, "xtask"] {
            assert!(workspace.by_name(name).is_some(), "{name}");
        }
        let lexer_dependents = workspace.reverse_dependencies("oxabl_lexer");
        assert!(lexer_dependents.contains("oxabl_parser"));
        assert!(!lexer_dependents.contains("oxabl_lexer"));
        // A dependency with its own table is read too.
        let manifest = parse_manifest(
            "[package]\nname = \"p\"\n[dependencies.oxabl_a]\npath = \"x\"\n\
             [dev-dependencies.oxabl_b]\npath = \"y\"\n\
             [target.'cfg(unix)'.dependencies]\noxabl_c = { path = \"z\" }\n",
        )
        .expect("a manifest");
        assert_eq!(manifest.name, "p");
        assert!(manifest.dependencies.contains("oxabl_a"));
        assert!(manifest.dependencies.contains("oxabl_c"));
        assert!(manifest.dev_dependencies.contains("oxabl_b"));
    }

    #[test]
    fn every_crate_directory_is_a_package_of_the_same_name() {
        let (workspace, _) = setup();
        for krate in &workspace.crates {
            if krate.directory.starts_with(CRATES_DIRECTORY) {
                assert_eq!(
                    krate.directory,
                    format!("{CRATES_DIRECTORY}{}", krate.name),
                    "a crate path names its package"
                );
            }
        }
    }

    /// Every crate that derives the schema of `oxabl.toml` is a schema crate.
    #[test]
    fn the_crates_that_derive_a_json_schema_select_the_schema_job() {
        let (workspace, _) = setup();
        for krate in crates_of(&workspace) {
            let directory = root().join(&krate.directory).join("src");
            if sources_contain(&directory, "JsonSchema") {
                assert!(
                    SCHEMA_CRATES.contains(&krate.name.as_str()),
                    "{} derives JsonSchema: add it to SCHEMA_CRATES",
                    krate.name
                );
            }
        }
    }

    /// A Rust file that reads a file of the repository's `resources/` must select its crate when
    /// that file changes. A path literal counts when, from the crate or from the file, it
    /// leads into `resources/` of the repository.
    #[test]
    fn the_crates_that_read_the_repository_resources_are_selected() {
        let (workspace, _) = setup();
        let selected = packages(&select_paths(&["resources/abl_keyword_index.json"]));
        let mut readers = BTreeSet::new();
        for krate in crates_of(&workspace) {
            let crate_directory = root().join(&krate.directory);
            for folder in ["tests", "src"] {
                let mut files = Vec::new();
                rust_files(&crate_directory.join(folder), &mut files);
                for file in files {
                    if reads_root_resources(&file, &crate_directory) {
                        readers.insert(krate.name.clone());
                    }
                }
            }
        }
        assert!(
            readers.contains("oxabl_parser"),
            "the scan finds a known reader"
        );
        assert!(
            !readers.contains("oxabl_build"),
            "its resources/ is its own"
        );
        for name in readers {
            assert!(
                selected.contains(&name) || name == UMBRELLA,
                "{name} reads resources/ and a change of it does not select it"
            );
        }
    }

    fn normalized(path: &Path) -> PathBuf {
        let mut out = PathBuf::new();
        for part in path.components() {
            match part {
                std::path::Component::ParentDir => {
                    out.pop();
                }
                other => out.push(other),
            }
        }
        out
    }

    fn reads_root_resources(file: &Path, crate_directory: &Path) -> bool {
        let Ok(text) = std::fs::read_to_string(file) else {
            return false;
        };
        let resources = normalized(&root().join("resources"));
        let file_directory = file.parent().unwrap_or(crate_directory);
        text.lines()
            .filter(|line| !line.trim_start().starts_with("//"))
            .any(|line| {
                // `include_str!` reads from the file's folder, a manifest-dir path from the crate's.
                let base = if line.contains("include_") {
                    file_directory
                } else {
                    crate_directory
                };
                line.split('"')
                    .skip(1)
                    .step_by(2)
                    .filter(|literal| literal.contains("resources"))
                    .any(|literal| {
                        let literal = literal.trim_start_matches('/');
                        normalized(&base.join(literal)).starts_with(&resources)
                    })
            })
    }

    /// A document that a test reads must not select the format check only.
    #[test]
    fn the_documents_that_a_rust_file_reads_are_known_inputs() {
        let (workspace, _) = setup();
        let mut found = BTreeSet::new();
        for krate in crates_of(&workspace) {
            for folder in ["tests", "src"] {
                collect_doc_reads(&root().join(&krate.directory).join(folder), &mut found);
            }
        }
        assert!(!found.is_empty(), "the scan finds the known readers");
        for document in found {
            let selection = select_paths(&[&document]);
            assert!(
                selection.tests != Tests::Nothing,
                "{document} is read by Rust code and selects no test"
            );
        }
    }

    /// The crates under `crates/`: the scans read their sources.
    fn crates_of(workspace: &Workspace) -> impl Iterator<Item = &Crate> {
        workspace
            .crates
            .iter()
            .filter(|c| c.directory.starts_with(CRATES_DIRECTORY))
    }

    fn rust_files(directory: &Path, out: &mut Vec<PathBuf>) {
        let Ok(listing) = std::fs::read_dir(directory) else {
            return;
        };
        for entry in listing.flatten() {
            let path = entry.path();
            if path.is_dir() {
                rust_files(&path, out);
            } else if path.extension().is_some_and(|e| e == "rs") {
                out.push(path);
            }
        }
    }

    fn sources_contain(directory: &Path, needle: &str) -> bool {
        let mut files = Vec::new();
        rust_files(directory, &mut files);
        files
            .iter()
            .filter_map(|file| std::fs::read_to_string(file).ok())
            .any(|text| text.contains(needle))
    }

    fn collect_doc_reads(directory: &Path, found: &mut BTreeSet<String>) {
        let mut files = Vec::new();
        rust_files(directory, &mut files);
        for file in files {
            let Ok(text) = std::fs::read_to_string(&file) else {
                continue;
            };
            for line in text.lines().filter(|l| !l.trim_start().starts_with("//")) {
                if let Some((_, rest)) = line.split_once("../docs/") {
                    let name: String = rest
                        .chars()
                        .take_while(|c| c.is_ascii_alphanumeric() || "-_./".contains(*c))
                        .collect();
                    found.insert(format!("docs/{name}"));
                }
            }
        }
    }
}
