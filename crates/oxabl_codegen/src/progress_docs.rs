//! Crawl the Progress OpenEdge ABL reference documentation and emit a static
//! JSON resource describing every documented built-in type.
//!
//! The docs platform (ZoomIn) exposes two endpoints this module relies on:
//!
//! * `.../bundle/{bundle}/toc/{page}.html` — returns the *entire* bundle
//!   navigation tree as a JSON object keyed by TOC node id. A single call
//!   yields every package and type in the bundle (the page name in the URL is
//!   ignored; any page returns the whole tree).
//! * `.../bundle/{bundle}/page/{page}.html` — a single page.
//!
//! The index is merged from **two bundles**:
//!
//! * `openedge-abl-api-reference-128` (the API reference) documents the
//!   `OpenEdge.*`/`Ccs.*` classes with a Javadoc-style renderer: one page per
//!   type carrying its inheritance header and full member detail tables.
//! * `abl-reference` (the ABL Reference) documents the `Progress.*` built-in
//!   classes with a different (DITA) renderer. Its type pages are *index-only*:
//!   supertype, interfaces, constructor signatures, and a name-only member
//!   list. Each member's full signature lives on a separate
//!   `*-method*`/`*-property*`/`*-event*` page, so those are fetched too.
//!
//! Only `docs-be.progress.com` serves the `toc/` endpoint; the public mirror
//! 404s it. This module therefore talks to the same host the checked-in
//! `abl_keyword_index.json` already points at.
//!
//! Two deliberate assumptions (both follow from what Progress does **not**
//! publish):
//!
//! * Every documented member is PUBLIC. Progress documents no private or
//!   protected API surface, so no access-modifier data is collected.
//! * Every parameter is a regular INPUT parameter. Progress's built-in
//!   classes return values rather than writing OUTPUT/INPUT-OUTPUT parameters,
//!   so members carry a `return_type` only when the page specifies one.
//!
//! Fetched pages are cached under
//! `$XDG_CACHE_HOME/oxabl/progress-docs/{bundle}/` (falling back to
//! `~/.cache`), keyed by section (`toc`/`page`) and page file name, so a full
//! re-run is cheap.

use scraper::{ElementRef, Html, Selector};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::thread;
use std::time::Duration;

pub const DEFAULT_BUNDLE_ID: &str = "openedge-abl-api-reference-128";
/// The "ABL Reference" bundle, which documents the `Progress.*` built-in
/// classes (plus every keyword/statement/function). Its type pages use a
/// different naming scheme and renderer than the API-reference bundle.
pub const ABL_REFERENCE_BUNDLE_ID: &str = "abl-reference";

const DOCS_HOST: &str = "https://docs-be.progress.com";
const USER_AGENT: &str = "oxabl-builtin-class-index/0.1 (+https://github.com/oxabl-project/oxabl)";
const THROTTLE: Duration = Duration::from_millis(80);

/// How a bundle's type pages encode dotted names in their file names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PageNaming {
    /// `OpenEdge.Core.String.html` — the file stem is the dotted name.
    Plain,
    /// `Progress.Lang.Object-class.html` / `...-interface.html` /
    /// `...-enumeration.html`, with generics HTML-encoded as `lt`/`gt`.
    DitaSuffixed,
}

impl PageNaming {
    fn for_bundle(bundle_id: &str) -> PageNaming {
        if bundle_id == ABL_REFERENCE_BUNDLE_ID {
            PageNaming::DitaSuffixed
        } else {
            PageNaming::Plain
        }
    }

    /// A page name that exists in the bundle, used as the `toc/` URL argument
    /// (the endpoint returns the whole tree regardless of which page is named).
    fn toc_page(bundle_id: &str) -> &'static str {
        if bundle_id == ABL_REFERENCE_BUNDLE_ID {
            "Keyword-Index.html"
        } else {
            "OpenEdge.package.html"
        }
    }

    /// Map a page file stem (filename minus `.html`) to a dotted type name, or
    /// `None` when the page is not a type page under this naming scheme.
    fn page_to_name(&self, stem: &str) -> Option<String> {
        match self {
            PageNaming::Plain => {
                if stem.ends_with(".package") || !stem.contains('.') {
                    return None;
                }
                Some(stem.to_string())
            }
            PageNaming::DitaSuffixed => {
                let dotted = stem
                    .strip_suffix("-class")
                    .or_else(|| stem.strip_suffix("-interface"))
                    .or_else(|| stem.strip_suffix("-enumeration"))?;
                Some(decode_generics(dotted))
            }
        }
    }
}

/// A type discovered in the catalog: its dotted name plus the bundle and page
/// file name it must be fetched from.
#[derive(Debug, Clone)]
struct TypeRef {
    name: String,
    bundle: String,
    page: String,
}

/// A member discovered on an `abl-reference` class page, needing its own
/// `*-method*` / `*-property*` / `*-event*` page fetch for the full signature.
#[derive(Debug, Clone)]
struct DitaMemberRef {
    kind: MemberKind,
    page: String,
    name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemberKind {
    Method,
    Property,
    Event,
}

// =============================================================================
// Data model
// =============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuiltinIndex {
    /// Bundles scraped, in crawl order. The built-in class index is merged
    /// from two bundles: `openedge-abl-api-reference-128` (OpenEdge.*/Ccs.*)
    /// and `abl-reference` (Progress.*).
    pub bundle_ids: Vec<String>,
    /// Human-readable release, e.g. `12.8` (derived from the versioned bundle).
    pub version: String,
    /// UTC timestamp of generation (RFC 3339).
    pub generated_at: String,
    /// Every package in the bundles, including the root packages, as dotted
    /// names (e.g. `Ccs`, `OpenEdge.Core.Collections`, `Progress.Lang`).
    pub packages: Vec<String>,
    /// Every documented type keyed by its fully-qualified dotted name
    /// (e.g. `OpenEdge.Core.String`, `Progress.Lang.Object`).
    pub types: BTreeMap<String, TypeEntry>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TypeKind {
    Class,
    Interface,
    Enum,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeEntry {
    pub kind: TypeKind,
    pub package: String,
    /// Reference page file name (relative to the bundle's `page/` directory).
    pub page: String,
    /// The immediate supertype (parent class or super-interface), when
    /// documented. ABL is single-inheritance, so one name suffices.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inherits: Option<String>,
    /// Interfaces the class implements, fully-qualified where documented.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub implements: Vec<String>,
    #[serde(default, skip_serializing_if = "is_false")]
    pub is_abstract: bool,
    #[serde(default, skip_serializing_if = "is_false")]
    pub is_final: bool,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub constructors: Vec<Member>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub methods: Vec<Member>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub properties: Vec<Member>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub events: Vec<Member>,
    /// Enum constant names, when the reference page lists them (the
    /// `abl-reference` bundle does; the API-reference bundle does not).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub enum_members: Vec<String>,
}

/// A documented member. Properties reuse this shape with `return_type` as the
/// property type; constructors and events have no `return_type`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Member {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub return_type: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub params: Vec<Param>,
    #[serde(default, skip_serializing_if = "is_false")]
    pub is_static: bool,
    #[serde(default, skip_serializing_if = "is_false")]
    pub is_abstract: bool,
    #[serde(default, skip_serializing_if = "is_false")]
    pub is_final: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Param {
    /// Parameter name as documented, when the detail table carries one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    pub type_name: String,
}

// =============================================================================
// CLI
// =============================================================================

/// Handle the `builtin-classes` codegen command.
///
/// Options (in any order):
/// * `--bundle <ids>`  bundles to scrape; repeatable or comma-separated
///   (default `openedge-abl-api-reference-128,abl-reference`)
/// * `--refresh`       ignore the on-disk page cache
/// * `--catalog-only`  print the merged package/type catalog, write nothing
/// * `--limit <n>`     fetch at most `n` type detail pages
/// * `--names <a,b,c>` fetch exactly these types (comma-separated dotted names)
/// * `--no-member-details`  index-only for `abl-reference` types (member names,
///   no `*-method*`/`*-property*` page fetches)
/// * `--out <path>`    output path, relative to the workspace root
pub fn run_builtin_classes(args: &[String]) -> Result<(), String> {
    let mut bundles: Vec<String> = Vec::new();
    let mut refresh = false;
    let mut catalog_only = false;
    let mut no_member_details = false;
    let mut limit: Option<usize> = None;
    let mut names: Option<Vec<String>> = None;
    let mut out_path = PathBuf::from("resources/progress_builtin_classes.json");

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--bundle" => {
                i += 1;
                let raw = args.get(i).ok_or("`--bundle` needs a value")?;
                bundles.extend(
                    raw.split(',')
                        .map(str::trim)
                        .filter(|s| !s.is_empty())
                        .map(str::to_string),
                );
            }
            "--refresh" => refresh = true,
            "--catalog-only" => catalog_only = true,
            "--no-member-details" => no_member_details = true,
            "--limit" => {
                i += 1;
                let raw = args.get(i).ok_or("`--limit` needs a value")?;
                limit = Some(
                    raw.parse()
                        .map_err(|_| format!("`--limit` expects an integer, got `{raw}`"))?,
                );
            }
            "--names" => {
                i += 1;
                let raw = args.get(i).ok_or("`--names` needs a value")?;
                names = Some(
                    raw.split(',')
                        .map(str::trim)
                        .filter(|s| !s.is_empty())
                        .map(str::to_string)
                        .collect(),
                );
            }
            "--out" => {
                i += 1;
                out_path = PathBuf::from(args.get(i).ok_or("`--out` needs a value")?);
            }
            other => return Err(format!("unknown option `{other}`")),
        }
        i += 1;
    }

    if bundles.is_empty() {
        bundles = vec![
            DEFAULT_BUNDLE_ID.to_string(),
            ABL_REFERENCE_BUNDLE_ID.to_string(),
        ];
    }

    let version = version_from_bundles(&bundles);

    // --- catalog: one toc/ fetch per bundle, merged and deduped by name ---
    let mut catalog: Vec<TypeRef> = Vec::new();
    let mut seen_names = HashSet::new();
    for bundle in &bundles {
        let naming = PageNaming::for_bundle(bundle);
        let cache_dir = cache_dir(bundle);
        eprintln!("fetching catalog for bundle {bundle}");
        let toc_json = fetch_or_cache(
            bundle,
            "toc",
            PageNaming::toc_page(bundle),
            &cache_dir,
            refresh,
        )?;
        let refs = parse_toc_types(&toc_json, naming)?;
        eprintln!("  {} types", refs.len());
        for (name, page) in refs {
            if seen_names.insert(name.clone()) {
                catalog.push(TypeRef {
                    name,
                    bundle: bundle.clone(),
                    page,
                });
            }
        }
    }
    let names_list: Vec<String> = catalog.iter().map(|r| r.name.clone()).collect();
    let packages = derive_packages(&names_list);
    eprintln!(
        "catalog: {} packages, {} types",
        packages.len(),
        catalog.len()
    );

    // --- selection ---
    let selected: Vec<&TypeRef> = match &names {
        Some(ns) => {
            let set: HashSet<&str> = ns.iter().map(|s| s.as_str()).collect();
            catalog
                .iter()
                .filter(|r| set.contains(r.name.as_str()))
                .collect()
        }
        None => catalog.iter().collect(),
    };
    let selected = selected
        .into_iter()
        .take(limit.unwrap_or(usize::MAX))
        .collect::<Vec<_>>();

    if catalog_only {
        println!("bundles: {}", bundles.join(", "));
        println!("packages ({}):", packages.len());
        for p in &packages {
            println!("  {p}");
        }
        println!("types ({}; showing first 20):", selected.len());
        for t in selected.iter().take(20) {
            println!("  {}", t.name);
        }
        return Ok(());
    }

    let mut index = BuiltinIndex {
        bundle_ids: bundles.clone(),
        version,
        generated_at: now_rfc3339(),
        packages,
        types: BTreeMap::new(),
    };

    let mut parsed = 0usize;
    let mut skipped = 0usize;
    let mut failed = Vec::new();
    for tr in selected {
        let cache_dir = cache_dir(&tr.bundle);
        let page_html = match fetch_or_cache(&tr.bundle, "page", &tr.page, &cache_dir, refresh) {
            Ok(h) => h,
            Err(e) => {
                failed.push((tr.name.clone(), e));
                continue;
            }
        };
        let naming = PageNaming::for_bundle(&tr.bundle);
        let mut te: TypeEntry;
        let member_inventory: Vec<DitaMemberRef>;
        match naming {
            PageNaming::Plain => {
                let Some(t) = parse_type_page(&page_html, &tr.name) else {
                    skipped += 1;
                    continue;
                };
                te = t;
                member_inventory = Vec::new();
            }
            PageNaming::DitaSuffixed => {
                let Some((t, inv)) = parse_dita_class_page(&page_html, &tr.name, &tr.page) else {
                    skipped += 1;
                    continue;
                };
                te = t;
                member_inventory = inv;
            }
        }

        if member_inventory.is_empty() {
            // Nothing further to fetch.
        } else if no_member_details {
            // Index-only: keep the member names from the class page.
            for mref in &member_inventory {
                let member = Member {
                    name: mref.name.clone(),
                    return_type: None,
                    params: Vec::new(),
                    is_static: false,
                    is_abstract: false,
                    is_final: false,
                };
                push_member(&mut te, mref.kind, member);
            }
        } else {
            for mref in &member_inventory {
                let member_html =
                    match fetch_or_cache(&tr.bundle, "page", &mref.page, &cache_dir, refresh) {
                        Ok(h) => h,
                        Err(e) => {
                            failed.push((format!("{}::{}", tr.name, mref.page), e));
                            continue;
                        }
                    };
                for member in parse_dita_member_page(&member_html, mref.kind) {
                    push_member(&mut te, mref.kind, member);
                }
            }
        }

        for members in [
            &mut te.constructors,
            &mut te.methods,
            &mut te.properties,
            &mut te.events,
        ] {
            sort_and_dedup(members);
        }

        index.types.insert(tr.name.clone(), te);
        parsed += 1;
    }

    // Resolve unqualified `inherits`/`implements` names (the DataAdmin class
    // pages render supertypes as plain text, no link) against the owning
    // type's package, when the qualified name exists in the index.
    qualify_unqualified_supertypes(&mut index);

    let out = workspace_root().join(&out_path);
    if let Some(parent) = out.parent() {
        fs::create_dir_all(parent).map_err(|e| e.to_string())?;
    }
    let json = serde_json::to_string_pretty(&index).map_err(|e| e.to_string())?;
    fs::write(&out, &json).map_err(|e| e.to_string())?;

    eprintln!(
        "wrote {} ({} types parsed, {} skipped as non-types, {} failed; {} bytes)",
        out.display(),
        parsed,
        skipped,
        failed.len(),
        json.len()
    );
    for (name, e) in &failed {
        eprintln!("  failed: {name}: {e}");
    }
    Ok(())
}

// =============================================================================
// Fetching and caching
// =============================================================================

fn fetch_or_cache(
    bundle_id: &str,
    section: &str,
    page: &str,
    cache_dir: &Path,
    refresh: bool,
) -> Result<String, String> {
    let dir = cache_dir.join(section);
    let path = dir.join(page);
    if !refresh && path.exists() {
        return fs::read_to_string(&path).map_err(|e| e.to_string());
    }
    let url = format!("{DOCS_HOST}/bundle/{bundle_id}/{section}/{page}");
    let body = http_get(&url)?;
    fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    fs::write(&path, &body).map_err(|e| e.to_string())?;
    thread::sleep(THROTTLE);
    Ok(body)
}

fn http_get(url: &str) -> Result<String, String> {
    let config = ureq::Agent::config_builder()
        .timeout_global(Some(Duration::from_secs(120)))
        .timeout_connect(Some(Duration::from_secs(30)))
        .build();
    let agent: ureq::Agent = config.new_agent();
    let mut response = agent
        .get(url)
        .header("User-Agent", USER_AGENT)
        .call()
        .map_err(|e| format!("{url}: {e}"))?;
    if response.status() != 200 {
        return Err(format!("HTTP {} for {url}", response.status()));
    }
    response
        .body_mut()
        .read_to_string()
        .map_err(|e| format!("{url}: {e}"))
}

fn cache_dir(bundle_id: &str) -> PathBuf {
    let base = std::env::var_os("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|h| PathBuf::from(h).join(".cache")))
        .unwrap_or_else(|| PathBuf::from(".cache"));
    base.join("oxabl").join("progress-docs").join(bundle_id)
}

fn workspace_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .unwrap() // crates/
        .parent()
        .unwrap() // workspace root
        .to_path_buf()
}

// =============================================================================
// TOC → catalog
// =============================================================================

/// Extract every documented type from a `toc/` JSON response. The endpoint
/// returns a JSON object keyed by TOC node id; each value is an HTML fragment
/// of that node's subtree. Each returned pair is the dotted type name and the
/// page file name (relative to the bundle's `page/` directory) it lives at.
/// The `naming` scheme decides which pages are type pages and how their file
/// names map to dotted names. Package pages and the root packages are
/// excluded — the roots are not even present in the JSON and are recovered
/// from type-name prefixes by [`derive_packages`].
fn parse_toc_types(json: &str, naming: PageNaming) -> Result<Vec<(String, String)>, String> {
    let tree: BTreeMap<String, String> =
        serde_json::from_str(json).map_err(|e| format!("malformed TOC JSON: {e}"))?;
    let a_sel = Selector::parse("a[href]").expect("valid selector");
    let mut out = Vec::new();
    let mut seen = HashSet::new();
    for fragment in tree.values() {
        let doc = Html::parse_fragment(fragment);
        for a in doc.select(&a_sel) {
            let Some(href) = a.value().attr("href") else {
                continue;
            };
            if !href.contains("/page/") {
                continue;
            }
            let Some(file) = href
                .rsplit('/')
                .next()
                .and_then(|s| s.strip_suffix(".html"))
            else {
                continue;
            };
            let Some(name) = naming.page_to_name(file) else {
                continue;
            };
            if seen.insert(name.clone()) {
                out.push((name, format!("{file}.html")));
            }
        }
    }
    Ok(out)
}

/// Derive the full package list from type names: every dotted prefix of a
/// type name is a package (`OpenEdge.Core.String` implies `OpenEdge` and
/// `OpenEdge.Core`). This recovers the root packages that the `toc/` JSON
/// omits and needs no second source.
pub fn derive_packages(type_names: &[String]) -> Vec<String> {
    let mut packages = BTreeMap::new();
    for name in type_names {
        let mut prefix = String::new();
        for (idx, segment) in name.split('.').enumerate() {
            if idx == 0 {
                prefix = segment.to_string();
                packages.insert(prefix.clone(), ());
                continue;
            }
            prefix.push('.');
            prefix.push_str(segment);
            packages.insert(prefix.clone(), ());
        }
        // The final dotted name is the type itself; drop it from the package
        // set (its own dotted prefix path is already recorded).
    }
    // Remove the type names themselves (they are not packages). Each type name
    // was inserted as the final prefix; remove any entry that is itself a type.
    let type_set: HashSet<&str> = type_names.iter().map(|s| s.as_str()).collect();
    let mut result: Vec<String> = packages
        .keys()
        .filter(|p| !type_set.contains(p.as_str()))
        .cloned()
        .collect();
    result.sort();
    result
}

// =============================================================================
// Type page → TypeEntry
// =============================================================================

/// Parse a type reference page. Returns `None` when the page is not a type
/// page (no `Type:` header row), so callers can skip it.
pub fn parse_type_page(html: &str, dotted_name: &str) -> Option<TypeEntry> {
    let doc = Html::parse_document(html);
    let header_sel = Selector::parse("div.unit_header table tr").ok()?;
    let td_sel = Selector::parse("td").ok()?;

    let mut declared_kind: Option<TypeKind> = None;
    let mut is_abstract = false;
    let mut is_final = false;
    let mut inherits: Option<String> = None;
    let mut implements: Vec<String> = Vec::new();

    for row in doc.select(&header_sel) {
        let tds: Vec<ElementRef> = row.select(&td_sel).collect();
        if tds.is_empty() {
            continue;
        }
        let label = tds[0].text().collect::<String>();
        let label = label.trim();
        if let Some(rest) = label.strip_prefix("Type:") {
            declared_kind = match rest.trim() {
                "Interface" => Some(TypeKind::Interface),
                "Class" => Some(TypeKind::Class),
                _ => None,
            };
            let (a, _, f) = parse_modifiers(&tds[0]);
            is_abstract = a;
            is_final = f;
            continue;
        }
        if tds.len() < 2 {
            continue;
        }
        match label {
            "Inherits:" => {
                inherits = non_empty(tds[1].text().collect::<String>());
            }
            "Implements:" => {
                implements = tds[1]
                    .text()
                    .collect::<String>()
                    .split(',')
                    .map(str::trim)
                    .filter(|s| !s.is_empty())
                    .map(str::to_string)
                    .collect();
            }
            _ => {}
        }
    }

    // A page with no `Type:` row is not a type page.
    let declared_kind = declared_kind?;

    let (package, _) = dotted_name.rsplit_once('.').unwrap_or(("", dotted_name));

    // Enum pages declare `Type: Class` but inherit `Progress.Lang.Enum`; that
    // inheritance is the only reliable enum signal (their pages list no enum
    // constants to scrape).
    let kind = match declared_kind {
        TypeKind::Interface => TypeKind::Interface,
        _ if inherits.as_deref() == Some("Progress.Lang.Enum") => TypeKind::Enum,
        _ => TypeKind::Class,
    };

    let mut constructors = parse_member_table(&doc, "table.constructor_detail");
    let mut methods = parse_member_table(&doc, "table.method_detail");
    let mut properties = parse_member_table(&doc, "table.property_detail");
    let mut events = parse_member_table(&doc, "table.event_detail");
    for members in [
        &mut constructors,
        &mut methods,
        &mut properties,
        &mut events,
    ] {
        sort_and_dedup(members);
    }

    Some(TypeEntry {
        kind,
        package: package.to_string(),
        page: format!("{dotted_name}.html"),
        inherits,
        implements,
        is_abstract,
        is_final,
        constructors,
        methods,
        properties,
        events,
        enum_members: Vec::new(),
    })
}

/// Sort a member list by name and drop exact duplicates (the docs emit some
/// byte-identical overload entries).
fn sort_and_dedup(members: &mut Vec<Member>) {
    members.sort_by(|a, b| a.name.cmp(&b.name));
    members.dedup();
}

fn push_member(te: &mut TypeEntry, kind: MemberKind, member: Member) {
    match kind {
        MemberKind::Method => te.methods.push(member),
        MemberKind::Property => te.properties.push(member),
        MemberKind::Event => te.events.push(member),
    }
}

/// Parse one member category's detail table (`table.{method,constructor,property,event}_detail`).
///
/// Detail tables are the authoritative source: they carry fully-qualified
/// parameter types (as links), parameter names, the `Returns` row, and the
/// STATIC/ABSTRACT/FINAL modifier flags. The summary tables are not read.
fn parse_member_table(doc: &Html, table_selector: &str) -> Vec<Member> {
    let Ok(table_sel) = Selector::parse(table_selector) else {
        return Vec::new();
    };
    let row_sel = Selector::parse("tr.table_content").expect("valid selector");
    let th_sel = Selector::parse("th").expect("valid selector");
    let h4_sel = Selector::parse("h4").expect("valid selector");
    let a_sel = Selector::parse("a").expect("valid selector");

    let mut out = Vec::new();
    for table in doc.select(&table_sel) {
        for row in table.select(&row_sel) {
            let Some(th) = row.select(&th_sel).next() else {
                continue;
            };
            let Some(h4) = th.select(&h4_sel).next() else {
                continue;
            };

            // The member anchor is the last `<a href="#...">` in the header.
            // Class-typed return values precede it as an ordinary link whose
            // href names the type page.
            let mut name_anchor: Option<ElementRef> = None;
            for a in h4.select(&a_sel) {
                if a.value().attr("href").is_some_and(|h| h.starts_with('#')) {
                    name_anchor = Some(a);
                }
            }
            let h4_text = h4.text().collect::<String>();
            let h4_text = h4_text.trim();
            let (name_anchor_text, has_anchor) = match name_anchor {
                Some(a) => (a.text().collect::<String>(), true),
                // Constructors are sometimes rendered without an anchor; the
                // whole `h4` is then the signature.
                None => (h4_text.to_string(), false),
            };
            let anchor_trimmed = name_anchor_text.trim();
            let (name, short_params) = parse_signature_anchor(anchor_trimmed);

            let return_type = if has_anchor {
                h4_text
                    .strip_suffix(anchor_trimmed)
                    .map(|prefix| prefix.trim().to_string())
                    .filter(|p| !p.is_empty())
            } else {
                None
            };

            let (is_static, is_abstract, is_final) = parse_modifiers(&th);
            let (mut params, returns) = parse_params(&th);
            if !short_params.is_empty() {
                if params.is_empty() {
                    // No structured Parameters table (rare) — fall back to the
                    // signature's short type names, unqualified.
                    params = short_params
                        .into_iter()
                        .map(|type_name| Param {
                            name: None,
                            type_name,
                        })
                        .collect();
                } else if short_params.len() == params.len() {
                    // The signature's short parameter list carries array-extent
                    // markers (`character[]`) that the structured Parameters
                    // table drops. Propagate them so scalar and extent
                    // overloads (`IsEmpty (character)` vs `IsEmpty (character[])`)
                    // stay distinct instead of collapsing into false duplicates.
                    for (short, param) in short_params.iter().zip(params.iter_mut()) {
                        if short.trim_end().ends_with("[]") && !param.type_name.ends_with("[]") {
                            param.type_name.push_str("[]");
                        }
                    }
                }
            }
            let return_type = return_type.or(returns);

            out.push(Member {
                name,
                return_type,
                params,
                is_static,
                is_abstract,
                is_final,
            });
        }
    }
    out
}

/// Read STATIC/ABSTRACT/FINAL flags from a member block. The modifier spans
/// render as `class="opt_"` (or `class="opt_STATIC"` etc.) with the modifier
/// in the `title` attribute when set.
fn parse_modifiers(container: &ElementRef) -> (bool, bool, bool) {
    let Ok(sel) = Selector::parse("span[class^=\"opt_\"]") else {
        return (false, false, false);
    };
    let mut is_static = false;
    let mut is_abstract = false;
    let mut is_final = false;
    for span in container.select(&sel) {
        let title = span.value().attr("title").unwrap_or("");
        let class = span.value().attr("class").unwrap_or("");
        if title == "STATIC" || class.contains("STATIC") {
            is_static = true;
        } else if title == "ABSTRACT" || class.contains("ABSTRACT") {
            is_abstract = true;
        } else if title == "FINAL" || class.contains("FINAL") {
            is_final = true;
        }
    }
    (is_static, is_abstract, is_final)
}

/// Read a member's parameters and `Returns` row from its detail block. Both
/// live in a nested `<table>` whose rows pair a `<b>` label with a value:
/// parameter rows are `<i><b>name</b></i> / type`, and the return row is
/// `<i><b>Returns</b></i> / type`. Description rows (no `<b>`) are skipped.
fn parse_params(th: &ElementRef) -> (Vec<Param>, Option<String>) {
    let row_sel = Selector::parse("tr").expect("valid selector");
    let td_sel = Selector::parse("td").expect("valid selector");
    let b_sel = Selector::parse("b").expect("valid selector");

    let mut params = Vec::new();
    let mut returns = None;
    for row in th.select(&row_sel) {
        let tds: Vec<ElementRef> = row.select(&td_sel).collect();
        if tds.len() != 2 {
            continue;
        }
        let Some(b) = tds[0].select(&b_sel).next() else {
            continue;
        };
        let label = b.text().collect::<String>();
        let label = label.trim();
        if label.is_empty() || label == "Parameters:" {
            continue;
        }
        let type_name = tds[1].text().collect::<String>().trim().to_string();
        if label == "Returns" {
            returns = non_empty(type_name);
        } else {
            params.push(Param {
                name: Some(label.to_string()),
                type_name,
            });
        }
    }
    (params, returns)
}

/// Split a signature like `CompareTo (ILongcharHolder)` into its name and the
/// (unqualified, short-form) parameter type names.
fn parse_signature_anchor(signature: &str) -> (String, Vec<String>) {
    let signature = signature.trim();
    if let Some(open) = signature.find('(')
        && signature.ends_with(')')
    {
        let name = signature[..open].trim().to_string();
        let inside = &signature[open + 1..signature.len() - 1];
        let params = inside
            .split(',')
            .map(str::trim)
            .filter(|p| !p.is_empty())
            .map(str::to_string)
            .collect();
        return (name, params);
    }
    (signature.to_string(), Vec::new())
}

fn non_empty(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}

fn is_false(b: &bool) -> bool {
    !*b
}

fn version_from_bundle(bundle_id: &str) -> String {
    let digits: String = bundle_id
        .chars()
        .rev()
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>()
        .chars()
        .rev()
        .collect();
    match digits.len() {
        0 | 1 => digits,
        n => format!("{}.{}", &digits[..n - 1], &digits[n - 1..]),
    }
}

/// Current time as an RFC 3339 UTC timestamp, computed without a date
/// dependency (civil-from-days conversion).
fn now_rfc3339() -> String {
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let days = (secs / 86_400) as i64;
    let rem = secs % 86_400;
    let (hour, minute, second) = (rem / 3600, (rem % 3600) / 60, rem % 60);

    let z = days + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let year = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let month = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    let year = if month <= 2 { year + 1 } else { year } as u32;
    format!("{year:04}-{month:02}-{day:02}T{hour:02}:{minute:02}:{second:02}Z")
}

fn version_from_bundles(bundles: &[String]) -> String {
    for bundle in bundles {
        let v = version_from_bundle(bundle);
        if !v.is_empty() {
            return v;
        }
    }
    String::new()
}

/// Resolve unqualified `inherits`/`implements` names against the owning type's
/// package. The DataAdmin class pages render supertypes as plain text without
/// a link, so the index would otherwise carry names like `DataAdminCollection`
/// instead of `OpenEdge.DataAdmin.DataAdminCollection`. Qualification only
/// applies when the qualified name actually exists in the index.
fn qualify_unqualified_supertypes(index: &mut BuiltinIndex) {
    let mut updates: Vec<(String, Option<String>, Vec<String>)> = Vec::new();
    for (name, te) in &index.types {
        let (package, _) = name.rsplit_once('.').unwrap_or(("", name));
        let qualified_inherits = te.inherits.as_deref().and_then(|s| {
            (!s.contains('.'))
                .then(|| format!("{package}.{s}"))
                .filter(|q| index.types.contains_key(q))
        });
        let qualified_implements = te
            .implements
            .iter()
            .map(|s| {
                if s.contains('.') {
                    s.clone()
                } else {
                    let q = format!("{package}.{s}");
                    if index.types.contains_key(&q) {
                        q
                    } else {
                        s.clone()
                    }
                }
            })
            .collect::<Vec<_>>();
        if qualified_inherits.is_some() || qualified_implements != te.implements {
            updates.push((name.clone(), qualified_inherits, qualified_implements));
        }
    }
    for (name, inherits, implements) in updates {
        if let Some(te) = index.types.get_mut(&name) {
            if let Some(inherits) = inherits {
                te.inherits = Some(inherits);
            }
            te.implements = implements;
        }
    }
}

// =============================================================================
// DITA ("ABL Reference") renderer — `Progress.*` type and member pages
// =============================================================================

/// Decode the `lt`/`gt` generic encoding used in `abl-reference` page file
/// names: `Progress.Collections.IListltTgt` → `Progress.Collections.IList<T>`.
fn decode_generics(name: &str) -> String {
    name.replace("lt", "<").replace("gt", ">")
}

/// Parse a `Progress.*` type page (the DITA renderer). The page carries the
/// supertype, implemented interfaces, constructor signatures, enum members,
/// and a *name-only* inventory of public members — the full signatures live on
/// separate `*-method*` / `*-property*` / `*-event*` pages returned as
/// [`DitaMemberRef`]s.
fn parse_dita_class_page(
    html: &str,
    name: &str,
    page: &str,
) -> Option<(TypeEntry, Vec<DitaMemberRef>)> {
    let doc = Html::parse_document(html);
    let article_sel = Selector::parse("article.dita").ok()?;
    let article = doc.select(&article_sel).next()?;
    let section_sel = Selector::parse("section.section").ok()?;
    let h2_sel = Selector::parse("h2.title.sectiontitle").ok()?;
    let xref_sel = Selector::parse("a.xref[href]").ok()?;
    let code_sel = Selector::parse("code").ok()?;
    let pre_sel = Selector::parse("pre.pre.codeblock").ok()?;

    let kind = if page.ends_with("-interface.html") {
        TypeKind::Interface
    } else if page.ends_with("-enumeration.html") {
        TypeKind::Enum
    } else {
        TypeKind::Class
    };
    let (package, _) = name.rsplit_once('.').unwrap_or(("", name));

    let mut inherits: Option<String> = None;
    let mut implements: Vec<String> = Vec::new();
    let mut constructors: Vec<Member> = Vec::new();
    let mut enum_members: Vec<String> = Vec::new();
    let mut inventory: Vec<DitaMemberRef> = Vec::new();

    for section in article.select(&section_sel) {
        let heading = section
            .select(&h2_sel)
            .next()
            .map(|h| h.text().collect::<String>())
            .unwrap_or_default();
        let heading = heading.trim();
        match heading {
            "Super Class" | "Super Interface" => {
                inherits = dita_supertype(&section, heading);
            }
            "Interfaces" => {
                implements = section
                    .select(&xref_sel)
                    .filter_map(|a| dita_href_to_name(&a))
                    .collect();
            }
            "Constructors" => {
                for pre in section.select(&pre_sel) {
                    let sig = pre.text().collect::<String>();
                    let (ctor_name, params) = parse_abl_signature(&sig);
                    constructors.push(Member {
                        name: ctor_name,
                        return_type: None,
                        params,
                        is_static: false,
                        is_abstract: false,
                        is_final: false,
                    });
                }
            }
            "Public Methods" | "Public Properties" | "Public Events" => {
                let mkind = match heading {
                    "Public Methods" => MemberKind::Method,
                    "Public Properties" => MemberKind::Property,
                    _ => MemberKind::Event,
                };
                for a in section.select(&xref_sel) {
                    let Some(href) = a.value().attr("href") else {
                        continue;
                    };
                    let file = href.split('#').next().unwrap_or(href);
                    if !file.ends_with(".html") {
                        continue;
                    }
                    inventory.push(DitaMemberRef {
                        kind: mkind,
                        page: file.to_string(),
                        name: member_name_from_link_text(&a.text().collect::<String>()),
                    });
                }
            }
            "Members" if kind == TypeKind::Enum => {
                let tr_sel = Selector::parse("tr").expect("valid selector");
                let td_sel = Selector::parse("td").expect("valid selector");
                for row in section.select(&tr_sel) {
                    // Enum constant names are in the first cell; the
                    // description column also contains <code> elements.
                    let Some(first_td) = row.select(&td_sel).next() else {
                        continue;
                    };
                    let Some(code) = first_td.select(&code_sel).next() else {
                        continue;
                    };
                    let t = code.text().collect::<String>();
                    let t = t.trim();
                    if !t.is_empty() {
                        enum_members.push(t.to_string());
                    }
                }
            }
            _ => {}
        }
    }

    let te = TypeEntry {
        kind,
        package: package.to_string(),
        page: page.to_string(),
        inherits,
        implements,
        is_abstract: false,
        is_final: false,
        constructors,
        methods: Vec::new(),
        properties: Vec::new(),
        events: Vec::new(),
        enum_members,
    };
    Some((te, inventory))
}
/// Strip a trailing type-kind word and provenance from a rendered type name:
/// `Progress.Lang.Enum class` → `Progress.Lang.Enum`,
/// `System.Object class from .NET` → `System.Object`.
fn strip_dita_type_word(val: &str) -> String {
    let val = val.split_whitespace().collect::<Vec<_>>().join(" ");
    let val = val.trim();
    for suffix in [
        " class from .NET",
        " interface from .NET",
        " class",
        " interface",
        " from .NET",
    ] {
        if let Some(stem) = val.strip_suffix(suffix) {
            let stem = stem.trim();
            if !stem.is_empty() {
                return stem.to_string();
            }
        }
    }
    val.to_string()
}

/// Supertype from a `Super Class` / `Super Interface` section. Prefers an
/// explicit xref link (its href carries the exact dotted name); falls back to
/// the section text with the heading and the trailing ` class`/` interface`
/// word stripped. `None` for the "does not inherit" prose stubs and a literal
/// `None`.
fn dita_supertype(section: &ElementRef, heading: &str) -> Option<String> {
    let xref_sel = Selector::parse("a.xref[href]").expect("valid selector");
    for a in section.select(&xref_sel) {
        if let Some(n) = dita_href_to_name(&a) {
            return Some(n);
        }
    }
    let text = section.text().collect::<String>();
    let text = text.replace(heading, "");
    let text = strip_dita_type_word(&text);
    if text.is_empty()
        || text.eq_ignore_ascii_case("None")
        || text.contains("does not inherit")
        || text.contains("does not implement")
    {
        None
    } else {
        Some(text)
    }
}

/// Dotted type name from an `abl-reference` xref href, e.g.
/// `Progress.Lang.Object-class.html#...` → `Progress.Lang.Object`.
fn dita_href_to_name(a: &ElementRef) -> Option<String> {
    let href = a.value().attr("href")?;
    let file = href.split('#').next()?;
    let stem = file.strip_suffix(".html")?;
    let dotted = stem
        .strip_suffix("-class")
        .or_else(|| stem.strip_suffix("-interface"))
        .or_else(|| stem.strip_suffix("-enumeration"))?;
    Some(decode_generics(dotted))
}

/// Member name from a class-page link text: `Clone( ) method (Progress.Lang.Object)`
/// → `Clone`, `NEXT-SIBLING property` → `NEXT-SIBLING`.
fn member_name_from_link_text(text: &str) -> String {
    let t = text.trim();
    // Strip a trailing ` (Disambiguator)` group, if present.
    let t = if t.ends_with(')') {
        match t.rfind(" (") {
            Some(idx) if t.ends_with(')') => t[..idx].trim_end().to_string(),
            _ => t.to_string(),
        }
    } else {
        t.to_string()
    };
    let t = [" method", " property", " event"]
        .iter()
        .find_map(|suffix| t.strip_suffix(suffix))
        .unwrap_or(&t);
    t.split('(').next().unwrap_or(t).trim().to_string()
}

/// Parse a `Progress.*` member page (method/property/event). Returns one
/// [`Member`] per signature: some pages (e.g. `JsonArray:Add`) concatenate
/// every overload into a single `Syntax` codeblock, so a page can yield many
/// members sharing its return type. If the page yields no signature
/// (properties carry none), the name falls back to the page `<h1>`.
fn parse_dita_member_page(html: &str, kind: MemberKind) -> Vec<Member> {
    let doc = Html::parse_document(html);
    let article_sel = Selector::parse("article.dita").expect("valid selector");
    let p_sel = Selector::parse("p.p").expect("valid selector");
    let pre_sel = Selector::parse("pre.pre.codeblock").expect("valid selector");
    let h1_sel = Selector::parse("h1").expect("valid selector");

    let mut return_type: Option<String> = None;
    let mut is_static = false;
    let mut signatures: Vec<(String, Vec<Param>)> = Vec::new();

    if let Some(article) = doc.select(&article_sel).next() {
        if let Some(pre) = article.select(&pre_sel).next() {
            let code = pre.text().collect::<String>();
            for sig in split_abl_signatures(&code) {
                signatures.push(parse_abl_signature(&sig));
            }
        }
        let label = match kind {
            MemberKind::Property => "Data type",
            _ => "Return type",
        };
        for p in article.select(&p_sel) {
            let text = p.text().collect::<String>();
            if let Some(rest) = strip_label_prefix(&text, label) {
                let val = strip_dita_type_word(rest);
                if !val.is_empty() {
                    return_type = Some(val);
                }
            }
            if let Some(rest) = strip_label_prefix(&text, "Access")
                && rest.contains("STATIC")
            {
                is_static = true;
            }
        }
        if signatures.is_empty()
            && let Some(h1) = article.select(&h1_sel).next()
        {
            signatures.push((
                member_name_from_link_text(&h1.text().collect::<String>()),
                Vec::new(),
            ));
        }
    }

    signatures
        .into_iter()
        .map(|(name, params)| Member {
            name,
            return_type: return_type.clone(),
            params,
            is_static,
            is_abstract: false,
            is_final: false,
        })
        .collect()
}

/// Split a member-page `Syntax` codeblock into individual signature strings.
/// Some pages list every overload of a method in one codeblock, e.g.
/// `Add ( INPUT value AS DATE ) Add ( INPUT value AS DATETIME ) ...` — each
/// new overload starts with `<name> (` after a `)`, so signatures are split at
/// those boundaries.
fn split_abl_signatures(code: &str) -> Vec<String> {
    let collapsed = code.split_whitespace().collect::<Vec<_>>().join(" ");
    let bytes = collapsed.as_bytes();
    let mut starts = vec![0usize];
    let mut i = 0usize;
    while i + 1 < bytes.len() {
        if bytes[i] != b')' || (i + 1 < bytes.len() && bytes[i + 1] != b' ') {
            i += 1;
            continue;
        }
        let mut j = i + 1;
        while j < bytes.len() && bytes[j] == b' ' {
            j += 1;
        }
        let ident_start = j;
        while j < bytes.len()
            && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'-' || bytes[j] == b'_')
        {
            j += 1;
        }
        let mut k = j;
        while k < bytes.len() && bytes[k] == b' ' {
            k += 1;
        }
        if k < bytes.len() && bytes[k] == b'(' && j > ident_start {
            starts.push(ident_start);
            i = k + 1;
            continue;
        }
        i += 1;
    }
    starts.sort_unstable();
    starts.dedup();
    starts
        .iter()
        .enumerate()
        .map(|(idx, &s)| {
            let end = starts.get(idx + 1).copied().unwrap_or(collapsed.len());
            collapsed[s..end].trim().to_string()
        })
        .collect()
}

/// Strip a label like `Return type:` from the start of a paragraph, matching
/// whitespace-insensitively: some pages render the label split across a
/// `<strong>` boundary (`<strong>Return</strong> type:`) so `text()` yields
/// `Returntype:`. Returns the remainder of the text, or `None` when the
/// paragraph does not start with that label.
fn strip_label_prefix<'a>(text: &'a str, label: &str) -> Option<&'a str> {
    let compact = label.split_whitespace().collect::<String>() + ":";
    let needle = compact.to_ascii_lowercase();
    let bytes = text.as_bytes();
    let mut ti = 0;
    let mut ni = 0;
    while ni < needle.len() {
        while ti < bytes.len() && bytes[ti].is_ascii_whitespace() {
            ti += 1;
        }
        if ti >= bytes.len() {
            return None;
        }
        if bytes[ti].to_ascii_lowercase() != needle.as_bytes()[ni] {
            return None;
        }
        ti += 1;
        ni += 1;
    }
    Some(&text[ti..])
}

/// Parse an ABL member signature from a docs codeblock, e.g.
/// `Equals ( INPUT OtherObj AS Progress.Lang.Object )` or
/// `PUBLIC Object ( )`. Strips a leading access modifier, returns the member
/// name and its parameters (mode, when present, is not stored — the data model
/// treats every built-in parameter as INPUT).
fn parse_abl_signature(signature: &str) -> (String, Vec<Param>) {
    // Collapse all whitespace (NBSP `\xa0`, line breaks, run-on spaces) into
    // single spaces — the docs wrap long signatures across lines.
    let normalized = signature.split_whitespace().collect::<Vec<_>>().join(" ");
    let mut sig = normalized.trim();
    for access in ["PUBLIC ", "PROTECTED ", "PRIVATE "] {
        if let Some(rest) = sig.strip_prefix(access) {
            sig = rest.trim_start();
            break;
        }
    }
    let Some(open) = sig.find('(') else {
        return (sig.trim().to_string(), Vec::new());
    };
    let name = sig[..open].trim().to_string();
    let inside = &sig[open + 1..];
    let Some(close) = inside.rfind(')') else {
        return (name, Vec::new());
    };
    let inside = &inside[..close];
    let params = split_abl_params(inside)
        .into_iter()
        .filter_map(parse_param)
        .collect();
    (name, params)
}

/// Split a parameter list on top-level commas, falling back to splitting on
/// the `INPUT`/`OUTPUT`/`INPUT-OUTPUT` mode keywords that begin each parameter
/// (some signatures render parameters across lines without commas).
fn split_abl_params(inside: &str) -> Vec<&str> {
    let comma_parts = split_top_level_commas(inside);
    if comma_parts.len() > 1 {
        return comma_parts;
    }
    const MODES: [&str; 3] = ["INPUT-OUTPUT", "OUTPUT", "INPUT"];
    let bytes = inside.as_bytes();
    let mut starts: Vec<usize> = vec![0];
    let mut search_from = 1;
    loop {
        let mut best: Option<usize> = None;
        for m in MODES {
            let needle = m.as_bytes();
            let mut j = search_from;
            while j + needle.len() <= bytes.len() {
                if bytes[j..j + needle.len()].eq_ignore_ascii_case(needle) {
                    let before_ok = j == 0 || bytes[j - 1].is_ascii_whitespace();
                    let after_ok = bytes
                        .get(j + needle.len())
                        .is_none_or(|b| b.is_ascii_whitespace());
                    if before_ok && after_ok {
                        best = Some(match best {
                            Some(b) => b.min(j),
                            None => j,
                        });
                    }
                    break;
                }
                j += 1;
            }
        }
        let Some(j) = best else {
            break;
        };
        starts.push(j);
        search_from = j + 1;
    }
    starts.sort_unstable();
    starts.dedup();
    let mut out = Vec::new();
    for w in starts.windows(2) {
        out.push(&inside[w[0]..w[1]]);
    }
    out.push(&inside[starts[starts.len() - 1]..]);
    out
}

/// Split a parameter list on commas that are not inside `<...>` or `(...)`
/// (generic type arguments like `IMap<K,V>` contain a comma).
fn split_top_level_commas(s: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let mut start = 0usize;
    let mut angle = 0i32;
    let mut paren = 0i32;
    for (i, ch) in s.char_indices() {
        match ch {
            '<' => angle += 1,
            '>' => angle -= 1,
            '(' => paren += 1,
            ')' => paren -= 1,
            ',' if angle == 0 && paren == 0 => {
                out.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    out.push(&s[start..]);
    out
}

/// Parse one parameter of the form `[INPUT|OUTPUT|INPUT-OUTPUT] name AS type`.
/// The mode, when present, is discarded (the data model assumes INPUT).
fn parse_param(raw: &str) -> Option<Param> {
    let mut p = raw.trim();
    if p.is_empty() {
        return None;
    }
    for mode in ["INPUT-OUTPUT ", "OUTPUT ", "INPUT "] {
        if let Some(rest) = p.strip_prefix(mode) {
            p = rest.trim_start();
            break;
        }
    }
    if let Some(idx) = find_ascii_case_insensitive(p, " as ") {
        let name = p[..idx].trim();
        let type_name = normalize_extent(&p[idx + 4..]);
        if !name.is_empty() && !type_name.is_empty() {
            return Some(Param {
                name: Some(name.to_string()),
                type_name,
            });
        }
    }
    // Fallback: no `AS` — treat the last token as the type.
    let tokens: Vec<&str> = p.split_whitespace().collect();
    if tokens.len() >= 2 {
        Some(Param {
            name: Some(tokens[..tokens.len() - 1].join(" ")),
            type_name: normalize_extent(tokens[tokens.len() - 1]),
        })
    } else {
        Some(Param {
            name: None,
            type_name: normalize_extent(p),
        })
    }
}

/// Normalize an array-extent marker to the `[]` form the index uses:
/// `CHARACTER EXTENT` and `CHARACTER EXTENT 10` both become `CHARACTER[]`
/// (the API-reference bundle spells extents as `character[]`).
fn normalize_extent(type_name: &str) -> String {
    let type_name = type_name.trim();
    if let Some(idx) = type_name.to_ascii_lowercase().find(" extent") {
        let (head, _) = type_name.split_at(idx);
        if !head.trim().is_empty() {
            return format!("{}[]", head.trim());
        }
    }
    type_name.to_string()
}

fn find_ascii_case_insensitive(haystack: &str, needle: &str) -> Option<usize> {
    let hay = haystack.to_ascii_lowercase();
    let ned = needle.to_ascii_lowercase();
    hay.find(&ned)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    const TOC_JSON: &str = r#"{
  "nav-1": "<ul class=\"list-links\"><li class=\"toc has-dd\" id=\"nav-2\"><div class=\"inner\"><a href=\"https://docs-be.progress.com/bundle/openedge-abl-api-reference-128/page/Ccs.BusinessLogic.package.html\">BusinessLogic</a></div></li><li class=\"toc has-dd\" id=\"nav-3\"><div class=\"inner\"><a href=\"https://docs-be.progress.com/bundle/openedge-abl-api-reference-128/page/Ccs.BusinessLogic.CommitScopeEnum.html\">CommitScopeEnum</a></div></li></ul>",
  "nav-2": "<ul class=\"list-links\"><li class=\"toc\"><div class=\"inner\"><a href=\"https://docs-be.progress.com/bundle/openedge-abl-api-reference-128/page/OpenEdge.Core.String.html\">String</a></div></li><li class=\"toc\"><div class=\"inner\"><a href=\"https://docs-be.progress.com/bundle/openedge-abl-api-reference-128/page/OpenEdge.Core.Collections.List.html\">List</a></div></li></ul>"
}"#;

    const CLASS_HTML: &str = r##"<html><body><article class="misc-html">
<table class="table_content" width="100%"><tbody><tr><td>
<div class="unit_header">
  <table width="100%">
    <tbody>
      <tr><td><span class="span_namespace"><b>Namespace:</b></span></td>
          <td><a href="OpenEdge.Core.package.html">OpenEdge.Core</a></td></tr>
      <tr><td><span class="span_pagetype"><b>Type:</b> Class
              <span class="opt_" data-initial="A" title=""></span>
              <span class="opt_" data-initial="F" title=""></span></span></td>
          <td><span class="span_classname"><font size="+2">Widget</font></span></td></tr>
      <tr><td><span class="span_parentclass"><b>Parent Classes:</b></span></td>
          <td class="span_parentclass"><div class="parentclasses"><div class="parentclassesitem"><table><tbody><tr><td>Progress.Lang.Object</td></tr></tbody></table></div><div class="parentclassesitem"><table><tbody><tr><td><div class="parentclassnode"></div></td><td><a href="OpenEdge.Core.Widget.html">OpenEdge.Core.Widget</a></td></tr></tbody></table></div></div><br /></td></tr>
      <tr><td><span class="span_inherits"><b>Inherits:</b></span></td>
          <td class="unit_inherits"><span class="span_inherits">Progress.Lang.Object</span></td></tr>
      <tr><td><span class="span_implements"><b>Implements:</b></span></td>
          <td class="unit_interface"><span class="span_implements"> <a href="OpenEdge.Core.ISupportEncoding.html">OpenEdge.Core.ISupportEncoding</a>, Progress.Collections.IHashable</span></td></tr>
    </tbody>
  </table>
</div>
</td></tr></tbody></table>

<table class="table_content table_content_details method_detail" width="100%">
  <tbody>
    <tr class="table_content">
      <th align="left" colspan="2" valign="top">
        <h4> INTEGER <a href="#a1"> DoThing (CHARACTER, OpenEdge.Core.Other)</a></h4>
        <table><tbody><tr>
          <td valign="center"><span class="opt_STATIC" data-initial="S" title="STATIC"></span></td>
          <td valign="center"><span class="opt_" data-initial="A" title=""></span></td>
          <td valign="center"><span class="opt_" data-initial="F" title=""></span></td>
        </tr></tbody></table>
        <div><dl><dd><dl>
          <table width="100%"><tbody>
            <tr><td colspan="2"><b>Parameters:</b></td></tr>
            <tr><td><i><b>pInput</b></i></td><td><a href="OpenEdge.Core.Other.html">OpenEdge.Core.Other</a></td></tr>
            <tr><td>&nbsp;</td><td>a description</td></tr>
            <tr><td><i><b>pName</b></i></td><td>CHARACTER</td></tr>
            <tr><td>&nbsp;</td><td></td></tr>
            <tr><td><i><b>Returns</b></i></td><td>INTEGER</td></tr>
            <tr><td>&nbsp;</td><td>result docs</td></tr>
          </tbody></table>
        </dl></dd></dl></div>
      </th>
    </tr>
    <tr class="table_content">
      <th align="left" colspan="2" valign="top">
        <h4> <a href="#a2"> Empty ()</a></h4>
        <table><tbody><tr>
          <td valign="center"><span class="opt_" data-initial="S" title=""></span></td>
          <td valign="center"><span class="opt_" data-initial="A" title=""></span></td>
          <td valign="center"><span class="opt_" data-initial="F" title=""></span></td>
        </tr></tbody></table>
        <div><dl><dd><dl><table width="100%"><tbody></tbody></table></dl></dd></dl></div>
      </th>
    </tr>
  </tbody>
</table>

<table class="table_content table_content_details property_detail" width="100%">
  <tbody>
    <tr class="table_content">
      <th align="left" colspan="2" valign="top">
        <h4><a href="OpenEdge.Core.Other.html">OpenEdge.Core.Other</a> <a href="#p1"> Count</a></h4>
        <div><dl><dd><dl><table width="100%"><tbody>
          <tr><td><i><b>Returns</b></i></td><td>OpenEdge.Core.Other</td></tr>
        </tbody></table></dl></dd></dl></div>
      </th>
    </tr>
  </tbody>
</table>

<table class="table_content table_content_details constructor_detail" width="100%">
  <tbody>
    <tr class="table_content">
      <th align="left" colspan="2" valign="top">
        <h4> Widget (CHARACTER)</h4>
        <div><dl><dd><dl><table width="100%"><tbody>
          <tr><td><i><b>pInit</b></i></td><td>CHARACTER</td></tr>
          <tr><td>&nbsp;</td><td></td></tr>
        </tbody></table></dl></dd></dl></div>
      </th>
    </tr>
  </tbody>
</table>
</article></body></html>"##;

    #[test]
    fn toc_parses_types_and_packages() {
        let pairs = parse_toc_types(TOC_JSON, PageNaming::Plain).expect("parses");
        let types: Vec<String> = pairs.into_iter().map(|(name, _)| name).collect();
        assert_eq!(
            types,
            vec![
                "Ccs.BusinessLogic.CommitScopeEnum".to_string(),
                "OpenEdge.Core.String".to_string(),
                "OpenEdge.Core.Collections.List".to_string()
            ]
        );
        let packages = derive_packages(&types);
        assert_eq!(
            packages,
            vec![
                "Ccs".to_string(),
                "Ccs.BusinessLogic".to_string(),
                "OpenEdge".to_string(),
                "OpenEdge.Core".to_string(),
                "OpenEdge.Core.Collections".to_string()
            ]
        );
    }

    #[test]
    fn class_page_parses_header() {
        let te = parse_type_page(CLASS_HTML, "OpenEdge.Core.Widget").expect("parses");
        assert_eq!(te.kind, TypeKind::Class);
        assert_eq!(te.package, "OpenEdge.Core");
        assert_eq!(te.page, "OpenEdge.Core.Widget.html");
        assert_eq!(te.inherits.as_deref(), Some("Progress.Lang.Object"));
        assert_eq!(
            te.implements,
            vec![
                "OpenEdge.Core.ISupportEncoding".to_string(),
                "Progress.Collections.IHashable".to_string()
            ]
        );
        assert!(!te.is_abstract);
        assert!(!te.is_final);
    }

    #[test]
    fn class_page_parses_methods() {
        let te = parse_type_page(CLASS_HTML, "OpenEdge.Core.Widget").expect("parses");
        assert_eq!(te.methods.len(), 2);

        let dothing = &te.methods[0];
        assert_eq!(dothing.name, "DoThing");
        assert_eq!(dothing.return_type.as_deref(), Some("INTEGER"));
        assert!(dothing.is_static);
        assert!(!dothing.is_abstract);
        assert!(!dothing.is_final);
        assert_eq!(dothing.params.len(), 2);
        assert_eq!(dothing.params[0].name.as_deref(), Some("pInput"));
        assert_eq!(dothing.params[0].type_name, "OpenEdge.Core.Other");
        assert_eq!(dothing.params[1].name.as_deref(), Some("pName"));
        assert_eq!(dothing.params[1].type_name, "CHARACTER");

        let empty = &te.methods[1];
        assert_eq!(empty.name, "Empty");
        assert_eq!(empty.return_type, None);
        assert!(empty.params.is_empty());
    }

    #[test]
    fn class_page_parses_property_and_constructor() {
        let te = parse_type_page(CLASS_HTML, "OpenEdge.Core.Widget").expect("parses");
        assert_eq!(te.properties.len(), 1);
        assert_eq!(te.properties[0].name, "Count");
        assert_eq!(
            te.properties[0].return_type.as_deref(),
            Some("OpenEdge.Core.Other")
        );
        assert!(te.properties[0].params.is_empty());

        assert_eq!(te.constructors.len(), 1);
        assert_eq!(te.constructors[0].name, "Widget");
        assert_eq!(te.constructors[0].return_type, None);
        assert_eq!(te.constructors[0].params[0].name.as_deref(), Some("pInit"));
        assert_eq!(te.constructors[0].params[0].type_name, "CHARACTER");
    }

    #[test]
    fn non_type_page_is_none() {
        let html = "<html><body><article class=\"misc-html\"><p>hello</p></article></body></html>";
        assert!(parse_type_page(html, "OpenEdge.Core.Widget").is_none());
    }

    const INTERFACE_HTML: &str = r#"<html><body><article class="misc-html">
<table class="table_content" width="100%"><tbody><tr><td>
<div class="unit_header">
  <table width="100%">
    <tbody>
      <tr><td><span class="span_pagetype"><b>Type:</b> Interface
              <span class="opt_" data-initial="A" title=""></span>
              <span class="opt_" data-initial="F" title=""></span></span></td>
          <td><span class="span_classname"><font size="+2">IWidget</font></span></td></tr>
      <tr><td><span class="span_inherits"><b>Inherits:</b></span></td>
          <td class="unit_inherits"><span class="span_inherits"><a href="Ccs.Common.IService.html">Ccs.Common.IService</a></span></td></tr>
    </tbody>
  </table>
</div>
</td></tr></tbody></table>
</article></body></html>"#;

    #[test]
    fn interface_page_kind_and_inherits_link() {
        let te = parse_type_page(INTERFACE_HTML, "Ccs.BusinessLogic.IWidget").expect("parses");
        assert_eq!(te.kind, TypeKind::Interface);
        assert_eq!(te.inherits.as_deref(), Some("Ccs.Common.IService"));
        assert!(te.methods.is_empty());
    }

    const ENUM_HTML: &str = r#"<html><body><article class="misc-html">
<table class="table_content" width="100%"><tbody><tr><td>
<div class="unit_header">
  <table width="100%">
    <tbody>
      <tr><td><span class="span_pagetype"><b>Type:</b> Class
              <span class="opt_" data-initial="A" title=""></span>
              <span class="opt_" data-initial="F" title=""></span></span></td>
          <td><span class="span_classname"><font size="+2">StatusEnum</font></span></td></tr>
      <tr><td><span class="span_inherits"><b>Inherits:</b></span></td>
          <td class="unit_inherits"><span class="span_inherits">Progress.Lang.Enum</span></td></tr>
    </tbody>
  </table>
</div>
</td></tr></tbody></table>
</article></body></html>"#;

    #[test]
    fn enum_page_kind_from_progress_lang_enum() {
        let te = parse_type_page(ENUM_HTML, "Ccs.BusinessLogic.StatusEnum").expect("parses");
        assert_eq!(te.kind, TypeKind::Enum);
        assert_eq!(te.inherits.as_deref(), Some("Progress.Lang.Enum"));
    }

    #[test]
    fn version_from_bundle_id() {
        assert_eq!(
            version_from_bundle("openedge-abl-api-reference-128"),
            "12.8"
        );
        assert_eq!(
            version_from_bundle("openedge-abl-api-reference-130"),
            "13.0"
        );
        assert_eq!(
            version_from_bundle("openedge-abl-api-reference-122"),
            "12.2"
        );
    }

    #[test]
    fn dita_naming_maps_suffixed_pages() {
        let naming = PageNaming::DitaSuffixed;
        assert_eq!(
            naming.page_to_name("Progress.Lang.Object-class"),
            Some("Progress.Lang.Object".to_string())
        );
        assert_eq!(
            naming.page_to_name("Progress.Lang.Error-interface"),
            Some("Progress.Lang.Error".to_string())
        );
        assert_eq!(
            naming.page_to_name("Progress.Collections.IListltTgt-interface"),
            Some("Progress.Collections.IList<T>".to_string())
        );
        assert_eq!(
            naming.page_to_name("Progress.Archive.SignaturePolicyIDs-enumeration"),
            Some("Progress.Archive.SignaturePolicyIDs".to_string())
        );
        assert_eq!(naming.page_to_name("ABSOLUTE-function"), None);
        assert_eq!(
            naming.page_to_name("Clone-method-Progress.Lang.Object"),
            None
        );
        assert_eq!(naming.page_to_name("Introduction-to-ABL-events"), None);
    }

    #[test]
    fn parse_abl_signature_handles_modes_and_generics() {
        let (name, params) =
            parse_abl_signature("Equals ( INPUT OtherObj AS Progress.Lang.Object )");
        assert_eq!(name, "Equals");
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].name.as_deref(), Some("OtherObj"));
        assert_eq!(params[0].type_name, "Progress.Lang.Object");

        let (name, params) = parse_abl_signature(
            "GetValue ( INPUT K AS CHARACTER, OUTPUT V AS Progress.Collections.IList<T> )",
        );
        assert_eq!(name, "GetValue");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].type_name, "CHARACTER");
        assert_eq!(params[1].type_name, "Progress.Collections.IList<T>");

        let (name, params) = parse_abl_signature("PUBLIC Object ( )");
        assert_eq!(name, "Object");
        assert!(params.is_empty());

        let (name, params) = parse_abl_signature("Clone( )");
        assert_eq!(name, "Clone");
        assert!(params.is_empty());
    }

    #[test]
    fn parse_abl_signature_handles_multiline_no_comma_params() {
        // The docs render some signatures across lines with NBSP padding and
        // no commas between parameters.
        let sig = "PUBLIC AppError ( INPUT ErrorMessage \u{a0}AS CHARACTER \n\u{a0}\u{a0}\u{a0}\u{a0} INPUT MessageNumber AS INTEGER )";
        let (name, params) = parse_abl_signature(sig);
        assert_eq!(name, "AppError");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name.as_deref(), Some("ErrorMessage"));
        assert_eq!(params[0].type_name, "CHARACTER");
        assert_eq!(params[1].name.as_deref(), Some("MessageNumber"));
        assert_eq!(params[1].type_name, "INTEGER");
    }

    #[test]
    fn strip_label_prefix_handles_split_strong() {
        // Some pages render "Return" in a <strong> and "type:" as plain text.
        assert_eq!(
            strip_label_prefix("Returntype: LOGICAL", "Return type"),
            Some(" LOGICAL")
        );
        assert_eq!(
            strip_label_prefix("Return type: LOGICAL", "Return type"),
            Some(" LOGICAL")
        );
        assert_eq!(
            strip_label_prefix("Data type: CHARACTER", "Data type"),
            Some(" CHARACTER")
        );
        assert_eq!(strip_label_prefix("Access: PUBLIC", "Return type"), None);
    }

    #[test]
    fn member_name_from_link_text_variants() {
        assert_eq!(
            member_name_from_link_text("Clone( ) method (Progress.Lang.Object)"),
            "Clone"
        );
        assert_eq!(
            member_name_from_link_text("NEXT-SIBLING property"),
            "NEXT-SIBLING"
        );
        assert_eq!(
            member_name_from_link_text("Get( ) method (List Collections)"),
            "Get"
        );
        assert_eq!(member_name_from_link_text("ToString( ) method"), "ToString");
    }

    const DITA_CLASS_HTML: &str = r#"<html><body><article class="dita" role="article"><h1 class="title topictitle1 reference" id="t">Progress.Lang.SysError class</h1><div class="body refbody">
<section class="section"><h2 class="title sectiontitle">Constructors</h2>
  <table class="table frame-all"><tbody class="tbody"><tr class="row rowsep-0"><td class="entry"><pre class="pre codeblock"><code>PUBLIC SysError ( )</code></pre></td></tr></tbody></table>
</section>
<section class="section"><h2 class="title sectiontitle">Super Class</h2>
  <p class="p"><a class="xref" href="Progress.Lang.AppError-class.html">Progress.Lang.AppError class</a></p>
</section>
<section class="section"><h2 class="title sectiontitle">Interfaces</h2>
  <p class="p">This class does not implement interfaces (beyond those it inherits from its base class).</p>
</section>
<section class="section"><h2 class="title sectiontitle">Public Properties</h2>
  <table class="table frame-all"><tbody class="tbody"><tr class="row rowsep-0">
    <td class="entry"><a class="xref" href="ReturnValue-property.html#ReturnValue-property">ReturnValue property</a></td>
  </tr></tbody></table>
</section>
<section class="section"><h2 class="title sectiontitle">Public Methods</h2>
  <table class="table frame-all"><tbody class="tbody"><tr class="row rowsep-0">
    <td class="entry"><a class="xref" href="AddMessage-method.html#AddMessage-method">AddMessage( ) method</a></td>
  </tr></tbody></table>
</section>
</div></article></body></html>"#;

    #[test]
    fn dita_class_page_parses_header_and_inventory() {
        let (te, inventory) = parse_dita_class_page(
            DITA_CLASS_HTML,
            "Progress.Lang.SysError",
            "Progress.Lang.SysError-class.html",
        )
        .expect("parses");
        assert_eq!(te.kind, TypeKind::Class);
        assert_eq!(te.package, "Progress.Lang");
        assert_eq!(te.page, "Progress.Lang.SysError-class.html");
        assert_eq!(te.inherits.as_deref(), Some("Progress.Lang.AppError"));
        assert!(te.implements.is_empty());
        assert_eq!(te.constructors.len(), 1);
        assert_eq!(te.constructors[0].name, "SysError");
        assert!(te.methods.is_empty());
        assert_eq!(inventory.len(), 2);
        assert_eq!(inventory[0].kind, MemberKind::Property);
        assert_eq!(inventory[0].name, "ReturnValue");
        assert_eq!(inventory[0].page, "ReturnValue-property.html");
        assert_eq!(inventory[1].kind, MemberKind::Method);
        assert_eq!(inventory[1].name, "AddMessage");
        assert_eq!(inventory[1].page, "AddMessage-method.html");
    }

    const DITA_ENUM_HTML: &str = r#"<html><body><article class="dita" role="article"><h1 class="title topictitle1 reference" id="t">Progress.Archive.SignaturePolicyIDs enumeration</h1><div class="body refbody">
<section class="section"><h2 class="title sectiontitle">Super Class</h2>
  <p class="p">Progress.Lang.Enum class</p>
</section>
<section class="section"><h2 class="title sectiontitle">Members</h2>
  <table class="table frame-all"><tbody class="tbody">
    <tr class="row rowsep-1"><td class="entry"><code class="ph codeph">Open</code></td><td class="entry">Indicates loading, e.g. <code class="ph codeph">.pl</code> files.</td></tr>
    <tr class="row rowsep-1"><td class="entry"><code class="ph codeph">Required</code></td><td class="entry">Some description.</td></tr>
  </tbody></table>
</section>
</div></article></body></html>"#;

    #[test]
    fn dita_enum_page_parses_kind_and_members() {
        let (te, inventory) = parse_dita_class_page(
            DITA_ENUM_HTML,
            "Progress.Archive.SignaturePolicyIDs",
            "Progress.Archive.SignaturePolicyIDs-enumeration.html",
        )
        .expect("parses");
        assert_eq!(te.kind, TypeKind::Enum);
        assert_eq!(te.inherits.as_deref(), Some("Progress.Lang.Enum"));
        assert_eq!(
            te.enum_members,
            vec!["Open".to_string(), "Required".to_string()]
        );
        assert!(inventory.is_empty());
    }

    const DITA_METHOD_HTML: &str = r#"<html><body><article class="dita" role="article"><h1 class="title topictitle1 reference" id="t">AddMessage( ) method</h1><div class="body refbody">
<section class="section">
  <p class="p">Adds a message.</p>
  <p class="p"><strong class="ph b">Return</strong> type: LOGICAL</p>
  <p class="p"><strong class="ph b">Access:</strong> PUBLIC</p>
</section>
<section class="section"><h2 class="title sectiontitle">Syntax</h2>
  <table class="table frame-all"><tbody class="tbody"><tr class="row rowsep-0"><td class="entry"><pre class="pre codeblock"><code>AddMessage ( INPUT pMsgNum AS INTEGER )</code></pre></td></tr></tbody></table>
</section>
</div></article></body></html>"#;

    #[test]
    fn dita_method_page_parses_signature() {
        let members = parse_dita_member_page(DITA_METHOD_HTML, MemberKind::Method);
        assert_eq!(members.len(), 1);
        let m = &members[0];
        assert_eq!(m.name, "AddMessage");
        assert_eq!(m.return_type.as_deref(), Some("LOGICAL"));
        assert_eq!(m.params.len(), 1);
        assert_eq!(m.params[0].name.as_deref(), Some("pMsgNum"));
        assert_eq!(m.params[0].type_name, "INTEGER");
        assert!(!m.is_static);
    }

    const DITA_MULTI_OVERLOAD_HTML: &str = r#"<html><body><article class="dita" role="article"><h1 class="title topictitle1 reference" id="t">Add( ) method</h1><div class="body refbody">
<section class="section">
  <p class="p"><strong class="ph b">Return type:</strong> LOGICAL</p>
</section>
<section class="section"><h2 class="title sectiontitle">Syntax</h2>
  <table class="table frame-all"><tbody class="tbody"><tr class="row rowsep-0"><td class="entry"><pre class="pre codeblock"><code>Add ( INPUT value AS DATE ) Add ( INPUT array-value AS DATE EXTENT ) Add ( INPUT index AS INTEGER )</code></pre></td></tr></tbody></table>
</section>
</div></article></body></html>"#;

    #[test]
    fn dita_member_page_splits_concatenated_overloads() {
        let members = parse_dita_member_page(DITA_MULTI_OVERLOAD_HTML, MemberKind::Method);
        assert_eq!(members.len(), 3);
        for m in &members {
            assert_eq!(m.name, "Add");
            assert_eq!(m.return_type.as_deref(), Some("LOGICAL"));
        }
        assert_eq!(members[0].params[0].type_name, "DATE");
        assert_eq!(members[1].params[0].type_name, "DATE[]");
        assert_eq!(members[2].params[0].type_name, "INTEGER");
    }

    const DITA_PROPERTY_HTML: &str = r#"<html><body><article class="dita" role="article"><h1 class="title topictitle1 reference" id="t">ReturnValue property</h1><div class="body refbody">
<section class="section">
  <p class="p"><strong class="ph b">Data type:</strong> <a class="xref" href="Progress.Lang.Object-class.html">Progress.Lang.Object class</a></p>
  <p class="p"><strong class="ph b">Access:</strong> PUBLIC Read-only</p>
</section>
</div></article></body></html>"#;

    #[test]
    fn dita_property_page_parses_type_and_falls_back_to_title() {
        let members = parse_dita_member_page(DITA_PROPERTY_HTML, MemberKind::Property);
        assert_eq!(members.len(), 1);
        let m = &members[0];
        assert_eq!(m.name, "ReturnValue");
        assert_eq!(m.return_type.as_deref(), Some("Progress.Lang.Object"));
        assert!(m.params.is_empty());
    }

    #[test]
    fn extent_markers_are_propagated() {
        let html = r##"<html><body><article class="misc-html">
<table class="table_content" width="100%"><tbody><tr><td>
<div class="unit_header">
  <table width="100%"><tbody>
    <tr><td><span class="span_pagetype"><b>Type:</b> Class</span></td></tr>
  </tbody></table>
</div>
</td></tr></tbody></table>
<table class="table_content table_content_details method_detail" width="100%"><tbody>
  <tr class="table_content"><th colspan="2">
    <h4> LOGICAL <a href="#a1"> IsEmpty (character[], character)</a></h4>
    <div><dl><dd><dl><table width="100%"><tbody>
      <tr><td><i><b>pcValue</b></i></td><td>CHARACTER</td></tr>
      <tr><td><i><b>pcList</b></i></td><td>CHARACTER</td></tr>
    </tbody></table></dl></dd></dl></div>
  </th></tr>
</tbody></table>
</article></body></html>"##;
        let te = parse_type_page(html, "OpenEdge.Core.String").expect("parses");
        assert_eq!(te.methods.len(), 1);
        assert_eq!(te.methods[0].name, "IsEmpty");
        assert_eq!(te.methods[0].params[0].type_name, "CHARACTER[]");
        assert_eq!(te.methods[0].params[1].type_name, "CHARACTER");
    }

    #[test]
    fn version_from_bundles_picks_versioned() {
        assert_eq!(version_from_bundles(&["abl-reference".to_string()]), "");
        assert_eq!(
            version_from_bundles(&[
                "openedge-abl-api-reference-128".to_string(),
                "abl-reference".to_string()
            ]),
            "12.8"
        );
    }

    #[test]
    fn strip_dita_type_word_variants() {
        assert_eq!(
            strip_dita_type_word("Progress.Lang.Enum class"),
            "Progress.Lang.Enum"
        );
        assert_eq!(
            strip_dita_type_word("Progress.Collections.ICollection<T> interface"),
            "Progress.Collections.ICollection<T>"
        );
        assert_eq!(
            strip_dita_type_word("System.Object class from .NET"),
            "System.Object"
        );
        assert_eq!(
            strip_dita_type_word(
                "System.ComponentModel.PropertyDescriptor class from\n        .NET"
            ),
            "System.ComponentModel.PropertyDescriptor"
        );
        assert_eq!(
            strip_dita_type_word("Progress.Lang.Object"),
            "Progress.Lang.Object"
        );
        assert_eq!(strip_dita_type_word("CHARACTER"), "CHARACTER");
    }

    #[test]
    fn unqualified_supertypes_are_qualified_by_package() {
        let mut index = BuiltinIndex {
            bundle_ids: vec!["b".to_string()],
            version: String::new(),
            generated_at: String::new(),
            packages: Vec::new(),
            types: BTreeMap::from([
                (
                    "OpenEdge.DataAdmin.AreaSet".to_string(),
                    TypeEntry {
                        kind: TypeKind::Class,
                        package: "OpenEdge.DataAdmin".to_string(),
                        page: "OpenEdge.DataAdmin.AreaSet.html".to_string(),
                        inherits: Some("DataAdminCollection".to_string()),
                        implements: vec!["Other".to_string()],
                        is_abstract: false,
                        is_final: false,
                        constructors: Vec::new(),
                        methods: Vec::new(),
                        properties: Vec::new(),
                        events: Vec::new(),
                        enum_members: Vec::new(),
                    },
                ),
                (
                    "OpenEdge.DataAdmin.DataAdminCollection".to_string(),
                    TypeEntry {
                        kind: TypeKind::Interface,
                        package: "OpenEdge.DataAdmin".to_string(),
                        page: "OpenEdge.DataAdmin.DataAdminCollection.html".to_string(),
                        inherits: None,
                        implements: Vec::new(),
                        is_abstract: false,
                        is_final: false,
                        constructors: Vec::new(),
                        methods: Vec::new(),
                        properties: Vec::new(),
                        events: Vec::new(),
                        enum_members: Vec::new(),
                    },
                ),
            ]),
        };
        qualify_unqualified_supertypes(&mut index);
        let area_set = &index.types["OpenEdge.DataAdmin.AreaSet"];
        assert_eq!(
            area_set.inherits.as_deref(),
            Some("OpenEdge.DataAdmin.DataAdminCollection")
        );
        // Unresolvable unqualified names are left alone.
        assert_eq!(area_set.implements, vec!["Other".to_string()]);
    }
}
