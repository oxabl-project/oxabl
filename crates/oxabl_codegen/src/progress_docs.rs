//! Crawl the Progress OpenEdge "ABL API Reference" documentation and emit a
//! static JSON resource describing every documented built-in type.
//!
//! The docs platform (ZoomIn) exposes two endpoints this module relies on:
//!
//! * `.../bundle/{bundle}/toc/{page}.html` — returns the *entire* bundle
//!   navigation tree as a JSON object keyed by TOC node id. A single call
//!   yields every package and type in the bundle (the page name in the URL is
//!   ignored; any page returns the whole tree).
//! * `.../bundle/{bundle}/page/{type}.html` — one type's reference page,
//!   carrying its inheritance header and one detail table per member category.
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
//!   classes return values rather than writing OUTPUT/INPUT-OUTPUT parameters
//!   (parameter mode never appears as structured docs data), so members carry
//!   a `return_type` only when the page specifies one.
//!
//! Fetched pages are cached under
//! `$XDG_CACHE_HOME/oxabl/progress-docs/{bundle}/pages/` (falling back to
//! `~/.cache`), keyed by page file name, so a full re-run is cheap.

use scraper::{ElementRef, Html, Selector};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::thread;
use std::time::Duration;

pub const DEFAULT_BUNDLE_ID: &str = "openedge-abl-api-reference-128";

const DOCS_HOST: &str = "https://docs-be.progress.com";
const USER_AGENT: &str = "oxabl-builtin-class-index/0.1 (+https://github.com/oxabl-project/oxabl)";
const THROTTLE: Duration = Duration::from_millis(80);

// =============================================================================
// Data model
// =============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuiltinIndex {
    /// Bundle identifier this was scraped from, e.g.
    /// `openedge-abl-api-reference-128`.
    pub bundle_id: String,
    /// Human-readable release, e.g. `12.8` (derived from the bundle id).
    pub version: String,
    /// UTC timestamp of generation (RFC 3339).
    pub generated_at: String,
    /// Every package in the bundle, including the root packages, as dotted
    /// names (e.g. `Ccs`, `OpenEdge.Core.Collections`).
    pub packages: Vec<String>,
    /// Every documented type keyed by its fully-qualified dotted name
    /// (e.g. `OpenEdge.Core.String`).
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
}

/// A documented member. Properties reuse this shape with `return_type` as the
/// property type; constructors and events have no `return_type`.
#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
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
/// * `--bundle <id>`      bundle to scrape (default `openedge-abl-api-reference-128`)
/// * `--refresh`          ignore the on-disk page cache
/// * `--catalog-only`     print the package/type catalog and write nothing
/// * `--limit <n>`        fetch at most `n` type detail pages
/// * `--names <a,b,c>`    fetch exactly these types (comma-separated dotted names)
/// * `--out <path>`       output path, relative to the workspace root
pub fn run_builtin_classes(args: &[String]) -> Result<(), String> {
    let mut bundle_id = DEFAULT_BUNDLE_ID.to_string();
    let mut refresh = false;
    let mut catalog_only = false;
    let mut limit: Option<usize> = None;
    let mut names: Option<Vec<String>> = None;
    let mut out_path = PathBuf::from("resources/progress_builtin_classes.json");

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--bundle" => {
                i += 1;
                bundle_id = args.get(i).ok_or("`--bundle` needs a value")?.clone();
            }
            "--refresh" => refresh = true,
            "--catalog-only" => catalog_only = true,
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

    let version = version_from_bundle(&bundle_id);
    let cache_dir = cache_dir(&bundle_id);

    eprintln!("fetching catalog for bundle {bundle_id} (version {version})");
    let toc_json = fetch_or_cache(
        &bundle_id,
        "toc",
        "OpenEdge.package.html",
        &cache_dir,
        refresh,
    )?;
    let type_names = parse_toc_types(&toc_json)?;
    let packages = derive_packages(&type_names);
    eprintln!(
        "catalog: {} packages, {} types",
        packages.len(),
        type_names.len()
    );

    let selected: Vec<String> = match &names {
        Some(ns) => ns
            .iter()
            .filter(|n| type_names.contains(n))
            .cloned()
            .collect(),
        None => type_names,
    };
    let selected = selected
        .into_iter()
        .take(limit.unwrap_or(usize::MAX))
        .collect::<Vec<_>>();

    if catalog_only {
        println!("packages ({}):", packages.len());
        for p in &packages {
            println!("  {p}");
        }
        println!("types ({}; showing first 20):", selected.len());
        for t in selected.iter().take(20) {
            println!("  {t}");
        }
        return Ok(());
    }

    let mut index = BuiltinIndex {
        bundle_id: bundle_id.clone(),
        version,
        generated_at: now_rfc3339(),
        packages,
        types: BTreeMap::new(),
    };

    let mut parsed = 0usize;
    let mut skipped = 0usize;
    let mut failed = Vec::new();
    for name in &selected {
        let page = format!("{name}.html");
        let html = match fetch_or_cache(&bundle_id, "page", &page, &cache_dir, refresh) {
            Ok(h) => h,
            Err(e) => {
                failed.push((name.clone(), e));
                continue;
            }
        };
        match parse_type_page(&html, name) {
            Some(te) => {
                index.types.insert(name.clone(), te);
                parsed += 1;
            }
            None => skipped += 1,
        }
    }

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

/// Extract every documented type name from the TOC JSON. The `toc/` endpoint
/// returns a JSON object keyed by TOC node id; each value is an HTML fragment
/// of that node's subtree. Package pages (names ending in `.package`) and the
/// root packages are excluded — the roots are not even present in the JSON
/// and are recovered from type-name prefixes by [`derive_packages`].
pub fn parse_toc_types(json: &str) -> Result<Vec<String>, String> {
    let tree: BTreeMap<String, String> =
        serde_json::from_str(json).map_err(|e| format!("malformed TOC JSON: {e}"))?;
    let a_sel = Selector::parse("a[href]").expect("valid selector");
    let mut types = Vec::new();
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
            if file.ends_with(".package") || !file.contains('.') {
                continue;
            }
            if seen.insert(file.to_string()) {
                types.push(file.to_string());
            }
        }
    }
    Ok(types)
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
        members.sort_by(|a, b| a.name.cmp(&b.name));
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
    })
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
            if params.is_empty() && !short_params.is_empty() {
                // No structured Parameters table (rare) — fall back to the
                // signature's short type names, unqualified.
                params = short_params
                    .into_iter()
                    .map(|type_name| Param {
                        name: None,
                        type_name,
                    })
                    .collect();
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
        let types = parse_toc_types(TOC_JSON).expect("parses");
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
}
