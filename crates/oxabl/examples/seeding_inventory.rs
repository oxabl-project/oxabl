//! seeding_inventory — walk the `oxabl_ast` for every ABL file under a path and
//! emit, for every `PROCEDURE`, `FUNCTION`, and class `METHOD`, the exact
//! placement an observability-span seeder needs. One row per routine.
//!
//! This is the U4 downstream consumer of the seeding pipeline (a peer of
//! `find_statements`): the parser already holds `Catch`/`Finally` as
//! first-class AST nodes, so *placement is computed here, from the tree* — the
//! TypeScript applier never re-lexes ABL to rediscover block boundaries.
//!
//! # What each row carries
//! - `kind`            Procedure | Function | Method
//! - `span_name`       KTD-9 span name: bare routine name for procedures and
//!                     functions; `<Class>:<method>` for methods.
//! - `seedable`        false when the routine must not be auto-seeded (abstract
//!                     method, interface member, empty body, or a placement the
//!                     parser cannot pin down safely). Non-seedable rows are
//!                     *emitted flagged*, never dropped, so the report reconciles.
//! - `flag_reason`     why a row is non-seedable (empty when seedable).
//! - `is_abstract`     the method's ABSTRACT flag.
//! - `has_finally`     the routine already has a routine-level FINALLY, so the
//!                     seeder merges the span close into it (R7) instead of
//!                     inserting the end-include.
//! - `extent_*`        full routine extent (line/col), for validation.
//! - `decl_*`          post-declaration-block insertion point — where the
//!                     start-include goes (line, col 1).
//! - `end_*`           pre-END insertion point, after any trailing CATCH blocks
//!                     — where the end-include (the FINALLY) goes (empty when
//!                     has_finally).
//! - `merge_*`         in-FINALLY merge point — the first line of the existing
//!                     FINALLY body (empty unless has_finally).
//!
//! # Placement method (why it is robust without re-lexing)
//! Statement/Expression nodes carry no span; only leaf tokens do. We take the
//! (min start, max end) over a node's leaf tokens, exactly like `find_statements`.
//! The one thing leaf spans cannot give directly is the offset of a block-closing
//! `END` keyword. Rather than depth-count raw tokens (ambiguous: `FOR` appears in
//! `DEFINE BUFFER b FOR t` and `OPEN QUERY q FOR EACH …`, which are not blocks),
//! we count **`End` tokens** — which are *only ever* block terminators — starting
//! at the routine body's last leaf, and take the N-th, where N is derived from the
//! AST's rightmost block spine (`rightmost_end_count`). Any trailing shape the
//! spine walker does not recognise flags the row non-seedable rather than guessing.
//!
//! # Determinism
//! Files are discovered in sorted order and routines are emitted in AST (source)
//! order, so re-runs are byte-identical.
//!
//! # Usage
//! ```text
//! cargo run -p oxabl --release --example seeding_inventory -- \
//!     <root-path> [--out inventory.csv] [--ext p,w,cls,i,v]
//! ```

use std::io::Write as _;
use std::path::{Path, PathBuf};

use oxabl_ast::{Statement, StatementKind as SK};
use oxabl_common::SourceMap;
use oxabl_lexer::{tokenize, Kind};
use oxabl_parser::Parser;
use walkdir::WalkDir;

const DEFAULT_EXTS: &[&str] = &["p", "w", "cls", "i", "v"];

struct Config {
    root: PathBuf,
    out: Option<PathBuf>,
    exts: Vec<String>,
}

/// One inventory row. Insertion points are 1-based (line, col); `col` is 1 for
/// the two include insertions (they take a whole line) and the FINALLY body
/// line for the merge. `None` for a point means "not applicable to this row".
struct Row {
    file: String,
    kind: String,
    span_name: String,
    seedable: bool,
    flag_reason: String,
    is_abstract: bool,
    has_finally: bool,
    extent: (usize, usize, usize, usize),
    decl: Option<(usize, usize)>,
    end: Option<(usize, usize)>,
    merge: Option<(usize, usize)>,
}

fn main() {
    let cfg = match parse_args() {
        Ok(c) => c,
        Err(msg) => {
            eprintln!("{msg}");
            eprintln!(
                "\nusage: seeding_inventory <root> [--out file.csv] [--ext p,w,cls,i,v]"
            );
            std::process::exit(2);
        }
    };

    let files = discover_files(&cfg.root, &cfg.exts);
    if files.is_empty() {
        eprintln!("No ABL files found under {}", cfg.root.display());
        std::process::exit(2);
    }
    eprintln!("Scanning {} file(s) for seedable routines…", files.len());

    let mut rows: Vec<Row> = Vec::new();
    let mut parse_failures = 0usize;
    let mut read_failures = 0usize;

    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(_) => {
                read_failures += 1;
                continue;
            }
        };

        let tokens =
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| tokenize(&source))) {
                Ok(t) => t,
                Err(_) => {
                    parse_failures += 1;
                    continue;
                }
            };
        // Byte offsets of every `End` token, ascending — the closer index.
        let end_offsets: Vec<usize> = tokens
            .iter()
            .filter(|t| t.kind == Kind::End)
            .map(|t| t.start)
            .collect();

        let mut parser = Parser::new(&tokens, &source);
        let program = parser.parse_program();
        if !program.errors.is_empty() {
            parse_failures += 1; // partial AST still walked below
        }

        let sm = SourceMap::new(&source);
        let file_display = file.display().to_string();
        let ctx = WalkCtx {
            sm: &sm,
            end_offsets: &end_offsets,
            file: &file_display,
        };
        walk_routines(&program.statements, None, false, &ctx, &mut rows);
    }

    if let Err(e) = write_csv(&cfg.out, &rows) {
        eprintln!("error writing CSV: {e}");
        std::process::exit(1);
    }

    let seedable = rows.iter().filter(|r| r.seedable).count();
    eprintln!(
        "Done: {} routine(s) ({} seedable, {} flagged) across {} file(s).",
        rows.len(),
        seedable,
        rows.len() - seedable,
        files.len()
    );
    if parse_failures > 0 {
        eprintln!("  {parse_failures} file(s) had parse errors (walked with partial AST).");
    }
    if read_failures > 0 {
        eprintln!("  {read_failures} file(s) could not be read (non-UTF-8 or I/O).");
    }
}

struct WalkCtx<'a> {
    sm: &'a SourceMap,
    end_offsets: &'a [usize],
    file: &'a str,
}

/// Recursively walk statements, emitting one row per Procedure/Function/Method
/// and descending into Class/Interface bodies (tracking the enclosing class
/// name and whether we are inside an INTERFACE, which makes members non-seedable).
fn walk_routines(
    stmts: &[Statement],
    class_name: Option<&str>,
    in_interface: bool,
    ctx: &WalkCtx,
    out: &mut Vec<Row>,
) {
    for s in stmts {
        match &s.kind {
            SK::Class { name, body, .. } => {
                walk_routines(body, Some(&name.name), false, ctx, out);
            }
            SK::Interface { name, body, .. } => {
                walk_routines(body, Some(&name.name), true, ctx, out);
            }
            SK::Procedure { name, body } => {
                out.push(build_routine(ctx, "Procedure", &name.name, false, in_interface, s, body));
            }
            SK::Function { name, body, .. } => {
                out.push(build_routine(ctx, "Function", &name.name, false, in_interface, s, body));
            }
            SK::Method {
                name,
                body,
                is_abstract,
                ..
            } => {
                let span_name = match class_name {
                    Some(c) => format!("{c}:{}", name.name),
                    None => name.name.clone(),
                };
                out.push(build_routine(
                    ctx,
                    "Method",
                    &span_name,
                    *is_abstract,
                    in_interface,
                    s,
                    body,
                ));
            }
            _ => {}
        }
    }
}

/// Compute placement for one routine node. `node` is the whole routine statement
/// (for the full extent); `body` is its direct body statement list.
fn build_routine(
    ctx: &WalkCtx,
    kind: &str,
    span_name: &str,
    is_abstract: bool,
    in_interface: bool,
    node: &Statement,
    body: &[Statement],
) -> Row {
    let extent = match statement_span(node) {
        Some((s, e)) => {
            let (ls, cs) = ctx.sm.lookup(s);
            let (le, ce) = ctx.sm.lookup(e.saturating_sub(1).max(s));
            (ls, cs, le, ce + 1)
        }
        None => (0, 0, 0, 0),
    };

    let mut row = Row {
        file: ctx.file.to_string(),
        kind: kind.to_string(),
        span_name: span_name.to_string(),
        seedable: false,
        flag_reason: String::new(),
        is_abstract,
        has_finally: false,
        extent,
        decl: None,
        end: None,
        merge: None,
    };

    // --- Non-seedable gates (emitted flagged, never dropped) ---
    if in_interface {
        row.flag_reason = "interface member".into();
        return row;
    }
    if is_abstract {
        row.flag_reason = "abstract method".into();
        return row;
    }
    let node_end = match statement_span(node) {
        Some((_, e)) => e,
        None => {
            row.flag_reason = "no source span (no leaf tokens)".into();
            return row;
        }
    };

    // --- decl point: first executable child (not a declaration, CATCH or FINALLY) ---
    let first_exec = body
        .iter()
        .find(|c| !is_declaration(&c.kind) && !is_catch_or_finally(&c.kind) && statement_span(c).is_some());
    let decl = match first_exec.and_then(statement_span) {
        Some((s, _)) => Some(line_col1(ctx.sm, s)),
        None => {
            row.flag_reason = "no locatable executable statement".into();
            return row;
        }
    };

    // --- FINALLY (R7): a routine-level FINALLY child means we merge, not insert ---
    let finally_child = body.iter().find(|c| matches!(c.kind, SK::Finally { .. }));
    if let Some(fin) = finally_child {
        row.has_finally = true;
        let fin_body = match &fin.kind {
            SK::Finally { body } => body,
            _ => unreachable!(),
        };
        let first = fin_body.iter().find_map(statement_span);
        match first {
            Some((s, _)) => {
                row.decl = decl;
                row.merge = Some(line_col1(ctx.sm, s));
                row.seedable = true;
                return row;
            }
            None => {
                // Empty (auto) FINALLY body — no leaf to anchor the merge line on.
                // Route to the human queue rather than guess (R10).
                row.flag_reason = "empty FINALLY body — manual merge required".into();
                row.decl = decl;
                return row;
            }
        }
    }

    // --- pre-END point: the N-th `End` after the body's last leaf ---
    let n = match rightmost_end_count(body) {
        Some(n) => n + 1, // + the routine's own END
        None => {
            row.flag_reason = "unrecognized trailing block — cannot place END include".into();
            row.decl = decl;
            return row;
        }
    };
    let routine_end = nth_end_at_or_after(ctx.end_offsets, node_end, n);
    match routine_end {
        Some(off) => {
            row.decl = decl;
            row.end = Some(line_col1(ctx.sm, off));
            row.seedable = true;
        }
        None => {
            row.flag_reason = "could not locate routine END keyword".into();
            row.decl = decl;
        }
    }
    row
}

/// (line, 1) for a byte offset — the two include insertions take a whole line,
/// so they anchor at column 1 of the line their reference offset falls on.
fn line_col1(sm: &SourceMap, offset: usize) -> (usize, usize) {
    let (line, _col) = sm.lookup(offset);
    (line, 1)
}

/// True for DEFINE-family declarations that make up the leading declaration
/// block. The start-include is placed *after* these.
fn is_declaration(kind: &SK) -> bool {
    matches!(kind, SK::VariableDeclaration { .. } | SK::DefineParameter { .. })
        || variant_name(kind).starts_with("Define")
}

fn is_catch_or_finally(kind: &SK) -> bool {
    matches!(kind, SK::Catch { .. } | SK::Finally { .. })
}

/// Number of block-closing `END` tokens that appear *after the last leaf* of a
/// body but *before* the enclosing routine's own END — i.e. the depth of the
/// rightmost block spine. `None` means the trailing shape isn't one we place
/// safely, so the caller flags the row.
fn rightmost_end_count(stmts: &[Statement]) -> Option<usize> {
    match stmts.last() {
        None => Some(0),
        Some(s) => end_count_of(s),
    }
}

fn end_count_of(s: &Statement) -> Option<usize> {
    match &s.kind {
        // END-closed blocks: their own END, plus whatever closes after their body.
        SK::Do { body, .. }
        | SK::Repeat { body, .. }
        | SK::ForEach { body, .. }
        | SK::Catch { body, .. }
        | SK::Finally { body, .. } => Some(1 + rightmost_end_count(body)?),
        SK::Case {
            when_branches,
            otherwise,
            ..
        } => {
            let inner = if let Some(o) = otherwise {
                rightmost_end_count(o)?
            } else if let Some(w) = when_branches.last() {
                rightmost_end_count(&w.body)?
            } else {
                0
            };
            Some(1 + inner) // END CASE
        }
        // IF is not END-closed; the closers come from whichever branch trails.
        SK::If {
            then_branch,
            else_branch,
            ..
        } => end_count_of(else_branch.as_ref().unwrap_or(then_branch)),
        SK::Label { body, .. } => end_count_of(body),
        SK::Block(body) => rightmost_end_count(body),
        // Nested routine/preproc/property as a trailing child: not something we
        // place against — flag for a human.
        SK::Procedure { .. }
        | SK::Function { .. }
        | SK::Method { .. }
        | SK::Class { .. }
        | SK::Interface { .. }
        | SK::Constructor { .. }
        | SK::Destructor { .. }
        | SK::Property { .. }
        | SK::PreprocIf(_) => None,
        // Any simple, non-block statement closes nothing.
        _ => Some(0),
    }
}

/// The start offset of the N-th (1-based) `End` token at or after `from`.
fn nth_end_at_or_after(end_offsets: &[usize], from: usize, n: usize) -> Option<usize> {
    end_offsets
        .iter()
        .filter(|&&o| o >= from)
        .nth(n.saturating_sub(1))
        .copied()
}

/// The leading identifier of a `StatementKind`'s derived `Debug`, i.e. its
/// variant name.
fn variant_name(kind: &SK) -> String {
    let dbg = format!("{kind:?}");
    dbg.split([' ', '{', '(', '\n'])
        .next()
        .unwrap_or("")
        .to_string()
}

/// (min start, max end) over every `Span { start, end }` in a statement's
/// derived `Debug` — the extent of all leaf tokens in its subtree. `None` when
/// the statement contains no spanned leaves.
fn statement_span(stmt: &Statement) -> Option<(usize, usize)> {
    let dbg = format!("{:?}", stmt.kind);
    let mut min_start: Option<usize> = None;
    let mut max_end: usize = 0;

    let needle = "Span { start: ";
    let mut rest = dbg.as_str();
    while let Some(pos) = rest.find(needle) {
        rest = &rest[pos + needle.len()..];
        let start_len = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
        let start: usize = rest[..start_len].parse().ok()?;
        rest = &rest[start_len..];
        let end_marker = "end: ";
        let epos = match rest.find(end_marker) {
            Some(p) => p,
            None => break,
        };
        rest = &rest[epos + end_marker.len()..];
        let end_len = rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(rest.len());
        let end: usize = rest[..end_len].parse().ok()?;
        rest = &rest[end_len..];

        min_start = Some(min_start.map_or(start, |m| m.min(start)));
        max_end = max_end.max(end);
    }
    min_start.map(|s| (s, max_end.max(s)))
}

fn write_csv(out: &Option<PathBuf>, rows: &[Row]) -> std::io::Result<()> {
    let mut buf = String::from(
        "file,kind,span_name,seedable,flag_reason,is_abstract,has_finally,\
extent_line_start,extent_col_start,extent_line_end,extent_col_end,\
decl_line,decl_col,end_line,end_col,merge_line,merge_col\n",
    );
    let pt = |p: Option<(usize, usize)>| -> (String, String) {
        match p {
            Some((l, c)) => (l.to_string(), c.to_string()),
            None => (String::new(), String::new()),
        }
    };
    for r in rows {
        let (dl, dc) = pt(r.decl);
        let (el, ec) = pt(r.end);
        let (ml, mc) = pt(r.merge);
        let cols = [
            csv_field(&r.file),
            csv_field(&r.kind),
            csv_field(&r.span_name),
            r.seedable.to_string(),
            csv_field(&r.flag_reason),
            r.is_abstract.to_string(),
            r.has_finally.to_string(),
            r.extent.0.to_string(),
            r.extent.1.to_string(),
            r.extent.2.to_string(),
            r.extent.3.to_string(),
            dl,
            dc,
            el,
            ec,
            ml,
            mc,
        ];
        buf.push_str(&cols.join(","));
        buf.push('\n');
    }
    match out {
        Some(path) => std::fs::write(path, buf),
        None => std::io::stdout().write_all(buf.as_bytes()),
    }
}

/// RFC-4180 CSV field: always quoted, interior `"` doubled.
fn csv_field(s: &str) -> String {
    let escaped = s.replace('"', "\"\"");
    format!("\"{escaped}\"")
}

fn discover_files(root: &Path, exts: &[String]) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if root.is_file() {
        return vec![root.to_path_buf()];
    }
    for entry in WalkDir::new(root).follow_links(true) {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };
        if !entry.file_type().is_file() {
            continue;
        }
        if let Some(ext) = entry.path().extension() {
            let ext = ext.to_string_lossy().to_lowercase();
            if exts.iter().any(|e| e == &ext) {
                files.push(entry.into_path());
            }
        }
    }
    files.sort();
    files
}

fn parse_args() -> Result<Config, String> {
    let mut args = std::env::args().skip(1);
    let mut root: Option<PathBuf> = None;
    let mut out: Option<PathBuf> = None;
    let mut exts: Vec<String> = DEFAULT_EXTS.iter().map(|s| s.to_string()).collect();

    while let Some(a) = args.next() {
        match a.as_str() {
            "--out" => out = Some(PathBuf::from(args.next().ok_or("--out requires a value")?)),
            "--ext" => {
                let v = args.next().ok_or("--ext requires a value")?;
                exts = v
                    .split(',')
                    .map(|s| s.trim().trim_start_matches('.').to_lowercase())
                    .filter(|s| !s.is_empty())
                    .collect();
            }
            "-h" | "--help" => return Err("seeding_inventory: emit span-seeding placement CSV".into()),
            other if other.starts_with('-') => return Err(format!("unknown flag: {other}")),
            other => {
                if root.is_some() {
                    return Err(format!("unexpected argument: {other}"));
                }
                root = Some(PathBuf::from(other));
            }
        }
    }

    Ok(Config {
        root: root.ok_or("missing <root> path")?,
        out,
        exts,
    })
}
