//! find_statements — walk the `oxabl_ast` for every ABL file under a path,
//! locate statements of a chosen kind, and emit them as CSV.
//!
//! This is a thin CLI over the parser + AST. `oxabl check`/`analyze` discard
//! the parsed tree (or only surface the semantic model); here we keep the AST
//! and inspect it directly.
//!
//! # What "kind" means
//! The `--kind` value is matched against the `StatementKind` enum *variant
//! name* (the leading identifier of its derived `Debug`). So `--kind Message`
//! matches `StatementKind::Message { .. }`, `--kind Display` matches
//! `Display { .. }`, `--kind Run` matches `Run { .. }`, etc. Change the flag to
//! target any other statement — nothing else needs to change.
//!
//! # How line numbers are derived
//! `Statement`/`Expression` nodes do not carry spans; only *leaf* nodes
//! (`Identifier`, integer/decimal/string/boolean/unknown literals, and include
//! references) do. For a matched node we take the min start / max end over
//! every `Span { start, end }` reachable in its `Debug` subtree, then map those
//! byte offsets to 1-based line/column positions via `SourceMap`. `content` is the raw
//! source between those offsets, with interior whitespace collapsed so each
//! statement is a single CSV row.
//!
//! Files are parsed *without* the preprocessor, so byte offsets (and therefore
//! line numbers) map to each file's own on-disk bytes rather than an expanded
//! buffer.
//!
//! # Usage
//! ```text
//! cargo run -p oxabl --release --example find_statements -- \
//!     <root-path> [--kind Message] [--out out.csv] [--ext p,w,cls,i,v]
//! ```

use std::io::Write as _;
use std::path::{Path, PathBuf};

use oxabl_ast::{Statement, StatementKind as SK};
use oxabl_common::SourceMap;
use walkdir::WalkDir;

/// Default file extensions scanned (lowercase, no dot).
const DEFAULT_EXTS: &[&str] = &["p", "w", "cls", "i", "v"];

struct Config {
    root: PathBuf,
    kind: String,
    out: Option<PathBuf>,
    exts: Vec<String>,
}

struct Row {
    file: String,
    kind: String,
    line_start: usize,
    col_start: usize,
    line_end: usize,
    col_end: usize,
    content: String,
}

fn main() {
    let cfg = match parse_args() {
        Ok(c) => c,
        Err(msg) => {
            eprintln!("{msg}");
            eprintln!(
                "\nusage: find_statements <root> [--kind Message] [--out file.csv] \
                 [--ext p,w,cls,i,v]"
            );
            std::process::exit(2);
        }
    };

    let files = discover_files(&cfg.root, &cfg.exts);
    if files.is_empty() {
        eprintln!("No ABL files found under {}", cfg.root.display());
        std::process::exit(2);
    }
    eprintln!(
        "Scanning {} file(s) for `{}` statements…",
        files.len(),
        cfg.kind
    );

    let mut rows: Vec<Row> = Vec::new();
    let mut parse_failures = 0usize;
    let mut read_failures = 0usize;
    let mut no_span = 0usize;

    for file in &files {
        let source = match std::fs::read_to_string(file) {
            Ok(s) => s,
            Err(_) => {
                read_failures += 1;
                continue;
            }
        };

        // Tokenize and parse through the shared guard, mirroring the main CLI: a
        // panic on one pathological file must not abort the whole scan. The
        // guard spans the parse too, not just the tokenize — the parser is as
        // much of the panic surface as the lexer. The parse is error-recovering,
        // so we get every matched statement we *can* reach even in files with
        // syntax the parser doesn't yet handle.
        let program = match oxabl::try_parse(&source) {
            Ok(program) => program,
            Err(_) => {
                parse_failures += 1;
                continue;
            }
        };
        if !program.errors.is_empty() {
            parse_failures += 1; // partial AST still walked below
        }

        let sm = SourceMap::new(&source);
        let file_display = file.display().to_string();
        walk(&program.statements, &cfg.kind, |stmt| {
            match statement_span(stmt) {
                Some((start, end)) => {
                    let (line_start, col_start) = sm.lookup(start);
                    // `end` is exclusive; look up the last byte actually covered.
                    // The reported end column is one past that byte, so a
                    // single-token span's [col_start, col_end) width matches its
                    // byte length.
                    let last = end.saturating_sub(1).max(start);
                    let (line_end, last_col) = sm.lookup(last);
                    rows.push(Row {
                        file: file_display.clone(),
                        kind: cfg.kind.clone(),
                        line_start,
                        col_start,
                        line_end,
                        col_end: last_col + 1,
                        content: collapse_ws(&source[start..end]),
                    });
                }
                None => no_span += 1,
            }
        });
    }

    if let Err(e) = write_csv(&cfg.out, &rows) {
        eprintln!("error writing CSV: {e}");
        std::process::exit(1);
    }

    eprintln!(
        "Done: {} `{}` statement(s) across {} file(s).",
        rows.len(),
        cfg.kind,
        files.len()
    );
    if no_span > 0 {
        eprintln!("  {no_span} match(es) had no source span (no leaf tokens) and were skipped.");
    }
    if parse_failures > 0 {
        eprintln!("  {parse_failures} file(s) had parse errors (walked with partial AST).");
    }
    if read_failures > 0 {
        eprintln!("  {read_failures} file(s) could not be read (non-UTF-8 or I/O).");
    }
}

/// Recursively visit `stmts`, invoking `emit` on every statement whose
/// `StatementKind` variant name equals `target`, and descending through every
/// variant that carries nested statements.
fn walk(stmts: &[Statement], target: &str, mut emit: impl FnMut(&Statement)) {
    // `emit` is borrowed mutably by the recursion, so route everything through
    // a helper that takes `&mut impl FnMut`.
    fn go(stmts: &[Statement], target: &str, emit: &mut dyn FnMut(&Statement)) {
        for s in stmts {
            if variant_name(&s.kind) == target {
                emit(s);
            }
            for child in child_statements(&s.kind) {
                go(child, target, emit);
            }
        }
    }
    go(stmts, target, &mut emit);
}

/// The leading identifier of a `StatementKind`'s derived `Debug`, i.e. its
/// variant name (`Message`, `Display`, `Run`, `Block`, …).
fn variant_name(kind: &SK) -> String {
    let dbg = format!("{kind:?}");
    dbg.split([' ', '{', '(', '\n'])
        .next()
        .unwrap_or("")
        .to_string()
}

/// Every nested `&[Statement]` a statement kind can contain. This is the full
/// set of statement-carrying variants in `oxabl_ast::StatementKind`; leaves
/// return an empty vec.
fn child_statements(kind: &SK) -> Vec<&[Statement]> {
    let mut out: Vec<&[Statement]> = Vec::new();
    match kind {
        SK::Block(b) => out.push(b),
        SK::Do { body, .. } => out.push(body),
        SK::If {
            then_branch,
            else_branch,
            ..
        } => {
            out.push(std::slice::from_ref(then_branch));
            if let Some(e) = else_branch {
                out.push(std::slice::from_ref(e));
            }
        }
        SK::Repeat { body, .. } => out.push(body),
        SK::ForEach { body, .. } => out.push(body),
        SK::Case {
            when_branches,
            otherwise,
            ..
        } => {
            for w in when_branches {
                out.push(&w.body);
            }
            if let Some(o) = otherwise {
                out.push(o);
            }
        }
        SK::Procedure { body, .. } => out.push(body),
        SK::Function { body, .. } => out.push(body),
        SK::Class { body, .. } => out.push(body),
        SK::Method {
            parameters, body, ..
        } => {
            out.push(parameters);
            out.push(body);
        }
        SK::Property {
            get_body,
            set_body,
            set_parameters,
            ..
        } => {
            if let Some(g) = get_body {
                out.push(g);
            }
            if let Some(s) = set_body {
                out.push(s);
            }
            out.push(set_parameters);
        }
        SK::Constructor {
            parameters, body, ..
        } => {
            out.push(parameters);
            out.push(body);
        }
        SK::Destructor { body, .. } => out.push(body),
        SK::Interface { body, .. } => out.push(body),
        SK::Catch { body, .. } => out.push(body),
        SK::Finally { body, .. } => out.push(body),
        SK::DefineEvent { parameters, .. } => out.push(parameters),
        SK::Label { body, .. } => out.push(std::slice::from_ref(body)),
        SK::PreprocIf(p) => {
            out.push(&p.then_branch);
            for (_, b) in &p.elseif_branches {
                out.push(b);
            }
            if let Some(e) = &p.else_branch {
                out.push(e);
            }
        }
        _ => {}
    }
    out
}

/// The (min start, max end) byte range over every `Span { start, end }` in a
/// statement's derived `Debug` — i.e. the extent of all leaf tokens in its
/// subtree. Returns `None` when the statement contains no spanned leaves.
fn statement_span(stmt: &Statement) -> Option<(usize, usize)> {
    let dbg = format!("{:?}", stmt.kind);
    let mut min_start: Option<usize> = None;
    let mut max_end: usize = 0;

    let needle = "Span { start: ";
    let mut rest = dbg.as_str();
    while let Some(pos) = rest.find(needle) {
        rest = &rest[pos + needle.len()..];
        // parse start digits
        let start_len = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());
        let start: usize = rest[..start_len].parse().ok()?;
        rest = &rest[start_len..];
        // advance to "end: "
        let end_marker = "end: ";
        let epos = match rest.find(end_marker) {
            Some(p) => p,
            None => break,
        };
        rest = &rest[epos + end_marker.len()..];
        let end_len = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());
        let end: usize = rest[..end_len].parse().ok()?;
        rest = &rest[end_len..];

        min_start = Some(min_start.map_or(start, |m| m.min(start)));
        max_end = max_end.max(end);
    }

    min_start.map(|s| (s, max_end.max(s)))
}

/// Collapse all runs of ASCII whitespace (including newlines) to single spaces
/// and trim, so each statement occupies one CSV row.
fn collapse_ws(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn write_csv(out: &Option<PathBuf>, rows: &[Row]) -> std::io::Result<()> {
    let mut buf = String::from("file,kind,line_start,col_start,line_end,col_end,content\n");
    for r in rows {
        buf.push_str(&csv_field(&r.file));
        buf.push(',');
        buf.push_str(&csv_field(&r.kind));
        buf.push(',');
        buf.push_str(&r.line_start.to_string());
        buf.push(',');
        buf.push_str(&r.col_start.to_string());
        buf.push(',');
        buf.push_str(&r.line_end.to_string());
        buf.push(',');
        buf.push_str(&r.col_end.to_string());
        buf.push(',');
        buf.push_str(&csv_field(&r.content));
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
    let mut kind = String::from("Message");
    let mut out: Option<PathBuf> = None;
    let mut exts: Vec<String> = DEFAULT_EXTS.iter().map(|s| s.to_string()).collect();

    while let Some(a) = args.next() {
        match a.as_str() {
            "--kind" => kind = args.next().ok_or("--kind requires a value")?,
            "--out" => out = Some(PathBuf::from(args.next().ok_or("--out requires a value")?)),
            "--ext" => {
                let v = args.next().ok_or("--ext requires a value")?;
                exts = v
                    .split(',')
                    .map(|s| s.trim().trim_start_matches('.').to_lowercase())
                    .filter(|s| !s.is_empty())
                    .collect();
            }
            "-h" | "--help" => return Err("find_statements: extract AST statements to CSV".into()),
            other if other.starts_with('-') => {
                return Err(format!("unknown flag: {other}"));
            }
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
        kind,
        out,
        exts,
    })
}
