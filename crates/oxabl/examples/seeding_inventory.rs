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
//! we count **`End` tokens** — which are *only ever* block terminators. The
//! routine's own END is the `(N + 1)`-th `End` token from the routine's first
//! leaf, where `N` is the number of block-closing ENDs the AST reports across the
//! *whole* body subtree (`count_block_ends`), so it lands after every nested block
//! and any trailing CATCH — never inside one. Anchoring at the first leaf (not a
//! trailing leaf that may sit mid-block) is what makes this hold when a routine's
//! last statement is spanless or a nested block is off the rightmost spine. Any
//! body shape whose ENDs cannot be counted reliably flags the row rather than
//! guessing. The decl point is resolved the same way — to a real statement
//! boundary — via ABL's `.`/`:`-followed-by-whitespace rule (see `compute_decl`).
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
use oxabl_lexer::{Kind, Token, tokenize};
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
            eprintln!("\nusage: seeding_inventory <root> [--out file.csv] [--ext p,w,cls,i,v]");
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
            tokens: &tokens,
            src: source.as_bytes(),
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
    /// Full token stream for the file — used to resolve the decl point to a real
    /// statement boundary (ABL's `.`/`:`-followed-by-whitespace rule).
    tokens: &'a [Token],
    /// Raw source bytes — for the whitespace-after-punctuation check.
    src: &'a [u8],
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
                out.push(build_routine(
                    ctx,
                    "Procedure",
                    &name.name,
                    false,
                    in_interface,
                    s,
                    body,
                ));
            }
            SK::Function { name, body, .. } => {
                out.push(build_routine(
                    ctx,
                    "Function",
                    &name.name,
                    false,
                    in_interface,
                    s,
                    body,
                ));
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
    let (node_start, _node_end) = match statement_span(node) {
        Some((s, e)) => (s, e),
        None => {
            row.flag_reason = "no source span (no leaf tokens)".into();
            return row;
        }
    };

    // --- decl point: the statement boundary AFTER the leading declaration region ---
    let decl = match compute_decl(ctx, node, body) {
        Ok(pt) => Some(pt),
        Err(reason) => {
            row.flag_reason = reason;
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

    // --- pre-END point: the routine block's OWN `END`, after any trailing CATCH ---
    // Count every block-closing `END` inside the routine body (not just the
    // rightmost spine). The routine's own END is the one immediately after all of
    // them: the (N+1)-th `End` token at or after the routine's first leaf. Anchoring
    // at the start (never a mid-body leaf) means trailing spanless statements — a
    // bare RETURN, an inner DO whose last child has no span — can no longer make the
    // count resolve to a nested block's END and attach the FINALLY to it (14144).
    let n = match count_block_ends(body) {
        Some(n) => n + 1, // + the routine's own END
        None => {
            row.flag_reason = "unrecognized block shape — cannot place END include".into();
            row.decl = decl;
            return row;
        }
    };
    let routine_end = nth_end_at_or_after(ctx.end_offsets, node_start, n);
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

/// Resolve the post-declaration decl point to a real statement boundary.
///
/// The include takes a whole line (col 1), so it must be inserted *between*
/// statements — never in the middle of one, and never before a `DEFINE …
/// PARAMETER`. Two legacy shapes broke the naive "start of the first executable
/// child" rule:
///
/// - (a) The whole routine body is wrapped in a single `DO:` block that also
///   holds the `DEFINE … PARAMETER` statements. The first *top-level* child is
///   then the `DO`, so anchoring to it lands the include before the parameters
///   (illegal ordering, and the routine-level FINALLY the end-include supplies
///   then attaches to a non-undoable inner block). We descend into that DO — but
///   only when it actually traps parameters — so the decl point lands after them.
///
/// - (b) The routine has no declaration block and its first executable statement
///   is multi-line (e.g. a leading `ASSIGN` whose first target is on the next
///   line). A leaf-span start reports the target line, not the keyword line, so
///   the include splices into the middle of the statement. We instead anchor to
///   the statement's *true* first token.
///
/// Method: find the first executable statement, then walk to the statement
/// boundary that precedes it. ABL's real rule decides the boundary — a `.` or
/// `:` **followed by whitespace/EOF** ends a statement / opens a block, while the
/// same character inside `a.b`, a decimal, or `"x":U` is not a boundary. The decl
/// line is the first significant token after that boundary — the executable's own
/// leading keyword. Any shape that cannot be pinned to such a boundary returns an
/// `Err`, which the caller turns into a non-seedable flag (fail safe, never guess).
fn compute_decl(
    ctx: &WalkCtx,
    node: &Statement,
    body: &[Statement],
) -> Result<(usize, usize), String> {
    let eff = effective_decl_body(body);

    let first_exec = eff.iter().find(|c| {
        !is_declaration(&c.kind) && !is_catch_or_finally(&c.kind) && statement_span(c).is_some()
    });
    let s0 = match first_exec.and_then(statement_span) {
        Some((s, _)) => s,
        None => return Err("no locatable executable statement".into()),
    };

    // Boundary preceding the first executable: prefer the nearest statement-
    // terminating period before it (end of the leading declaration region); if
    // there is none (no declaration block), fall back to the routine header's
    // block-opening colon. Either anchors us cleanly ahead of the executable.
    let node_start = statement_span(node).map(|(s, _)| s).unwrap_or(0);
    let last_period_end = ctx
        .tokens
        .iter()
        .filter(|t| {
            t.kind == Kind::Period
                && t.start >= node_start
                && t.end <= s0
                && ws_or_eof(ctx.src, t.end)
        })
        .map(|t| t.end)
        .max();
    let boundary = match last_period_end {
        Some(e) => e,
        None => ctx
            .tokens
            .iter()
            .find(|t| {
                t.kind == Kind::Colon
                    && t.start >= node_start
                    && t.end <= s0
                    && ws_or_eof(ctx.src, t.end)
            })
            .map(|t| t.end)
            .ok_or_else(|| {
                "cannot locate a statement boundary before the first executable".to_string()
            })?,
    };

    // The executable's true first token = first significant token after the
    // boundary (comments/preproc-end skipped). Bounded by s0 as a safety net.
    let true_start = ctx
        .tokens
        .iter()
        .filter(|t| t.start >= boundary && t.start <= s0 && !is_trivia(t.kind))
        .map(|t| t.start)
        .min()
        .unwrap_or(s0);

    Ok(line_col1(ctx.sm, true_start))
}

/// Descend through a leading whole-body `DO:` envelope that traps the routine's
/// `DEFINE … PARAMETER` statements, returning the body the decl point should be
/// computed against. Parameters may only appear at a routine's top level, so a
/// `DO` whose leading region contains one is never an ordinary control block —
/// it is the legacy "wrap the whole body" idiom (a). A control `DO` (loop,
/// transaction, `IF … THEN DO:`) never holds parameters and is left alone, so the
/// include stays in the routine's top-level block. Bounded against pathological
/// nesting.
fn effective_decl_body(body: &[Statement]) -> &[Statement] {
    let mut cur = body;
    for _ in 0..4 {
        match cur.first().map(|s| &s.kind) {
            Some(SK::Do { body: inner, .. }) if leading_region_has_parameter(inner) => {
                cur = inner;
            }
            _ => break,
        }
    }
    cur
}

/// True when a `DEFINE … PARAMETER` appears in the leading declaration region of
/// `stmts` (before the first executable statement).
fn leading_region_has_parameter(stmts: &[Statement]) -> bool {
    for s in stmts {
        if matches!(s.kind, SK::DefineParameter { .. }) {
            return true;
        }
        // Keep scanning across the declaration region (and spanless artifacts such
        // as DEFINE QUERY, which the parser may not model); stop at the first
        // real executable statement.
        if !is_declaration(&s.kind) && !is_catch_or_finally(&s.kind) && statement_span(s).is_some()
        {
            return false;
        }
    }
    false
}

/// A `.`/`:` at `off` ends a statement / opens a block only when what follows is
/// whitespace or end-of-input — ABL's actual tokenizing rule. This is what tells
/// a statement-terminating `.` apart from the `.` in `db.tbl`, a decimal, or the
/// `:` in `obj:method` and `"x":U`.
fn ws_or_eof(src: &[u8], off: usize) -> bool {
    off >= src.len() || src[off].is_ascii_whitespace()
}

/// Comment / preprocessor-boundary tokens carry no statement structure and are
/// skipped when looking for a statement's true first token.
fn is_trivia(kind: Kind) -> bool {
    matches!(kind, Kind::Comment | Kind::PreprocEnd | Kind::Eof)
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
    matches!(
        kind,
        SK::VariableDeclaration { .. } | SK::DefineParameter { .. }
    ) || variant_name(kind).starts_with("Define")
}

fn is_catch_or_finally(kind: &SK) -> bool {
    matches!(kind, SK::Catch { .. } | SK::Finally { .. })
}

/// Total number of block-closing `END` tokens inside `stmts` — every nested
/// block anywhere in the subtree, not just the rightmost spine. The routine's own
/// END is the one right after all of these, so the caller adds 1 and takes the
/// (total + 1)-th `End` token from the routine start. Counting the *whole* subtree
/// (rather than following the last statement) is what keeps the anchor off inner
/// block ENDs when a trailing statement is spanless or a nested block does not sit
/// on the rightmost spine. `None` means a shape whose END tokens we cannot count
/// reliably (a nested routine, property, etc.), so the caller flags the row.
fn count_block_ends(stmts: &[Statement]) -> Option<usize> {
    let mut total = 0;
    for s in stmts {
        total += block_ends_of(s)?;
    }
    Some(total)
}

fn block_ends_of(s: &Statement) -> Option<usize> {
    match &s.kind {
        // END-closed blocks: their own END, plus every END inside their body.
        SK::Do { body, .. }
        | SK::Repeat { body, .. }
        | SK::ForEach { body, .. }
        | SK::Catch { body, .. }
        | SK::Finally { body, .. } => Some(1 + count_block_ends(body)?),
        SK::Case {
            when_branches,
            otherwise,
            ..
        } => {
            let mut inner = 0;
            for w in when_branches {
                inner += count_block_ends(&w.body)?;
            }
            if let Some(o) = otherwise {
                inner += count_block_ends(o)?;
            }
            Some(1 + inner) // END CASE
        }
        // IF is not END-closed; count the ENDs in both branches.
        SK::If {
            then_branch,
            else_branch,
            ..
        } => {
            let mut c = block_ends_of(then_branch)?;
            if let Some(e) = else_branch {
                c += block_ends_of(e)?;
            }
            Some(c)
        }
        SK::Label { body, .. } => block_ends_of(body),
        SK::Block(body) => count_block_ends(body),
        // `&IF …&THEN …&ELSE …&ENDIF` emits no `END` tokens, but its branches are
        // all present in unpreprocessed source — count the block ENDs in each.
        SK::PreprocIf(p) => {
            let mut c = count_block_ends(&p.then_branch)?;
            for (_, b) in &p.elseif_branches {
                c += count_block_ends(b)?;
            }
            if let Some(e) = &p.else_branch {
                c += count_block_ends(e)?;
            }
            Some(c)
        }
        // Nested routine/property as a body child: not something we can count END
        // tokens through safely — flag for a human.
        SK::Procedure { .. }
        | SK::Function { .. }
        | SK::Method { .. }
        | SK::Class { .. }
        | SK::Interface { .. }
        | SK::Constructor { .. }
        | SK::Destructor { .. }
        | SK::Property { .. } => None,
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
        let start_len = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());
        let start: usize = rest[..start_len].parse().ok()?;
        rest = &rest[start_len..];
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
            "-h" | "--help" => {
                return Err("seeding_inventory: emit span-seeding placement CSV".into());
            }
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

#[cfg(test)]
mod tests {
    //! Placement fixtures. Every snippet here is synthetic, minimal ABL written to
    //! reproduce a *structural shape* — not derived from any real source. Names are
    //! deliberately neutral (doStuff, cFoo, ttThing, …).
    use super::*;

    /// Run the same walk `main` runs, over an in-memory snippet.
    fn rows_for(src: &str) -> Vec<Row> {
        let tokens = tokenize(src);
        let end_offsets: Vec<usize> = tokens
            .iter()
            .filter(|t| t.kind == Kind::End)
            .map(|t| t.start)
            .collect();
        let mut parser = Parser::new(&tokens, src);
        let program = parser.parse_program();
        let sm = SourceMap::new(src);
        let ctx = WalkCtx {
            sm: &sm,
            end_offsets: &end_offsets,
            tokens: &tokens,
            src: src.as_bytes(),
            file: "fixture.p",
        };
        let mut rows = Vec::new();
        walk_routines(&program.statements, None, false, &ctx, &mut rows);
        rows
    }

    fn row<'a>(rows: &'a [Row], name: &str) -> &'a Row {
        rows.iter()
            .find(|r| r.span_name == name)
            .expect("row exists")
    }

    /// Trimmed text of a 1-based source line — what the include would be inserted
    /// *before*.
    fn line_text(src: &str, line: usize) -> String {
        src.lines().nth(line - 1).unwrap_or("").trim().to_string()
    }

    /// 1-based line a substring first appears on.
    fn line_of(src: &str, needle: &str) -> usize {
        src.lines()
            .position(|l| l.contains(needle))
            .expect("needle present")
            + 1
    }

    // ── Shape (a): DEFINE … PARAMETER trapped inside a leading whole-body DO. ──
    // The naive "first top-level child" rule anchors to the DO and lands the decl
    // point before the parameters; the decl point must instead fall after them,
    // at the first executable inside the DO.
    const SHAPE_A: &str = "\
PROCEDURE doStuff:
DO:
    DEFINE INPUT PARAMETER pThing AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER pResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoo AS CHARACTER NO-UNDO.
    cFoo = \"x\".
    pResult = cFoo.
END.
END PROCEDURE.
";

    #[test]
    fn shape_a_params_in_do_lands_after_params() {
        let rows = rows_for(SHAPE_A);
        let r = row(&rows, "doStuff");
        assert!(r.seedable, "should be seedable: {}", r.flag_reason);
        let (dl, dc) = r.decl.expect("decl point");
        assert_eq!(dc, 1, "include takes a whole line");
        // Lands on the first executable, not before the parameters.
        assert_eq!(line_text(SHAPE_A, dl), "cFoo = \"x\".");
        let last_param = line_of(SHAPE_A, "DEFINE OUTPUT PARAMETER");
        assert!(
            dl > last_param,
            "decl {dl} must be past the params (line {last_param})"
        );
    }

    // ── Shape (b): no declaration block; first statement is a multi-line ASSIGN. ─
    // A leaf-span start reports the first *target* line; the decl point must be the
    // ASSIGN keyword line so the include is not spliced into the statement.
    const SHAPE_B: &str = "\
PROCEDURE assignFirst:
   ASSIGN
      cFoo = \"a\"
      cBar = \"b\".
   cBaz = \"c\".
END PROCEDURE.
";

    #[test]
    fn shape_b_multiline_assign_lands_on_keyword_line() {
        let rows = rows_for(SHAPE_B);
        let r = row(&rows, "assignFirst");
        assert!(r.seedable, "should be seedable: {}", r.flag_reason);
        let (dl, dc) = r.decl.expect("decl point");
        assert_eq!(dc, 1);
        // The ASSIGN keyword line — never a line in the middle of the statement.
        assert_eq!(line_text(SHAPE_B, dl), "ASSIGN");
        assert!(
            !line_text(SHAPE_B, dl).starts_with("cFoo"),
            "must not splice mid-ASSIGN"
        );
    }

    // ── Control: ordinary declaration block, then executable. ──
    const NORMAL: &str = "\
PROCEDURE plain:
   DEFINE INPUT PARAMETER pIn AS INTEGER NO-UNDO.
   DEFINE VARIABLE cFoo AS CHARACTER NO-UNDO.

   cFoo = \"y\".
   pIn = 1.
END PROCEDURE.
";

    #[test]
    fn normal_decl_block_lands_after_declarations() {
        let rows = rows_for(NORMAL);
        let r = row(&rows, "plain");
        assert!(r.seedable);
        let (dl, _) = r.decl.expect("decl point");
        assert_eq!(line_text(NORMAL, dl), "cFoo = \"y\".");
        assert!(dl > line_of(NORMAL, "DEFINE VARIABLE"));
    }

    // ── Control: a genuine control DO (loop) with a local var but NO parameter
    // must NOT be descended into — the include stays in the routine's top-level
    // block, ahead of the DO. ──
    const CONTROL_DO: &str = "\
PROCEDURE looper:
   DEFINE VARIABLE iIdx AS INTEGER NO-UNDO.
   DO iIdx = 1 TO 10:
      DEFINE VARIABLE cTmp AS CHARACTER NO-UNDO.
      cTmp = STRING(iIdx).
   END.
END PROCEDURE.
";

    #[test]
    fn control_do_without_params_is_not_descended() {
        let rows = rows_for(CONTROL_DO);
        let r = row(&rows, "looper");
        assert!(r.seedable);
        let (dl, _) = r.decl.expect("decl point");
        // Anchored to the DO header (top level), not inside the loop body.
        assert!(
            line_text(CONTROL_DO, dl).starts_with("DO "),
            "got: {}",
            line_text(CONTROL_DO, dl)
        );
        assert!(dl < line_of(CONTROL_DO, "cTmp = STRING"));
    }

    // ── Fail-safe: an empty routine body has no statement boundary to anchor to
    // and is flagged non-seedable rather than guessed. ──
    const EMPTY_BODY: &str = "\
PROCEDURE hollow:
END PROCEDURE.
";

    #[test]
    fn empty_body_is_flagged_non_seedable() {
        let rows = rows_for(EMPTY_BODY);
        let r = row(&rows, "hollow");
        assert!(!r.seedable);
        assert_eq!(r.flag_reason, "no locatable executable statement");
        assert!(r.decl.is_none());
    }

    // ── Determinism: identical input yields identical rows. ──
    #[test]
    fn determinism_same_input_same_rows() {
        let a = rows_for(SHAPE_A);
        let b = rows_for(SHAPE_A);
        assert_eq!(a.len(), b.len());
        for (x, y) in a.iter().zip(b.iter()) {
            assert_eq!(x.span_name, y.span_name);
            assert_eq!(x.decl, y.decl);
            assert_eq!(x.end, y.end);
            assert_eq!(x.seedable, y.seedable);
        }
    }

    // ════════════════════════════════════════════════════════════════════════
    // END-point resolution: the pre-END insert must land on the ROUTINE's own
    // END, never an inner block's. The routine close is written `END PROCEDURE.`
    // in these fixtures; every inner block closes with a bare `END.` (or
    // `END CATCH.`/`END FINALLY.`), so asserting the end line starts with
    // `END PROCEDURE` proves it is the routine block, not a nested one.
    // ════════════════════════════════════════════════════════════════════════

    /// Trimmed end-point line for the single routine in `src`.
    fn end_line_text(src: &str, name: &str) -> String {
        let rows = rows_for(src);
        let r = row(&rows, name);
        assert!(r.seedable, "{name} should be seedable: {}", r.flag_reason);
        let (el, ec) = r.end.expect("end point");
        assert_eq!(ec, 1);
        line_text(src, el)
    }

    // (1) Trailing plain inner DO … END right before the routine END. The DO's
    // last child is a spanless RETURN — the exact mechanism that let the old
    // rightmost-spine counter miss the inner DO's END and anchor there.
    const END_TRAILING_DO: &str = "\
PROCEDURE endsWithDo:
   DEFINE VARIABLE iX AS INTEGER NO-UNDO.
   iX = 1.
   DO:
      iX = 2.
      RETURN.
   END.
END PROCEDURE.
";
    #[test]
    fn end_trailing_inner_do_resolves_to_routine_end() {
        assert_eq!(
            end_line_text(END_TRAILING_DO, "endsWithDo"),
            "END PROCEDURE."
        );
    }

    // (2) Whole-body DO envelope with a trapped parameter and an inner IF-DO whose
    // branch ends in a spanless RETURN (the save-key shape).
    const END_WHOLE_BODY_DO: &str = "\
PROCEDURE wholeBodyDo:
DO:
   DEFINE INPUT PARAMETER pIn AS INTEGER NO-UNDO.
   DEFINE VARIABLE iX AS INTEGER NO-UNDO.
   iX = pIn.
   IF iX > 0 THEN
   DO:
      iX = 0.
      RETURN.
   END.
END.
END PROCEDURE.
";
    #[test]
    fn end_whole_body_do_resolves_to_routine_end() {
        let rows = rows_for(END_WHOLE_BODY_DO);
        let r = row(&rows, "wholeBodyDo");
        assert!(r.seedable, "{}", r.flag_reason);
        // decl after the trapped parameter (inside the DO), end on the routine END.
        let (dl, _) = r.decl.expect("decl");
        assert!(dl > line_of(END_WHOLE_BODY_DO, "DEFINE INPUT PARAMETER"));
        assert_eq!(
            end_line_text(END_WHOLE_BODY_DO, "wholeBodyDo"),
            "END PROCEDURE."
        );
    }

    // (3) Trailing CATCH after an inner DO — the FINALLY must land after END CATCH,
    // at the routine END.
    const END_CATCH_AFTER_DO: &str = "\
PROCEDURE catchAfterDo:
   DEFINE VARIABLE iX AS INTEGER NO-UNDO.
   DO:
      iX = 1.
   END.
   CATCH eErr AS Progress.Lang.Error:
      iX = 2.
   END CATCH.
END PROCEDURE.
";
    #[test]
    fn end_trailing_catch_lands_after_catch_at_routine_end() {
        let rows = rows_for(END_CATCH_AFTER_DO);
        let r = row(&rows, "catchAfterDo");
        assert!(r.seedable, "{}", r.flag_reason);
        let (el, _) = r.end.expect("end");
        assert_eq!(line_text(END_CATCH_AFTER_DO, el), "END PROCEDURE.");
        assert!(
            el > line_of(END_CATCH_AFTER_DO, "END CATCH."),
            "FINALLY must follow the CATCH"
        );
    }

    // (4) Existing FINALLY after an inner DO — merge into the ROUTINE's FINALLY,
    // do not emit an end-include.
    const END_FINALLY_AFTER_DO: &str = "\
PROCEDURE finallyAfterDo:
   DEFINE VARIABLE iX AS INTEGER NO-UNDO.
   DO:
      iX = 1.
   END.
   FINALLY:
      iX = 2.
   END FINALLY.
END PROCEDURE.
";
    #[test]
    fn end_existing_finally_merges_into_routine_finally() {
        let rows = rows_for(END_FINALLY_AFTER_DO);
        let r = row(&rows, "finallyAfterDo");
        assert!(r.seedable, "{}", r.flag_reason);
        assert!(r.has_finally, "routine-level FINALLY should be detected");
        assert!(r.end.is_none(), "no end-include when merging");
        let (ml, mc) = r.merge.expect("merge point");
        assert_eq!(mc, 1);
        // Merge line is the FINALLY body's first statement, inside the routine's
        // own FINALLY (past the inner DO's END).
        assert_eq!(line_text(END_FINALLY_AFTER_DO, ml), "iX = 2.");
        assert!(ml > line_of(END_FINALLY_AFTER_DO, "FINALLY:"));
    }

    // (5) Nested DO within DO.
    const END_NESTED_DO: &str = "\
PROCEDURE nestedDo:
   DEFINE VARIABLE iX AS INTEGER NO-UNDO.
   DO:
      DO:
         iX = 1.
      END.
   END.
END PROCEDURE.
";
    #[test]
    fn end_nested_do_resolves_to_routine_end() {
        assert_eq!(end_line_text(END_NESTED_DO, "nestedDo"), "END PROCEDURE.");
    }

    // (6) Determinism re-check across the end-resolution fixtures.
    #[test]
    fn end_resolution_is_deterministic() {
        for src in [
            END_TRAILING_DO,
            END_WHOLE_BODY_DO,
            END_CATCH_AFTER_DO,
            END_FINALLY_AFTER_DO,
            END_NESTED_DO,
        ] {
            let a = rows_for(src);
            let b = rows_for(src);
            assert_eq!(a.len(), b.len());
            for (x, y) in a.iter().zip(b.iter()) {
                assert_eq!(
                    (&x.span_name, x.decl, x.end, x.merge, x.seedable),
                    (&y.span_name, y.decl, y.end, y.merge, y.seedable)
                );
            }
        }
    }
}
