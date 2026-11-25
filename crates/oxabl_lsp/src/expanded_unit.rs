//! `oxabl/expandedUnit`: the source oxabl parsed for a compilation unit, after
//! preprocessing, for a read-only inspection view.
//!
//! The answer is the expanded text with a marker line before and after each
//! include expansion, so a client can fold each one, a map from each line to
//! the physical line it came from, the include chain of each region, the
//! findings of the same analysis placed in the expanded text with their
//! physical location, and what made the expansion reduced. An open include
//! uses its selected compilation context, or is expanded alone in fragment mode.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use lsp_server::{ErrorCode, Message, Request, Response};
use lsp_types::{DiagnosticSeverity, Position, PositionEncodingKind, Range, Uri};
use oxabl_analyze::{
    CollectedDiagnostics, DiagnosticSource, ExpandedFile, IncludeExpansion, TextLossKind,
};
use oxabl_common::{Diagnostic, FileId, Severity};
use oxabl_daemon::canonical_path;
use oxabl_daemon::db::{ExpandedUnit, compute_expanded_unit};
use ropey::Rope;
use serde::{Deserialize, Serialize};

use crate::position::byte_to_position;
use crate::{Server, path_to_uri, uri_to_path};

pub(crate) const METHOD: &str = "oxabl/expandedUnit";

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Params {
    text_document: lsp_types::TextDocumentIdentifier,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Answer {
    root: PathBuf,
    /// `root`, or `fragment` for an include expanded alone.
    mode: &'static str,
    /// `complete`, `reduced` or `failed`.
    status: &'static str,
    problems: Vec<Problem>,
    text: String,
    regions: Vec<Region>,
    /// Each line's physical source: an index into `files` and a 0-based line.
    lines: Vec<Option<(usize, u32)>>,
    files: Vec<String>,
    diagnostics: Vec<Finding>,
    /// Every file the expansion read, so a client knows when to ask again.
    dependencies: Vec<String>,
}

#[derive(Serialize)]
struct Problem {
    message: String,
    location: Option<Location>,
}

#[derive(Serialize, Clone)]
struct Location {
    uri: String,
    range: Range,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Region {
    /// The lines of the opening and closing marker.
    start_line: u32,
    end_line: u32,
    path: PathBuf,
    uri: String,
    depth: u32,
    /// The `{...}` sites from the root down to this include, outermost first.
    chain: Vec<Location>,
}

#[derive(Serialize)]
struct Finding {
    range: Range,
    severity: DiagnosticSeverity,
    code: String,
    message: String,
    /// Where the finding is in the file that holds it.
    source: Location,
}

impl Server<'_> {
    pub(crate) fn handle_expanded_unit(&mut self, req: Request) -> anyhow::Result<()> {
        let params: Params = match serde_json::from_value(req.params) {
            Ok(params) => params,
            Err(error) => {
                return self.respond(Response::new_err(
                    req.id,
                    ErrorCode::InvalidParams as i32,
                    format!("oxabl-lsp: invalid {METHOD} params: {error}"),
                ));
            }
        };
        let uri = params.text_document.uri;
        let Some(path) = uri_to_path(&uri) else {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::InvalidParams as i32,
                format!("oxabl-lsp: {} is not a file", uri.as_str()),
            ));
        };
        let path = canonical_path(&path);
        let (root, mode) = if oxabl_workspace::is_include_fragment(&path) {
            match self.contexts.selected(&uri) {
                Some(root) => (root.clone(), "root"),
                None => (path, "fragment"),
            }
        } else {
            (path, "root")
        };
        let Some((snapshot, schema, overlay)) = self.host.with(|sessions| {
            let session = sessions.get(&self.session_root)?;
            Some((
                session.database().clone(),
                session.schema_handle(),
                session.buffer_overlay(),
            ))
        }) else {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::RequestFailed as i32,
                "oxabl-lsp: no session".to_string(),
            ));
        };
        let encoding = self.encoding.clone();
        let sender = self.connection.sender.clone();
        let id = req.id.clone();
        let spawned = oxabl_common::stack::builder()
            .name("oxabl-expanded-unit".to_string())
            .spawn({
                let sender = sender.clone();
                move || {
                    let answered = oxabl_common::catch_panic(|| {
                        compute_expanded_unit(&snapshot, schema, &root, overlay)
                            .map(|unit| answer(&root, mode, unit, &encoding))
                    });
                    let response = match answered {
                        Ok(Some(answer)) => Response::new_ok(id, answer),
                        Ok(None) => Response::new_err(
                            id,
                            ErrorCode::ContentModified as i32,
                            "oxabl-lsp: an edit replaced this expansion".to_string(),
                        ),
                        Err(panic) => Response::new_err(
                            id,
                            ErrorCode::InternalError as i32,
                            format!("oxabl-lsp: the expansion failed: {panic}"),
                        ),
                    };
                    let _ = sender.send(Message::Response(response));
                }
            });
        if let Err(error) = spawned {
            return self.respond(Response::new_err(
                req.id,
                ErrorCode::InternalError as i32,
                format!("oxabl-lsp: cannot start the expansion: {error}"),
            ));
        }
        Ok(())
    }
}

fn answer(
    root: &Path,
    mode: &'static str,
    unit: Result<ExpandedUnit, String>,
    encoding: &PositionEncodingKind,
) -> Answer {
    let unit = match unit {
        Ok(unit) => unit,
        Err(reason) => {
            return Answer {
                root: root.to_path_buf(),
                mode,
                status: "failed",
                problems: vec![Problem {
                    message: format!("{} gives no expansion: {reason}", root.display()),
                    location: None,
                }],
                text: format!(
                    "/* oxabl: no expanded compilation unit for {}: {reason} */\n",
                    root.display()
                ),
                regions: Vec::new(),
                lines: vec![None],
                files: Vec::new(),
                diagnostics: Vec::new(),
                dependencies: uri_strings(&[root.to_path_buf()]),
            };
        }
    };
    let Some(expanded) = unit.expansion.expanded() else {
        // `compute_expanded_unit` refuses a failed expansion, so this is not reached.
        return answer(
            root,
            mode,
            Err("its preprocessing failed".to_string()),
            encoding,
        );
    };
    Renderer::new(&unit, expanded, mode, encoding).render()
}

/// The index of `path` in `files`, added when it is not there.
fn file_index(files: &mut Vec<PathBuf>, path: &Path) -> usize {
    let path = canonical_path(path);
    match files.iter().position(|known| *known == path) {
        Some(at) => at,
        None => {
            files.push(path);
            files.len() - 1
        }
    }
}

/// The URIs of `paths`, each once.
fn uri_strings(paths: &[PathBuf]) -> Vec<String> {
    let mut uris: Vec<String> = Vec::with_capacity(paths.len());
    for uri in paths.iter().map(|path| uri_string(path)) {
        if !uris.contains(&uri) {
            uris.push(uri);
        }
    }
    uris
}

/// The URI of a path as the file it names: an include found through `.` on
/// the include path is read as `dir/./name.i`.
fn uri_string(path: &Path) -> String {
    path_to_uri(&canonical_path(path)).map_or_else(String::new, |uri: Uri| uri.as_str().to_string())
}

/// One file the expansion read: its text and where its lines start.
struct Source {
    rope: Rope,
    line_starts: Vec<u32>,
}

impl Source {
    fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        line_starts.extend(
            text.bytes()
                .enumerate()
                .filter(|(_, byte)| *byte == b'\n')
                .map(|(at, _)| at as u32 + 1),
        );
        Source {
            rope: Rope::from_str(text),
            line_starts,
        }
    }

    fn line_of(&self, offset: u32) -> u32 {
        (self.line_starts.partition_point(|start| *start <= offset) - 1) as u32
    }
}

enum Event {
    Start(usize),
    End(usize),
    Elided,
}

struct Renderer<'a> {
    unit: &'a ExpandedUnit,
    expanded: &'a ExpandedFile,
    mode: &'static str,
    encoding: &'a PositionEncodingKind,
    sources: HashMap<PathBuf, Source>,
    out: String,
    /// `(expanded offset, output offset, length)` of each copied stretch.
    segments: Vec<(u32, u32, u32)>,
    /// The output offset of each marker line, and the expansion it marks.
    markers: Vec<(u32, Option<usize>)>,
}

impl<'a> Renderer<'a> {
    fn new(
        unit: &'a ExpandedUnit,
        expanded: &'a ExpandedFile,
        mode: &'static str,
        encoding: &'a PositionEncodingKind,
    ) -> Self {
        let sources = unit
            .sources
            .iter()
            .map(|(path, text): (&PathBuf, &Arc<str>)| (path.clone(), Source::new(text)))
            .collect();
        Renderer {
            unit,
            expanded,
            mode,
            encoding,
            sources,
            out: String::new(),
            segments: Vec::new(),
            markers: Vec::new(),
        }
    }

    /// The file that wrote a byte of the expansion: the root, or an include.
    fn path_of(&self, file: FileId) -> Option<&'a Path> {
        if file == self.expanded.root_file() {
            Some(&self.unit.root)
        } else {
            self.expanded.file_path(file)
        }
    }

    fn location(&self, path: &Path, start: u32, end: u32) -> Location {
        let range = match self.sources.get(path) {
            Some(source) => Range::new(
                byte_to_position(&source.rope, start as usize, self.encoding),
                byte_to_position(&source.rope, end as usize, self.encoding),
            ),
            None => Range::default(),
        };
        Location {
            uri: uri_string(path),
            range,
        }
    }

    fn render(mut self) -> Answer {
        let expansions = self.expanded.include_expansions();
        let depth = |mut at: usize| {
            let mut depth = 1u32;
            while let Some(parent) = expansions[at].parent {
                depth += 1;
                at = parent;
            }
            depth
        };
        let problems = self.problems();
        let status = if problems.is_empty() {
            "complete"
        } else {
            "reduced"
        };
        self.header(status, &problems);

        let mut events: Vec<(u32, u8, i64, Event)> = Vec::new();
        for (at, expansion) in expansions.iter().enumerate() {
            if expansion.expanded.end > expansion.expanded.start {
                let d = i64::from(depth(at));
                events.push((expansion.expanded.start, 2, d, Event::Start(at)));
                events.push((expansion.expanded.end, 0, -d, Event::End(at)));
            }
        }
        for offset in self.expanded.elided_include_offsets() {
            events.push((*offset, 1, 0, Event::Elided));
        }
        events.sort_by_key(|(offset, rank, order, _)| (*offset, *rank, *order));

        let text = self.expanded.text.as_str();
        let mut at = 0u32;
        for (offset, _, _, event) in events {
            self.copy(text, at, offset);
            at = offset.max(at);
            let line = match event {
                Event::Start(index) => {
                    let expansion = &expansions[index];
                    format!(
                        "/* ▼ {} {} */",
                        expansion
                            .path
                            .file_name()
                            .map_or_else(String::new, |name| name.to_string_lossy().into_owned()),
                        self.chain_text(&expansions, index)
                    )
                }
                Event::End(index) => format!(
                    "/* ▲ {} */",
                    expansions[index]
                        .path
                        .file_name()
                        .map_or_else(String::new, |name| name.to_string_lossy().into_owned())
                ),
                Event::Elided => "/* ✖ an include did not arrive here: not found, unreadable, a cycle or past the depth limit */".to_string(),
            };
            let index = match event {
                Event::Start(index) | Event::End(index) => Some(index),
                Event::Elided => None,
            };
            self.marker(&line, index);
        }
        self.copy(text, at, text.len() as u32);

        let out_source = Source::new(&self.out);
        let mut files: Vec<PathBuf> = Vec::new();
        // Regions, from the marker lines.
        let mut opened: HashMap<usize, u32> = HashMap::new();
        let mut regions = Vec::new();
        for (offset, index) in &self.markers {
            let Some(index) = index else { continue };
            let line = out_source.line_of(*offset);
            match opened.remove(index) {
                None => {
                    opened.insert(*index, line);
                }
                Some(start_line) => {
                    let path = expansions[*index].path;
                    regions.push(Region {
                        start_line,
                        end_line: line,
                        path: canonical_path(path),
                        uri: uri_string(path),
                        depth: depth(*index),
                        chain: self.chain(&expansions, *index),
                    });
                }
            }
        }
        regions.sort_by_key(|region| (region.start_line, region.depth));

        // Each line's physical source.
        let mut lines = Vec::with_capacity(out_source.line_starts.len());
        for start in &out_source.line_starts {
            let marker = self.markers.iter().find(|(offset, _)| offset == start);
            let physical = match marker {
                Some((_, Some(index))) => Some((expansions[*index].path, 0)),
                Some((_, None)) => None,
                None => self.expanded_at(*start).and_then(|virt| {
                    let (file, real) = self.expanded.resolve_offset(virt)?;
                    let path = self.path_of(file)?;
                    let line = self.sources.get(path)?.line_of(real);
                    Some((path, line))
                }),
            };
            lines.push(physical.map(|(path, line)| (file_index(&mut files, path), line)));
        }

        let diagnostics = self.findings(&out_source);
        Answer {
            root: self.unit.root.clone(),
            mode: self.mode,
            status,
            problems,
            text: self.out,
            regions,
            lines,
            files: files.iter().map(|path| uri_string(path)).collect(),
            diagnostics,
            dependencies: uri_strings(&self.unit.dependencies()),
        }
    }

    fn header(&mut self, status: &str, problems: &[Problem]) {
        let mode = match self.mode {
            "fragment" => " in fragment mode (the include alone, with no root)",
            _ => "",
        };
        self.out.push_str(&format!(
            "/* oxabl: the expanded compilation unit of {}{mode}, read-only.\n",
            self.unit.root.display()
        ));
        self.out.push_str(&format!("   Expansion: {status}.\n"));
        for problem in problems {
            self.out
                .push_str(&format!("   - {}\n", problem.message.replace("*/", "* /")));
        }
        self.out
            .push_str("   Each include is between a ▼ line and a ▲ line. */\n");
    }

    /// Copy the expanded text `[from, to)`.
    fn copy(&mut self, text: &str, from: u32, to: u32) {
        if to <= from {
            return;
        }
        let Some(piece) = text.get(from as usize..to as usize) else {
            return;
        };
        self.segments.push((from, self.out.len() as u32, to - from));
        self.out.push_str(piece);
    }

    fn marker(&mut self, line: &str, index: Option<usize>) {
        if !self.out.is_empty() && !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.markers.push((self.out.len() as u32, index));
        self.out.push_str(line);
        self.out.push('\n');
    }

    /// The expanded offset an output offset copies, if it copies one.
    fn expanded_at(&self, out: u32) -> Option<u32> {
        let at = self.segments.partition_point(|(_, start, _)| *start <= out);
        let (virt, start, len) = self.segments.get(at.checked_sub(1)?)?;
        (out < start + len).then(|| virt + (out - start))
    }

    /// The output offset of an expanded offset.
    fn output_at(&self, virt: u32) -> Option<u32> {
        let at = self
            .segments
            .partition_point(|(start, _, _)| *start <= virt);
        let (start, out, len) = self.segments.get(at.checked_sub(1)?)?;
        (virt <= start + len).then(|| out + (virt - start))
    }

    /// The `{...}` sites from the root down to expansion `index`.
    fn chain(&self, expansions: &[IncludeExpansion<'_>], index: usize) -> Vec<Location> {
        let mut steps = Vec::new();
        let mut at = Some(index);
        while let Some(current) = at {
            let expansion = &expansions[current];
            let writer = match expansion.parent {
                Some(parent) => expansions[parent].path,
                None => self.unit.root.as_path(),
            };
            if let Some(site) = expansion.site {
                steps.push(self.location(writer, site.start, site.end));
            }
            at = expansion.parent;
        }
        steps.reverse();
        steps
    }

    fn chain_text(&self, expansions: &[IncludeExpansion<'_>], index: usize) -> String {
        let mut names = Vec::new();
        let mut at = Some(index);
        while let Some(current) = at {
            let expansion = &expansions[current];
            let writer = match expansion.parent {
                Some(parent) => expansions[parent].path,
                None => self.unit.root.as_path(),
            };
            let name = writer
                .file_name()
                .map_or_else(String::new, |name| name.to_string_lossy().into_owned());
            names.push(match expansion.site {
                Some(site) => {
                    let line = self
                        .sources
                        .get(writer)
                        .map_or(0, |s| s.line_of(site.start));
                    format!("{name}:{}", line + 1)
                }
                None => name,
            });
            at = expansion.parent;
        }
        names.reverse();
        format!(
            "{} (from {})",
            canonical_path(expansions[index].path).display(),
            names.join(" → ")
        )
    }

    /// What makes the expansion reduced.
    fn problems(&self) -> Vec<Problem> {
        let mut problems = Vec::new();
        let at = |file: FileId, start: u32, end: u32| {
            self.path_of(file)
                .map(|path| self.location(path, start, end))
        };
        for include in self.expanded.unresolved_includes() {
            problems.push(Problem {
                message: format!("include `{}` was not found", include.name),
                location: at(
                    include.site.file,
                    include.site.span.start,
                    include.site.span.end,
                ),
            });
        }
        for include in self.expanded.nested_unresolved_includes() {
            problems.push(Problem {
                message: format!("include `{}` was not found", include.name),
                location: at(
                    include.site.file,
                    include.site.span.start,
                    include.site.span.end,
                ),
            });
        }
        for include in self.expanded.unreadable_includes() {
            problems.push(Problem {
                message: format!(
                    "include `{}` ({}) could not be read",
                    include.name,
                    include.path.display()
                ),
                location: at(
                    include.site.file,
                    include.site.span.start,
                    include.site.span.end,
                ),
            });
        }
        for loss in self.expanded.text_losses() {
            problems.push(Problem {
                message: text_loss(loss.kind).to_string(),
                location: at(loss.site.file, loss.site.span.start, loss.site.span.end),
            });
        }
        for diagnostic in self.expanded.nested_preprocessor_diagnostics() {
            problems.push(Problem {
                message: format!("{}: {}", diagnostic.code.0, diagnostic.message),
                location: at(
                    diagnostic.span.file,
                    diagnostic.span.span.start,
                    diagnostic.span.span.end,
                ),
            });
        }
        // An include reached through two problems is one line, not two.
        problems.dedup_by(|a, b| {
            a.message == b.message
                && a.location.as_ref().map(|l| (&l.uri, l.range))
                    == b.location.as_ref().map(|l| (&l.uri, l.range))
        });
        problems
    }

    /// Every finding of the analysis, placed in the output with its physical
    /// location. A finding whose text the expansion did not keep sits on the
    /// first line.
    fn findings(&self, out_source: &Source) -> Vec<Finding> {
        let diagnostics: &CollectedDiagnostics = &self.unit.diagnostics;
        let root_file = self.expanded.root_file();
        let mut findings = Vec::new();
        let mut place = |diagnostic: &Diagnostic, file: FileId, path: &Path, at_site: bool| {
            let span = diagnostic.span.span;
            let width = span.end.saturating_sub(span.start);
            // A parse error in an include is also reported at the include's
            // `{...}` site, whose bytes the expansion did not keep: it sits
            // where the text of that site starts.
            let (virt, width) = match self.expanded.expanded_offset(file, span.start) {
                Some(virt) => (Some(virt), width),
                None if at_site => (self.expanded.include_site_start(span), 0),
                None => (None, width),
            };
            let range = virt
                .and_then(|virt| self.output_at(virt))
                .map(|start| {
                    Range::new(
                        byte_to_position(&out_source.rope, start as usize, self.encoding),
                        byte_to_position(&out_source.rope, (start + width) as usize, self.encoding),
                    )
                })
                .unwrap_or(Range::new(Position::new(0, 0), Position::new(0, 0)));
            findings.push(Finding {
                range,
                severity: severity(diagnostic.severity),
                code: diagnostic.code.0.to_string(),
                message: diagnostic.message.clone(),
                source: self.location(path, span.start, span.end),
            });
        };
        for collected in diagnostics.all() {
            place(
                &collected.diagnostic,
                root_file,
                &self.unit.root,
                collected.source == DiagnosticSource::Parse,
            );
        }
        for finding in &diagnostics.include_origin {
            place(
                &finding.diagnostic,
                finding.diagnostic.span.file,
                &finding.path,
                false,
            );
        }
        for diagnostic in self.expanded.nested_preprocessor_diagnostics() {
            if let Some(path) = self.expanded.file_path(diagnostic.span.file) {
                place(diagnostic, diagnostic.span.file, path, false);
            }
        }
        findings
    }
}

fn text_loss(kind: TextLossKind) -> &'static str {
    match kind {
        TextLossKind::IncludeDepthLimit => "an include is past the include depth limit",
        TextLossKind::ExpansionLimit => "the preprocessor reached its expansion limit",
        TextLossKind::UnclosedConditional => "an &IF is never closed",
        TextLossKind::UnknownPredefinedName => {
            "an &IF was decided on a predefined name with no value"
        }
        TextLossKind::UnmodelledCondition => {
            "an &IF was decided on a condition oxabl does not model"
        }
        TextLossKind::UnmatchedArgumentBrace => {
            "an include argument leaves a brace of an include reference unmatched, or ends it early"
        }
        TextLossKind::ArgumentEndsWithTilde => "a substituted value ends with ~",
        _ => "the preprocessor dropped text",
    }
}

fn severity(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Info => DiagnosticSeverity::INFORMATION,
        Severity::Hint => DiagnosticSeverity::HINT,
    }
}
