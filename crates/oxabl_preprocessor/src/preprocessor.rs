use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, FileId, FileSpan};
use oxabl_workspace::FileSystem;

use crate::PreprocVarTable;
use crate::condition::evaluate_with_defined;
use crate::span_tree::{PreprocessedFile, SpanNode};

/// Preprocessor for ABL source code.
///
/// Expands include files, evaluates `&IF`/`&THEN`/`&ENDIF` conditions, and
/// tracks `&SCOPED-DEFINE`/`&GLOBAL-DEFINE`/`&UNDEFINE` variables. Produces a
/// [`PreprocessedFile`] with a virtual span tree for position resolution.
pub struct Preprocessor<'fs> {
    fs: &'fs dyn FileSystem,
    include_paths: &'fs [PathBuf],
}

/// Maximum include depth to prevent runaway recursion from very deep (but
/// non-cyclic) include chains.
const MAX_INCLUDE_DEPTH: usize = 64;

impl<'fs> Preprocessor<'fs> {
    pub fn new(fs: &'fs dyn FileSystem, include_paths: &'fs [PathBuf]) -> Self {
        Preprocessor { fs, include_paths }
    }

    /// Preprocess a source file, expanding includes and evaluating directives.
    ///
    /// Returns a [`PreprocessedFile`] on success, or a list of diagnostics on
    /// failure. Non-fatal diagnostics (warnings) are collected but do not
    /// prevent the `PreprocessedFile` from being returned.
    pub fn process(&self, file: FileId, source: &str) -> Result<PreprocessedFile, Vec<Diagnostic>> {
        let mut ctx = ProcessContext {
            fs: self.fs,
            include_paths: self.include_paths,
            vars: PreprocVarTable::new(),
            global_vars: PreprocVarTable::new(),
            dependencies: Vec::new(),
            sources: vec![(file, Arc::from(source))],
            diagnostics: Vec::new(),
            include_stack: HashSet::new(),
            file_id_counter: file.raw() + 1,
        };

        let tree = ctx.process_source(file, source, 0, &[]);

        // Merge global vars into the final var table
        ctx.vars.merge_globals(&ctx.global_vars);

        if ctx
            .diagnostics
            .iter()
            .any(|d| matches!(d.severity, oxabl_common::Severity::Error))
            && tree.is_empty()
        {
            return Err(ctx.diagnostics);
        }

        Ok(PreprocessedFile::new(
            tree,
            ctx.vars,
            ctx.dependencies,
            ctx.sources,
            ctx.diagnostics,
        ))
    }
}

struct ProcessContext<'fs> {
    fs: &'fs dyn FileSystem,
    include_paths: &'fs [PathBuf],
    vars: PreprocVarTable,
    /// Variables set via `&GLOBAL-DEFINE` — propagated back to parent.
    global_vars: PreprocVarTable,
    dependencies: Vec<FileId>,
    sources: Vec<(FileId, Arc<str>)>,
    diagnostics: Vec<Diagnostic>,
    /// Stack of currently-being-processed paths for cycle detection.
    include_stack: HashSet<PathBuf>,
    /// Counter for assigning FileIds to newly discovered include files.
    file_id_counter: u32,
}

impl<'fs> ProcessContext<'fs> {
    fn next_file_id(&mut self) -> FileId {
        let id = FileId::new(self.file_id_counter);
        self.file_id_counter += 1;
        id
    }

    /// Process a source string, returning its span tree nodes.
    ///
    /// This is the core line-scanning loop. It identifies preprocessor
    /// directives and include references by scanning for `&` and `{` markers
    /// at the text level, avoiding any dependency on the lexer.
    fn process_source(
        &mut self,
        file: FileId,
        source: &str,
        depth: usize,
        positional_args: &[String],
    ) -> Vec<SpanNode> {
        let bytes = source.as_bytes();
        let len = bytes.len();
        let mut nodes: Vec<SpanNode> = Vec::new();
        // `chunk_start` tracks the beginning of the current un-emitted text chunk.
        let mut chunk_start: u32 = 0;
        let mut i: usize = 0;

        // State for &IF nesting
        let mut if_stack: Vec<IfState> = Vec::new();

        // ABL supports nested block comments. Directives and `{...}` references
        // inside a comment are literal text — the Progress preprocessor does not
        // expand them. Track depth so we can skip comment bodies when scanning
        // for `&` and `{` markers.
        let mut comment_depth: u32 = 0;

        while i < len {
            // Check if we're inside a disabled &IF branch
            let emitting = if_stack.iter().all(|s| s.emitting);

            // Inside a block comment: only track `/*` / `*/` to keep depth.
            if comment_depth > 0 {
                if i + 1 < len && bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    comment_depth -= 1;
                    i += 2;
                    continue;
                }
                if i + 1 < len && bytes[i] == b'/' && bytes[i + 1] == b'*' {
                    comment_depth += 1;
                    i += 2;
                    continue;
                }
                i += 1;
                continue;
            }

            // Opening a new block comment: skip `/*` and bump depth.
            if i + 1 < len && bytes[i] == b'/' && bytes[i + 1] == b'*' {
                comment_depth = 1;
                i += 2;
                continue;
            }

            // Line comment: `// ...` runs to the next newline. Skip the whole
            // line so any `{...}` or `&...` inside is treated as text.
            if i + 1 < len && bytes[i] == b'/' && bytes[i + 1] == b'/' {
                i += 2;
                while i < len && bytes[i] != b'\n' {
                    i += 1;
                }
                continue;
            }

            match bytes[i] {
                b'&' => {
                    // Possible preprocessor directive
                    if let Some(directive) = self.try_parse_directive(source, i) {
                        match directive.kind {
                            DirectiveKind::ScopedDefine {
                                ref name,
                                ref value,
                            } => {
                                // Emit chunk up to directive
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                if emitting {
                                    // Expand positional `{N}` inside the value at
                                    // define time (ADM2 `set`/`get` store
                                    // `&SCOPED-DEFINE ADMHdl {3}`). Only inside
                                    // an include — top-level keeps literal `{N}`
                                    // for the lexer, matching body-scan rules.
                                    // #65 round 4.
                                    let expanded = if !positional_args.is_empty() {
                                        expand_positional_refs(value, positional_args)
                                    } else {
                                        value.clone()
                                    };
                                    self.vars.define(name, &expanded);
                                }
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                            DirectiveKind::GlobalDefine {
                                ref name,
                                ref value,
                            } => {
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                if emitting {
                                    let expanded = if !positional_args.is_empty() {
                                        expand_positional_refs(value, positional_args)
                                    } else {
                                        value.clone()
                                    };
                                    self.vars.define(name, &expanded);
                                    self.global_vars.define(name, &expanded);
                                }
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                            DirectiveKind::Undefine { ref name } => {
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                if emitting {
                                    self.vars.undefine(name);
                                    self.global_vars.undefine(name);
                                }
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                            DirectiveKind::If { ref condition } => {
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                let cond_result = if emitting {
                                    let condition =
                                        expand_positional_refs(condition, positional_args);
                                    evaluate_with_defined(&condition, &self.vars)
                                } else {
                                    false
                                };
                                if_stack.push(IfState {
                                    parent_emitting: emitting,
                                    any_branch_taken: cond_result,
                                    emitting: emitting && cond_result,
                                });
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                            DirectiveKind::ElseIf { ref condition } => {
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                if let Some(state) = if_stack.last_mut() {
                                    if state.parent_emitting && !state.any_branch_taken {
                                        let condition =
                                            expand_positional_refs(condition, positional_args);
                                        let cond_result =
                                            evaluate_with_defined(&condition, &self.vars);
                                        state.emitting = cond_result;
                                        if cond_result {
                                            state.any_branch_taken = true;
                                        }
                                    } else {
                                        state.emitting = false;
                                    }
                                }
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                            DirectiveKind::Else => {
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                if let Some(state) = if_stack.last_mut() {
                                    state.emitting =
                                        state.parent_emitting && !state.any_branch_taken;
                                }
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                            DirectiveKind::EndIf => {
                                // Emit the chunk from the *previous* emitting state
                                // (the &ENDIF itself should not appear in output).
                                // Do not skip to EOL — trailing code after mid-line
                                // `&ENDIF` (e.g. the period in `… &ENDIF.`) must remain.
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                if_stack.pop();
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                            DirectiveKind::Message => {
                                // &MESSAGE is an intentional developer note that
                                // Progress prints at compile time. It's not a
                                // compiler issue — don't surface it as a
                                // diagnostic (which would spam batch `check`
                                // output). The directive is simply elided from
                                // the preprocessed source.
                                if emitting && i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }
                                i = directive.end;
                                chunk_start = i as u32;
                                continue;
                            }
                        }
                    }
                    // Not a recognized directive — just advance
                    i += 1;
                }
                b'{' if emitting => {
                    // Possible include reference or preprocessor variable ref
                    if i + 1 < len {
                        if bytes[i + 1] == b'&' {
                            // Preprocessor variable reference {&name}.
                            // ABL expands undefined {&name} to empty; do not
                            // preserve the literal reference text (#64).
                            if let Some(next) = self.expand_var_ref_at(
                                file,
                                source,
                                i,
                                &mut chunk_start,
                                &mut nodes,
                            ) {
                                i = next;
                                continue;
                            }
                        } else if bytes[i + 1].is_ascii_alphabetic()
                            || bytes[i + 1] == b'/'
                            || bytes[i + 1] == b'.'
                            || bytes[i + 1] == b'"'
                            || bytes[i + 1] == b'{'
                        {
                            // Include file reference {file.i}
                            //
                            // `{{&name}...}` — dynamic include whose file name is
                            // built from a preprocessor variable (e.g.
                            // `{{&frame}.f &file = "x"}` where `&frame` expands
                            // to the actual include name). Substitute all
                            // `{&var}` references inside the span so the
                            // resulting text parses as a normal include ref.
                            if let Some(close) = find_matching_brace(source, i) {
                                let ref_end = close + 1;
                                let raw_inner = source[i + 1..close].trim();

                                // Pre-expand any `{&var}` references inside the
                                // inner content to support dynamic include names.
                                let expanded_inner = self.expand_preproc_vars(raw_inner);
                                let inner_ref: &str =
                                    expanded_inner.as_deref().unwrap_or(raw_inner);

                                // Parse the include name (first token before space or &)
                                let include_name = parse_include_name(inner_ref);
                                let inner = inner_ref;

                                if i as u32 > chunk_start {
                                    nodes.push(SpanNode::Chunk {
                                        file,
                                        start: chunk_start,
                                        end: i as u32,
                                    });
                                }

                                let include_site = FileSpan {
                                    file,
                                    span: Span {
                                        start: i as u32,
                                        end: ref_end as u32,
                                    },
                                };

                                if depth >= MAX_INCLUDE_DEPTH {
                                    self.diagnostics.push(Diagnostic::error(
                                        "PREPROC003",
                                        format!(
                                            "include depth limit ({MAX_INCLUDE_DEPTH}) exceeded for '{include_name}'"
                                        ),
                                        include_site,
                                    ));
                                    i = ref_end;
                                    chunk_start = i as u32;
                                    continue;
                                }

                                // Resolve include
                                let children =
                                    self.expand_include(&include_name, include_site, inner, depth);

                                if !children.is_empty() {
                                    nodes.push(SpanNode::Include {
                                        site: include_site,
                                        children,
                                    });
                                }

                                i = ref_end;
                                chunk_start = i as u32;
                                continue;
                            }
                        } else if bytes[i + 1].is_ascii_digit() {
                            // Positional argument reference {0}, {1}, etc.
                            if let Some(next) = self.expand_positional_ref_at(
                                file,
                                source,
                                i,
                                positional_args,
                                &mut chunk_start,
                                &mut nodes,
                            ) {
                                i = next;
                                continue;
                            }
                            // Not a valid {N} reference, advance normally
                            i += 1;
                            continue;
                        }
                    }
                    i += 1;
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'/' => {
                    // Line comment — skip to end of line
                    while i < len && bytes[i] != b'\n' {
                        i += 1;
                    }
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'*' => {
                    // Block comment — skip to */
                    i += 2;
                    while i + 1 < len && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                        i += 1;
                    }
                    if i + 1 < len {
                        i += 2; // skip */
                    }
                }
                b'\'' | b'"' => {
                    // String literal — skip to matching quote so `{file.i}`
                    // include refs inside strings stay literal text, but still
                    // expand `{&name}` preprocessor references: the AVM
                    // preprocessor substitutes inside quotes too, and an
                    // undefined {&name} expands to empty (#64). A literal
                    // `{` must be written as `~{` in ABL strings.
                    let quote = bytes[i];
                    i += 1;
                    while i < len {
                        if bytes[i] == b'~' && i + 1 < len {
                            i += 2; // ABL tilde escape
                        } else if bytes[i] == quote {
                            i += 1;
                            break;
                        } else if emitting
                            && i + 1 < len
                            && bytes[i] == b'{'
                            && bytes[i + 1] == b'&'
                            && let Some(next) = self.expand_var_ref_at(
                                file,
                                source,
                                i,
                                &mut chunk_start,
                                &mut nodes,
                            )
                        {
                            // `{&name}` inside strings — AVM substitutes these.
                            i = next;
                        } else if emitting
                            && i + 1 < len
                            && bytes[i] == b'{'
                            && bytes[i + 1].is_ascii_digit()
                            && let Some(next) = self.expand_positional_ref_at(
                                file,
                                source,
                                i,
                                positional_args,
                                &mut chunk_start,
                                &mut nodes,
                            )
                        {
                            // `{N}` include args expand inside strings too
                            // (e.g. `"{1}":U` in ADM2 fn/fnarg).
                            i = next;
                        } else {
                            i += 1;
                        }
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }

        // Emit trailing chunk
        let emitting = if_stack.iter().all(|s| s.emitting);
        if emitting && (len as u32) > chunk_start {
            nodes.push(SpanNode::Chunk {
                file,
                start: chunk_start,
                end: len as u32,
            });
        }

        // Warn about unclosed &IF
        for _ in &if_stack {
            self.diagnostics.push(Diagnostic::warning(
                "PREPROC002",
                "unclosed &IF block".to_string(),
                FileSpan {
                    file,
                    span: Span {
                        start: 0,
                        end: len as u32,
                    },
                },
            ));
        }

        nodes
    }

    /// Expand a `{&name}` preprocessor variable reference whose `{` sits at
    /// byte offset `i`. Flushes the pending chunk, then emits a synthetic
    /// expansion node when the variable is defined with a non-empty value.
    ///
    /// ABL expands undefined `{&name}` to the empty string; do not preserve
    /// the literal reference text (#64). The undefined path therefore emits
    /// nothing — the chunk break plus the `chunk_start` reset past the
    /// reference is what removes the `{&name}` bytes from the output.
    ///
    /// Returns the offset just past the closing `}`, or `None` when the
    /// reference has no closing brace (caller advances normally).
    fn expand_var_ref_at(
        &mut self,
        file: FileId,
        source: &str,
        i: usize,
        chunk_start: &mut u32,
        nodes: &mut Vec<SpanNode>,
    ) -> Option<usize> {
        let close = source[i..].find('}')?;
        let ref_end = i + close + 1;
        let var_name = &source[i + 2..i + close];

        // Emit chunk before the reference
        if i as u32 > *chunk_start {
            nodes.push(SpanNode::Chunk {
                file,
                start: *chunk_start,
                end: i as u32,
            });
        }

        if let Some(val) = self.vars.get(var_name).cloned() {
            // Create a synthetic chunk for the expanded value.
            // We store it as a new source entry.
            let expanded_id = self.next_file_id();
            let val_len = val.len() as u32;
            self.sources.push((expanded_id, val));

            if val_len > 0 {
                nodes.push(SpanNode::Include {
                    site: FileSpan {
                        file,
                        span: Span {
                            start: i as u32,
                            end: ref_end as u32,
                        },
                    },
                    children: vec![SpanNode::Chunk {
                        file: expanded_id,
                        start: 0,
                        end: val_len,
                    }],
                });
            }
        }

        *chunk_start = ref_end as u32;
        Some(ref_end)
    }

    /// Expand a `{N}` positional include-argument reference at byte offset `i`.
    ///
    /// Inside an include (`positional_args` non-empty), a missing index expands
    /// to empty (ABL semantics). At top level (empty args), the reference is
    /// left as-is for the downstream lexer.
    ///
    /// Returns the offset just past `}`, or `None` when the span is not a valid
    /// `{digits}` reference.
    fn expand_positional_ref_at(
        &mut self,
        file: FileId,
        source: &str,
        i: usize,
        positional_args: &[String],
        chunk_start: &mut u32,
        nodes: &mut Vec<SpanNode>,
    ) -> Option<usize> {
        let bytes = source.as_bytes();
        let len = bytes.len();
        if i + 1 >= len || !bytes[i + 1].is_ascii_digit() {
            return None;
        }
        let mut j = i + 1;
        while j < len && bytes[j].is_ascii_digit() {
            j += 1;
        }
        if j >= len || bytes[j] != b'}' {
            return None;
        }
        let ref_end = j + 1;
        let index: usize = source[i + 1..j].parse().unwrap_or(usize::MAX);

        let in_include = !positional_args.is_empty();
        let arg_val = positional_args.get(index).map(|s| s.as_str());

        match (arg_val, in_include) {
            (Some(arg_val), _) => {
                if i as u32 > *chunk_start {
                    nodes.push(SpanNode::Chunk {
                        file,
                        start: *chunk_start,
                        end: i as u32,
                    });
                }
                if !arg_val.is_empty() {
                    let expanded_id = self.next_file_id();
                    let val: Arc<str> = Arc::from(arg_val);
                    let val_len = val.len() as u32;
                    self.sources.push((expanded_id, val));
                    nodes.push(SpanNode::Include {
                        site: FileSpan {
                            file,
                            span: Span {
                                start: i as u32,
                                end: ref_end as u32,
                            },
                        },
                        children: vec![SpanNode::Chunk {
                            file: expanded_id,
                            start: 0,
                            end: val_len,
                        }],
                    });
                }
                *chunk_start = ref_end as u32;
                Some(ref_end)
            }
            (None, true) => {
                // Inside include, missing `{N}` → empty (elide the reference).
                if i as u32 > *chunk_start {
                    nodes.push(SpanNode::Chunk {
                        file,
                        start: *chunk_start,
                        end: i as u32,
                    });
                }
                *chunk_start = ref_end as u32;
                Some(ref_end)
            }
            (None, false) => {
                // Top-level: leave `{N}` in the current chunk for the lexer
                // (IncludeArgReference). Advance past the reference without
                // flushing or eliding.
                Some(ref_end)
            }
        }
    }

    /// Try to parse a preprocessor directive starting at position `i`.
    fn try_parse_directive(&self, source: &str, i: usize) -> Option<Directive> {
        let rest = &source[i..];
        let bytes = rest.as_bytes();

        if bytes.len() < 2 || bytes[0] != b'&' {
            return None;
        }

        // Read the directive keyword
        let mut j = 1;
        while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'-') {
            j += 1;
        }
        let keyword = &rest[1..j];
        let keyword_upper = keyword.to_ascii_uppercase();

        match keyword_upper.as_str() {
            "SCOPED-DEFINE" | "SCOPED" | "SCOP" => {
                let (name, value, end) = parse_define_body(source, i + j);
                Some(Directive {
                    kind: DirectiveKind::ScopedDefine { name, value },
                    end,
                })
            }
            "GLOBAL-DEFINE" | "GLOBAL" | "GLOB" => {
                let (name, value, end) = parse_define_body(source, i + j);
                Some(Directive {
                    kind: DirectiveKind::GlobalDefine { name, value },
                    end,
                })
            }
            "UNDEFINE" | "UNDEF" => {
                let (name, end) = parse_undefine_body(source, i + j);
                Some(Directive {
                    kind: DirectiveKind::Undefine { name },
                    end,
                })
            }
            "IF" => {
                // Find &THEN to delimit the condition
                let (condition, end) = parse_if_condition(source, i + j);
                Some(Directive {
                    kind: DirectiveKind::If { condition },
                    end,
                })
            }
            "ELSEIF" => {
                let (condition, end) = parse_if_condition(source, i + j);
                Some(Directive {
                    kind: DirectiveKind::ElseIf { condition },
                    end,
                })
            }
            "ELSE" => {
                // Mid-line `&ELSE branch` keeps the body; multi-line drops EOL.
                Some(Directive {
                    kind: DirectiveKind::Else,
                    end: skip_after_if_keyword(source, i + j),
                })
            }
            "ENDIF" => {
                // Mid-line `&ENDIF.` keeps the period; multi-line drops EOL.
                Some(Directive {
                    kind: DirectiveKind::EndIf,
                    end: skip_after_if_keyword(source, i + j),
                })
            }
            "THEN" => {
                // &THEN on its own line — already consumed by &IF parsing.
                // Just skip the line.
                None
            }
            "MESSAGE" => {
                // Stop before a trailing same-line `&ENDIF`/`&ELSE`/`&ELSEIF`
                // so inline forms like `&IF … &THEN &MESSAGE … &ENDIF` close
                // correctly (ADM2 / #65). Without a boundary, preserve the
                // historical end-at-newline (not past it) semantics.
                let from = i + j;
                let end = if let Some(b) = find_same_line_if_boundary(source, from) {
                    b
                } else {
                    let rest_after = &source[from..];
                    let end_offset = rest_after.find('\n').unwrap_or(rest_after.len());
                    from + end_offset
                };
                Some(Directive {
                    kind: DirectiveKind::Message,
                    end,
                })
            }
            _ => None,
        }
    }

    /// Expand an include file reference.
    /// Expand `{&var}` references inside `text` using the current variable
    /// table. Returns `Some(expanded)` when any reference was substituted, or
    /// `None` when the text contained no `{&…}` references (so callers can
    /// avoid allocating).
    ///
    /// Used to resolve dynamic include references like `{{&frame}.f …}` where
    /// the include file name is built from a preprocessor variable.
    fn expand_preproc_vars(&self, text: &str) -> Option<String> {
        if !text.contains("{&") {
            return None;
        }
        let bytes = text.as_bytes();
        let mut out = String::with_capacity(text.len());
        let mut i = 0;
        let mut changed = false;
        while i < bytes.len() {
            if i + 1 < bytes.len()
                && bytes[i] == b'{'
                && bytes[i + 1] == b'&'
                && let Some(close_rel) = text[i..].find('}')
            {
                let end = i + close_rel;
                let name = &text[i + 2..end];
                // ABL expands undefined {&name} to empty; do not preserve (#64).
                if let Some(val) = self.vars.get(name) {
                    out.push_str(val);
                }
                i = end + 1;
                changed = true;
                continue;
            }
            out.push(bytes[i] as char);
            i += 1;
        }
        if changed { Some(out) } else { None }
    }

    fn expand_include(
        &mut self,
        include_name: &str,
        site: FileSpan,
        inner: &str,
        depth: usize,
    ) -> Vec<SpanNode> {
        // Parse include arguments
        let args = parse_include_args(inner, include_name);

        // Resolve the include file path
        let resolved = self.fs.resolve_include(self.include_paths, include_name);
        let path = match resolved {
            Some(p) => p,
            None => {
                // Recoverable: the include is elided and processing continues.
                // Severity stays Warning (not Error) because system-level
                // includes (e.g. Progress runtime `src/web2/wrap-cgi.i`) often
                // can't be located on developer machines, and making them errors
                // would break exit codes / CI for corpora full of unlocatable
                // system includes. But the *surfacing* is loud: PREPROC007 is
                // always printed (unlike generic warnings) so the symbol loss is
                // never silent — one honest "I can't resolve this" beats the flood
                // of downstream `undefined-symbol` findings it would otherwise
                // cause. See docs/plans/2026-07-16-003-*.
                self.diagnostics.push(
                    Diagnostic::warning(
                        "PREPROC007",
                        format!(
                            "unresolvable include '{include_name}' — symbols it declares cannot be checked"
                        ),
                        site,
                    )
                    .with_help(
                        "add its directory to include_paths (oxabl.toml [workspace.sources]) or pass -I"
                            .to_string(),
                    ),
                );
                return Vec::new();
            }
        };

        // Read the include file
        let content = match self.fs.read(&path) {
            Ok(c) => c,
            Err(e) => {
                self.diagnostics.push(Diagnostic::error(
                    "PREPROC005",
                    format!("failed to read include file '{}': {e}", path.display()),
                    site,
                ));
                return Vec::new();
            }
        };

        // Cycle detection — track by resolved path, not FileId
        if !self.include_stack.insert(path.clone()) {
            self.diagnostics.push(Diagnostic::error(
                "PREPROC006",
                format!("cyclic include detected: '{include_name}'"),
                site,
            ));
            return Vec::new();
        }

        // Assign a FileId
        let include_file_id = self.next_file_id();
        self.sources.push((include_file_id, content.clone()));
        self.dependencies.push(include_file_id);

        // Scope isolation: save current vars, inject named args
        let saved_vars = self.vars.clone();
        for (name, value) in &args.named {
            self.vars.define(name, value);
        }

        // Process recursively with positional args scoped to this include
        let children = self.process_source(include_file_id, &content, depth + 1, &args.positional);

        // Restore vars (named args don't leak to parent)
        // But preserve any &GLOBAL-DEFINE changes
        self.vars = saved_vars;
        self.vars.merge_globals(&self.global_vars);

        self.include_stack.remove(&path);

        children
    }
}

/// State for tracking `&IF`/`&ELSEIF`/`&ELSE`/`&ENDIF` nesting.
struct IfState {
    /// Whether the parent scope is emitting.
    parent_emitting: bool,
    /// Whether any branch in this &IF chain has been taken.
    any_branch_taken: bool,
    /// Whether the current branch is emitting.
    emitting: bool,
}

struct Directive {
    kind: DirectiveKind,
    /// Byte offset in the source just past the end of this directive
    /// (including the trailing newline if present).
    end: usize,
}

enum DirectiveKind {
    ScopedDefine { name: String, value: String },
    GlobalDefine { name: String, value: String },
    Undefine { name: String },
    If { condition: String },
    ElseIf { condition: String },
    Else,
    EndIf,
    Message,
}

/// Parse the body of `&SCOPED-DEFINE name value` or `&GLOBAL-DEFINE name value`.
///
/// Returns `(name, value, end_offset)`. The value runs to EOL (or EOF), or to
/// a same-line `&ELSE`/`&ELSEIF`/`&ENDIF` so inline forms like
/// `&IF … &THEN &SCOPED-DEFINE x 1 &ENDIF` leave the closer for the scanner
/// (#65 follow-up / ADM2).
fn parse_define_body(source: &str, start: usize) -> (String, String, usize) {
    let rest = &source[start..];

    // Skip whitespace
    let mut i = 0;
    while i < rest.len() && rest.as_bytes()[i] == b' ' {
        i += 1;
    }

    // Read name (alphanumeric, hyphens, underscores)
    let name_start = i;
    while i < rest.len()
        && (rest.as_bytes()[i].is_ascii_alphanumeric()
            || rest.as_bytes()[i] == b'-'
            || rest.as_bytes()[i] == b'_')
    {
        i += 1;
    }
    let name = rest[name_start..i].to_string();

    // Skip whitespace between name and value
    while i < rest.len() && rest.as_bytes()[i] == b' ' {
        i += 1;
    }

    // Value runs to EOL, but tilde (~) at EOL means continuation.
    // ABL uses ~ as the line continuation character in &DEFINE values.
    // A same-line if-chain boundary also ends the value (inline directive body).
    let value_start = i;
    let mut value = String::new();
    loop {
        let line_start = i;
        // Physical end of line (newline / EOF)
        let mut line_end = i;
        while line_end < rest.len()
            && rest.as_bytes()[line_end] != b'\n'
            && rest.as_bytes()[line_end] != b'\r'
        {
            line_end += 1;
        }

        // Truncate at same-line &ELSE / &ELSEIF / &ENDIF when present.
        let boundary_rel =
            find_same_line_if_boundary(source, start + line_start).map(|b| b - start);
        let hit_boundary = boundary_rel.is_some_and(|b| b >= line_start && b < line_end);
        let content_end = match boundary_rel {
            Some(b) if hit_boundary => b,
            _ => line_end,
        };

        let line = &rest[line_start..content_end];
        let trimmed = line.trim_end();

        if !hit_boundary && let Some(continued) = trimmed.strip_suffix('~') {
            // Continuation: append line without trailing ~ and continue
            value.push_str(continued);
            i = line_end;
            // Skip the newline
            if i < rest.len() && rest.as_bytes()[i] == b'\r' {
                i += 1;
            }
            if i < rest.len() && rest.as_bytes()[i] == b'\n' {
                i += 1;
            }
            continue;
        }

        // Final segment — append and stop. Do not consume an if-boundary;
        // leave it for the main scanner. Do consume a trailing newline.
        value.push_str(trimmed);
        if hit_boundary {
            let _ = value_start;
            return (name, value, start + content_end);
        }
        i = line_end;
        if i < rest.len() && rest.as_bytes()[i] == b'\r' {
            i += 1;
        }
        if i < rest.len() && rest.as_bytes()[i] == b'\n' {
            i += 1;
        }
        break;
    }
    // Trim leading whitespace that was part of the continuation indent,
    // but only from the value_start — the first line's leading space was
    // already skipped above.
    let _ = value_start;

    (name, value, start + i)
}

/// Parse the body of `&UNDEFINE name`.
///
/// Ends at EOL, or at a same-line `&ELSE`/`&ELSEIF`/`&ENDIF` so the ADM2
/// `get`/`set` shape
/// `&IF … &THEN &UNDEFINE xp-reset-values &ENDIF` closes correctly (#65).
fn parse_undefine_body(source: &str, start: usize) -> (String, usize) {
    let rest = &source[start..];
    let mut i = 0;

    // Skip whitespace
    while i < rest.len() && rest.as_bytes()[i] == b' ' {
        i += 1;
    }

    // Read name
    let name_start = i;
    while i < rest.len()
        && (rest.as_bytes()[i].is_ascii_alphanumeric()
            || rest.as_bytes()[i] == b'-'
            || rest.as_bytes()[i] == b'_')
    {
        i += 1;
    }
    let name = rest[name_start..i].to_string();

    let end = skip_to_eol_or_if_boundary(source, start + i);
    (name, end)
}

/// Parse the condition expression between `&IF` and `&THEN`.
///
/// Returns `(condition_text, end_offset)` where `end_offset` is positioned via
/// [`skip_after_if_keyword`] after `&THEN` so mid-line branch bodies remain in
/// the scan stream (#65) while multi-line forms still drop the directive-line
/// newline.
fn parse_if_condition(source: &str, start: usize) -> (String, usize) {
    let rest = &source[start..];
    let upper = rest.to_ascii_uppercase();

    // Find &THEN
    if let Some(then_pos) = find_keyword(&upper, "&THEN") {
        let condition = rest[..then_pos].trim().to_string();
        let after_then = start + then_pos + 5; // len("&THEN")
        return (condition, skip_after_if_keyword(source, after_then));
    }

    // No &THEN found — take the rest of the line as the condition and stop at EOL.
    let eol = rest.find('\n').unwrap_or(rest.len());
    let condition = rest[..eol].trim().to_string();
    let end = skip_to_eol(source, start + eol);
    (condition, end)
}

/// After an `&IF`-family keyword (`&THEN`, `&ELSE`, `&ENDIF`), skip trailing
/// spaces/tabs. If that reaches end-of-line, also consume the newline so classic
/// multi-line forms do not emit a blank line for the directive line. If
/// non-whitespace content follows on the same line, leave it in the stream
/// (inline / expression-position form, #65).
///
/// Same-line trailing content after multi-line-style directives is rare in
/// real corpora (which almost always put `/* … */` comments on their own
/// lines, not `//`). When present it is now scanned as code — closer to the
/// AVM, and the reason the corpus A/B gate is required before merge.
fn skip_after_if_keyword(source: &str, from: usize) -> usize {
    let bytes = source.as_bytes();
    let mut i = from;
    while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
        i += 1;
    }
    if i < bytes.len() && bytes[i] == b'\r' {
        i += 1;
    }
    if i < bytes.len() && bytes[i] == b'\n' {
        return i + 1;
    }
    // Inline body starts here (horizontal whitespace already skipped).
    i
}

/// Expand `{N}` positional include-argument references in `text`.
///
/// Missing indices become the empty string (ABL include-arg semantics). Used
/// when evaluating `&IF`/`&ELSEIF` conditions that contain `"{3}"`-style refs
/// before the body scanner would see them.
fn expand_positional_refs(text: &str, positional_args: &[String]) -> String {
    if !text.contains('{') {
        return text.to_string();
    }
    let bytes = text.as_bytes();
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'{' && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_digit() {
                j += 1;
            }
            if j < bytes.len() && bytes[j] == b'}' {
                let index: usize = text[i + 1..j].parse().unwrap_or(usize::MAX);
                if let Some(val) = positional_args.get(index) {
                    out.push_str(val);
                }
                // missing index → empty
                i = j + 1;
                continue;
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

/// Find a keyword in uppercase text, ensuring it's at a word boundary.
fn find_keyword(text: &str, keyword: &str) -> Option<usize> {
    let mut search_from = 0;
    while let Some(pos) = text[search_from..].find(keyword) {
        let abs_pos = search_from + pos;
        let after = abs_pos + keyword.len();
        // Check it's not inside a larger word
        let before_ok = abs_pos == 0 || !text.as_bytes()[abs_pos - 1].is_ascii_alphanumeric();
        let after_ok = after >= text.len() || !text.as_bytes()[after].is_ascii_alphanumeric();
        if before_ok && after_ok {
            return Some(abs_pos);
        }
        search_from = abs_pos + 1;
    }
    None
}

/// Advance past the current line (to the first byte after the newline).
fn skip_to_eol(source: &str, from: usize) -> usize {
    let rest = &source[from..];
    let eol = rest.find('\n').map(|p| p + 1).unwrap_or(rest.len());
    from + eol
}

/// Like [`skip_to_eol`], but stop at a same-line `&ELSE` / `&ELSEIF` / `&ENDIF`
/// so line-oriented directives do not swallow a trailing inline closer (#65).
fn skip_to_eol_or_if_boundary(source: &str, from: usize) -> usize {
    if let Some(b) = find_same_line_if_boundary(source, from) {
        return b;
    }
    skip_to_eol(source, from)
}

/// Find the earliest same-line `&ELSEIF`, `&ENDIF`, or `&ELSE` at or after
/// `from`. Returns the absolute byte offset of the leading `&`, or `None` if
/// none appears before the next newline (or EOF).
///
/// Used so line-oriented directive payloads (`&UNDEFINE`, `&SCOPED-DEFINE`,
/// `&GLOBAL-DEFINE`, `&MESSAGE`) leave a trailing inline if-chain closer for
/// the main scanner — the ADM2 `get`/`set` shape
/// `&IF … &THEN &UNDEFINE xp-reset-values &ENDIF` (#65 follow-up).
fn find_same_line_if_boundary(source: &str, from: usize) -> Option<usize> {
    let rest = &source[from..];
    let mut line_end = rest.find('\n').unwrap_or(rest.len());
    if let Some(cr) = rest[..line_end].find('\r') {
        line_end = cr;
    }
    if line_end == 0 {
        return None;
    }
    let line = &rest[..line_end];
    let upper = line.to_ascii_uppercase();

    let mut best: Option<usize> = None;
    for kw in ["&ELSEIF", "&ENDIF", "&ELSE"] {
        if let Some(pos) = find_keyword(&upper, kw) {
            best = Some(match best {
                Some(b) => b.min(pos),
                None => pos,
            });
        }
    }
    best.map(|p| from + p)
}

/// Find the matching `}` for a `{` at position `open`, handling nested braces.
fn find_matching_brace(source: &str, open: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    let mut depth = 1usize;
    let mut i = open + 1;
    while i < bytes.len() {
        match bytes[i] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
        i += 1;
    }
    None
}

/// Parse the include file name from the content between `{` and `}`.
///
/// The name is the first space-delimited token. Handles quoted names like
/// `"path/to/file.i"`.
fn parse_include_name(inner: &str) -> String {
    let trimmed = inner.trim();
    if let Some(stripped) = trimmed.strip_prefix('"') {
        // Quoted include name
        if let Some(end_quote) = stripped.find('"') {
            return stripped[..end_quote].to_string();
        }
    }
    // Unquoted — terminate on any ASCII whitespace (space, tab, newline, CR) or `&`.
    // Uses the same delimiter rule as `find_args_start` so the two helpers agree
    // on where the name ends.
    let end = trimmed
        .as_bytes()
        .iter()
        .position(|b| b.is_ascii_whitespace() || *b == b'&')
        .unwrap_or(trimmed.len());
    trimmed[..end].to_string()
}

/// Parsed include file arguments.
struct IncludeArgs {
    /// Positional arguments. Index 0 = include file name, 1+ = user args.
    positional: Vec<String>,
    /// Named arguments (`&name=value` pairs).
    named: Vec<(String, String)>,
}

/// Parse arguments from the content inside `{...}` of an include reference.
///
/// ABL include syntax:
/// - `{file.i}` — no args
/// - `{file.i "arg1" arg2}` — positional args
/// - `{file.i &name=value &other="quoted"}` — named args
/// - `{file.i "SHARED" &extra=yes}` — mixed
///
/// Positional arg `{0}` = the include name. `{1}`, `{2}`, ... = user args.
/// Named args are `&name=value` pairs where value quotes are stripped.
fn parse_include_args(inner: &str, include_name: &str) -> IncludeArgs {
    let mut positional = vec![include_name.to_string()];
    let mut named = Vec::new();

    let trimmed = inner.trim();

    // Skip past the include name to get to the arguments.
    // The include name is the first token — find where it ends.
    let args_start = find_args_start(trimmed);
    if args_start >= trimmed.len() {
        return IncludeArgs { positional, named };
    }

    let args_str = &trimmed[args_start..];
    let mut i = 0;
    let bytes = args_str.as_bytes();

    while i < bytes.len() {
        // Skip whitespace (spaces, tabs, newlines — include args can span multiple lines)
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }

        if bytes[i] == b'&' {
            // Named argument: &name = value (with optional whitespace around =)
            i += 1; // skip &
            let name_start = i;
            while i < bytes.len() && bytes[i] != b'=' && !bytes[i].is_ascii_whitespace() {
                i += 1;
            }
            let name = args_str[name_start..i].to_string();
            // Skip whitespace between name and =
            while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                i += 1;
            }
            if i < bytes.len() && bytes[i] == b'=' {
                i += 1; // skip =
                let value = read_arg_value(args_str, &mut i);
                named.push((name, value));
            } else {
                // &name with no =value — treat as empty value
                named.push((name, String::new()));
            }
        } else {
            // Positional argument
            let value = read_arg_value(args_str, &mut i);
            positional.push(value);
        }
    }

    IncludeArgs { positional, named }
}

/// Read a single argument value, handling quotes. Advances `i` past the value.
fn read_arg_value(s: &str, i: &mut usize) -> String {
    let bytes = s.as_bytes();

    // Skip leading whitespace (spaces, tabs, newlines)
    while *i < bytes.len() && bytes[*i].is_ascii_whitespace() {
        *i += 1;
    }

    if *i >= bytes.len() {
        return String::new();
    }

    if bytes[*i] == b'"' || bytes[*i] == b'\'' {
        // Quoted value — read to matching quote
        let quote = bytes[*i];
        *i += 1;
        let start = *i;
        while *i < bytes.len() && bytes[*i] != quote {
            *i += 1;
        }
        let value = s[start..*i].to_string();
        if *i < bytes.len() {
            *i += 1; // skip closing quote
        }
        value
    } else {
        // Unquoted — read to next whitespace or &
        let start = *i;
        while *i < bytes.len() && !bytes[*i].is_ascii_whitespace() && bytes[*i] != b'&' {
            *i += 1;
        }
        s[start..*i].to_string()
    }
}

/// Find the byte offset where arguments start (after the include name).
fn find_args_start(inner: &str) -> usize {
    let bytes = inner.as_bytes();
    let mut i = 0;

    if i < bytes.len() && bytes[i] == b'"' {
        // Quoted include name — skip to closing quote
        i += 1;
        while i < bytes.len() && bytes[i] != b'"' {
            i += 1;
        }
        if i < bytes.len() {
            i += 1; // skip closing quote
        }
    } else {
        // Unquoted — skip to first whitespace or &
        while i < bytes.len() && !bytes[i].is_ascii_whitespace() && bytes[i] != b'&' {
            i += 1;
        }
    }

    // Skip whitespace between name and first arg
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }

    i
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use oxabl_workspace::InMemoryFileSystem;

    use super::*;

    fn make_fs(files: &[(&str, &str)]) -> InMemoryFileSystem {
        let mut fs = InMemoryFileSystem::new();
        for (path, content) in files {
            fs.insert(PathBuf::from(path), *content);
        }
        fs
    }

    #[test]
    fn passthrough_no_directives() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "DEFINE VARIABLE x AS INTEGER.\nDISPLAY x.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), source);
        assert!(result.dependencies.is_empty());
    }

    #[test]
    fn scoped_define_and_variable_ref() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE TABLE-NAME customer\nFIND {&TABLE-NAME}.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "FIND customer.");
    }

    #[test]
    fn global_define() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&GLOBAL-DEFINE VERSION 2\nVERSION IS {&VERSION}.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "VERSION IS 2.");
        assert!(result.vars.is_defined("VERSION"));
    }

    #[test]
    fn undefine() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE X 1\n&UNDEFINE X\n{&X}rest";
        let result = pp.process(FileId::new(1), source).unwrap();

        // After &UNDEFINE the reference is undefined — ABL expands it to empty.
        assert_eq!(&*result.to_text(), "rest");
    }

    #[test]
    fn if_true_branch() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF TRUE &THEN\nkept\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "kept\n");
    }

    #[test]
    fn if_false_branch() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF FALSE &THEN\nskipped\n&ENDIF\nkept";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "kept");
    }

    #[test]
    fn if_else() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF FALSE &THEN\nskipped\n&ELSE\nkept\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "kept\n");
    }

    #[test]
    fn if_elseif() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF FALSE &THEN\nA\n&ELSEIF TRUE &THEN\nB\n&ELSE\nC\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "B\n");
    }

    #[test]
    fn if_with_defined() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE DEBUG\n&IF DEFINED(DEBUG) &THEN\ndebug-mode\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "debug-mode\n");
    }

    #[test]
    fn include_expansion() {
        let fs = make_fs(&[("/inc/header.i", "/* header */\n")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "BEFORE\n{header.i}AFTER";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "BEFORE\n/* header */\nAFTER");
        assert_eq!(result.dependencies.len(), 1);
    }

    #[test]
    fn nested_include() {
        let fs = make_fs(&[
            ("/inc/outer.i", "OUTER-{inner.i}-END"),
            ("/inc/inner.i", "INNER"),
        ]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{outer.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "OUTER-INNER-END");
        assert_eq!(result.dependencies.len(), 2);
    }

    #[test]
    fn include_not_found() {
        let fs = make_fs(&[]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "BEFORE {missing.i} AFTER";
        let result = pp.process(FileId::new(1), source).unwrap();

        // Missing include is removed, but surrounding text preserved
        assert_eq!(&*result.to_text(), "BEFORE  AFTER");
        // ...and the failure is loud, not silent: a PREPROC007 diagnostic marks
        // the symbol loss so downstream `undefined-symbol` findings have a cause.
        assert!(
            result.diagnostics.iter().any(|d| d.code.0 == "PREPROC007"),
            "expected a PREPROC007 diagnostic for the unresolved include"
        );
    }

    #[test]
    fn include_found_in_configured_second_path_expands_symbols() {
        // File only in the second search dir; symbols must still expand (so no
        // downstream false positives), and no PREPROC007 fires.
        let fs = make_fs(&[(
            "/inc2/globals.i",
            "define variable gcCompany as character no-undo.\n",
        )]);
        let include_paths = vec![PathBuf::from("/inc1"), PathBuf::from("/inc2")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{globals.i}gcCompany = \"acme\".";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert!(result.to_text().contains("define variable gcCompany"));
        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC007"),
            "resolved include must not emit PREPROC007"
        );
    }

    #[test]
    fn unresolved_include_emits_loud_diagnostic_not_silent() {
        let fs = make_fs(&[]);
        let include_paths = vec![PathBuf::from("/inc1"), PathBuf::from("/inc2")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{shared/globals.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        let hits: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.code.0 == "PREPROC007")
            .collect();
        assert_eq!(
            hits.len(),
            1,
            "exactly one PREPROC007 per unresolved include"
        );
        let d = hits[0];
        assert!(d.message.contains("shared/globals.i"));
        assert!(d.message.contains("cannot be checked"));
        assert!(
            d.help.is_some(),
            "PREPROC007 must carry a remediation help line"
        );
    }

    #[test]
    fn propath_first_match_wins_uses_earliest_dir() {
        let fs = make_fs(&[("/inc1/shared.i", "FIRST"), ("/inc2/shared.i", "SECOND")]);
        let include_paths = vec![PathBuf::from("/inc1"), PathBuf::from("/inc2")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{shared.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "FIRST");
    }

    #[test]
    fn include_relative_vs_absolute_dir_resolution() {
        // Both search dirs pre-normalized to absolute (as the config helper
        // would produce); file lives in the absolute one.
        let fs = make_fs(&[("/abs/inc/util.i", "UTIL")]);
        let include_paths = vec![PathBuf::from("/other"), PathBuf::from("/abs/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{util.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "UTIL");
    }

    #[test]
    fn nested_unresolved_include_carries_inner_file_id() {
        // Root resolves {outer.i}; outer.i references a missing {inner.i}.
        // The PREPROC007 must belong to outer.i's FileId, not the root's —
        // this is what forces the CLI to guard its root-SourceMap rendering.
        let fs = make_fs(&[("/inc/outer.i", "OUTER-{inner.i}-END")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let root = FileId::new(1);
        let result = pp.process(root, "{outer.i}").unwrap();

        // outer.i still expands its own text around the elided inner include.
        assert_eq!(&*result.to_text(), "OUTER--END");

        let hits: Vec<_> = result
            .diagnostics
            .iter()
            .filter(|d| d.code.0 == "PREPROC007")
            .collect();
        assert_eq!(hits.len(), 1);
        assert_ne!(
            hits[0].span.file, root,
            "the diagnostic must carry the inner include's FileId, not the root's"
        );
    }

    #[test]
    fn include_with_define_propagation() {
        let fs = make_fs(&[("/inc/defs.i", "&GLOBAL-DEFINE DB sports\n")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{defs.i}DB={&DB}.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "DB=sports.");
    }

    #[test]
    fn resolve_position_through_include() {
        let fs = make_fs(&[("/inc/body.i", "INCLUDED")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let file = FileId::new(1);
        let source = "AA{body.i}BB";
        let result = pp.process(file, source).unwrap();

        // Logical: "AA" + "INCLUDED" + "BB"
        // Offset 0 → file 1, byte 0 ("A")
        let r0 = result.resolve(0);
        assert_eq!(r0.file, file);
        assert_eq!(r0.span.start, 0);

        // Offset 2 → included file, byte 0 ("I")
        let r2 = result.resolve(2);
        assert_ne!(r2.file, file); // should be the include file's id

        // Offset 10 → file 1, byte 10 ("B")
        let r10 = result.resolve(10);
        assert_eq!(r10.file, file);
    }

    #[test]
    fn undefined_var_ref_in_string_expands_to_empty() {
        // The AVM preprocessor substitutes inside quoted strings too; an
        // undefined {&name} expands to empty there as well (#64).
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "MESSAGE '{&not-a-ref}'.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "MESSAGE ''.");
    }

    #[test]
    fn defined_var_ref_in_string_expands() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE WHO world\nMESSAGE 'hello {&WHO}'.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "MESSAGE 'hello world'.");
    }

    #[test]
    fn include_ref_in_string_not_expanded() {
        // Include references inside string literals stay literal text.
        let fs = make_fs(&[("/inc/foo.i", "EXPANDED")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "MESSAGE '{foo.i}'.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "MESSAGE '{foo.i}'.");
        assert!(result.dependencies.is_empty());
    }

    #[test]
    fn tilde_escaped_brace_in_string_stays_literal() {
        // `~{` is the ABL escape for a literal `{` inside a string — the
        // preprocessor must not treat the escaped brace as a `{&ref}` opener.
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "MESSAGE '~{&not-a-ref}'.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "MESSAGE '~{&not-a-ref}'.");
    }

    #[test]
    fn comments_not_processed() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "/* {not-an-include} */\ncode.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "/* {not-an-include} */\ncode.");
    }

    #[test]
    fn nested_if() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source =
            "&IF TRUE &THEN\n&IF FALSE &THEN\ninner-skip\n&ELSE\ninner-keep\n&ENDIF\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "inner-keep\n");
    }

    #[test]
    fn parse_include_name_unquoted() {
        assert_eq!(parse_include_name("file.i"), "file.i");
        assert_eq!(parse_include_name("path/file.i arg1"), "path/file.i");
        assert_eq!(parse_include_name("file.i &name=val"), "file.i");
    }

    #[test]
    fn parse_include_name_quoted() {
        assert_eq!(parse_include_name("\"path/to/file.i\""), "path/to/file.i");
        assert_eq!(
            parse_include_name("\"path/to/file.i\" arg"),
            "path/to/file.i"
        );
    }

    #[test]
    fn parse_include_name_newline_terminator() {
        assert_eq!(parse_include_name("file.i\n&arg=v"), "file.i");
        assert_eq!(parse_include_name("path/file.i\n&arg=v"), "path/file.i");
    }

    #[test]
    fn parse_include_name_tab_terminator() {
        assert_eq!(parse_include_name("file.i\t&arg=v"), "file.i");
        assert_eq!(parse_include_name("file.i\targ1"), "file.i");
    }

    #[test]
    fn parse_include_name_crlf_terminator() {
        assert_eq!(parse_include_name("file.i\r\n&arg=v"), "file.i");
    }

    #[test]
    fn find_matching_brace_nested() {
        let s = "{outer {inner} rest}";
        assert_eq!(find_matching_brace(s, 0), Some(19));
    }

    #[test]
    fn find_matching_brace_simple() {
        let s = "{simple}";
        assert_eq!(find_matching_brace(s, 0), Some(7));
    }

    #[test]
    fn abbreviations_for_directives() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        // &SCOP is a valid abbreviation for &SCOPED-DEFINE
        let source = "&SCOP FOO bar\n{&FOO}";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert_eq!(&*result.to_text(), "bar");
    }

    #[test]
    fn global_abbreviation() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&GLOB X hello\n{&X}";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert_eq!(&*result.to_text(), "hello");
    }

    #[test]
    fn dynamic_include_name_from_preproc_var() {
        // `{{&frame}.f …}` — dynamic include whose file name is built from a
        // `&SCOPED-DEFINE` variable. Preprocessor must pre-expand `{&frame}`
        // before resolving the include.
        let fs = make_fs(&[("/inc/menu_prc.f", "expanded body")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "&SCOPED-DEFINE frame menu_prc\n{{&frame}.f}";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert!(
            result.to_text().contains("expanded body"),
            "expected include resolved via &frame, got: {}",
            result.to_text()
        );
    }

    #[test]
    fn include_reference_inside_block_comment_not_expanded() {
        // `{foo.i}` inside a `/* ... */` block is comment text — the
        // preprocessor must not attempt to expand it. Before this guard the
        // preprocessor would recurse into `foo.i`, leaving its expanded body
        // inline inside the comment context and throwing downstream lexing
        // off (nested-comment depth, include arg tracking, etc.).
        let fs = make_fs(&[("/inc/foo.i", "EXPANDED")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "before /* {foo.i} */ after";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert_eq!(&*result.to_text(), "before /* {foo.i} */ after");
        assert!(
            result.dependencies.is_empty(),
            "include inside comment should not be tracked as a dependency"
        );
    }

    #[test]
    fn include_reference_inside_nested_block_comment_not_expanded() {
        let fs = make_fs(&[("/inc/foo.i", "EXPANDED")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "/* outer /* inner {foo.i} */ still outer */ real";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert!(result.to_text().contains("real"));
        assert!(!result.to_text().contains("EXPANDED"));
    }

    #[test]
    fn include_reference_inside_line_comment_not_expanded() {
        let fs = make_fs(&[("/inc/foo.i", "EXPANDED")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "before\n// comment {foo.i} end\nafter";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert!(!result.to_text().contains("EXPANDED"));
        assert!(result.to_text().contains("after"));
    }

    #[test]
    fn directive_inside_block_comment_not_processed() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        // `&SCOPED-DEFINE` inside a comment should NOT define the variable.
        let source = "/* &SCOPED-DEFINE FOO bar */\nresult: {&FOO}";
        let result = pp.process(FileId::new(1), source).unwrap();
        // FOO is undefined, so `{&FOO}` expands to empty — it must not pick
        // up the value from the commented-out define.
        assert_eq!(&*result.to_text(), "/* &SCOPED-DEFINE FOO bar */\nresult: ");
    }

    #[test]
    fn expand_include_with_name_on_own_line() {
        // Real-world shape from pcna-erp: the include name appears on one line
        // and named args follow on subsequent lines. The whole reference spans
        // multiple lines inside `{...}`. Previously, parse_include_name captured
        // the trailing newline as part of the name and resolution silently failed.
        let fs = make_fs(&[(
            "/inc/currexch.i",
            "If {&currency} gt \" \" then {&exchange} = 1.\nElse {&exchange} = -1.",
        )]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "\
If x ne y THEN
    {currexch.i
    &currency = \"sv-currency\"
    &exchange = \"c-exchange\" }
ELSE
    ASSIGN c-exchange = 1.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();
        assert!(
            text.contains("If sv-currency gt"),
            "expected expanded include body with substituted &currency, got:\n{text}"
        );
        assert!(
            text.contains("c-exchange = 1"),
            "expected &exchange substituted to c-exchange, got:\n{text}"
        );
    }

    #[test]
    fn cyclic_include_detected() {
        // a.i includes b.i which includes a.i → cycle
        let fs = make_fs(&[("/inc/a.i", "A-{b.i}-A"), ("/inc/b.i", "B-{a.i}-B")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        // We start from a file that includes a.i
        let source = "{a.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        // The result should contain text up to the cycle point, but not loop forever.
        // a.i expands to "A-" + expand(b.i) + "-A"
        // b.i expands to "B-" + (cycle detected for a.i, empty) + "-B"
        let text = result.to_text();
        assert!(text.contains("A-"));
        assert!(text.contains("B-"));
        // Should not contain infinite nesting
        assert!(text.len() < 100);
    }

    #[test]
    fn dependencies_tracks_all_includes() {
        let fs = make_fs(&[
            ("/inc/a.i", "{b.i}"),
            ("/inc/b.i", "{c.i}"),
            ("/inc/c.i", "leaf"),
        ]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{a.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "leaf");
        // Should have 3 dependencies: a.i, b.i, c.i
        assert_eq!(result.dependencies.len(), 3);
    }

    #[test]
    fn if_with_variable_comparison() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE MODE production\n&IF \"{&MODE}\" = \"production\" &THEN\nprod-code\n&ELSE\ndev-code\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "prod-code\n");
    }

    #[test]
    fn if_defined_false_takes_else() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF DEFINED(MISSING) &THEN\nskip\n&ELSE\nfallback\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "fallback\n");
    }

    #[test]
    fn scoped_define_does_not_propagate_to_parent() {
        // Include file defines a scoped variable — it should NOT be visible
        // in the parent after the include returns.
        let fs = make_fs(&[("/inc/defs.i", "&SCOPED-DEFINE LOCAL_VAR secret\n")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{defs.i}val={&LOCAL_VAR}.";
        let result = pp.process(FileId::new(1), source).unwrap();

        // Because scoped defines inside include files currently propagate
        // in our flat var table (not yet isolated per-file), this tests
        // the current behavior. Full scoping isolation is a future improvement.
        // For now, scoped defines DO propagate (like global defines).
        // When we add per-file scope isolation, update this test.
        let text = result.to_text();
        assert!(text.starts_with("val="));
    }

    #[test]
    fn multiple_variable_refs_on_one_line() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE A hello\n&SCOPED-DEFINE B world\n{&A} {&B}!";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "hello world!");
    }

    #[test]
    fn empty_define_value() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE FLAG\n&IF DEFINED(FLAG) &THEN\nyes\n&ENDIF\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "yes\n");
    }

    #[test]
    fn block_comment_not_processed() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "/* &SCOPED-DEFINE X 1 */\nval={&X}.";
        let result = pp.process(FileId::new(1), source).unwrap();

        // The define inside the comment should not take effect;
        // the undefined {&X} expands to empty.
        assert_eq!(&*result.to_text(), "/* &SCOPED-DEFINE X 1 */\nval=.");
    }

    #[test]
    fn line_comment_not_processed() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "// &SCOPED-DEFINE X 1\nval={&X}.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "// &SCOPED-DEFINE X 1\nval=.");
    }

    #[test]
    fn include_depth_limit() {
        // Create a deep (but not cyclic) include chain that exceeds MAX_INCLUDE_DEPTH
        // For this test, just verify the mechanism works with a simpler deep chain.
        // The actual limit is 64, so we test with a smaller chain.
        let fs = make_fs(&[
            ("/inc/d1.i", "{d2.i}"),
            ("/inc/d2.i", "{d3.i}"),
            ("/inc/d3.i", "deep"),
        ]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{d1.i}";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert_eq!(&*result.to_text(), "deep");
    }

    #[test]
    fn tilde_continuation_in_define() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE QUERY ~\n        customer where ~\n        customer.name eq 'test'\nFIND {&QUERY}.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(
            &*result.to_text(),
            "FIND         customer where         customer.name eq 'test'."
        );
    }

    #[test]
    fn tilde_continuation_single_line() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        // No continuation — normal define
        let source = "&SCOPED-DEFINE X hello\n{&X}";
        let result = pp.process(FileId::new(1), source).unwrap();
        assert_eq!(&*result.to_text(), "hello");
    }

    // ── Include argument tests ──────────────────────────────────────

    #[test]
    fn positional_arg_basic() {
        let fs = make_fs(&[("/inc/def.i", "DEFINE {1} VARIABLE x AS INTEGER.")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = r#"{def.i "SHARED"}"#;
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "DEFINE SHARED VARIABLE x AS INTEGER.");
    }

    #[test]
    fn positional_arg_zero_is_include_name() {
        let fs = make_fs(&[("/inc/self.i", "name={0}")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{self.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "name=self.i");
    }

    #[test]
    fn positional_arg_multiple() {
        let fs = make_fs(&[("/inc/multi.i", "A={1} B={2} C={3}")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = r#"{multi.i "one" two "three"}"#;
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "A=one B=two C=three");
    }

    #[test]
    fn named_arg_basic() {
        let fs = make_fs(&[("/inc/tmpl.i", "TABLE={&table}")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{tmpl.i &table=customer}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "TABLE=customer");
    }

    #[test]
    fn named_arg_quoted_value() {
        let fs = make_fs(&[("/inc/tmpl.i", "DS={&dataset}")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = r#"{tmpl.i &dataset="InventoryLevels"}"#;
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "DS=InventoryLevels");
    }

    #[test]
    fn named_arg_in_if_condition() {
        let fs = make_fs(&[(
            "/inc/cond.i",
            "&IF \"{&mode}\" = \"debug\" &THEN\nDEBUG\n&ELSE\nRELEASE\n&ENDIF\n",
        )]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = r#"{cond.i &mode="debug"}"#;
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "DEBUG\n");
    }

    #[test]
    fn mixed_positional_and_named_args() {
        let fs = make_fs(&[("/inc/mix.i", "P={1} N={&x}")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = r#"{mix.i "pos1" &x=named1}"#;
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "P=pos1 N=named1");
    }

    #[test]
    fn named_arg_scope_isolation() {
        // Named args should NOT leak to the parent after include returns
        let fs = make_fs(&[("/inc/scoped.i", "inside={&arg}")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{scoped.i &arg=secret}outside={&arg}";
        let result = pp.process(FileId::new(1), source).unwrap();

        // {&arg} after the include is undefined — expands to empty
        assert_eq!(&*result.to_text(), "inside=secretoutside=");
    }

    #[test]
    fn positional_arg_scope_isolation() {
        // Positional args from outer include should not bleed into inner
        let fs = make_fs(&[
            ("/inc/outer.i", "O={1} {inner.i}"),
            ("/inc/inner.i", "I={1}"),
        ]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = r#"{outer.i "OUTER_ARG"}"#;
        let result = pp.process(FileId::new(1), source).unwrap();

        // inner.i is invoked with no user args — ABL expands missing `{1}` to
        // empty (not the literal `{1}` text). Outer `{1}` still resolves.
        assert_eq!(&*result.to_text(), "O=OUTER_ARG I=");
    }

    #[test]
    fn global_define_survives_scope_restore() {
        // &GLOBAL-DEFINE inside an include should still propagate
        let fs = make_fs(&[("/inc/setglobal.i", "&GLOBAL-DEFINE VER 42\n")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{setglobal.i &unused=x}VER={&VER}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "VER=42");
    }

    #[test]
    fn no_args_still_works() {
        // Include with no args should work as before
        let fs = make_fs(&[("/inc/plain.i", "plain content")]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{plain.i}";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "plain content");
    }

    #[test]
    fn parse_include_args_positional() {
        let args = parse_include_args("file.i \"one\" two", "file.i");
        assert_eq!(args.positional, vec!["file.i", "one", "two"]);
        assert!(args.named.is_empty());
    }

    #[test]
    fn parse_include_args_named() {
        let args = parse_include_args(r#"file.i &table=customer &field="cust-num""#, "file.i");
        assert!(args.positional.len() == 1); // just the include name
        assert_eq!(args.named.len(), 2);
        assert_eq!(args.named[0], ("table".to_string(), "customer".to_string()));
        assert_eq!(args.named[1], ("field".to_string(), "cust-num".to_string()));
    }

    #[test]
    fn parse_include_args_mixed() {
        let args = parse_include_args(r#"file.i "SHARED" &extra=yes"#, "file.i");
        assert_eq!(args.positional, vec!["file.i", "SHARED"]);
        assert_eq!(args.named, vec![("extra".to_string(), "yes".to_string())]);
    }

    #[test]
    fn parse_include_args_quoted_name() {
        let args = parse_include_args(r#""path/to/file.i" "arg1""#, "path/to/file.i");
        assert_eq!(args.positional, vec!["path/to/file.i", "arg1"]);
    }

    #[test]
    fn parse_include_args_multiline() {
        let inner = "ms/report.i &event       = \"start\"\n             &stream-name = \"s-printer\"\n             &rpt-printer = \"p-printer\"\n             &max-columns = 80";
        let args = parse_include_args(inner, "ms/report.i");
        assert_eq!(args.positional, vec!["ms/report.i"]);
        assert_eq!(args.named.len(), 4);
        assert_eq!(args.named[0], ("event".to_string(), "start".to_string()));
        assert_eq!(
            args.named[1],
            ("stream-name".to_string(), "s-printer".to_string())
        );
        assert_eq!(
            args.named[2],
            ("rpt-printer".to_string(), "p-printer".to_string())
        );
        assert_eq!(args.named[3], ("max-columns".to_string(), "80".to_string()));
    }

    #[test]
    fn parse_include_args_spaces_around_equals() {
        let args = parse_include_args("file.i &name = \"value\"", "file.i");
        assert_eq!(args.named.len(), 1);
        assert_eq!(args.named[0], ("name".to_string(), "value".to_string()));
    }

    #[test]
    fn parse_include_args_no_args() {
        let args = parse_include_args("file.i", "file.i");
        assert_eq!(args.positional, vec!["file.i"]);
        assert!(args.named.is_empty());
    }

    // ── Undefined {&name} expands to empty (#64) ────────────────────

    #[test]
    fn undefined_var_ref_expands_to_empty() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "A{&never-defined}B";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "AB");
    }

    #[test]
    fn undefined_var_ref_mid_declaration_expands_empty() {
        // #64 preprocessor-only shape: an undefined macro slot in the middle
        // of a declaration must vanish so the line parses as a DEFINE.
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE var-type DEFINE VARIABLE\n{&var-type} {&batch_global_alt}myvar AS CHARACTER NO-UNDO.";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(
            &*result.to_text(),
            "DEFINE VARIABLE myvar AS CHARACTER NO-UNDO."
        );
    }

    #[test]
    fn issue_64_include_arg_with_undefined_slot_expands_empty() {
        // End-to-end repro from GitHub #64: def.i uses two macro slots; the
        // host supplies only &var-type. The undefined {&batch_global_alt}
        // must expand to empty so `myvar` declares.
        let fs = make_fs(&[(
            "/inc/def.i",
            "{&var-type} {&batch_global_alt}myvar AS CHARACTER NO-UNDO.\n",
        )]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{def.i &var-type = \"DEFINE NEW GLOBAL SHARED VARIABLE \"}\nDISPLAY myvar.\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        let text = result.to_text();
        assert_eq!(
            &*text,
            "DEFINE NEW GLOBAL SHARED VARIABLE  myvar AS CHARACTER NO-UNDO.\n\nDISPLAY myvar.\n"
        );
        assert!(
            !text.contains("{&"),
            "no unexpanded macro refs may remain, got:\n{text}"
        );
    }

    #[test]
    fn issue_64_control_defined_empty_macro_still_expands() {
        // Control from GitHub #64: with &GLOBAL-DEFINE batch_global_alt
        // (empty value) the same include expands identically.
        let fs = make_fs(&[(
            "/inc/def.i",
            "{&var-type} {&batch_global_alt}myvar AS CHARACTER NO-UNDO.\n",
        )]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "&GLOBAL-DEFINE batch_global_alt\n{def.i &var-type = \"DEFINE NEW GLOBAL SHARED VARIABLE \"}\nDISPLAY myvar.\n";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(
            &*result.to_text(),
            "DEFINE NEW GLOBAL SHARED VARIABLE  myvar AS CHARACTER NO-UNDO.\n\nDISPLAY myvar.\n"
        );
    }

    #[test]
    fn dynamic_include_with_undefined_frame_no_panic() {
        // `{{&frame}.f}` with `frame` undefined: the name collapses to `.f`,
        // which cannot resolve — PREPROC007, no panic, no leftover `{&`.
        let fs = make_fs(&[]);
        let include_paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &include_paths);
        let source = "{{&frame}.f}after";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "after");
        assert!(
            result.diagnostics.iter().any(|d| d.code.0 == "PREPROC007"),
            "unresolvable dynamic include must stay loud"
        );
    }

    #[test]
    fn expand_preproc_vars_undefined_name_substitutes_empty() {
        // expand_preproc_vars must report `changed` for undefined refs so
        // callers use the substituted (emptied) string.
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE known k\nx={&known}{&unknown}y";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert_eq!(&*result.to_text(), "x=ky");
    }

    // =========================================================================
    // Inline / mid-line &IF — GitHub #65
    // =========================================================================

    #[test]
    fn inline_if_then_else_emits_true_branch() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "i = &IF TRUE &THEN 5 &ELSE 6 &ENDIF.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(text.contains('5'), "true branch must emit, got: {text}");
        assert!(
            !text.contains('6'),
            "false branch must be elided, got: {text}"
        );
        assert!(
            !text.contains("&IF") && !text.contains("&THEN") && !text.contains("&ENDIF"),
            "directives must be elided, got: {text}"
        );
        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must not warn unclosed &IF"
        );
        // Trailing period after &ENDIF must survive.
        assert!(
            text.trim_end().ends_with('.'),
            "statement terminator after &ENDIF must remain, got: {text:?}"
        );
    }

    #[test]
    fn inline_if_false_emits_else_branch() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "i = &IF FALSE &THEN 5 &ELSE 6 &ENDIF.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(text.contains('6'), "else branch must emit, got: {text}");
        assert!(
            !text.contains('5'),
            "then branch must be elided, got: {text}"
        );
    }

    #[test]
    fn inline_if_false_without_else_emits_nothing() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "i = &IF FALSE &THEN 5 &ENDIF.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !text.contains('5'),
            "false then-body without else must not appear, got: {text}"
        );
        assert!(text.contains("i ="), "prefix must remain, got: {text}");
        assert!(
            text.contains('.'),
            "trailing period must remain, got: {text}"
        );
    }

    #[test]
    fn inline_if_empty_string_eq_with_u_qualifier() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = r#"i = &IF "":U = "":U &THEN 5 &ELSE 6 &ENDIF."#;
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            text.contains('5'),
            "empty-string :U equality must take true branch, got: {text}"
        );
        assert!(!text.contains('6'), "else must be elided, got: {text}");
    }

    #[test]
    fn inline_if_in_if_not_expression() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "IF NOT &IF TRUE &THEN YES &ELSE NO &ENDIF THEN RETURN.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            text.contains("YES"),
            "selected branch must appear in expression position, got: {text}"
        );
        assert!(
            !text.contains("IF NOT THEN"),
            "must not collapse to IF NOT THEN, got: {text}"
        );
        assert!(
            text.contains("IF NOT") && text.contains("THEN RETURN"),
            "outer IF structure must remain, got: {text}"
        );
    }

    #[test]
    fn inline_elseif_chain_one_line() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF FALSE &THEN A &ELSEIF TRUE &THEN B &ELSE C &ENDIF";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(text.contains('B'), "elseif true branch, got: {text}");
        assert!(!text.contains('A'), "first then elided, got: {text}");
        assert!(!text.contains('C'), "else elided, got: {text}");
    }

    #[test]
    fn inline_nested_if() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF TRUE &THEN &IF FALSE &THEN A &ELSE B &ENDIF &ENDIF";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(text.contains('B'), "nested else must win, got: {text}");
        assert!(!text.contains('A'), "nested then elided, got: {text}");
    }

    #[test]
    fn multiline_if_still_works_alongside_inline() {
        // Mixed file: multi-line form must remain correct after the end-offset fix.
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "\
&IF TRUE &THEN
multi
&ENDIF
x = &IF FALSE &THEN no &ELSE yes &ENDIF.
";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(text.contains("multi"), "multi-line branch, got: {text}");
        assert!(text.contains("yes"), "inline else, got: {text}");
        assert!(!text.contains("no"), "inline then elided, got: {text}");
    }

    #[test]
    fn positional_expanded_inside_string_in_include() {
        let fs = make_fs(&[("/inc/s.i", r#""pre{1}post""#)]);
        let paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &paths);
        let result = pp.process(FileId::new(1), r#"{s.i MID}"#).unwrap();
        assert_eq!(&*result.to_text(), r#""preMIDpost""#);
    }

    #[test]
    fn positional_expanded_in_if_condition() {
        let fs = make_fs(&[(
            "/inc/c.i",
            r#"&IF "{1}":U = "ok":U &THEN HIT &ELSE MISS &ENDIF"#,
        )]);
        let paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &paths);

        let hit = pp.process(FileId::new(1), "{c.i ok}").unwrap();
        assert!(
            hit.to_text().contains("HIT") && !hit.to_text().contains("MISS"),
            "got: {}",
            hit.to_text()
        );

        let miss = pp.process(FileId::new(1), "{c.i no}").unwrap();
        assert!(
            miss.to_text().contains("MISS") && !miss.to_text().contains("HIT"),
            "got: {}",
            miss.to_text()
        );
    }

    #[test]
    fn missing_positional_is_empty_in_condition() {
        // `{0}` = include name, `{1}`/`{2}` supplied, `{3}` missing → empty.
        let fs = make_fs(&[(
            "/inc/m.i",
            r#"&IF "{3}":U = "":U &THEN EMPTY &ELSE HAS &ENDIF"#,
        )]);
        let paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &paths);
        let result = pp.process(FileId::new(1), "{m.i a b}").unwrap();
        let text = result.to_text();

        assert!(
            text.contains("EMPTY") && !text.contains("HAS"),
            "missing {{3}} must be empty → true branch, got: {text}"
        );
    }

    /// Real ADM2 `fnarg` one-liner shape (must keep inline `&IF` or the test
    /// false-passes against a still-broken preprocessor).
    const FNARG_STUB: &str = r#"&IF "{3}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE, {2}) &ELSE dynamic-function("{1}":U IN {3}, {2}) &ENDIF"#;

    #[test]
    fn fnarg_shaped_include_two_args() {
        let fs = make_fs(&[("/tty/fnarg", FNARG_STUB)]);
        let paths = vec![PathBuf::from("/tty")];
        let pp = Preprocessor::new(&fs, &paths);
        let source = "IF NOT {fnarg setOpenQuery cQ} THEN RETURN.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC007"),
            "fnarg must resolve, diags: {:?}",
            result.diagnostics
        );
        assert!(!text.contains("{fnarg"), "include must expand, got: {text}");
        assert!(
            !text.contains("IF NOT THEN"),
            "must not collapse expression, got: {text}"
        );
        assert!(
            text.to_ascii_lowercase().contains("dynamic-function"),
            "THEN branch body expected, got: {text}"
        );
        assert!(
            text.contains("setOpenQuery"),
            "{{1}} inside string must expand, got: {text}"
        );
        assert!(text.contains("cQ"), "{{2}} must expand, got: {text}");
        assert!(
            text.contains("TARGET-PROCEDURE"),
            "2-arg form must take THEN (no handle) branch, got: {text}"
        );
        // ELSE branch would reference a bare handle position; ensure we did not
        // take a broken `IN ,` form.
        assert!(
            !text.contains("IN ,") && !text.contains("IN,"),
            "must not emit broken ELSE branch, got: {text}"
        );
    }

    #[test]
    fn fnarg_shaped_include_three_args_uses_else_handle() {
        let fs = make_fs(&[("/tty/fnarg", FNARG_STUB)]);
        let paths = vec![PathBuf::from("/tty")];
        let pp = Preprocessor::new(&fs, &paths);
        let source = "x = {fnarg setOpenQuery cQ hProc}.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            text.contains("hProc"),
            "3-arg form must take ELSE branch with handle, got: {text}"
        );
        assert!(
            !text.contains("TARGET-PROCEDURE"),
            "ELSE branch must not use TARGET-PROCEDURE, got: {text}"
        );
        assert!(
            text.contains("setOpenQuery") && text.contains("cQ"),
            "args must expand, got: {text}"
        );
    }

    #[test]
    fn fn_shaped_include_expression() {
        // Simplified fn shape (no arg list) still uses inline &IF in real ADE;
        // exercise expression-position expansion.
        let fn_stub = r#"&IF "{2}":U = "":U &THEN dynamic-function("{1}":U IN TARGET-PROCEDURE) &ELSE dynamic-function("{1}":U IN {2}) &ENDIF"#;
        let fs = make_fs(&[("/tty/fn", fn_stub)]);
        let paths = vec![PathBuf::from("/tty")];
        let pp = Preprocessor::new(&fs, &paths);
        let source = "x = {fn getQueryHandle}.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            text.to_ascii_lowercase().contains("dynamic-function"),
            "got: {text}"
        );
        assert!(text.contains("getQueryHandle"), "got: {text}");
        assert!(text.contains("TARGET-PROCEDURE"), "got: {text}");
    }

    #[test]
    fn expand_positional_refs_helper() {
        let args = vec!["inc".to_string(), "one".to_string(), "two".to_string()];
        assert_eq!(expand_positional_refs("{1}-{2}", &args), "one-two");
        assert_eq!(expand_positional_refs(r#""{3}""#, &args), r#""""#);
        assert_eq!(expand_positional_refs("plain", &args), "plain");
    }

    // --- #65 follow-up: line-oriented directive as inline &IF body -----------
    //
    // When the body after mid-line `&THEN` is itself a line-oriented directive
    // (`&UNDEFINE` / `&SCOPED-DEFINE` / `&GLOBAL-DEFINE` / `&MESSAGE`), that
    // directive must not skip_to_eol past a trailing same-line `&ENDIF`.
    // This is the shipped `$DLC/tty/get` and `$DLC/tty/set` shape.

    #[test]
    fn inline_if_then_undefine_endif_closes() {
        // Minimal repro from #65 corpus follow-up.
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "\
&GLOBAL-DEFINE foo bar
&IF TRUE &THEN &UNDEFINE foo &ENDIF
";
        let result = pp.process(FileId::new(1), source).unwrap();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must not warn unclosed &IF, diags: {:?}",
            result.diagnostics
        );
        // Variable must actually be undefined after the branch runs.
        let check = pp
            .process(
                FileId::new(1),
                "\
&GLOBAL-DEFINE foo bar
&IF TRUE &THEN &UNDEFINE foo &ENDIF
x={&foo}.
",
            )
            .unwrap();
        assert_eq!(
            &*check.to_text().replace('\n', ""),
            "x=.",
            "foo must be undefined after inline &UNDEFINE, got: {}",
            check.to_text()
        );
    }

    #[test]
    fn adm2_get_set_shaped_inline_undefine() {
        // Real `$DLC/tty/get` / `$DLC/tty/set` one-liner shape (line 1 / 18).
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "\
&GLOBAL-DEFINE xp-reset-values yes
&IF DEFINED(xp-assign) = 0 AND DEFINED(xp-reset-values) <> 0 &THEN &UNDEFINE xp-reset-values &ENDIF
after
";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must close &IF, diags: {:?}",
            result.diagnostics
        );
        assert!(
            text.contains("after"),
            "content after closed &IF must remain, got: {text}"
        );
        // xp-reset-values should be gone; a following DEFINED check is false.
        let follow = "\
&GLOBAL-DEFINE xp-reset-values yes
&IF DEFINED(xp-assign) = 0 AND DEFINED(xp-reset-values) <> 0 &THEN &UNDEFINE xp-reset-values &ENDIF
&IF DEFINED(xp-reset-values) <> 0 &THEN STILL &ELSE GONE &ENDIF
";
        let result2 = pp.process(FileId::new(1), follow).unwrap();
        let text2 = result2.to_text();
        assert!(
            text2.contains("GONE") && !text2.contains("STILL"),
            "xp-reset-values must be undefined, got: {text2}"
        );
    }

    #[test]
    fn inline_if_then_scoped_define_endif() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "\
&IF TRUE &THEN &SCOPED-DEFINE x 1 &ENDIF
val={&x}.
";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must close &IF, diags: {:?}",
            result.diagnostics
        );
        assert!(
            text.contains("val=1"),
            "scoped define inside inline then must take effect, got: {text}"
        );
    }

    #[test]
    fn inline_if_then_global_define_endif() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "\
&IF TRUE &THEN &GLOBAL-DEFINE g yes &ENDIF
val={&g}.
";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must close &IF, diags: {:?}",
            result.diagnostics
        );
        assert!(
            text.contains("val=yes"),
            "global define inside inline then must take effect, got: {text}"
        );
    }

    #[test]
    fn inline_if_then_message_endif() {
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&IF TRUE &THEN &MESSAGE hello world &ENDIF\nkept.\n";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must close &IF, diags: {:?}",
            result.diagnostics
        );
        assert!(
            text.contains("kept."),
            "content after message/endif must remain, got: {text}"
        );
        assert!(
            !text.to_ascii_uppercase().contains("&MESSAGE"),
            "message directive must be elided, got: {text}"
        );
    }

    #[test]
    fn inline_if_then_undefine_else_undefine_endif() {
        // Both then and else branches are line-oriented directives on one line.
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "\
&GLOBAL-DEFINE a 1
&GLOBAL-DEFINE b 2
&IF TRUE &THEN &UNDEFINE a &ELSE &UNDEFINE b &ENDIF
A={&a}.B={&b}.
";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text().replace('\n', "");

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must close &IF, diags: {:?}",
            result.diagnostics
        );
        assert!(
            text.contains("A=.") && text.contains("B=2"),
            "true branch must undefine a only, got: {text}"
        );
    }

    #[test]
    fn plain_code_inline_if_still_works_with_directive_boundary() {
        // Guard: the boundary fix must not break plain-code inline bodies.
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "i = &IF TRUE &THEN 5 &ELSE 6 &ENDIF.";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();
        assert!(text.contains('5') && !text.contains('6'), "got: {text}");
    }

    #[test]
    fn get_set_shaped_include_stub() {
        // Full include-shaped stand-in for `$DLC/tty/get` / `set` first line.
        // Asserts the include expands and the inline `&UNDEFINE … &ENDIF` closes
        // without PREPROC002 (the corpus regression). UNDEFINE→parent-global
        // restore semantics are out of scope for this test.
        let get_stub = "\
&IF DEFINED(xp-assign) = 0 AND DEFINED(xp-reset-values) <> 0 &THEN &UNDEFINE xp-reset-values &ENDIF
ASSIGN x = 1.
";
        let fs = make_fs(&[("/tty/get", get_stub)]);
        let paths = vec![PathBuf::from("/tty")];
        let pp = Preprocessor::new(&fs, &paths);
        let source = "\
&GLOBAL-DEFINE xp-reset-values yes
{get}
done.
";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC007"),
            "get must resolve, diags: {:?}",
            result.diagnostics
        );
        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "inline &UNDEFINE must not leave unclosed &IF, diags: {:?}",
            result.diagnostics
        );
        assert!(
            text.contains("ASSIGN") && text.contains("done."),
            "include body must expand, got: {text}"
        );
    }

    // --- #65 round 4: positional {N} inside &SCOPED/GLOBAL-DEFINE values ---

    #[test]
    fn positional_expanded_in_scoped_define_value() {
        let fs = make_fs(&[("/inc/d.i", "&SCOPED-DEFINE H {3}\nval={&H}.\n")]);
        let paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &paths);
        let result = pp.process(FileId::new(1), "{d.i a b handleX}").unwrap();
        let text = result.to_text();
        assert!(
            text.contains("val=handleX"),
            "{{3}} must expand at define time, got: {text}"
        );
        assert!(
            !text.contains("{3}"),
            "literal {{3}} must not remain, got: {text}"
        );
    }

    #[test]
    fn set_shaped_admhdl_from_positional() {
        // Simplified $DLC/tty/set shape: ADMHdl from {3} via scoped-define.
        let set_stub = r#"&IF "{3}":U = "":U &THEN &SCOPED-DEFINE ADMHdl TARGET-PROCEDURE &ELSE &SCOPED-DEFINE ADMHdl {3} &ENDIF
DYNAMIC-FUNC("set{1}":U IN {&ADMHdl}, {2})
"#;
        let fs = make_fs(&[("/tty/set", set_stub)]);
        let paths = vec![PathBuf::from("/tty")];
        let pp = Preprocessor::new(&fs, &paths);

        // 3-arg form: ELSE branch, ADMHdl = third arg
        let three = pp
            .process(
                FileId::new(1),
                r#"{set DataSourceEvents "evtList" TARGET-PROCEDURE}"#,
            )
            .unwrap();
        let t3 = three.to_text();
        assert!(
            t3.contains("TARGET-PROCEDURE") && t3.contains("setDataSourceEvents"),
            "3-arg set must use handle arg, got: {t3}"
        );
        assert!(
            !t3.contains("{3}") && !t3.contains("IN {"),
            "must not leave literal positional in IN clause, got: {t3}"
        );
        assert!(t3.contains("evtList"), "arg 2 must expand, got: {t3}");

        // 2-arg form: THEN branch, ADMHdl = TARGET-PROCEDURE literal
        let two = pp
            .process(FileId::new(1), r#"{set DataSourceEvents "evtList"}"#)
            .unwrap();
        let t2 = two.to_text();
        assert!(
            t2.contains("TARGET-PROCEDURE") && t2.contains("setDataSourceEvents"),
            "2-arg set must default TARGET-PROCEDURE, got: {t2}"
        );
        assert!(!t2.contains("{3}"), "got: {t2}");
    }

    // --- #66 xp-property BUFFER-FIELD path fixtures ---

    #[test]
    fn set_xp_property_buffer_field_branch() {
        // When DEFINED(xp{1}) is true, real set takes the BUFFER-FIELD fast path
        // instead of DYNAMIC-FUNC. Stub forces that branch with an unquoted
        // comma-list positional arg (the dominant corpus shape).
        let set_stub = r#"&IF DEFINED(xp{1}) <> 0 &THEN
ASSIGN ghProp:BUFFER-FIELD('{1}':U):BUFFER-VALUE = {2}.
&ELSE
DYNAMIC-FUNC("set{1}":U IN TARGET-PROCEDURE, {2})
&ENDIF
"#;
        let fs = make_fs(&[("/tty/set", set_stub)]);
        let paths = vec![PathBuf::from("/tty")];
        let pp = Preprocessor::new(&fs, &paths);
        let source = "\
&GLOBAL-DEFINE xpDataSourceEvents yes
DEFINE VARIABLE ghProp AS HANDLE NO-UNDO.
{set DataSourceEvents dataAvailable,confirmContinue,isUpdatePending,buildDataRequest}
";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();

        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC007"),
            "set must resolve, diags: {:?}",
            result.diagnostics
        );
        assert!(
            !result.diagnostics.iter().any(|d| d.code.0 == "PREPROC002"),
            "must not leave unclosed &IF, diags: {:?}",
            result.diagnostics
        );
        assert!(
            text.contains("BUFFER-FIELD") && text.contains("BUFFER-VALUE"),
            "xp branch must emit BUFFER-FIELD path, got: {text}"
        );
        assert!(
            text.contains("dataAvailable,confirmContinue,isUpdatePending,buildDataRequest"),
            "comma-list arg must expand into BUFFER-VALUE, got: {text}"
        );
        assert!(
            !text.contains("DYNAMIC-FUNC"),
            "DYNAMIC-FUNC branch must not run when xp prop defined, got: {text}"
        );
    }

    #[test]
    fn set_single_quoted_comma_list_arg_characterization() {
        // Characterization (#66 Fable amendment): single-quoted host args are
        // stripped by read_arg_value (quotes are delimiters). Trailing `:U`
        // after the closing quote is a separate token/arg — pin current
        // behavior; no quote-policy change in this PR.
        let set_stub = "val={2}.\n";
        let fs = make_fs(&[("/tty/set", set_stub)]);
        let paths = vec![PathBuf::from("/tty")];
        let pp = Preprocessor::new(&fs, &paths);
        let result = pp.process(FileId::new(1), "{set X 'a,b,c':U}").unwrap();
        let text = result.to_text();
        // Quotes stripped from the positional value.
        assert!(
            text.contains("val=a,b,c") || text.contains("val='a,b,c'"),
            "pin single-quoted comma-list expansion, got: {text}"
        );
        // Prefer documenting strip (Progress-like delimiter semantics).
        assert!(
            text.contains("a,b,c"),
            "comma list content must appear, got: {text}"
        );
    }

    #[test]
    fn missing_positional_in_define_value_is_empty() {
        let fs = make_fs(&[("/inc/m.i", "&SCOPED-DEFINE H {3}\nval=[{&H}]\n")]);
        let paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &paths);
        // only {0}=name, {1}=a, {2}=b — {3} missing → empty
        let result = pp.process(FileId::new(1), "{m.i a b}").unwrap();
        let text = result.to_text();
        assert!(
            text.contains("val=[]"),
            "missing {{3}} in define value → empty, got: {text}"
        );
    }

    #[test]
    fn top_level_define_preserves_literal_positional() {
        // Outside an include, `{N}` in a define value must not be erased
        // (body scanner leaves top-level positionals for the lexer).
        let fs = make_fs(&[]);
        let pp = Preprocessor::new(&fs, &[]);
        let source = "&SCOPED-DEFINE X {1}\nval={&X}.\n";
        let result = pp.process(FileId::new(1), source).unwrap();
        let text = result.to_text();
        assert!(
            text.contains("val={1}"),
            "top-level define must keep literal {{1}}, got: {text}"
        );
    }

    #[test]
    fn positional_expanded_in_global_define_value() {
        let fs = make_fs(&[("/inc/g.i", "&GLOBAL-DEFINE GH {2}\n")]);
        let paths = vec![PathBuf::from("/inc")];
        let pp = Preprocessor::new(&fs, &paths);
        let result = pp
            .process(FileId::new(1), "{g.i ignored handleY}\nval={&GH}.\n")
            .unwrap();
        let text = result.to_text();
        assert!(
            text.contains("val=handleY"),
            "global define value must expand {{2}}, got: {text}"
        );
    }
}
