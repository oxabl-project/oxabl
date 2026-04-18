//! Hand-written recursive-descent parser for Progress `.df` dump files.
//!
//! Grammar follows Riverside Software's `DumpFileGrammar.g4`
//! (MIT, sonar-openedge) — see `docs/plans/2026-04-16-004-feat-semantic-layer-v1-plan.md`
//! §`oxabl_schema`. The v1 scope covers `ADD TABLE`, `ADD FIELD`, and
//! `ADD INDEX` with their attributes; other directives (`ADD SEQUENCE`,
//! `ADD DATABASE`, `ADD CONSTRAINT`, `UPDATE`, `DROP`, `RENAME`,
//! `@annotations`, and the `PSC` trailer) are accepted and silently skipped
//! so that format drift never hard-errors the loader.
//!
//! Unknown attributes on a recognised directive are captured in
//! `Field::extras` as opaque `(name, value)` pairs so they round-trip into
//! any future formatter or schema inspector.

use oxabl_ast::Span;
use oxabl_common::{Diagnostic, FileId, FileSpan};

use crate::atom::fold_atom;
use crate::diagnostics::SCHEMA0001;
use crate::schema::{Field, Index, IndexField, SchemaType, Table};

/// Parse the contents of a single `.df` file into table/field/index
/// records, ready to be merged into a [`crate::Schema`] by the loader.
pub fn parse_df(source: &str, file_id: FileId) -> ParseOutcome {
    let (tokens, mut diags) = Tokenizer::run(source, file_id);
    let mut parser = Parser {
        src: source,
        tokens: &tokens,
        idx: 0,
        file_id,
        outcome: ParseOutcome::default(),
    };
    parser.parse_dump();
    let mut outcome = parser.outcome;
    outcome.diagnostics.append(&mut diags);
    outcome
}

/// Records produced by parsing one `.df` file. The loader is responsible for
/// stitching fields and indexes into their parent tables (they may appear in
/// the file before the parent `ADD TABLE` directive only via `UPDATE`, which
/// v1 skips).
#[derive(Debug, Default)]
pub struct ParseOutcome {
    pub tables: Vec<Table>,
    /// Field entries associated with the quoted table name they declare
    /// themselves `OF`. The loader matches this back to a `Table` by folded
    /// atom.
    pub fields: Vec<PendingField>,
    pub indexes: Vec<PendingIndex>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub struct PendingField {
    pub table: oxabl_lexer::oxabl_atom::OxablAtom,
    pub table_display: String,
    pub field: Field,
}

#[derive(Debug)]
pub struct PendingIndex {
    pub table: oxabl_lexer::oxabl_atom::OxablAtom,
    pub table_display: String,
    pub index: Index,
}

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokKind {
    Word,
    QuotedString,
    Annotation,
    LParen,
    RParen,
    Equals,
    Comma,
    Dot,
}

#[derive(Debug)]
struct Token {
    kind: TokKind,
    start: u32,
    end: u32,
    /// `true` iff this is the first non-whitespace/comment token on its
    /// physical line. Used to detect directive boundaries — a top-level
    /// keyword at `line_start` terminates the previous directive's option
    /// list.
    line_start: bool,
}

struct Tokenizer<'a> {
    bytes: &'a [u8],
    pos: usize,
    at_line_start: bool,
    tokens: Vec<Token>,
    diagnostics: Vec<Diagnostic>,
    file_id: FileId,
}

impl<'a> Tokenizer<'a> {
    fn run(source: &'a str, file_id: FileId) -> (Vec<Token>, Vec<Diagnostic>) {
        let mut t = Tokenizer {
            bytes: source.as_bytes(),
            pos: 0,
            at_line_start: true,
            tokens: Vec::with_capacity(source.len() / 6),
            diagnostics: Vec::new(),
            file_id,
        };
        t.skip_bom();
        t.run_loop();
        (t.tokens, t.diagnostics)
    }

    fn skip_bom(&mut self) {
        if self.bytes.starts_with(&[0xEF, 0xBB, 0xBF]) {
            self.pos = 3;
        }
    }

    fn run_loop(&mut self) {
        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            match b {
                b' ' | b'\t' | b'\r' => {
                    self.pos += 1;
                }
                b'\n' => {
                    self.pos += 1;
                    self.at_line_start = true;
                }
                b'#' => {
                    // Line comment: skip to end of line (common in hand-edited .df).
                    while self.pos < self.bytes.len() && self.bytes[self.pos] != b'\n' {
                        self.pos += 1;
                    }
                }
                b'/' if self.peek(1) == Some(b'*') => {
                    self.skip_block_comment();
                }
                b'(' => self.push_punct(TokKind::LParen, 1),
                b')' => self.push_punct(TokKind::RParen, 1),
                b'=' => self.push_punct(TokKind::Equals, 1),
                b',' => self.push_punct(TokKind::Comma, 1),
                b'.' => self.push_punct(TokKind::Dot, 1),
                b'"' => self.read_quoted_string(),
                b'@' => self.read_annotation(),
                _ => self.read_word(),
            }
        }
    }

    fn peek(&self, offset: usize) -> Option<u8> {
        self.bytes.get(self.pos + offset).copied()
    }

    fn push_punct(&mut self, kind: TokKind, len: usize) {
        let start = self.pos as u32;
        self.pos += len;
        let end = self.pos as u32;
        let line_start = self.at_line_start;
        self.at_line_start = false;
        self.tokens.push(Token {
            kind,
            start,
            end,
            line_start,
        });
    }

    fn skip_block_comment(&mut self) {
        let start = self.pos;
        self.pos += 2; // consume `/*`
        while self.pos + 1 < self.bytes.len() {
            if self.bytes[self.pos] == b'*' && self.bytes[self.pos + 1] == b'/' {
                self.pos += 2;
                return;
            }
            if self.bytes[self.pos] == b'\n' {
                self.at_line_start = true;
            }
            self.pos += 1;
        }
        // Unterminated — consume to EOF and flag.
        self.pos = self.bytes.len();
        self.diagnostics.push(Diagnostic::error(
            SCHEMA0001,
            "unterminated `/* ... */` comment".to_string(),
            self.span(start, self.pos),
        ));
    }

    fn read_quoted_string(&mut self) {
        let start = self.pos;
        let line_start = self.at_line_start;
        self.at_line_start = false;
        self.pos += 1; // consume opening "
        loop {
            if self.pos >= self.bytes.len() {
                self.diagnostics.push(Diagnostic::error(
                    SCHEMA0001,
                    "unterminated quoted string".to_string(),
                    self.span(start, self.pos),
                ));
                break;
            }
            let b = self.bytes[self.pos];
            if b == b'"' {
                // `""` is an embedded quote; keep going.
                if self.peek(1) == Some(b'"') {
                    self.pos += 2;
                    continue;
                }
                self.pos += 1;
                break;
            }
            // `.df` quoted strings may span multiple source lines (see
            // Riverside `DumpFileGrammar.g4` — `QUOTED_STRING`'s `~'"'`
            // matches any non-quote char, newlines included). Track
            // `at_line_start` so tokens following the close-quote stay
            // correctly classified.
            if b == b'\n' {
                self.at_line_start = true;
            }
            self.pos += 1;
        }
        let end = self.pos as u32;
        self.tokens.push(Token {
            kind: TokKind::QuotedString,
            start: start as u32,
            end,
            line_start,
        });
    }

    fn read_annotation(&mut self) {
        let start = self.pos;
        self.pos += 1; // @
        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            if b.is_ascii_alphanumeric() || b == b'.' || b == b'_' {
                self.pos += 1;
            } else {
                break;
            }
        }
        let end = self.pos as u32;
        let line_start = self.at_line_start;
        self.at_line_start = false;
        self.tokens.push(Token {
            kind: TokKind::Annotation,
            start: start as u32,
            end,
            line_start,
        });
    }

    fn read_word(&mut self) {
        let start = self.pos;
        while self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            // UNQUOTED_STRING excludes: space tab newline " ' ( ) = , . #
            // `.` is excluded so `.` is always a standalone terminator token.
            match b {
                b' ' | b'\t' | b'\r' | b'\n' | b'"' | b'\'' | b'(' | b')' | b'=' | b',' | b'.'
                | b'#' => break,
                _ => self.pos += 1,
            }
        }
        let end = self.pos as u32;
        if end == start as u32 {
            // Defensive: a char we don't know how to tokenize. Skip it with
            // a diagnostic rather than looping forever.
            self.diagnostics.push(Diagnostic::error(
                SCHEMA0001,
                format!("unexpected byte 0x{:02x}", self.bytes[start]),
                self.span(start, start + 1),
            ));
            self.pos = start + 1;
            return;
        }
        let line_start = self.at_line_start;
        self.at_line_start = false;
        self.tokens.push(Token {
            kind: TokKind::Word,
            start: start as u32,
            end,
            line_start,
        });
    }

    fn span(&self, start: usize, end: usize) -> FileSpan {
        FileSpan {
            file: self.file_id,
            span: Span {
                start: start as u32,
                end: end as u32,
            },
        }
    }
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct Parser<'a> {
    src: &'a str,
    tokens: &'a [Token],
    idx: usize,
    file_id: FileId,
    outcome: ParseOutcome,
}

/// Top-level directive keywords. Encountering one of these as a `Word` at
/// `line_start` terminates the current directive's option list.
const DIRECTIVE_KEYWORDS: &[&str] = &["ADD", "UPDATE", "CHANGE", "DROP", "RENAME"];

impl<'a> Parser<'a> {
    fn parse_dump(&mut self) {
        while self.idx < self.tokens.len() {
            // Swallow trailing footer markers: `.` at line_start, optional
            // `PSC`, `cpstream=...`, trailing digits.
            if self.peek_kind() == Some(TokKind::Dot) && self.tokens[self.idx].line_start {
                self.skip_footer();
                continue;
            }
            if self.peek_kind() == Some(TokKind::Annotation) {
                self.skip_annotation();
                continue;
            }
            let Some(word) = self.peek_word_upper() else {
                // Stray punctuation / unrecognised construct — skip one token
                // and keep going.
                self.emit_unexpected();
                self.idx += 1;
                continue;
            };
            match word.as_str() {
                "ADD" => self.parse_add(),
                "UPDATE" | "CHANGE" | "DROP" | "RENAME" => {
                    // Consume the starting keyword so skip doesn't immediately
                    // re-match on its own `line_start` and stall.
                    self.idx += 1;
                    self.skip_to_next_directive();
                }
                _ => {
                    self.emit_unexpected();
                    self.idx += 1;
                    self.skip_to_next_directive();
                }
            }
        }
    }

    fn parse_add(&mut self) {
        // Consume `ADD`.
        self.idx += 1;
        // Optional `UNIQUE` modifier between `ADD` and `INDEX`.
        let unique = matches!(self.peek_word_upper().as_deref(), Some("UNIQUE"));
        if unique {
            self.idx += 1;
        }
        let Some(kind) = self.peek_word_upper() else {
            self.emit_unexpected();
            if self.idx < self.tokens.len() {
                self.idx += 1;
            }
            self.skip_to_next_directive();
            return;
        };
        match kind.as_str() {
            "TABLE" => {
                self.idx += 1;
                self.parse_add_table();
            }
            "FIELD" => {
                self.idx += 1;
                self.parse_add_field();
            }
            "INDEX" => {
                self.idx += 1;
                self.parse_add_index(unique);
            }
            _ => {
                // ADD SEQUENCE / DATABASE / CONSTRAINT / anything else:
                // silently skip to the next directive.
                self.skip_to_next_directive();
            }
        }
    }

    fn parse_add_table(&mut self) {
        let Some((display_name, source)) = self.expect_quoted() else {
            self.skip_to_next_directive();
            return;
        };
        // Optional `TYPE <ident>` — we ignore the value.
        if matches!(self.peek_word_upper().as_deref(), Some("TYPE")) {
            self.idx += 1;
            if self.peek_kind() == Some(TokKind::Word) {
                self.idx += 1;
            }
        }
        let mut table = Table {
            name: fold_atom(&display_name),
            display_name,
            fields: Vec::new(),
            indexes: Vec::new(),
            area: None,
            description: None,
            dump_name: None,
            frozen: false,
            hidden: false,
            label: None,
            valexp: None,
            valmsg: None,
            source,
        };
        self.consume_table_options(&mut table);
        self.outcome.tables.push(table);
    }

    fn consume_table_options(&mut self, t: &mut Table) {
        loop {
            if self.is_at_next_directive() {
                break;
            }
            let Some(attr) = self.peek_word_upper() else {
                // Non-word token at an attribute position — skip to recover.
                self.idx += 1;
                continue;
            };
            match attr.as_str() {
                "AREA" => {
                    self.idx += 1;
                    t.area = self.expect_quoted().map(|(s, _)| s);
                }
                "DESCRIPTION" => {
                    self.idx += 1;
                    t.description = self.expect_quoted().map(|(s, _)| s);
                }
                "LABEL" => {
                    self.idx += 1;
                    t.label = self.expect_quoted().map(|(s, _)| s);
                }
                "DUMP-NAME" => {
                    self.idx += 1;
                    t.dump_name = self.expect_quoted().map(|(s, _)| s);
                }
                "FROZEN" => {
                    self.idx += 1;
                    t.frozen = true;
                }
                "HIDDEN" => {
                    self.idx += 1;
                    t.hidden = true;
                }
                "VALEXP" => {
                    self.idx += 1;
                    t.valexp = self.expect_quoted().map(|(s, _)| s);
                }
                "VALMSG" | "VALMSG-SA" | "LABEL-SA" => {
                    self.idx += 1;
                    let v = self.expect_quoted().map(|(s, _)| s);
                    if attr == "VALMSG" {
                        t.valmsg = v;
                    }
                }
                "TABLE-TRIGGER" => self.skip_table_trigger(),
                _ => self.skip_unknown_attr(),
            }
        }
    }

    fn parse_add_field(&mut self) {
        let Some((field_name, field_source)) = self.expect_quoted() else {
            self.skip_to_next_directive();
            return;
        };
        if !self.expect_word_ci("OF") {
            self.skip_to_next_directive();
            return;
        }
        let Some((table_name, _)) = self.expect_quoted() else {
            self.skip_to_next_directive();
            return;
        };
        if !self.expect_word_ci("AS") {
            self.skip_to_next_directive();
            return;
        }
        let data_type = match self.expect_word() {
            Some(text) => SchemaType::classify(&text),
            None => {
                self.skip_to_next_directive();
                return;
            }
        };
        let mut field = Field {
            name: fold_atom(&field_name),
            display_name: field_name,
            data_type,
            extent: None,
            mandatory: false,
            case_sensitive: false,
            format: None,
            label: None,
            initial: None,
            description: None,
            decimals: None,
            position: None,
            order: None,
            max_width: None,
            help: None,
            valexp: None,
            valmsg: None,
            extras: Vec::new(),
            source: field_source,
        };
        self.consume_field_options(&mut field);
        self.outcome.fields.push(PendingField {
            table: fold_atom(&table_name),
            table_display: table_name,
            field,
        });
    }

    fn consume_field_options(&mut self, f: &mut Field) {
        loop {
            if self.is_at_next_directive() {
                break;
            }
            let Some(attr) = self.peek_word_upper() else {
                self.idx += 1;
                continue;
            };
            match attr.as_str() {
                "FORMAT" => {
                    self.idx += 1;
                    f.format = self.expect_string_or_word();
                }
                "INITIAL" => {
                    self.idx += 1;
                    f.initial = self.expect_string_or_word();
                }
                "LABEL" => {
                    self.idx += 1;
                    f.label = self.expect_string_or_word();
                }
                "DESCRIPTION" => {
                    self.idx += 1;
                    f.description = self.expect_string_or_word();
                }
                "HELP" => {
                    self.idx += 1;
                    f.help = self.expect_string_or_word();
                }
                "VALEXP" => {
                    self.idx += 1;
                    f.valexp = self.expect_string_or_word();
                }
                "VALMSG" => {
                    self.idx += 1;
                    f.valmsg = self.expect_string_or_word();
                }
                "POSITION" => {
                    self.idx += 1;
                    f.position = self.expect_u32();
                }
                "ORDER" => {
                    self.idx += 1;
                    f.order = self.expect_u32();
                }
                "MAX-WIDTH" => {
                    self.idx += 1;
                    f.max_width = self.expect_u32();
                }
                "DECIMALS" => {
                    self.idx += 1;
                    f.decimals = self.expect_u32();
                }
                "EXTENT" => {
                    self.idx += 1;
                    f.extent = self.expect_u32();
                }
                "MANDATORY" => {
                    self.idx += 1;
                    f.mandatory = true;
                }
                "CASE-SENSITIVE" => {
                    self.idx += 1;
                    f.case_sensitive = true;
                }
                "NOT-CASE-SENSITIVE" => {
                    self.idx += 1;
                    f.case_sensitive = false;
                }
                "FIELD-TRIGGER" => self.skip_field_trigger(),
                _ => self.collect_extra(f, &attr),
            }
        }
    }

    fn collect_extra(&mut self, f: &mut Field, attr: &str) {
        let attr_atom = fold_atom(attr);
        self.idx += 1;
        let value = self.collect_attr_value();
        f.extras.push((attr_atom, value));
    }

    /// Collect a free-form attribute value: zero or more tokens on the same
    /// logical option until we see the next attribute keyword (line_start)
    /// or next directive.
    fn collect_attr_value(&mut self) -> String {
        let mut parts: Vec<String> = Vec::new();
        while self.idx < self.tokens.len() {
            let t = &self.tokens[self.idx];
            if t.line_start {
                break;
            }
            match t.kind {
                TokKind::Word => parts.push(self.word_text(t).to_string()),
                TokKind::QuotedString => parts.push(self.quoted_string_contents(t)),
                TokKind::LParen | TokKind::RParen | TokKind::Equals | TokKind::Comma => {
                    parts.push(self.raw_slice(t).to_string());
                }
                TokKind::Dot | TokKind::Annotation => break,
            }
            self.idx += 1;
        }
        parts.join(" ")
    }

    fn parse_add_index(&mut self, unique: bool) {
        let Some((name, source)) = self.expect_quoted() else {
            self.skip_to_next_directive();
            return;
        };
        if !self.expect_word_ci("ON") {
            self.skip_to_next_directive();
            return;
        }
        let Some((table_name, _)) = self.expect_quoted() else {
            self.skip_to_next_directive();
            return;
        };
        let mut index = Index {
            name: fold_atom(&name),
            display_name: name,
            unique,
            primary: false,
            word: false,
            inactive: false,
            area: None,
            description: None,
            fields: Vec::new(),
            source,
        };
        self.consume_index_options(&mut index);
        self.outcome.indexes.push(PendingIndex {
            table: fold_atom(&table_name),
            table_display: table_name,
            index,
        });
    }

    fn consume_index_options(&mut self, i: &mut Index) {
        loop {
            if self.is_at_next_directive() {
                break;
            }
            let Some(attr) = self.peek_word_upper() else {
                self.idx += 1;
                continue;
            };
            match attr.as_str() {
                "AREA" => {
                    self.idx += 1;
                    i.area = self.expect_quoted().map(|(s, _)| s);
                }
                "UNIQUE" => {
                    self.idx += 1;
                    i.unique = true;
                }
                "PRIMARY" => {
                    self.idx += 1;
                    i.primary = true;
                }
                "WORD" => {
                    self.idx += 1;
                    i.word = true;
                }
                "INACTIVE" => {
                    self.idx += 1;
                    i.inactive = true;
                }
                "DESCRIPTION" => {
                    self.idx += 1;
                    i.description = self.expect_quoted().map(|(s, _)| s);
                }
                "INDEX-FIELD" => {
                    self.idx += 1;
                    if let Some(field) = self.parse_index_field() {
                        i.fields.push(field);
                    }
                }
                "IS-LOCAL" => {
                    self.idx += 1;
                }
                _ => self.skip_unknown_attr(),
            }
        }
    }

    fn parse_index_field(&mut self) -> Option<IndexField> {
        let (name, _) = self.expect_quoted()?;
        let mut ascending = true;
        let mut abbreviated = false;
        let mut unsorted = false;
        while let Some(w) = self.peek_word_upper() {
            match w.as_str() {
                "ASCENDING" => {
                    ascending = true;
                    self.idx += 1;
                }
                "DESCENDING" => {
                    ascending = false;
                    self.idx += 1;
                }
                "ABBREVIATED" => {
                    abbreviated = true;
                    self.idx += 1;
                }
                "UNSORTED" => {
                    unsorted = true;
                    self.idx += 1;
                }
                _ => break,
            }
        }
        Some(IndexField {
            name: fold_atom(&name),
            display_name: name,
            ascending,
            abbreviated,
            unsorted,
        })
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    fn peek_kind(&self) -> Option<TokKind> {
        self.tokens.get(self.idx).map(|t| t.kind)
    }

    fn peek_word_upper(&self) -> Option<String> {
        let t = self.tokens.get(self.idx)?;
        if t.kind != TokKind::Word {
            return None;
        }
        Some(self.word_text(t).to_ascii_uppercase())
    }

    fn is_at_next_directive(&self) -> bool {
        let Some(t) = self.tokens.get(self.idx) else {
            return true;
        };
        match t.kind {
            TokKind::Annotation => true,
            TokKind::Dot if t.line_start => true,
            TokKind::Word if t.line_start => {
                let upper = self.word_text(t).to_ascii_uppercase();
                DIRECTIVE_KEYWORDS.contains(&upper.as_str())
            }
            _ => false,
        }
    }

    fn expect_quoted(&mut self) -> Option<(String, FileSpan)> {
        let t = self.tokens.get(self.idx)?;
        if t.kind != TokKind::QuotedString {
            self.emit_at(
                self.idx,
                format!("expected quoted string, got `{}`", self.raw_slice(t)),
            );
            return None;
        }
        let span = FileSpan {
            file: self.file_id,
            span: Span {
                start: t.start,
                end: t.end,
            },
        };
        let text = self.quoted_string_contents(t);
        self.idx += 1;
        Some((text, span))
    }

    fn expect_word(&mut self) -> Option<String> {
        let t = self.tokens.get(self.idx)?;
        if t.kind != TokKind::Word {
            self.emit_at(
                self.idx,
                format!("expected identifier, got `{}`", self.raw_slice(t)),
            );
            return None;
        }
        let text = self.word_text(t).to_string();
        self.idx += 1;
        Some(text)
    }

    fn expect_word_ci(&mut self, expected: &str) -> bool {
        let Some(word) = self.peek_word_upper() else {
            self.emit_at(self.idx, format!("expected `{expected}`"));
            return false;
        };
        if word.eq_ignore_ascii_case(expected) {
            self.idx += 1;
            true
        } else {
            self.emit_at(self.idx, format!("expected `{expected}`, got `{word}`"));
            false
        }
    }

    /// Accept either a quoted string or a single unquoted word — mirrors the
    /// `('?' | UNQUOTED_STRING | QUOTED_STRING)` alternatives in the grammar
    /// for many field attributes.
    fn expect_string_or_word(&mut self) -> Option<String> {
        let t = self.tokens.get(self.idx)?;
        match t.kind {
            TokKind::QuotedString => {
                let v = self.quoted_string_contents(t);
                self.idx += 1;
                Some(v)
            }
            TokKind::Word => {
                let v = self.word_text(t).to_string();
                self.idx += 1;
                Some(v)
            }
            _ => {
                self.emit_at(
                    self.idx,
                    format!("expected string or word, got `{}`", self.raw_slice(t)),
                );
                None
            }
        }
    }

    fn expect_u32(&mut self) -> Option<u32> {
        let t = self.tokens.get(self.idx)?;
        if t.kind != TokKind::Word {
            return None;
        }
        let text = self.word_text(t);
        let parsed = text.parse::<i64>().ok()?;
        self.idx += 1;
        if parsed < 0 {
            None
        } else {
            Some(parsed.min(u32::MAX as i64) as u32)
        }
    }

    fn skip_to_next_directive(&mut self) {
        while self.idx < self.tokens.len() && !self.is_at_next_directive() {
            self.idx += 1;
        }
    }

    fn skip_unknown_attr(&mut self) {
        // Unknown attribute on a known directive: consume the attr name and
        // its value(s) until the next attribute or directive. This keeps us
        // resilient to format drift across OpenEdge versions.
        self.idx += 1;
        while self.idx < self.tokens.len() {
            let t = &self.tokens[self.idx];
            if t.line_start {
                break;
            }
            if self.is_at_next_directive() {
                break;
            }
            self.idx += 1;
        }
    }

    fn skip_table_trigger(&mut self) {
        // TABLE-TRIGGER "name" (OVERRIDE? NO-OVERRIDE? PROCEDURE "name" (CRC "x")? | DELETE)
        self.idx += 1;
        while self.idx < self.tokens.len() {
            let t = &self.tokens[self.idx];
            if t.line_start {
                // Next attribute or directive.
                break;
            }
            self.idx += 1;
        }
    }

    fn skip_field_trigger(&mut self) {
        self.idx += 1;
        while self.idx < self.tokens.len() {
            let t = &self.tokens[self.idx];
            if t.line_start {
                break;
            }
            self.idx += 1;
        }
    }

    fn skip_annotation(&mut self) {
        // @name ( key=value , ... ) .
        self.idx += 1;
        if self.peek_kind() == Some(TokKind::LParen) {
            let mut depth = 0u32;
            while self.idx < self.tokens.len() {
                match self.tokens[self.idx].kind {
                    TokKind::LParen => depth += 1,
                    TokKind::RParen => {
                        depth -= 1;
                        if depth == 0 {
                            self.idx += 1;
                            break;
                        }
                    }
                    _ => {}
                }
                self.idx += 1;
            }
        }
        // Optional trailing `.`.
        if self.peek_kind() == Some(TokKind::Dot) {
            self.idx += 1;
        }
    }

    fn skip_footer(&mut self) {
        // `.` PSC? (word '=' (word|number))? (. number?)?
        // Easiest: consume every remaining token. A `.df` file has at most
        // one footer and nothing valid follows it.
        self.idx = self.tokens.len();
    }

    fn emit_at(&mut self, idx: usize, msg: String) {
        let span = match self.tokens.get(idx) {
            Some(t) => FileSpan {
                file: self.file_id,
                span: Span {
                    start: t.start,
                    end: t.end,
                },
            },
            None => FileSpan {
                file: self.file_id,
                span: Span {
                    start: self.src.len() as u32,
                    end: self.src.len() as u32,
                },
            },
        };
        self.outcome
            .diagnostics
            .push(Diagnostic::error(SCHEMA0001, msg, span));
    }

    fn emit_unexpected(&mut self) {
        let snippet = self
            .tokens
            .get(self.idx)
            .map(|t| self.raw_slice(t).to_string())
            .unwrap_or_default();
        self.emit_at(self.idx, format!("unexpected `{snippet}`"));
    }

    fn raw_slice(&self, t: &Token) -> &'a str {
        &self.src[t.start as usize..t.end as usize]
    }

    fn word_text(&self, t: &Token) -> &'a str {
        self.raw_slice(t)
    }

    fn quoted_string_contents(&self, t: &Token) -> String {
        let raw = self.raw_slice(t);
        // Strip opening/closing quote; decode `""` → `"`.
        if raw.len() < 2 || !raw.starts_with('"') {
            return raw.to_string();
        }
        let end = if raw.ends_with('"') && raw.len() >= 2 {
            raw.len() - 1
        } else {
            raw.len()
        };
        let inner = &raw[1..end];
        if !inner.contains("\"\"") {
            return inner.to_string();
        }
        let mut out = String::with_capacity(inner.len());
        let bytes = inner.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'"' && bytes.get(i + 1) == Some(&b'"') {
                out.push('"');
                i += 2;
            } else {
                out.push(bytes[i] as char);
                i += 1;
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use oxabl_common::FileId;

    fn parse(src: &str) -> ParseOutcome {
        parse_df(src, FileId::new(1))
    }

    #[test]
    fn empty_input_produces_no_records() {
        let out = parse("");
        assert!(out.tables.is_empty());
        assert!(out.fields.is_empty());
        assert!(out.indexes.is_empty());
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn parses_minimal_add_table() {
        let out = parse("ADD TABLE \"Customer\"\n");
        assert_eq!(out.tables.len(), 1);
        assert_eq!(out.tables[0].display_name, "Customer");
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn parses_add_table_with_attributes() {
        let out = parse(
            r#"
ADD TABLE "Customer"
  AREA "Inventory"
  DESCRIPTION "Customer table."
  DUMP-NAME "customer"
"#,
        );
        assert_eq!(out.tables.len(), 1);
        let t = &out.tables[0];
        assert_eq!(t.area.as_deref(), Some("Inventory"));
        assert_eq!(t.description.as_deref(), Some("Customer table."));
        assert_eq!(t.dump_name.as_deref(), Some("customer"));
    }

    #[test]
    fn parses_add_field() {
        let out = parse(
            r#"
ADD FIELD "CustNum" OF "Customer" AS integer
  FORMAT ">>,>>9"
  INITIAL "0"
  LABEL "Customer Num"
  POSITION 2
  MAX-WIDTH 4
  ORDER 10
  MANDATORY
"#,
        );
        assert_eq!(out.fields.len(), 1);
        let pf = &out.fields[0];
        assert_eq!(pf.table_display, "Customer");
        assert_eq!(pf.field.display_name, "CustNum");
        assert_eq!(pf.field.data_type, SchemaType::Integer);
        assert_eq!(pf.field.format.as_deref(), Some(">>,>>9"));
        assert_eq!(pf.field.initial.as_deref(), Some("0"));
        assert_eq!(pf.field.label.as_deref(), Some("Customer Num"));
        assert_eq!(pf.field.position, Some(2));
        assert_eq!(pf.field.max_width, Some(4));
        assert_eq!(pf.field.order, Some(10));
        assert!(pf.field.mandatory);
    }

    #[test]
    fn parses_add_index_with_fields() {
        let out = parse(
            r#"
ADD UNIQUE INDEX "custnum" ON "Customer"
  AREA "Inventory"
  PRIMARY
  INDEX-FIELD "CustNum" ASCENDING
"#,
        );
        assert_eq!(out.indexes.len(), 1);
        let pi = &out.indexes[0];
        assert_eq!(pi.table_display, "Customer");
        assert!(pi.index.unique);
        assert!(pi.index.primary);
        assert_eq!(pi.index.area.as_deref(), Some("Inventory"));
        assert_eq!(pi.index.fields.len(), 1);
        assert_eq!(pi.index.fields[0].display_name, "CustNum");
        assert!(pi.index.fields[0].ascending);
    }

    #[test]
    fn multi_field_index() {
        let out = parse(
            r#"
ADD INDEX "composite" ON "Order"
  INDEX-FIELD "CustNum" ASCENDING
  INDEX-FIELD "OrderDate" DESCENDING
"#,
        );
        assert_eq!(out.indexes.len(), 1);
        assert_eq!(out.indexes[0].index.fields.len(), 2);
        assert!(out.indexes[0].index.fields[0].ascending);
        assert!(!out.indexes[0].index.fields[1].ascending);
    }

    #[test]
    fn skips_add_sequence() {
        let out = parse(
            r#"
ADD SEQUENCE "NextNum"
  INITIAL 1000
  INCREMENT 1

ADD TABLE "Customer"
"#,
        );
        assert_eq!(out.tables.len(), 1);
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn skips_update_drop_rename() {
        let out = parse(
            r#"
UPDATE TABLE "Old"
  ENCRYPTION YES
DROP TABLE "Gone"
RENAME FIELD "a" OF "T" TO "b"
ADD TABLE "Kept"
"#,
        );
        assert_eq!(out.tables.len(), 1);
        assert_eq!(out.tables[0].display_name, "Kept");
    }

    #[test]
    fn handles_line_comments() {
        let out = parse(
            r#"
# hand-edited note
ADD TABLE "Customer"
  # comment inside directive
  AREA "Main"
"#,
        );
        assert_eq!(out.tables.len(), 1);
        assert_eq!(out.tables[0].area.as_deref(), Some("Main"));
    }

    #[test]
    fn handles_crlf_line_endings() {
        let src = "ADD TABLE \"Customer\"\r\n  AREA \"Main\"\r\n";
        let out = parse(src);
        assert_eq!(out.tables.len(), 1);
        assert_eq!(out.tables[0].area.as_deref(), Some("Main"));
    }

    #[test]
    fn strips_utf8_bom() {
        let mut src = vec![0xEF, 0xBB, 0xBF];
        src.extend_from_slice(b"ADD TABLE \"Customer\"\n");
        let out = parse(std::str::from_utf8(&src).unwrap());
        assert_eq!(out.tables.len(), 1);
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn embedded_quote_decodes() {
        let out = parse(r#"ADD TABLE "Quote""Test""#);
        assert_eq!(out.tables.len(), 1);
        assert_eq!(out.tables[0].display_name, "Quote\"Test");
    }

    #[test]
    fn unknown_attr_round_trips_to_extras() {
        let out = parse(
            r#"
ADD FIELD "f" OF "T" AS integer
  SHADOW-COL "col"
  FOREIGN-POS 5
  FORMAT "zz9"
"#,
        );
        assert_eq!(out.fields.len(), 1);
        let f = &out.fields[0].field;
        assert_eq!(f.format.as_deref(), Some("zz9"));
        assert!(
            f.extras.iter().any(|(k, _)| *k == fold_atom("SHADOW-COL")),
            "expected SHADOW-COL in extras; got {:?}",
            f.extras
        );
        assert!(
            f.extras.iter().any(|(k, _)| *k == fold_atom("FOREIGN-POS")),
            "expected FOREIGN-POS in extras; got {:?}",
            f.extras
        );
    }

    #[test]
    fn missing_quoted_name_emits_diagnostic_and_recovers() {
        let out = parse(
            r#"
ADD TABLE
ADD TABLE "Customer"
"#,
        );
        assert_eq!(out.tables.len(), 1);
        assert_eq!(out.tables[0].display_name, "Customer");
        assert!(!out.diagnostics.is_empty());
    }

    #[test]
    fn unknown_data_type_round_trips_as_unknown() {
        let out = parse("ADD FIELD \"x\" OF \"T\" AS com-handle\n");
        let dt = &out.fields[0].field.data_type;
        match dt {
            SchemaType::Unknown(atom) => assert_eq!(*atom, fold_atom("com-handle")),
            other => panic!("expected Unknown, got {other:?}"),
        }
    }

    #[test]
    fn footer_psc_trailer_is_ignored() {
        let src = r#"
ADD TABLE "X"
.
PSC
cpstream=ISO8859-15
.
0000060070
"#;
        let out = parse(src);
        assert_eq!(out.tables.len(), 1);
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn annotation_directive_is_skipped() {
        let out = parse(
            r#"
@Some.Annotation(key="value").
ADD TABLE "Customer"
"#,
        );
        assert_eq!(out.tables.len(), 1);
    }

    #[test]
    fn add_sequence_then_add_field_associates_field() {
        let out = parse(
            r#"
ADD SEQUENCE "NextNum"
  INITIAL 1

ADD TABLE "T"
ADD FIELD "f" OF "T" AS character
"#,
        );
        assert_eq!(out.tables.len(), 1);
        assert_eq!(out.fields.len(), 1);
        assert_eq!(out.fields[0].table_display, "T");
    }
}
