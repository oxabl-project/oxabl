//! The fixes of a finding, mapped from the expanded text to the root file's
//! own bytes (`docs/design/quick-fixes.md`, "From expanded text to the file").
//!
//! A fix with one edit that does not map is dropped whole; the finding stays.

use oxabl_ast::Span;
use oxabl_common::{Edit, FileSpan, Fix, FixCondition};
use oxabl_lexer::{Kind, Token};
use oxabl_preprocessor::BranchState;

use super::{ChunkOrigin, ExpandedFile};

/// Maps the fixes of the findings of one analysed file.
pub(super) struct FixMapper<'a> {
    expanded: &'a ExpandedFile,
    tokens: &'a [Token],
    /// Rules 1 and 3: the file is a root, not a fragment, and its parse did
    /// not recover.
    allowed: bool,
}

impl<'a> FixMapper<'a> {
    pub(super) fn new(
        expanded: &'a ExpandedFile,
        tokens: &'a [Token],
        fragment: bool,
        parse_recovered: bool,
    ) -> Self {
        FixMapper {
            expanded,
            tokens,
            allowed: !fragment && !parse_recovered,
        }
    }

    /// The fixes that map, with their edits in the root's own bytes.
    pub(super) fn map(&self, fixes: Vec<Fix>) -> Vec<Fix> {
        if !self.allowed {
            return Vec::new();
        }
        fixes
            .into_iter()
            .filter_map(|fix| self.map_fix(fix))
            .collect()
    }

    fn map_fix(&self, mut fix: Fix) -> Option<Fix> {
        let mut edits = fix
            .edits
            .iter()
            .map(|edit| self.map_edit(edit))
            .collect::<Option<Vec<_>>>()?;
        if edits.is_empty() && fix.deferred.is_none() {
            return None;
        }
        edits.sort_by_key(|edit| (edit.span.span.start, edit.span.span.end));
        if edits
            .windows(2)
            .any(|pair| pair[0].span.span.end > pair[1].span.span.start)
        {
            return None;
        }
        if !fix
            .conditions
            .iter()
            .all(|condition| self.expanded.fix_condition_holds(condition, &edits))
        {
            return None;
        }
        fix.edits = edits;
        Some(fix)
    }

    /// Rules 2, 4 and 5 for one edit.
    fn map_edit(&self, edit: &Edit) -> Option<Edit> {
        let expanded = self.expanded;
        let root = expanded.root;
        if edit.span.file != root || edit.statement.file != root {
            return None;
        }
        let statement = edit.statement.span;
        // Rule 5: an elided reference can put text in the statement.
        if expanded
            .elided_offsets
            .iter()
            .any(|&at| statement.start <= at && at <= statement.end)
        {
            return None;
        }
        // No text of the statement comes from an include or a substitution.
        let statement = expanded.own_span(statement, false)?;
        // Rule 2.
        let span = if edit.span.span.start == edit.span.span.end {
            self.insertion_point(edit.span.span.start)?
        } else {
            expanded.own_span(edit.span.span, true)?
        };
        // Rule 4.
        if expanded.in_branch(span, |state| state == BranchState::Undecided) {
            return None;
        }
        Some(Edit {
            span: FileSpan { file: root, span },
            new_text: edit.new_text.clone(),
            statement: FileSpan {
                file: root,
                span: statement,
            },
        })
    }

    /// The root offset of an insertion at expanded offset `at`. Its anchor is
    /// the token that ends at `at`, else the token that starts there; the
    /// anchor must be own text of the root, in the chunk that holds `at`.
    fn insertion_point(&self, at: u32) -> Option<Span> {
        let at_usize = at as usize;
        let real = |token: &Token| token.kind != Kind::Eof && token.start < token.end;
        let ends = self.tokens.partition_point(|t| t.end < at_usize);
        let anchor = self.tokens[ends..]
            .iter()
            .take_while(|t| t.start <= at_usize)
            .find(|t| real(t) && t.end == at_usize)
            .or_else(|| {
                self.tokens[ends..]
                    .iter()
                    .take_while(|t| t.start <= at_usize)
                    .find(|t| real(t) && t.start == at_usize)
            })?;
        let anchor = Span {
            start: anchor.start as u32,
            end: anchor.end as u32,
        };
        let expanded = self.expanded;
        if expanded.chunks.is_empty() {
            return Some(Span { start: at, end: at });
        }
        let chunk = expanded.chunk_at(anchor.start)?;
        let chunk_end = chunk.virt_start + chunk.len;
        let own = chunk.origin == ChunkOrigin::Root && chunk.file == expanded.root.raw();
        if !own || anchor.end > chunk_end || at < chunk.virt_start || at > chunk_end {
            return None;
        }
        let real = chunk.real_start + (at - chunk.virt_start);
        Some(Span {
            start: real,
            end: real,
        })
    }
}

impl ExpandedFile {
    /// The root bytes of the expanded span `span`, when each of its bytes is
    /// own text of the root. With `one_chunk`, the bytes must also be in one
    /// chunk, so no directive or reference was taken out between them.
    fn own_span(&self, span: Span, one_chunk: bool) -> Option<Span> {
        if span.start >= span.end {
            return None;
        }
        if self.chunks.is_empty() {
            return Some(span);
        }
        let first = self.chunk_index(span.start)?;
        let mut last = first;
        for (index, chunk) in self.chunks.iter().enumerate().skip(first) {
            if chunk.virt_start >= span.end {
                break;
            }
            if chunk.origin != ChunkOrigin::Root || chunk.file != self.root.raw() {
                return None;
            }
            last = index;
        }
        let (start_chunk, end_chunk) = (&self.chunks[first], &self.chunks[last]);
        let end_of_last = end_chunk.virt_start + end_chunk.len;
        if span.end > end_of_last || (one_chunk && first != last) {
            return None;
        }
        Some(Span {
            start: start_chunk.real_start + (span.start - start_chunk.virt_start),
            end: end_chunk.real_start + (span.end - end_chunk.virt_start),
        })
    }

    /// The index of the chunk that holds expanded offset `virt`.
    fn chunk_index(&self, virt: u32) -> Option<usize> {
        self.chunks
            .binary_search_by(|chunk| {
                if virt < chunk.virt_start {
                    std::cmp::Ordering::Greater
                } else if virt >= chunk.virt_start + chunk.len {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            })
            .ok()
    }

    /// Whether the root span `span` touches the body of an `&IF` branch of the
    /// root whose state `matches` accepts. `true` when the expansion did not
    /// record the branches: that is not known.
    fn in_branch(&self, span: Span, matches: impl Fn(BranchState) -> bool) -> bool {
        if !self.branches_tracked {
            return true;
        }
        self.branches.iter().any(|branch| {
            let body = branch.body.span;
            branch.body.file == self.root
                && matches(branch.state)
                && if span.start == span.end {
                    body.start <= span.start && span.start <= body.end
                } else {
                    span.start < body.end && span.end > body.start
                }
        })
    }

    /// Whether `condition` holds for the fix edits `edits`, which are in the
    /// root's own bytes. A fact the expansion did not record does not hold.
    pub fn fix_condition_holds(&self, condition: &FixCondition, edits: &[Edit]) -> bool {
        match condition {
            FixCondition::OutsideConditionalBranch => !edits
                .iter()
                .any(|edit| self.in_branch(edit.span.span, |_| true)),
            FixCondition::NotInSkippedText(word) => self.skipped_text.iter().all(|skipped| {
                self.skipped_text_of(skipped)
                    .is_some_and(|text| !holds_word(text, word))
            }),
            FixCondition::WholeExpansion => {
                self.branches_tracked
                    && !self.missed_expansion
                    && self.text_losses.is_empty()
                    && !self.lost_include_text()
            }
        }
    }
}

/// Whether `text` holds `word` as a whole word, in any letter case.
fn holds_word(text: &str, word: &str) -> bool {
    let is_name =
        |b: u8| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'#' | b'$' | b'%' | b'&');
    let (text, word) = (text.as_bytes(), word.as_bytes());
    if word.is_empty() || word.len() > text.len() {
        return false;
    }
    (0..=text.len() - word.len()).any(|at| {
        text[at..at + word.len()].eq_ignore_ascii_case(word)
            && (at == 0 || !is_name(text[at - 1]))
            && text.get(at + word.len()).is_none_or(|b| !is_name(*b))
    })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use oxabl_common::{Edit, FileId, FileSpan, Fix, FixCondition};
    use oxabl_lexer::tokenize;
    use oxabl_workspace::{InMemoryFileSystem, PreprocessorConfig};

    use super::super::{ExpandedFile, PreprocessMode, expand_source_tracking};
    use super::{FixMapper, holds_word};
    use oxabl_ast::Span;

    const ROOT: FileId = FileId::new(1);

    fn expand(files: &[(&str, &str)], source: &str) -> ExpandedFile {
        let mut fs = InMemoryFileSystem::new();
        for (path, text) in files {
            fs.insert(PathBuf::from(path), *text);
        }
        expand_source_tracking(
            ROOT,
            Some("/w/main.p"),
            source,
            &fs,
            &[PathBuf::from("/w")],
            &PreprocessorConfig::default(),
            PreprocessMode::TrackBranches,
        )
        .expect("expands")
    }

    /// A fix with one edit at `span` of the expanded text, of the statement
    /// `statement`.
    fn fix(span: (u32, u32), statement: (u32, u32), new_text: &str) -> Fix {
        let at = |(start, end)| FileSpan {
            file: ROOT,
            span: Span { start, end },
        };
        Fix::new(
            "Fix",
            Edit {
                span: at(span),
                new_text: new_text.to_string(),
                statement: at(statement),
            },
            true,
        )
    }

    fn mapped(expanded: &ExpandedFile, fix: Fix) -> Vec<(u32, u32)> {
        let tokens = tokenize(&expanded.text);
        FixMapper::new(expanded, &tokens, false, false)
            .map(vec![fix])
            .iter()
            .flat_map(|fix| {
                fix.edits
                    .iter()
                    .map(|e| (e.span.span.start, e.span.span.end))
            })
            .collect()
    }

    fn at(text: &str, needle: &str) -> u32 {
        text.find(needle).expect("needle") as u32
    }

    #[test]
    fn an_insertion_after_include_text_does_not_map() {
        let expanded = expand(&[("/w/a.i", "DISPLAY 2.")], "{a.i}\nDISPLAY 1.\n");
        let text = expanded.text.clone();
        let own = (at(&text, "DISPLAY 1"), at(&text, "DISPLAY 1") + 10);
        // After the period the include wrote.
        let after_include = at(&text, "2.") + 2;
        assert_eq!(
            mapped(&expanded, fix((after_include, after_include), own, " x")),
            []
        );
        // After the root's own period.
        let after_own = own.1;
        assert_eq!(
            mapped(&expanded, fix((after_own, after_own), own, " x")),
            [(16, 16)]
        );
    }

    #[test]
    fn a_replacement_of_include_text_does_not_map() {
        let expanded = expand(&[("/w/a.i", "DISPLAY 2.")], "{a.i}\nDISPLAY 1.\n");
        let text = expanded.text.clone();
        let own = (at(&text, "DISPLAY 1"), at(&text, "DISPLAY 1") + 10);
        let include = (at(&text, "DISPLAY 2"), at(&text, "DISPLAY 2") + 7);
        assert_eq!(mapped(&expanded, fix(include, own, "MESSAGE")), []);
        let root = (own.0, own.0 + 7);
        assert_eq!(mapped(&expanded, fix(root, own, "MESSAGE")), [(6, 13)]);
    }

    #[test]
    fn a_replacement_across_a_directive_does_not_map() {
        let source = "DISPLAY 1.\n&SCOPED-DEFINE X 1\nDISPLAY 2.\n";
        let expanded = expand(&[], source);
        let text = expanded.text.clone();
        let whole = (at(&text, "DISPLAY 1"), at(&text, "DISPLAY 2") + 10);
        assert_eq!(mapped(&expanded, fix(whole, whole, "")), []);
    }

    fn holds(expanded: &ExpandedFile, condition: FixCondition, edit_at: u32) -> bool {
        let edit = Edit {
            span: FileSpan {
                file: ROOT,
                span: Span {
                    start: edit_at,
                    end: edit_at,
                },
            },
            new_text: String::new(),
            statement: FileSpan {
                file: ROOT,
                span: Span {
                    start: edit_at,
                    end: edit_at,
                },
            },
        };
        expanded.fix_condition_holds(&condition, &[edit])
    }

    #[test]
    fn outside_a_conditional_branch() {
        let source = "DISPLAY 1.\n&IF 1 = 1 &THEN\nDISPLAY 2.\n&ENDIF\nDISPLAY 3.\n";
        let expanded = expand(&[], source);
        let inside = at(source, "DISPLAY 2");
        let outside = at(source, "DISPLAY 3");
        assert!(!holds(
            &expanded,
            FixCondition::OutsideConditionalBranch,
            inside
        ));
        assert!(holds(
            &expanded,
            FixCondition::OutsideConditionalBranch,
            outside
        ));
    }

    #[test]
    fn not_in_skipped_text() {
        let source = "DISPLAY 1.\n&IF 1 = 2 &THEN\nDISPLAY iCount.\n&ENDIF\n";
        let expanded = expand(&[], source);
        let word = |w: &str| FixCondition::NotInSkippedText(w.to_string());
        assert!(!holds(&expanded, word("ICOUNT"), 0));
        assert!(holds(&expanded, word("iOther"), 0));
        // Skipped text in an include counts too.
        let expanded = expand(
            &[("/w/a.i", "&IF 1 = 2 &THEN\nDISPLAY iCount.\n&ENDIF\n")],
            "{a.i}\nDISPLAY 1.\n",
        );
        assert!(!holds(&expanded, word("iCount"), 0));
    }

    #[test]
    fn whole_expansion() {
        let expanded = expand(&[], "DISPLAY 1.\n");
        assert!(holds(&expanded, FixCondition::WholeExpansion, 0));
        let expanded = expand(&[], "{missing.i}\nDISPLAY 1.\n");
        assert!(!holds(&expanded, FixCondition::WholeExpansion, 0));
        let expanded = expand(
            &[],
            "&IF \"{&PROVERSION}\" = \"\" &THEN\n&ENDIF\nDISPLAY 1.\n",
        );
        assert!(!holds(&expanded, FixCondition::WholeExpansion, 0));
    }

    #[test]
    fn a_word_is_found_whole_and_in_any_case() {
        assert!(holds_word("DISPLAY iCount.", "icount"));
        assert!(!holds_word("DISPLAY iCount2.", "iCount"));
        assert!(!holds_word("DISPLAY x-iCount.", "iCount"));
        assert!(holds_word("iCount", "ICOUNT"));
    }
}
