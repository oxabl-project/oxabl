//! Which items match the word, and in which order they come.

/// How well a label matches the word, best first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Quality {
    /// The same text, without case.
    Same,
    /// The word starts the label.
    Prefix,
    /// The characters of the word start parts of the label, in order.
    PartStarts,
    /// Any other match from the start of the label.
    Other,
    /// A match that starts at a later part of the label (`cu` in `bCust`).
    Inner,
}

/// Whether `label` matches `word`, and how well. The characters of the word
/// appear in the label in the same order, without case, and the first one is
/// the first character of the label or of a part of it. A label that matches
/// a word also matches each prefix of it, so a client can filter the list
/// itself.
pub(crate) fn quality(word: &str, label: &str) -> Option<Quality> {
    if word.is_empty() {
        return Some(Quality::Other);
    }
    let w = word.as_bytes();
    let l = label.as_bytes();
    if l.len() >= w.len() && l[..w.len()].eq_ignore_ascii_case(w) {
        return Some(if l.len() == w.len() {
            Quality::Same
        } else {
            Quality::Prefix
        });
    }
    if l.first()?.eq_ignore_ascii_case(&w[0])
        && let Some(quality) = match_from(l, &w[1..], 1)
    {
        return Some(quality);
    }
    // A match from a later part: no list of the parts is made, as most
    // labels have no part that starts with the first character of the word.
    (1..l.len())
        .filter(|at| l[*at].eq_ignore_ascii_case(&w[0]) && is_part_start(l, *at))
        .find_map(|at| match_from(l, &w[1..], at + 1))
        .map(|_| Quality::Inner)
}

/// How the rest of a word, `rest`, matches `label` after byte `from`: on the
/// starts of parts, or anywhere in order. `None` when it does not.
fn match_from(label: &[u8], rest: &[u8], from: usize) -> Option<Quality> {
    let mut at = from;
    for c in rest {
        let found = label
            .get(at..)?
            .iter()
            .position(|b| b.eq_ignore_ascii_case(c))?;
        at += found + 1;
    }
    let mut at = from;
    for c in rest {
        match (at..label.len())
            .find(|start| label[*start].eq_ignore_ascii_case(c) && is_part_start(label, *start))
        {
            Some(start) => at = start + 1,
            None => return Some(Quality::Other),
        }
    }
    Some(Quality::PartStarts)
}

/// Whether byte `at` of `label` starts a part of it: the first byte, a byte
/// after `-`, `_`, `.`, `/` or `\`, and an upper case letter after a lower
/// case one.
fn is_part_start(label: &[u8], at: usize) -> bool {
    if at == 0 {
        return true;
    }
    let prev = label[at - 1];
    let here = label[at];
    matches!(prev, b'-' | b'_' | b'.' | b'/' | b'\\') && here.is_ascii_alphanumeric()
        || prev.is_ascii_lowercase() && here.is_ascii_uppercase()
}

/// How near an item is to the cursor, nearest first.
pub(crate) mod nearness {
    /// A name of the innermost scope; each outer scope adds one.
    pub const SCOPE: u8 = 0;
    /// A keyword, a built-in function or a snippet.
    pub const KEYWORD: u8 = 50;
    /// A table or a field of the schema.
    pub const SCHEMA: u8 = 60;
    /// An include file (`.i`) after `{`.
    pub const INCLUDE: u8 = 70;
    /// A directory, a program, or another file.
    pub const PATH: u8 = 71;
    /// A file after `{` that is not an include file.
    pub const OTHER_FILE: u8 = 72;
}

/// How well an item fits the place, best first.
pub(crate) mod fit {
    /// Its type fits the type the place expects.
    pub const FITS: u8 = 0;
    /// A field of a record the statement names, in its `WHERE`.
    pub const STATEMENT_FIELD: u8 = 1;
    pub const OTHER: u8 = 2;
}

/// The `sortText` of an item: a match from the start of the label before one
/// at a later part, then fit, match quality, nearness, and the label without
/// case. A type that fits does not lift a match in the middle of a name over
/// one the person typed the start of.
pub(crate) fn sort_text(fit: u8, quality: Quality, nearness: u8, label: &str) -> String {
    format!(
        "{}{}{}{:02}{}",
        u8::from(quality == Quality::Inner),
        fit.min(9),
        quality as u8,
        nearness.min(99),
        label.to_ascii_lowercase()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_first_character_must_start_the_label_or_a_part() {
        assert_eq!(quality("cusnu", "CustNum"), Some(Quality::Other));
        assert_eq!(quality("cn", "CustNum"), Some(Quality::PartStarts));
        assert_eq!(quality("nu", "CustNum"), Some(Quality::Inner));
        assert_eq!(quality("cu", "bCust"), Some(Quality::Inner));
        assert_eq!(quality("us", "CustNum"), None);
        assert_eq!(quality("fi", "FIND"), Some(Quality::Prefix));
        assert_eq!(quality("find", "FIND"), Some(Quality::Same));
        assert_eq!(quality("fi", "DEFINE"), None);
        assert_eq!(quality("ne", "NO-ERROR"), Some(Quality::PartStarts));
        assert_eq!(quality("er", "NO-ERROR"), Some(Quality::Inner));
        assert_eq!(quality("", "DEFINE"), Some(Quality::Other));
        assert_eq!(quality("a", ""), None);
        assert_eq!(quality("dtz", "DATETIME-TZ"), Some(Quality::Other));
        assert_eq!(quality("dt", "DATETIME-TZ"), Some(Quality::PartStarts));
        assert_eq!(quality("d-t", "DATETIME-TZ"), Some(Quality::Other));
    }

    #[test]
    fn a_match_from_the_start_comes_before_a_fitting_inner_one() {
        let prefix = sort_text(fit::OTHER, quality("c", "cName").unwrap(), 0, "cName");
        let inner = sort_text(fit::FITS, quality("c", "iCount").unwrap(), 0, "iCount");
        assert!(prefix < inner, "{prefix} {inner}");
        let fits = sort_text(fit::FITS, Quality::Prefix, 0, "cTotal");
        assert!(fits < prefix, "fit still decides in one band");
    }

    #[test]
    fn a_match_is_kept_by_each_prefix_of_the_word() {
        for (word, label) in [
            ("cusnu", "CustNum"),
            ("dtz", "DATETIME-TZ"),
            ("vari", "VARIABLE"),
            ("cust", "bCust"),
        ] {
            for end in 1..=word.len() {
                assert!(
                    quality(&word[..end], label).is_some(),
                    "{word} {end} {label}"
                );
            }
        }
    }
}
