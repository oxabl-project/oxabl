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
    /// Any other match.
    Other,
}

/// Whether `label` matches `word`, and how well. The characters of the word
/// appear in the label in the same order, without case, and the first one is
/// the first character of the label. A label that matches a word also matches
/// each prefix of it, so a client can filter the list itself.
pub(crate) fn quality(word: &str, label: &str) -> Option<Quality> {
    if word.is_empty() {
        return Some(Quality::Other);
    }
    let w = word.as_bytes();
    let l = label.as_bytes();
    if !l.first()?.eq_ignore_ascii_case(&w[0]) {
        return None;
    }
    if l.len() >= w.len() && l[..w.len()].eq_ignore_ascii_case(w) {
        return Some(if l.len() == w.len() {
            Quality::Same
        } else {
            Quality::Prefix
        });
    }
    // A subsequence, with the first character at the start.
    let mut at = 1;
    for c in &w[1..] {
        let found = l[at..].iter().position(|b| b.eq_ignore_ascii_case(c))?;
        at += found + 1;
    }
    let starts = part_starts(l);
    let mut at = 1;
    let mut on_starts = true;
    for c in &w[1..] {
        match starts
            .iter()
            .find(|start| **start >= at && l[**start].eq_ignore_ascii_case(c))
        {
            Some(start) => at = start + 1,
            None => {
                on_starts = false;
                break;
            }
        }
    }
    Some(if on_starts {
        Quality::PartStarts
    } else {
        Quality::Other
    })
}

/// The byte index of the first character of each part of `label`: after `-`,
/// `_` or `.`, and an upper case letter after a lower case one.
fn part_starts(label: &[u8]) -> Vec<usize> {
    let mut starts = vec![0];
    for i in 1..label.len() {
        let prev = label[i - 1];
        let here = label[i];
        if matches!(prev, b'-' | b'_' | b'.' | b'/' | b'\\') && here.is_ascii_alphanumeric()
            || prev.is_ascii_lowercase() && here.is_ascii_uppercase()
        {
            starts.push(i);
        }
    }
    starts
}

/// How near an item is to the cursor, nearest first.
pub(crate) mod nearness {
    /// A name of the innermost scope; each outer scope adds one.
    pub const SCOPE: u8 = 0;
    /// A keyword, a built-in function or a snippet.
    pub const KEYWORD: u8 = 50;
    /// A table or a field of the schema.
    pub const SCHEMA: u8 = 60;
}

/// The `sortText` of an item: fit, match quality, nearness, then the label
/// without case.
pub(crate) fn sort_text(fits: bool, quality: Quality, nearness: u8, label: &str) -> String {
    format!(
        "{}{}{:02}{}",
        if fits { 0 } else { 1 },
        quality as u8,
        nearness.min(99),
        label.to_ascii_lowercase()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_first_character_must_start_the_label() {
        assert_eq!(quality("cusnu", "CustNum"), Some(Quality::Other));
        assert_eq!(quality("cn", "CustNum"), Some(Quality::PartStarts));
        assert_eq!(quality("nu", "CustNum"), None);
        assert_eq!(quality("fi", "FIND"), Some(Quality::Prefix));
        assert_eq!(quality("find", "FIND"), Some(Quality::Same));
        assert_eq!(quality("fi", "DEFINE"), None);
        assert_eq!(quality("", "DEFINE"), Some(Quality::Other));
        assert_eq!(quality("dtz", "DATETIME-TZ"), Some(Quality::Other));
        assert_eq!(quality("dt", "DATETIME-TZ"), Some(Quality::PartStarts));
        assert_eq!(quality("d-t", "DATETIME-TZ"), Some(Quality::Other));
    }

    #[test]
    fn a_match_is_kept_by_each_prefix_of_the_word() {
        for (word, label) in [
            ("cusnu", "CustNum"),
            ("dtz", "DATETIME-TZ"),
            ("vari", "VARIABLE"),
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
