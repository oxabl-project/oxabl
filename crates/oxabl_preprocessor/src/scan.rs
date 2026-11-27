//! Byte scanners for the body scan of the preprocessor: they jump to the next
//! byte that the scan acts on with `memchr` and give the position the byte by
//! byte scan gave.

/// The position of the first byte at or after `from` that the body scan of
/// `process_source` acts on outside a comment (`&`, `{`, `/`, `'` or `"`), or
/// `bytes.len()`. Every byte before it is plain text.
///
/// `next_quote` holds the first quote at or after an earlier position, so the
/// quotes are searched for once for each quote and the other three bytes only
/// up to it.
pub(crate) fn next_plain_stop(bytes: &[u8], from: usize, next_quote: &mut Option<usize>) -> usize {
    let quote = match *next_quote {
        Some(at) if at >= from => at,
        _ => {
            let at = find_either(b'\'', b'"', bytes, from);
            *next_quote = Some(at);
            at
        }
    };
    memchr::memchr3(b'&', b'{', b'/', &bytes[from..quote]).map_or(quote, |offset| from + offset)
}

/// The position of the first byte at or after `from` that a quoted string scan
/// acts on: a `~` escape, the closing `quote`, and, in text that is emitted, a
/// `{` that may start a reference. `bytes.len()` when there is none.
pub(crate) fn next_string_trigger(bytes: &[u8], from: usize, quote: u8, emitting: bool) -> usize {
    let found = if emitting {
        memchr::memchr3(b'~', quote, b'{', &bytes[from..])
    } else {
        memchr::memchr2(b'~', quote, &bytes[from..])
    };
    found.map_or(bytes.len(), |offset| from + offset)
}

/// The position of the first `first` or `second` at or after `from`, or
/// `bytes.len()`.
fn find_either(first: u8, second: u8, bytes: &[u8], from: usize) -> usize {
    memchr::memchr2(first, second, &bytes[from..]).map_or(bytes.len(), |offset| from + offset)
}

/// The position of the next `\n` at or after `from`, or `bytes.len()`.
pub(crate) fn skip_line(bytes: &[u8], from: usize) -> usize {
    memchr::memchr(b'\n', &bytes[from..]).map_or(bytes.len(), |offset| from + offset)
}

/// Skip the body of a block comment that is `depth` levels deep, from `from`.
/// ABL block comments nest. It stops after the `*/` that closes the outermost
/// level, or at the end of `bytes`. It gives the position and the depth.
pub(crate) fn skip_comment_body(bytes: &[u8], from: usize, mut depth: u32) -> (usize, u32) {
    let mut i = from;
    while depth > 0 {
        let Some(offset) = memchr::memchr2(b'*', b'/', &bytes[i..]) else {
            return (bytes.len(), depth);
        };
        let at = i + offset;
        i = match (bytes[at], bytes.get(at + 1)) {
            (b'*', Some(b'/')) => {
                depth -= 1;
                at + 2
            }
            (b'/', Some(b'*')) => {
                depth += 1;
                at + 2
            }
            _ => at + 1,
        };
    }
    (i, depth)
}

#[cfg(test)]
mod tests {
    use super::*;

    // The byte by byte scans these functions replaced.
    fn plain_stop(bytes: &[u8], mut i: usize) -> usize {
        while i < bytes.len() && !matches!(bytes[i], b'&' | b'{' | b'/' | b'\'' | b'"') {
            i += 1;
        }
        i
    }

    fn string_trigger(bytes: &[u8], mut i: usize, quote: u8, emitting: bool) -> usize {
        while i < bytes.len() {
            let b = bytes[i];
            if b == b'~' || b == quote || (emitting && b == b'{') {
                break;
            }
            i += 1;
        }
        i
    }

    fn line_end(bytes: &[u8], mut i: usize) -> usize {
        while i < bytes.len() && bytes[i] != b'\n' {
            i += 1;
        }
        i
    }

    fn comment_body(bytes: &[u8], mut i: usize, mut depth: u32) -> (usize, u32) {
        while i < bytes.len() && depth > 0 {
            if i + 1 < bytes.len() && bytes[i] == b'*' && bytes[i + 1] == b'/' {
                depth -= 1;
                i += 2;
            } else if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
                depth += 1;
                i += 2;
            } else {
                i += 1;
            }
        }
        (i, depth)
    }

    /// Every text of up to `max_len` symbols over `alphabet`.
    fn for_each_text(alphabet: &[&str], max_len: usize, check: &mut dyn FnMut(&str)) {
        let mut texts = vec![String::new()];
        for _ in 0..=max_len {
            let mut longer = Vec::new();
            for text in &texts {
                check(text);
                for symbol in alphabet {
                    longer.push(format!("{text}{symbol}"));
                }
            }
            texts = longer;
        }
    }

    // The trigger bytes, a multi-byte character, a continuation byte pattern
    // next to them, CR and a plain letter.
    const ALPHABET: [&str; 11] = ["/", "*", "\n", "&", "{", "~", "'", "\"", "a", "é", "\r"];

    // Only what a comment scan reads, so that long texts stay cheap.
    const COMMENT_ALPHABET: [&str; 4] = ["/", "*", "a", "é"];

    #[test]
    fn plain_stop_matches_the_byte_scan() {
        for_each_text(&ALPHABET, 5, &mut |text| {
            let bytes = text.as_bytes();
            for from in 0..=bytes.len() {
                let mut quote = None;
                assert_eq!(
                    next_plain_stop(bytes, from, &mut quote),
                    plain_stop(bytes, from),
                    "{text:?} from {from}"
                );
            }
        });
    }

    #[test]
    fn plain_stop_keeps_its_quote_between_calls() {
        // The cache must hold across calls that move forward in one text.
        for_each_text(&ALPHABET, 5, &mut |text| {
            let bytes = text.as_bytes();
            let mut quote = None;
            for from in 0..=bytes.len() {
                assert_eq!(
                    next_plain_stop(bytes, from, &mut quote),
                    plain_stop(bytes, from),
                    "{text:?} from {from}"
                );
            }
        });
    }

    #[test]
    fn string_trigger_matches_the_byte_scan() {
        for_each_text(&ALPHABET, 4, &mut |text| {
            let bytes = text.as_bytes();
            for from in 0..=bytes.len() {
                for quote in *b"'\"" {
                    for emitting in [false, true] {
                        assert_eq!(
                            next_string_trigger(bytes, from, quote, emitting),
                            string_trigger(bytes, from, quote, emitting),
                            "{text:?} from {from} quote {quote} emitting {emitting}"
                        );
                    }
                }
            }
        });
    }

    #[test]
    fn line_end_matches_the_byte_scan() {
        for_each_text(&ALPHABET, 4, &mut |text| {
            let bytes = text.as_bytes();
            for from in 0..=bytes.len() {
                assert_eq!(
                    skip_line(bytes, from),
                    line_end(bytes, from),
                    "{text:?} from {from}"
                );
            }
        });
    }

    #[test]
    fn comment_body_matches_the_byte_scan() {
        for_each_text(&COMMENT_ALPHABET, 8, &mut |text| {
            let bytes = text.as_bytes();
            for from in 0..=bytes.len() {
                for depth in 1..=2 {
                    assert_eq!(
                        skip_comment_body(bytes, from, depth),
                        comment_body(bytes, from, depth),
                        "{text:?} from {from} depth {depth}"
                    );
                }
            }
        });
    }

    #[test]
    fn a_comment_that_never_closes_ends_at_the_end_of_the_text() {
        let text = b"/* a /* b */ c";
        assert_eq!(skip_comment_body(text, 2, 1), (text.len(), 1));
    }

    #[test]
    fn nested_comments_close_level_by_level() {
        // From just after the first `/*`: the inner `/*` makes depth 2, the
        // first `*/` gives depth 1 and the second closes the outer level.
        let text = b"/* a /* b */ c */ d";
        assert_eq!(skip_comment_body(text, 2, 1), (17, 0));
    }
}
