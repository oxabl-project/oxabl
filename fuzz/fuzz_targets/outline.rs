#![no_main]

use libfuzzer_sys::fuzz_target;
use oxabl_pipeline::{OutlineSymbol, outline};

// The rules of docs/design/document-outline.md, "Rules that every outline obeys".
fn check(source: &str, symbols: &[OutlineSymbol], parent: Option<(u32, u32)>) {
    let len = source.len();
    let mut previous_end = None;
    for s in symbols {
        let (a, b) = (s.span.start, s.span.end);
        let (c, d) = (s.selection_span.start, s.selection_span.end);
        let at = |o: u32| o as usize;
        assert!(
            a <= c && c < d && d <= b && at(b) <= len,
            "rule 2: offsets not ordered or past the end: {s:?}"
        );
        for o in [a, b, c, d] {
            assert!(
                source.is_char_boundary(at(o)),
                "rule 2: offset {o} is not on a character boundary: {s:?}"
            );
        }
        if let Some((start, end)) = parent {
            assert!(
                start <= a && b <= end,
                "rule 3: child is outside its parent: {s:?}"
            );
        }
        assert!(
            previous_end.is_none_or(|end| end <= a),
            "rule 3: siblings are out of order or overlap: {s:?}"
        );
        previous_end = Some(b);
        assert!(
            !s.name.is_empty() && !s.name.contains(['\n', '\r']) && s.name == s.name.trim(),
            "rule 4: bad name {:?}: {s:?}",
            s.name
        );
        check(source, &s.children, Some((a, b)));
    }
}

// The outline never panics, obeys the rules and is the same on a second run.
fuzz_target!(|data: &[u8]| {
    let source = String::from_utf8_lossy(data);
    let first = match outline(&source) {
        Ok(symbols) => symbols,
        Err(p) => panic!("rule 1: outline panicked: {p:?}"),
    };
    check(&source, &first, None);
    match outline(&source) {
        Ok(second) => assert_eq!(first, second, "rule 5: the outline is not deterministic"),
        Err(p) => panic!("rule 1: outline panicked on the second run: {p:?}"),
    }
});
