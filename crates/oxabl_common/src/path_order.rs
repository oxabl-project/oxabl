//! One order for the paths of a workspace: the order of `Path::cmp`, at the
//! speed of a byte compare.
//!
//! `Path::cmp` splits both paths into components on each comparison, which
//! made it the main cost of sorting the files of a large workspace. A plain
//! byte compare is fast but gives another order: `app-old/x.p` sorts before
//! `app/x.p`, because `-` is below `/`. [`cmp_paths`] compares bytes and puts
//! a separator below every other byte, which is the component order.

use std::cmp::Ordering;
use std::path::Path;

/// Compare two paths in the order of [`Path::cmp`].
///
/// The two orders are equal for paths with no `.` or `..` component and no
/// repeated or trailing separator: canonical paths and the paths of
/// `normalize_lexically`, which are the paths that a workspace sorts.
pub fn cmp_paths(a: &Path, b: &Path) -> Ordering {
    let a = a.as_os_str().as_encoded_bytes();
    let b = b.as_os_str().as_encoded_bytes();
    for (x, y) in a.iter().zip(b) {
        if x == y {
            continue;
        }
        let order = rank(*x).cmp(&rank(*y));
        if order != Ordering::Equal {
            return order;
        }
    }
    a.len().cmp(&b.len())
}

/// A separator is below every byte that a component name can hold.
fn rank(byte: u8) -> u16 {
    if byte.is_ascii() && std::path::is_separator(char::from(byte)) {
        0
    } else {
        u16::from(byte) + 1
    }
}
