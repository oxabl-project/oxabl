//! `cmp_paths` gives the order of `Path::cmp` for the paths a workspace sorts.

use oxabl_common::cmp_paths;
use std::cmp::Ordering;
use std::path::{Path, PathBuf};

#[test]
fn a_directory_sorts_before_a_sibling_that_extends_its_name() {
    let pairs = [
        ("/w/app/x.p", "/w/app-old/x.p"),
        ("/w/app/x.p", "/w/app.bak/x.p"),
        ("/w/app/x.p", "/w/app old/x.p"),
        ("/w/app/x.p", "/w/app!/x.p"),
        ("/w/app", "/w/app/x.p"),
        ("/w/a/b.p", "/w/ab.p"),
        ("/w/src/z.p", "/w/src-old/a.p"),
        ("/w/é/x.p", "/w/éa/x.p"),
    ];
    for (first, second) in pairs {
        let (a, b) = (Path::new(first), Path::new(second));
        assert_eq!(a.cmp(b), Ordering::Less, "Path::cmp {first} {second}");
        assert_eq!(
            cmp_paths(a, b),
            Ordering::Less,
            "cmp_paths {first} {second}"
        );
        assert_eq!(
            cmp_paths(b, a),
            Ordering::Greater,
            "cmp_paths {second} {first}"
        );
    }
    assert_eq!(
        cmp_paths(Path::new("/w/a.p"), Path::new("/w/a.p")),
        Ordering::Equal
    );
}

/// Every pair of many generated paths compares as `Path::cmp` compares it.
#[test]
fn agrees_with_path_cmp_on_generated_paths() {
    const ALPHABET: [&str; 9] = ["a", "b", "-", ".", " ", "_", "Z", "é", "!"];
    let mut seed: u64 = 0x9E37_79B9_7F4A_7C15;
    let mut next = move |bound: u64| -> usize {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        usize::try_from(seed % bound).unwrap_or(0)
    };
    let mut paths: Vec<PathBuf> = Vec::new();
    for _ in 0..400 {
        let mut path = String::from("/w");
        for _ in 0..=next(3) {
            let mut component = String::new();
            for _ in 0..=next(3) {
                component.push_str(ALPHABET[next(9)]);
            }
            if component == "." || component == ".." {
                component.push('a');
            }
            path.push('/');
            path.push_str(&component);
        }
        paths.push(PathBuf::from(path));
    }
    for a in &paths {
        for b in &paths {
            assert_eq!(
                cmp_paths(a, b),
                a.cmp(b),
                "{} vs {}",
                a.display(),
                b.display()
            );
        }
    }
}
