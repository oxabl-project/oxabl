//! Writes a synthetic ABL workspace for timing `oxabl build --plan`.
//!
//! `cargo run --release -p oxabl_build --example synthetic_tree -- <dir> [modules]`
//! writes about 28,000 files in `modules` (default 140) directories: programs
//! that include common and module includes, include chains two levels deep,
//! classes that inherit from earlier classes of their module, and a few `RUN`
//! targets.

use std::fmt::Write as _;
use std::fs;
use std::path::PathBuf;

struct Random(u64);

impl Random {
    fn next(&mut self, bound: u64) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (self.0 >> 33) % bound
    }
}

const PROGRAMS: usize = 130;
const INCLUDES: usize = 50;
const CLASSES: usize = 20;

fn filler(out: &mut String, random: &mut Random, name: &str, lines: u64) {
    for line in 0..lines {
        match random.next(4) {
            0 => writeln!(out, "DEFINE VARIABLE {name}_v{line} AS INTEGER NO-UNDO.").unwrap(),
            1 => writeln!(
                out,
                "ASSIGN {name}_v{} = {line} + 1.",
                line.saturating_sub(1)
            )
            .unwrap(),
            2 => writeln!(out, "MESSAGE \"{name} line {line}\" VIEW-AS ALERT-BOX.").unwrap(),
            _ => writeln!(out, "/* {name} comment {line} to pad the file */").unwrap(),
        }
    }
}

fn main() {
    let mut args = std::env::args().skip(1);
    let root = PathBuf::from(args.next().expect("usage: synthetic_tree <dir> [modules]"));
    let modules: usize = args.next().map_or(140, |m| m.parse().expect("modules"));
    let mut random = Random(42);

    fs::create_dir_all(root.join("inc/common")).unwrap();
    fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"synthetic\"\n\n[workspace.sources]\ndirectories = [\"src\"]\ninclude_paths = [\"inc\", \"src\"]\n",
    )
    .unwrap();
    for i in 0..10 {
        let mut text = String::new();
        filler(&mut text, &mut random, &format!("common{i}"), 20);
        fs::write(root.join(format!("inc/common/c{i}.i")), text).unwrap();
    }

    for m in 0..modules {
        let include_dir = root.join(format!("inc/m{m}"));
        let source_dir = root.join(format!("src/m{m}"));
        let class_dir = root.join(format!("src/pkg/m{m}"));
        for dir in [&include_dir, &source_dir, &class_dir] {
            fs::create_dir_all(dir).unwrap();
        }
        for i in 0..INCLUDES {
            let mut text = String::new();
            if i > 4 && random.next(3) == 0 {
                writeln!(text, "{{m{m}/i{}.i}}", random.next(i as u64)).unwrap();
            }
            filler(&mut text, &mut random, &format!("i{m}_{i}"), 25);
            fs::write(include_dir.join(format!("i{i}.i")), text).unwrap();
        }
        for c in 0..CLASSES {
            let mut text = String::new();
            let parent = if c > 0 {
                format!(" INHERITS pkg.m{m}.Class{}", random.next(c as u64))
            } else {
                String::new()
            };
            writeln!(text, "CLASS pkg.m{m}.Class{c}{parent}:").unwrap();
            writeln!(text, "  {{m{m}/i{}.i}}", random.next(INCLUDES as u64)).unwrap();
            for member in 0..12 {
                writeln!(
                    text,
                    "  DEFINE PUBLIC PROPERTY p{member} AS INTEGER NO-UNDO GET. SET."
                )
                .unwrap();
            }
            writeln!(text, "END CLASS.").unwrap();
            fs::write(class_dir.join(format!("Class{c}.cls")), text).unwrap();
        }
        for p in 0..PROGRAMS {
            let mut text = String::new();
            writeln!(text, "{{common/c{}.i}}", random.next(10)).unwrap();
            for _ in 0..3 + random.next(4) {
                writeln!(text, "{{m{m}/i{}.i}}", random.next(INCLUDES as u64)).unwrap();
            }
            for _ in 0..random.next(3) {
                writeln!(
                    text,
                    "DEFINE VARIABLE o{} AS pkg.m{m}.Class{} NO-UNDO.",
                    random.next(1_000_000),
                    random.next(CLASSES as u64)
                )
                .unwrap();
            }
            if random.next(3) == 0 {
                writeln!(text, "RUN m{m}/p{}.p.", random.next(PROGRAMS as u64)).unwrap();
            }
            let lines = 40 + random.next(40);
            filler(&mut text, &mut random, &format!("p{m}_{p}"), lines);
            fs::write(source_dir.join(format!("p{p}.p")), text).unwrap();
        }
    }
}
