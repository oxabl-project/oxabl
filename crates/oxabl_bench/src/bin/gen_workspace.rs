//! Write a synthetic ABL workspace.
//!
//! Usage: gen-workspace [--preset ci|medium|erp] [--seed N] [--programs N]
//!        [--classes N] [--stats] <dir>
//!
//! The directory must not exist or must be empty. `--stats` prints the shape
//! and writes nothing when no directory is given.

use std::path::PathBuf;
use std::process::ExitCode;

use oxabl_bench::generate::{Preset, generate};

fn usage() -> ExitCode {
    eprintln!(
        "usage: gen-workspace [--preset ci|medium|erp] [--seed N] [--programs N] [--classes N] [--stats] <dir>"
    );
    ExitCode::from(2)
}

fn main() -> ExitCode {
    let mut preset = Preset::Ci;
    let mut seed = None;
    let mut programs = None;
    let mut classes = None;
    let mut stats = false;
    let mut dir: Option<PathBuf> = None;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        let mut number = |what: &str| -> Option<u64> {
            let value = args.next()?;
            match value.parse() {
                Ok(n) => Some(n),
                Err(_) => {
                    eprintln!("error: {what} wants a number, not {value:?}");
                    None
                }
            }
        };
        match arg.as_str() {
            "--preset" => match args.next().as_deref().and_then(Preset::named) {
                Some(p) => preset = p,
                None => return usage(),
            },
            "--seed" => match number("--seed") {
                Some(n) => seed = Some(n),
                None => return usage(),
            },
            "--programs" => match number("--programs") {
                Some(n) => programs = Some(n as usize),
                None => return usage(),
            },
            "--classes" => match number("--classes") {
                Some(n) => classes = Some(n as usize),
                None => return usage(),
            },
            "--stats" => stats = true,
            "-h" | "--help" => {
                let _ = usage();
                return ExitCode::SUCCESS;
            }
            _ if arg.starts_with('-') => return usage(),
            _ if dir.is_none() => dir = Some(PathBuf::from(arg)),
            _ => return usage(),
        }
    }
    if dir.is_none() && !stats {
        return usage();
    }

    let mut knobs = preset.knobs();
    if let Some(seed) = seed {
        knobs.seed = seed;
    }
    if let Some(programs) = programs {
        knobs.programs = programs;
    }
    if let Some(classes) = classes {
        knobs.classes = classes;
    }
    let workspace = generate(&knobs);

    if let Some(dir) = &dir {
        let occupied = std::fs::read_dir(dir).is_ok_and(|mut entries| entries.next().is_some());
        if occupied {
            eprintln!("error: {} is not empty", dir.display());
            return ExitCode::from(2);
        }
        if let Err(e) = workspace.write(dir) {
            eprintln!("error: cannot write {}: {e}", dir.display());
            return ExitCode::FAILURE;
        }
    }
    if stats || dir.is_none() {
        let s = workspace.stats();
        println!("files                    {}", s.files);
        println!("roots                    {}", s.roots);
        println!("includes                 {}", s.includes);
        println!("bytes                    {}", s.bytes);
        println!("lines                    {}", s.lines);
        println!("largest fan-in           {:?}", s.top_fan_in);
        println!("includes over 1000 roots {}", s.includes_over_1000_roots);
        println!("largest fan-out          {}", s.max_fan_out);
        println!("deepest include nesting  {}", s.max_depth);
        println!("largest expanded root    {} lines", s.max_expanded_lines);
        println!("roots over 30000 lines   {}", s.roots_over_30000_lines);
    }
    ExitCode::SUCCESS
}
