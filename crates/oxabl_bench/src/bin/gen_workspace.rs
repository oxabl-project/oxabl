//! Write a synthetic ABL workspace.
//!
//! Usage: gen-workspace [--preset ci|medium|erp] [--seed N] [--programs N]
//!        [--classes N] [--include-findings N] [--stats] <dir>
//!
//! The directory must not exist or must be empty. `--stats` prints the shape
//! and writes nothing when no directory is given.

use std::path::PathBuf;
use std::process::ExitCode;

use oxabl_bench::generate::{Preset, Spread, Stats, generate};

fn usage() -> ExitCode {
    eprintln!(
        "usage: gen-workspace [--preset ci|medium|erp] [--seed N] [--programs N] [--classes N] [--include-findings N] [--stats] <dir>"
    );
    ExitCode::from(2)
}

fn main() -> ExitCode {
    let mut preset = Preset::Ci;
    let mut seed = None;
    let mut programs = None;
    let mut classes = None;
    let mut include_findings = None;
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
            "--include-findings" => match number("--include-findings") {
                Some(n) => include_findings = Some(n as usize),
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
    if let Some(n) = include_findings {
        knobs.include_findings = n;
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
        print_stats(&workspace.stats());
    }
    ExitCode::SUCCESS
}

fn spread(name: &str, s: Spread) {
    println!(
        "{name:<24} mean {:>6}  p50 {:>7}  p90 {:>7}  p99 {:>7}  max {:>7}",
        s.mean, s.p50, s.p90, s.p99, s.max
    );
}

fn print_stats(s: &Stats) {
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
    spread("lines per file", s.lines_per_file);
    spread("lines per root", s.root_lines);
    spread("lines per include", s.include_lines);
    spread("direct includes", s.direct_includes);
    spread("closure per root", s.closure);
    spread("depth per root", s.depth);
    spread("expanded lines per root", s.expanded_lines);
    spread("direct fan-in", s.direct_fan_in);
    println!("top direct fan-in        {:?}", s.top_direct_fan_in);
    println!("includes never included  {}", s.never_included);
    spread("temp-tables per file", s.temp_tables_per_file);
    let c = &s.counts;
    println!("&SCOPED-DEFINE           {}", c.scoped_define);
    println!("&GLOBAL-DEFINE           {}", c.global_define);
    println!("&IF                      {}", c.if_directive);
    println!("{{&name}}                  {}", c.name_refs);
    println!("{{1}}                      {}", c.arg_refs);
    println!(
        "temp-tables              {} in {} files",
        c.temp_tables, c.files_with_temp_tables
    );
    println!("datasets                 {}", c.datasets);
    println!("CLASS / INHERITS         {} / {}", c.classes, c.inherits);
    println!(
        "procedures/functions/methods {} / {} / {}",
        c.procedures, c.functions, c.methods
    );
    println!("FIND / FOR EACH          {} / {}", c.find, c.for_each);
    println!("RUN / NEW class          {} / {}", c.run, c.new_object);
    println!(
        "NEW SHARED / SHARED      {} / {} ({} producer files)",
        c.new_shared, c.define_shared, c.producers
    );
}
