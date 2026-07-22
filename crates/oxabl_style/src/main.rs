use std::env;
use std::process::ExitCode;

use oxabl_style::StyleGuide;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let command = args.get(1).map(|s| s.as_str()).unwrap_or("preset");

    match command {
        "preset" => {
            let name = args.get(2).map(|s| s.as_str()).unwrap_or("oestandards");
            match name {
                "oestandards" => match StyleGuide::oestandards().to_toml() {
                    Ok(s) => println!("{s}"),
                    Err(e) => {
                        eprintln!("error: {e}");
                        return ExitCode::from(2);
                    }
                },
                "consultingwerk" => match StyleGuide::consultingwerk().to_toml() {
                    Ok(s) => println!("{s}"),
                    Err(e) => {
                        eprintln!("error: {e}");
                        return ExitCode::from(2);
                    }
                },
                other => {
                    eprintln!(
                        "error: unknown preset `{other}` (use `oestandards` or `consultingwerk`)"
                    );
                    return ExitCode::from(1);
                }
            }
        }
        "validate" => {
            let path = match args.get(2) {
                Some(p) => p,
                None => {
                    eprintln!("usage: oxabl-style validate <path>");
                    return ExitCode::from(1);
                }
            };
            let content = match std::fs::read_to_string(path) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("error: cannot read `{path}`: {e}");
                    return ExitCode::from(2);
                }
            };
            match StyleGuide::from_toml(&content) {
                Ok(_) => {
                    eprintln!("{path}: valid");
                }
                Err(e) => {
                    eprintln!("{path}: {e}");
                    return ExitCode::from(3);
                }
            }
        }
        "diff" => {
            let path = match args.get(2) {
                Some(p) => p,
                None => {
                    eprintln!("usage: oxabl-style diff <path>");
                    return ExitCode::from(1);
                }
            };
            let content = match std::fs::read_to_string(path) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("error: cannot read `{path}`: {e}");
                    return ExitCode::from(2);
                }
            };
            let user = match StyleGuide::from_toml(&content) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("{path}: {e}");
                    return ExitCode::from(3);
                }
            };
            let base = StyleGuide::default_base();
            if user.to_toml().ok() == base.to_toml().ok() {
                eprintln!("{path}: matches default base (no deviations)");
            } else {
                eprintln!("{path}: differs from default base");
                match base.to_toml() {
                    Ok(s) => println!("--- Base:\n{s}"),
                    Err(e) => eprintln!("error: {e}"),
                }
            }
        }
        _ => {
            eprintln!("usage: oxabl-style <command> [args]");
            eprintln!("commands:");
            eprintln!("  preset <name>     Print a named preset as TOML (default: oestandards)");
            eprintln!("  validate <path>   Validate a TOML style guide file");
            eprintln!("  diff <path>       Diff a config against the default base");
            return ExitCode::from(1);
        }
    }

    ExitCode::SUCCESS
}
