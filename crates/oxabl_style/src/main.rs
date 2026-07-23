use std::env;
use std::process::ExitCode;

use oxabl_style::StyleGuide;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let command = args.get(1).map(|s| s.as_str()).unwrap_or("preset");

    match command {
        "preset" => {
            let name = args.get(2).map(|s| s.as_str()).unwrap_or("oestandards");
            match StyleGuide::from_preset_name(name) {
                Some(guide) => match guide.to_toml() {
                    Ok(s) => println!("{s}"),
                    Err(e) => {
                        eprintln!("error: {e}");
                        return ExitCode::from(2);
                    }
                },
                None => {
                    eprintln!(
                        "error: unknown preset `{name}` (use `oestandards` or `consultingwerk`)"
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
            let deviations = match style_diff(&base, &user) {
                Ok(d) => d,
                Err(e) => {
                    eprintln!("error: {e}");
                    return ExitCode::from(2);
                }
            };
            if deviations.is_empty() {
                eprintln!("{path}: matches default base (no deviations)");
            } else {
                eprintln!(
                    "{path}: {} field(s) differ from default base",
                    deviations.len()
                );
                for (field, base_val, user_val) in deviations {
                    println!("{field}: {base_val} => {user_val}");
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

/// Field-by-field deviations of `user` from `base`, as
/// `(field_name, base_value, user_value)` string triples.
///
/// Both guides are serialized to TOML and compared key-by-key, so the diff
/// stays in sync with the struct automatically as fields are added. `Option`
/// fields that are `None` serialize to no key and render as `(unset)` on
/// whichever side omits them.
fn style_diff(
    base: &StyleGuide,
    user: &StyleGuide,
) -> Result<Vec<(String, String, String)>, toml::ser::Error> {
    let base_tbl: toml::Table =
        toml::from_str(&base.to_toml()?).expect("serialized StyleGuide must parse as a TOML table");
    let user_tbl: toml::Table =
        toml::from_str(&user.to_toml()?).expect("serialized StyleGuide must parse as a TOML table");

    let mut keys: Vec<&String> = base_tbl.keys().chain(user_tbl.keys()).collect();
    keys.sort_unstable();
    keys.dedup();

    let render = |v: Option<&toml::Value>| match v {
        Some(val) => val.to_string(),
        None => "(unset)".to_string(),
    };

    let mut out = Vec::new();
    for key in keys {
        let b = base_tbl.get(key);
        let u = user_tbl.get(key);
        if b != u {
            out.push((key.clone(), render(b), render(u)));
        }
    }
    Ok(out)
}
