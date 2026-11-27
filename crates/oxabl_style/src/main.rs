use std::env;
use std::process::ExitCode;

use oxabl_style::StyleGuide;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let command = args.get(1).map(|s| s.as_str()).unwrap_or("preset");

    match command {
        "preset" => preset(args.get(2).map(|s| s.as_str()).unwrap_or("oestandards")),
        "validate" => validate(args.get(2)),
        "diff" => diff(args.get(2)),
        _ => {
            eprintln!("usage: oxabl-style <command> [args]");
            eprintln!("commands:");
            eprintln!("  preset <name>     Print a named preset as TOML (default: oestandards)");
            eprintln!("  validate <path>   Validate a TOML style guide file");
            eprintln!("  diff <path>       Diff a config against the default base");
            ExitCode::from(1)
        }
    }
}

fn preset(name: &str) -> ExitCode {
    let Some(guide) = StyleGuide::from_preset_name(name) else {
        eprintln!("error: unknown preset `{name}` (use `oestandards` or `consultingwerk`)");
        return ExitCode::from(1);
    };
    match guide.to_toml() {
        Ok(s) => {
            println!("{s}");
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::from(2)
        }
    }
}

/// Read the file at the path argument, or return the exit code to use.
fn read_path_arg<'a>(
    command: &str,
    arg: Option<&'a String>,
) -> Result<(&'a String, String), ExitCode> {
    let Some(path) = arg else {
        eprintln!("usage: oxabl-style {command} <path>");
        return Err(ExitCode::from(1));
    };
    match std::fs::read_to_string(path) {
        Ok(content) => Ok((path, content)),
        Err(e) => {
            eprintln!("error: cannot read `{path}`: {e}");
            Err(ExitCode::from(2))
        }
    }
}

fn validate(arg: Option<&String>) -> ExitCode {
    let (path, content) = match read_path_arg("validate", arg) {
        Ok(read) => read,
        Err(code) => return code,
    };
    match StyleGuide::from_toml(&content) {
        Ok(_) => {
            eprintln!("{path}: valid");
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("{path}: {e}");
            ExitCode::from(3)
        }
    }
}

fn diff(arg: Option<&String>) -> ExitCode {
    let (path, content) = match read_path_arg("diff", arg) {
        Ok(read) => read,
        Err(code) => return code,
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
) -> Result<Vec<(String, String, String)>, String> {
    let table = |guide: &StyleGuide| -> Result<toml::Table, String> {
        let text = guide.to_toml().map_err(|e| e.to_string())?;
        toml::from_str(&text).map_err(|e| format!("serialized StyleGuide is not a TOML table: {e}"))
    };
    let base_tbl = table(base)?;
    let user_tbl = table(user)?;

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
