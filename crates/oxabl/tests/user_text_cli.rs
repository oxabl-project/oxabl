//! Text a user reads — every subcommand's `--help` and the `oxabl.toml` JSON
//! Schema — carries no internal requirement or plan tags.

use std::process::Command;

const COMMANDS: [&str; 9] = [
    "check",
    "format",
    "lsp",
    "schema",
    "explain",
    "build",
    "analyze",
    "conformance",
    "daemon",
];

fn oxabl(args: &[&str]) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(args)
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(0), "{args:?}");
    String::from_utf8(output.stdout).unwrap()
}

/// Words shaped like `KTD7`, `R15`, `A3`, `D2`, or `Slice 3`.
fn tags(text: &str) -> Vec<String> {
    let words: Vec<&str> = text
        .split(|c: char| !c.is_ascii_alphanumeric())
        .filter(|w| !w.is_empty())
        .collect();
    let mut found = Vec::new();
    for (i, word) in words.iter().enumerate() {
        let digits = word.trim_start_matches(|c: char| c.is_ascii_uppercase());
        let letters = &word[..word.len() - digits.len()];
        let tagged = !digits.is_empty()
            && digits.chars().all(|c| c.is_ascii_digit())
            && ["KTD", "R", "A", "D", "U"].contains(&letters);
        let slice = *word == "Slice"
            && words
                .get(i + 1)
                .is_some_and(|n| n.chars().all(|c| c.is_ascii_digit()));
        if tagged || slice {
            found.push(word.to_string());
        }
    }
    found
}

#[test]
fn the_scanner_finds_what_it_is_meant_to_find() {
    assert_eq!(
        tags("a (KTD7) b R15 and A3, D2; Slice 3"),
        ["KTD7", "R15", "A3", "D2", "Slice"]
    );
    assert!(tags("exit 2, RFC 2119, UTF-8, R, A").is_empty());
}

#[test]
fn help_of_every_command_has_no_internal_tags() {
    let top = oxabl(&["--help"]);
    for visible in ["check", "format", "lsp", "schema", "explain", "build"] {
        assert!(top.contains(visible), "{top}");
    }
    for command in COMMANDS {
        let help = oxabl(&[command, "--help"]);
        assert!(
            tags(&help).is_empty(),
            "`oxabl {command} --help` carries {:?}",
            tags(&help)
        );
    }
    assert!(tags(&top).is_empty(), "{:?}", tags(&top));
}

#[test]
fn every_advertised_command_is_covered() {
    let top = oxabl(&["--help"]);
    let listed = top
        .lines()
        .skip_while(|l| !l.starts_with("Commands:"))
        .skip(1)
        .take_while(|l| l.starts_with("  "))
        .filter_map(|l| l.split_whitespace().next())
        .filter(|name| *name != "help")
        .collect::<Vec<_>>();
    assert!(listed.contains(&"check"), "{top}");
    for name in listed {
        assert!(COMMANDS.contains(&name), "`{name}` is not scanned");
    }
}

#[test]
fn schema_has_no_internal_tags() {
    let schema = oxabl(&["schema"]);
    assert!(tags(&schema).is_empty(), "{:?}", tags(&schema));
}
