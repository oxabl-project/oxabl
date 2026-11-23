//! The two parts of the worker program: what the compile key hashes, and what
//! it leaves out.

use oxabl_build::worker::program::{
    COMPILE_PART, DRIVER_PART, compile_digest, hash, key_digest, key_digest_of, source,
};
use oxabl_lexer::{Kind, tokenize};

/// The BLAKE3 hash of the compile part. An edit of
/// `resources/oxabl_worker_compile.p` changes it, and with it every compile
/// key of every user: change this value in the same diff on purpose, and say
/// so in the release notes. The alias in `program.rs` stops applying then.
const COMPILE_PART_DIGEST: &str =
    "aaff9bcafe00f377f5b7610f549adfe348fac37d1cf6b0eec1009ce10e49c999";

/// The BLAKE3 hash of the whole worker program of release 5.3.2, which that
/// release put in the worker section of every compile key.
const RELEASED_PROGRAM_DIGEST: &str =
    "cdd13b1ffca241532b7ab473551f2f6d3b31a9680baf30b7dbd363603bc9f019";

#[test]
fn the_compile_part_is_the_one_the_digest_pins() {
    assert_eq!(
        compile_digest().to_hex().as_str(),
        COMPILE_PART_DIGEST,
        "the compile part changed: every compile key moves with it"
    );
}

#[test]
fn the_key_keeps_the_released_digest_for_exactly_the_pinned_compile_part() {
    assert_eq!(key_digest().to_hex().as_str(), RELEASED_PROGRAM_DIGEST);
    let pinned = blake3::Hash::from_hex(COMPILE_PART_DIGEST).unwrap();
    assert_eq!(
        key_digest_of(pinned).to_hex().as_str(),
        RELEASED_PROGRAM_DIGEST
    );
}

#[test]
fn any_other_compile_part_stands_for_itself_in_the_key() {
    for edited in [
        format!("{COMPILE_PART} "),
        COMPILE_PART.replacen("TRUE", "FALSE", 1),
        String::new(),
    ] {
        let digest = blake3::hash(edited.as_bytes());
        assert_ne!(digest, compile_digest());
        assert_eq!(key_digest_of(digest), digest, "an edited compile part");
    }
    let released = blake3::Hash::from_hex(RELEASED_PROGRAM_DIGEST).unwrap();
    assert_eq!(
        key_digest_of(released),
        released,
        "the alias is not a chain"
    );
}

#[test]
fn the_program_is_the_header_the_compile_part_and_the_driver_part() {
    let text = source();
    assert!(text.starts_with("/* oxabl-worker protocol 1 */\n"));
    assert!(text.contains(COMPILE_PART) && text.ends_with(DRIVER_PART));
    assert_eq!(hash(), blake3::hash(text.as_bytes()));
    assert_ne!(
        hash(),
        compile_digest(),
        "the program hash and the compile hash name different texts"
    );
}

/// A statement of the driver part that can change how a unit compiles. Each
/// rule has the snippets that must trip it, so the deny list proves itself.
struct Rule {
    name: &'static str,
    matches: fn(&[Token]) -> bool,
    snippets: &'static [&'static str],
}

struct Token {
    kind: Kind,
    text: String,
}

fn is(window: &[Token], at: usize, kind: Kind) -> bool {
    window.get(at).is_some_and(|token| token.kind == kind)
}

fn named(window: &[Token], at: usize, name: &str) -> bool {
    window.get(at).is_some_and(|token| token.text == name)
}

const DENY: &[Rule] = &[
    Rule {
        name: "COMPILE",
        matches: |w| is(w, 0, Kind::Compile),
        snippets: &["COMPILE x.p SAVE.", "compi x.p SAVE."],
    },
    Rule {
        name: "assignment to PROPATH",
        matches: |w| is(w, 0, Kind::Propath) && is(w, 1, Kind::Equals),
        snippets: &["PROPATH = \"x\".", "ASSIGN PROPATH = \"x\"."],
    },
    Rule {
        name: "assignment to a SESSION or COMPILER attribute",
        matches: |w| {
            (is(w, 0, Kind::Session) || is(w, 0, Kind::Compiler))
                && is(w, 1, Kind::Colon)
                && is(w, 3, Kind::Equals)
        },
        snippets: &[
            "SESSION:DATE-FORMAT = \"ymd\".",
            "COMPILER:MULTI-COMPILE = FALSE.",
            "ASSIGN SESSION:YEAR-OFFSET = 1900.",
        ],
    },
    Rule {
        name: "CREATE ALIAS or DELETE ALIAS",
        matches: |w| (is(w, 0, Kind::Create) || is(w, 0, Kind::Delete)) && is(w, 1, Kind::Alias),
        snippets: &["CREATE ALIAS a FOR DATABASE b.", "DELETE ALIAS a."],
    },
    Rule {
        name: "CONNECT or DISCONNECT",
        matches: |w| is(w, 0, Kind::Connect) || is(w, 0, Kind::Disconnect),
        snippets: &["CONNECT -db x.", "DISCONNECT x."],
    },
    Rule {
        name: "RUN VALUE",
        matches: |w| is(w, 0, Kind::Run) && is(w, 1, Kind::Value),
        snippets: &["RUN VALUE(\"x.p\")."],
    },
    Rule {
        name: "OS-COMMAND",
        matches: |w| is(w, 0, Kind::OsCommand),
        snippets: &["OS-COMMAND SILENT \"x\"."],
    },
    Rule {
        name: "a preprocessor definition",
        matches: |w| is(w, 0, Kind::PreprocGlobalDefine) || is(w, 0, Kind::PreprocScopedDefine),
        snippets: &["&GLOBAL-DEFINE x 1", "&SCOPED-DEFINE x 1"],
    },
    Rule {
        name: "a super or persistent procedure",
        matches: |w| {
            named(w, 0, "add-super-procedure")
                || named(w, 0, "remove-super-procedure")
                || is(w, 0, Kind::Persistent)
        },
        snippets: &[
            "THIS-PROCEDURE:ADD-SUPER-PROCEDURE(h).",
            "SESSION:ADD-SUPER-PROCEDURE(h).",
            "SESSION:REMOVE-SUPER-PROCEDURE(h).",
            "RUN x.p PERSISTENT SET h.",
        ],
    },
];

/// The names of the rules that the text trips.
fn violations(text: &str) -> Vec<&'static str> {
    let tokens: Vec<Token> = tokenize(text)
        .into_iter()
        .filter(|token| token.kind != Kind::Comment)
        .map(|token| Token {
            kind: token.kind,
            text: text[token.start..token.end].to_ascii_lowercase(),
        })
        .collect();
    let mut found = Vec::new();
    for at in 0..tokens.len() {
        for rule in DENY {
            if (rule.matches)(&tokens[at..]) && !found.contains(&rule.name) {
                found.push(rule.name);
            }
        }
    }
    found
}

#[test]
fn the_driver_part_holds_no_statement_that_can_change_a_compile() {
    assert_eq!(violations(DRIVER_PART), Vec::<&str>::new());
}

#[test]
fn each_rule_of_the_lint_trips_on_its_snippets() {
    for rule in DENY {
        assert!(!rule.snippets.is_empty(), "{} has no snippet", rule.name);
        for snippet in rule.snippets {
            let driver = format!("{DRIVER_PART}\n{snippet}\n");
            assert_eq!(
                violations(&driver),
                [rule.name],
                "the driver with `{snippet}` must trip exactly `{}`",
                rule.name
            );
        }
    }
}

#[test]
fn a_snippet_in_a_comment_or_a_string_is_not_a_statement() {
    let driver =
        format!("{DRIVER_PART}\n/* COMPILE x. PROPATH = \"a\". */\nDISPLAY \"COMPILE\".\n");
    assert_eq!(violations(&driver), Vec::<&str>::new());
}

#[test]
fn the_lint_sees_the_statements_that_make_the_compile_part_the_compile() {
    let found = violations(COMPILE_PART);
    for rule in [
        "RUN VALUE",
        "assignment to PROPATH",
        "assignment to a SESSION or COMPILER attribute",
    ] {
        assert!(found.contains(&rule), "{rule} is not in {found:?}");
    }
}
