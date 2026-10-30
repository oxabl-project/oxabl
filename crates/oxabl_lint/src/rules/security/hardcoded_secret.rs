//! `hardcoded-secret` (ABLSEC008), group `security`.
//!
//! Fires where a secret is written into the source. This rule has a different
//! shape from the other sink rules: a non-literal argument is the finding of
//! those, and here a *literal* is. Anyone who can read the code, the
//! repository or the compiled `.r` file can read the secret, and it cannot be
//! changed without a release.
//!
//! # The set
//!
//! | Form | What is a secret |
//! |---|---|
//! | `CONNECT ... -P word`, `-P "text"`, `CONNECT VALUE("... -P word")`, `handle:CONNECT("... -P word")` | the password |
//! | any string literal that holds `oech1::` | an encoded password |
//! | `ENCRYPT(data, key)`, `DECRYPT(data, key)` | a literal `key` |
//! | `GENERATE-PBE-KEY(passphrase, ...)` | a literal `passphrase` |
//! | `SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = "text"` and `...-IV` | the value |
//! | `name = "text"`, `ASSIGN name = "text"`, `DEFINE VARIABLE name ... INITIAL "text"` | the value, when `name` looks like a password |
//!
//! ```abl
//! CONNECT -db sales -H dbhost -S 20000 -U app -P "s3cret".
//! ```
//!
//! Fixed — read the secret at run time from a protected store (an environment
//! variable, a secrets service or a file outside the code with tight
//! permissions):
//!
//! ```abl
//! CONNECT -db sales -H dbhost -S 20000 -U app -P VALUE(cPassword).
//! ```
//!
//! # Message
//!
//! A finding never repeats the literal. The message shows the value as `"****"`,
//! and an `oech1::` literal as `oech1::****`. This keeps the secret out of
//! terminal output, CI logs and SARIF files.
//!
//! # Password-like names
//!
//! A name is password-like when one of its words is in the list of
//! [`Heuristics::secret_words`](super::names::Heuristics): `password`, `pwd`,
//! `passphrase`, `secret`, `apikey`, `privatekey` and a few more (see
//! [`names`](super::names) for how a name is split into words). The list is
//! short on purpose: a substring such as `pass` or a plain `key` would match
//! `cPassenger` and `cKey`. A name that also holds a word such as `label`,
//! `prompt`, `field` or `message` is text about a password, and is not
//! reported (`cPasswordLabel = "Password"`). An empty string and `?` are not
//! secrets.
//!
//! # Text that is about a password
//!
//! A password-like name often holds text for a page or a message instead of
//! the secret. Such a literal is not reported:
//!
//! - markup: a tag (`<` with a letter, `/` or `!` after it, and a later `>`),
//!   or an HTML entity (`&lt;`, `&gt;`, `&amp;`, `&quot;`, `&nbsp;`, `&#`);
//! - a template: a `SUBSTITUTE` marker `&1` to `&9`, a `%s`-style format, or a
//!   `{{name}}` or `${name}` placeholder;
//! - a label or prompt: text that ends with `:`, or that holds a space and one
//!   of the secret words (`"Enter your new password"`).
//!
//! - a link: a `javascript:` value, or an `http://` or `https://` URL with no
//!   user and password before the host and no query parameter named like a
//!   password (`https://example.com/reset` is a link,
//!   `https://bob:pw@host/` and `https://host/?pwd=x` are not).
//!
//! A single `<`, `&` or `{` in a value does not make it text: `"a<3b"` and
//! `"{s3cret}"` are still reported, and so is the word `"password"` alone. Code that embeds the linter can pass its own lists to
//! [`run_with`].
//!
//! # Where the rule looks
//!
//! Modelled assignments and declarations; `CONNECT`, which the parser does not
//! model, by lexing the statement; calls in expressions and, by lexing, in
//! other unmodelled statements; and every string literal of the file for
//! `oech1::`. A secret in a literal that is passed as a procedure or method
//! argument (`oAuth:Login("bob", "hunter2")`) is not seen, and neither is a
//! secret in an `INITIAL` value of a temp-table field or a property.

use oxabl_ast::{Expression, ExpressionKind, Span, Statement, StatementKind};
use oxabl_common::{Diagnostic, Severity};
use oxabl_lexer::{Kind, Lexer};
use oxabl_semantic::{AnalysisContext, Semantic};

use super::calls::{CallKind, Site, last_name, scan, string_literal, token_string};
use super::literal::{StatementText, cover, leading_word};
use super::names::{Heuristics, defaults};
use super::{ABLSEC008, finding, meta};
use crate::registry::RuleMeta;

/// Kebab-case rule name.
pub const NAME: &str = "hardcoded-secret";
/// Rule group, enabled together with its siblings.
pub const GROUP: &str = "security";
/// Severity when the config does not set one.
pub const DEFAULT_SEVERITY: Severity = Severity::Error;
/// Security severity on the 0.0 to 10.0 scale, as a decimal string.
pub const SECURITY_SEVERITY: &str = "8.0";
/// One-line description for listings.
pub const DESCRIPTION: &str =
    "a password, key or encoded secret is written into the source as a literal";

/// Remediation text, shown with each finding and in rule listings.
pub const HELP: &str = "Read the secret at run time from a protected store, such as an environment \
     variable, a secrets service or a file outside the code with tight permissions, and rotate \
     any secret that was in the source. A literal in code is readable by everyone who can read \
     the code or the compiled file.";

/// Registry metadata.
pub const META: RuleMeta = meta(
    ABLSEC008,
    NAME,
    DESCRIPTION,
    DEFAULT_SEVERITY,
    SECURITY_SEVERITY,
    HELP,
);

/// The shown form of a secret.
const MASK: &str = "\"****\"";

/// The `SECURITY-POLICY` attributes that hold key material.
const KEY_ATTRIBUTES: [&str; 2] = ["SYMMETRIC-ENCRYPTION-KEY", "SYMMETRIC-ENCRYPTION-IV"];

/// Entry point.
pub fn run(program: &[Statement], _sem: &Semantic, ctx: &AnalysisContext) -> Vec<Diagnostic> {
    run_with(program, ctx, defaults())
}

/// Entry point with the word lists of `heuristics` for password-like names.
pub fn run_with(
    program: &[Statement],
    ctx: &AnalysisContext,
    heuristics: &Heuristics,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut report = |span: Span, message: String| {
        diags.push(finding(
            ctx,
            DEFAULT_SEVERITY,
            ABLSEC008,
            span,
            message,
            HELP,
        ));
    };
    oech1_literals(ctx.source, &mut report);
    scan(
        ctx,
        program,
        &["CONNECT", "ENCRYPT", "DECRYPT", "GENERATE-PBE-KEY"],
        &mut |site| match site {
            Site::Statement(stmt) => statement(ctx, stmt, heuristics, &mut report),
            Site::Call(call) => {
                if call.kind == CallKind::Method && call.is("CONNECT") {
                    let hit = call
                        .args
                        .first()
                        .and_then(|a| a.literal.as_deref())
                        .is_some_and(has_password_option);
                    if hit {
                        report(
                            call.span,
                            format!("`CONNECT` passes a hard-coded password ({MASK})"),
                        );
                    }
                    return;
                }
                let (what, arg) = if call.is("ENCRYPT") || call.is("DECRYPT") {
                    (call.name.to_ascii_uppercase(), call.args.get(1))
                } else if call.is("GENERATE-PBE-KEY") {
                    ("GENERATE-PBE-KEY".to_string(), call.args.first())
                } else {
                    return;
                };
                if arg
                    .and_then(|a| a.literal.as_deref())
                    .is_some_and(is_secret_text)
                {
                    report(
                        call.span,
                        format!("`{what}` uses a hard-coded key ({MASK})"),
                    );
                }
            }
        },
    );
    diags
}

/// Whether a literal can be a secret: it is not blank and is not an `oech1::`
/// value, which has its own finding.
fn is_secret_text(text: &str) -> bool {
    !text.trim().is_empty() && !has_oech1(text)
}

fn has_oech1(text: &str) -> bool {
    text.as_bytes()
        .windows(7)
        .any(|w| w.eq_ignore_ascii_case(b"oech1::"))
}

/// Whether a literal is markup, a template or a label rather than a secret;
/// see the module docs.
fn is_text_about_secret(text: &str, heuristics: &Heuristics) -> bool {
    let lower = text.to_ascii_lowercase();
    let b = lower.as_bytes();
    let tag = b.iter().enumerate().any(|(i, &c)| {
        c == b'<'
            && b.get(i + 1)
                .is_some_and(|n| n.is_ascii_alphabetic() || *n == b'/' || *n == b'!')
            && b[i + 1..].contains(&b'>')
    });
    let entity = ["&lt;", "&gt;", "&amp;", "&quot;", "&nbsp;", "&#"]
        .iter()
        .any(|e| lower.contains(e));
    let marker = b.windows(2).any(|w| {
        (w[0] == b'&' && (b'1'..=b'9').contains(&w[1]))
            || (w[0] == b'%' && (w[1] == b's' || w[1] == b'd' || w[1].is_ascii_digit()))
    });
    let placeholder = (lower.contains("{{") && lower.contains("}}"))
        || lower.find("${").is_some_and(|i| lower[i..].contains('}'));
    let trimmed = text.trim();
    let label = trimmed.ends_with(':')
        || (trimmed.contains(char::is_whitespace)
            && super::names::segments(trimmed)
                .iter()
                .any(|w| heuristics.secret_words.contains(w)));
    tag || entity || marker || placeholder || label || is_plain_link(trimmed, heuristics)
}

/// A `javascript:` value, or an HTTP URL that carries no credentials.
fn is_plain_link(text: &str, heuristics: &Heuristics) -> bool {
    let lower = text.to_ascii_lowercase();
    if lower.starts_with("javascript:") {
        return true;
    }
    let Some(rest) = lower
        .strip_prefix("https://")
        .or_else(|| lower.strip_prefix("http://"))
    else {
        return false;
    };
    let authority = rest.split(['/', '?', '#']).next().unwrap_or("");
    if authority.contains('@') {
        return false;
    }
    let query = rest.split_once('?').map_or("", |(_, q)| q);
    !query.split('&').any(|pair| {
        pair.split_once('=')
            .is_some_and(|(k, v)| !v.is_empty() && heuristics.is_secret(k))
    })
}

/// Whether a connection string holds `-P` and a word after it.
fn has_password_option(text: &str) -> bool {
    let mut words = text.split_whitespace();
    while let Some(word) = words.next() {
        if word == "-P" {
            return words.next().is_some_and(|w| !has_oech1(w));
        }
    }
    false
}

/// Report each string literal that holds `oech1::`, and each bare `oech1::`.
fn oech1_literals(source: &str, report: &mut impl FnMut(Span, String)) {
    if !source
        .as_bytes()
        .windows(5)
        .any(|w| w.eq_ignore_ascii_case(b"oech1"))
    {
        return;
    }
    let tokens: Vec<_> = Lexer::new(source).collect();
    for (i, t) in tokens.iter().enumerate() {
        let text = &source[t.start..t.end];
        let hit = match t.kind {
            Kind::StringLiteral => has_oech1(text),
            Kind::Identifier => {
                text.eq_ignore_ascii_case("oech1")
                    && tokens
                        .get(i + 1)
                        .is_some_and(|n| n.kind == Kind::DoubleColon && n.start == t.end)
            }
            _ => false,
        };
        if hit {
            report(
                Span {
                    start: t.start as u32,
                    end: t.end as u32,
                },
                "an `oech1::` literal is an encoded password that ships with the code \
                 (oech1::****)"
                    .to_string(),
            );
        }
    }
}

fn statement(
    ctx: &AnalysisContext,
    stmt: &Statement,
    heuristics: &Heuristics,
    report: &mut impl FnMut(Span, String),
) {
    match &stmt.kind {
        StatementKind::Assignment { target, value } => {
            assignment(stmt, target, value, heuristics, report);
        }
        StatementKind::Assign { assignments } => {
            for pair in assignments {
                assignment(stmt, &pair.target, &pair.value, heuristics, report);
            }
        }
        StatementKind::VariableDeclaration {
            name,
            initial_value: Some(value),
            ..
        } => {
            if let Some(text) = string_literal(value)
                && is_secret_text(&text)
                && !is_text_about_secret(&text, heuristics)
                && heuristics.is_secret_value(&name.name)
            {
                report(
                    cover(name.span, value.span, stmt.span),
                    format!("`{}` holds a hard-coded secret ({MASK})", name.name),
                );
            }
        }
        StatementKind::Skipped { .. }
            if leading_word(ctx.source, stmt).as_deref() == Some("CONNECT") =>
        {
            if let Some(st) = StatementText::of(ctx.source, stmt) {
                connect(&st, report);
            }
        }
        _ => {}
    }
}

fn assignment(
    stmt: &Statement,
    target: &Expression,
    value: &Expression,
    heuristics: &Heuristics,
    report: &mut impl FnMut(Span, String),
) {
    if !string_literal(value)
        .is_some_and(|t| is_secret_text(&t) && !is_text_about_secret(&t, heuristics))
    {
        return;
    }
    let Some(name) = last_name(target) else {
        return;
    };
    let is_key = matches!(&target.kind, ExpressionKind::MemberAccess { .. })
        && KEY_ATTRIBUTES.iter().any(|a| a.eq_ignore_ascii_case(name));
    if is_key || heuristics.is_secret_value(name) {
        let span = cover(target.span, value.span, stmt.span);
        let message = if is_key {
            format!("`{name}` is set to a hard-coded key ({MASK})")
        } else {
            format!("`{name}` holds a hard-coded secret ({MASK})")
        };
        report(span, message);
    }
}

/// The `-P` option and the connection strings of a `CONNECT` statement.
fn connect(st: &StatementText, report: &mut impl FnMut(Span, String)) {
    let mut i = 0;
    while i < st.len() {
        let option = st.is(i, Kind::Minus)
            && st.is_word(i + 1, "P")
            && st.token(i + 1).start == st.token(i).end;
        if option && i + 2 < st.len() {
            let value = i + 2;
            let hit = match st.token(value).kind {
                Kind::StringLiteral => is_secret_text(&token_string(st, value)),
                Kind::Value | Kind::Period | Kind::Minus | Kind::LeftParen => false,
                _ => !(st.is_word(value, "oech1") && st.is(value + 1, Kind::DoubleColon)),
            };
            if hit {
                report(
                    st.span(i..value + 1),
                    format!("`CONNECT` passes a hard-coded password ({MASK})"),
                );
            }
            i = value + 1;
            continue;
        }
        if st.is(i, Kind::StringLiteral) && has_password_option(&token_string(st, i)) {
            report(
                st.span(i..i + 1),
                format!("`CONNECT` passes a hard-coded password ({MASK})"),
            );
        }
        i += 1;
    }
}
