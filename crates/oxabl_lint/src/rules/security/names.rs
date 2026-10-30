//! Name heuristics for the rules that guess intent from an identifier.
//!
//! A name is split into words at `-`, `_`, `.`, digits and case changes, so
//! `cPassword`, `pc-password`, `PASSWORD` and `user_pwd` all hold the word
//! `password` or `pwd`, and `cPasswordLabel` also holds `label`. A name matches
//! a list when one of its words, or two neighbouring words joined (`api` +
//! `key`), is in the list. A word is never matched as a substring, so
//! `passenger` is not password-like.
//!
//! The lists are the defaults of [`Heuristics`]. Code that embeds the linter
//! can pass its own to the `run_with` function of a rule.

use std::sync::LazyLock;

/// Word lists, all lower case.
#[derive(Debug, Clone)]
pub struct Heuristics {
    /// Words of a name that holds a password or another secret.
    pub secret_words: Vec<String>,
    /// Words of a name that holds a token, a nonce or a one-time value.
    pub token_words: Vec<String>,
    /// Words of a name that holds an identifier.
    pub id_words: Vec<String>,
    /// Words that mark a secret-like name as text about the secret (a label, a
    /// prompt, a field name), not the secret itself.
    pub exempt_words: Vec<String>,
}

fn words(list: &[&str]) -> Vec<String> {
    list.iter().map(|w| (*w).to_string()).collect()
}

impl Default for Heuristics {
    fn default() -> Self {
        Heuristics {
            secret_words: words(&[
                "password",
                "passwords",
                "passwd",
                "pwd",
                "passphrase",
                "passcode",
                "secret",
                "apikey",
                "privatekey",
                "credential",
                "credentials",
            ]),
            token_words: words(&["token", "nonce", "otp", "csrf", "xsrf", "session", "salt"]),
            id_words: words(&["id", "uid", "guid", "uuid", "sid"]),
            exempt_words: words(&[
                "label",
                "lbl",
                "prompt",
                "caption",
                "title",
                "hint",
                "msg",
                "message",
                "error",
                "err",
                "regex",
                "pattern",
                "field",
                "column",
                "header",
                "text",
                "name",
                "param",
                "parameter",
                "policy",
                "rule",
                "min",
                "max",
                "length",
                "len",
                "count",
                "expired",
                "required",
                "changed",
                "flag",
                "type",
                "format",
                "mask",
                "placeholder",
                "description",
                "desc",
                "file",
                "path",
                "dir",
                "url",
                "uri",
                "page",
                "form",
                "button",
                "btn",
                "link",
                "href",
                "hide",
                "show",
                "visible",
                "display",
                "style",
                "class",
                "css",
                "id",
            ]),
        }
    }
}

/// The defaults, built once.
pub fn defaults() -> &'static Heuristics {
    static DEFAULTS: LazyLock<Heuristics> = LazyLock::new(Heuristics::default);
    &DEFAULTS
}

/// The lower-case words of `name`.
pub fn segments(name: &str) -> Vec<String> {
    let chars: Vec<char> = name.chars().collect();
    let mut out = Vec::new();
    let mut current = String::new();
    for (i, &c) in chars.iter().enumerate() {
        if !c.is_alphabetic() {
            if !current.is_empty() {
                out.push(std::mem::take(&mut current));
            }
            continue;
        }
        let boundary = c.is_uppercase()
            && i > 0
            && chars[i - 1].is_alphabetic()
            && (chars[i - 1].is_lowercase() || chars.get(i + 1).is_some_and(|n| n.is_lowercase()));
        if boundary && !current.is_empty() {
            out.push(std::mem::take(&mut current));
        }
        current.extend(c.to_lowercase());
    }
    if !current.is_empty() {
        out.push(current);
    }
    out
}

fn holds(parts: &[String], list: &[String]) -> bool {
    parts.iter().enumerate().any(|(i, part)| {
        list.contains(part)
            || parts
                .get(i + 1)
                .is_some_and(|next| list.contains(&format!("{part}{next}")))
    })
}

impl Heuristics {
    /// Whether `name` looks like it holds a password or another secret.
    pub fn is_secret(&self, name: &str) -> bool {
        holds(&segments(name), &self.secret_words)
    }

    /// Whether `name` looks like it holds a secret, and is not text about one.
    pub fn is_secret_value(&self, name: &str) -> bool {
        let parts = segments(name);
        holds(&parts, &self.secret_words) && !holds(&parts, &self.exempt_words)
    }

    /// Whether `name` looks like it holds a secret or a token.
    pub fn is_secret_or_token(&self, name: &str) -> bool {
        let parts = segments(name);
        holds(&parts, &self.secret_words) || holds(&parts, &self.token_words)
    }

    /// Whether `name` looks like a token, nonce, secret or identifier, a value
    /// that must not be guessable.
    pub fn is_unguessable(&self, name: &str) -> bool {
        let parts = segments(name);
        holds(&parts, &self.secret_words)
            || holds(&parts, &self.token_words)
            || holds(&parts, &self.id_words)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splits_at_case_changes_separators_and_digits() {
        assert_eq!(segments("cPassword"), ["c", "password"]);
        assert_eq!(segments("pc-user_pwd2"), ["pc", "user", "pwd"]);
        assert_eq!(segments("HTTPServer"), ["http", "server"]);
        assert_eq!(segments("PASSWORD"), ["password"]);
        assert_eq!(segments("Acme.Auth"), ["acme", "auth"]);
    }

    #[test]
    fn matches_words_and_joined_neighbours_not_substrings() {
        let h = Heuristics::default();
        assert!(h.is_secret("cPassword"));
        assert!(h.is_secret("api-key"));
        assert!(h.is_secret("cApiKey"));
        assert!(h.is_secret("PWD"));
        assert!(!h.is_secret("cPassenger"));
        assert!(!h.is_secret("cKey"));
        assert!(!h.is_secret("cSecretary"));
    }

    #[test]
    fn exempt_words_mark_text_about_a_secret() {
        let h = Heuristics::default();
        assert!(h.is_secret_value("cPassword"));
        assert!(!h.is_secret_value("cPasswordLabel"));
        assert!(!h.is_secret_value("lcPasswordPrompt"));
        assert!(h.is_secret("cPasswordLabel"));
        assert!(!h.is_secret_value("cForgotPasswordHref"));
        assert!(!h.is_secret_value("cHideChngPwd"));
    }

    #[test]
    fn tokens_and_ids() {
        let h = Heuristics::default();
        assert!(h.is_secret_or_token("cResetToken"));
        assert!(h.is_unguessable("iSessionId"));
        assert!(h.is_unguessable("cUserId"));
        assert!(!h.is_unguessable("cIdentity"));
        assert!(!h.is_unguessable("iCount"));
    }
}
