//! The keywords completion offers: the statements that are valid at a
//! statement start, by the kind of scope that holds the cursor, and the words
//! that may follow `DEFINE`.

use oxabl_lexer::Kind;

use crate::keyword_docs::{DocKind, KEYWORD_DOCS};

/// Where a statement starts, as far as the statements that are valid there go.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Place {
    /// The file scope of a procedure file (`.p`, `.w`, `.i`).
    ProcedureFile,
    /// The file scope of a class file before its `CLASS`, `INTERFACE` or
    /// `ENUM` statement.
    ClassHeader,
    ClassBody,
    InterfaceBody,
    EnumBody,
    /// The body of a procedure, function, method, constructor, destructor,
    /// property accessor, trigger or block.
    Routine,
}

/// Where a snippet is valid (see `snippets.rs`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SnippetPlace {
    /// The file scope of a procedure file only.
    ProcedureFile,
    ClassHeader,
    ClassBody,
    /// A statement of a procedure file or a routine body.
    Statement,
}

impl SnippetPlace {
    pub(crate) fn allows(self, place: Place) -> bool {
        match self {
            SnippetPlace::ProcedureFile => place == Place::ProcedureFile,
            SnippetPlace::ClassHeader => place == Place::ClassHeader,
            SnippetPlace::ClassBody => place == Place::ClassBody,
            SnippetPlace::Statement => matches!(place, Place::ProcedureFile | Place::Routine),
        }
    }
}

/// The statements only a class file holds.
const CLASS_ONLY: &[&str] = &[
    "CLASS",
    "INTERFACE",
    "ENUM",
    "METHOD",
    "CONSTRUCTOR",
    "DESTRUCTOR",
];

/// The statements only the file scope holds.
const FILE_ONLY: &[&str] = &[
    "PROCEDURE",
    "FUNCTION",
    "USING",
    "BLOCK-LEVEL",
    "ROUTINE-LEVEL",
];

/// The statements of a class file before its type.
const CLASS_HEADER: &[&str] = &[
    "USING",
    "CLASS",
    "INTERFACE",
    "ENUM",
    "BLOCK-LEVEL",
    "ROUTINE-LEVEL",
];

const CLASS_BODY: &[&str] = &["DEFINE", "METHOD", "CONSTRUCTOR", "DESTRUCTOR", "ON", "END"];
const INTERFACE_BODY: &[&str] = &["DEFINE", "METHOD", "END"];
const ENUM_BODY: &[&str] = &["DEFINE", "END"];

/// One keyword item: the word, upper case, and the title of its entry in the
/// documentation index, when it has one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Keyword {
    pub word: &'static str,
    pub title: Option<&'static str>,
}

/// The first word of each statement of the documentation index, each once, in
/// the order of the index.
fn statement_words() -> impl Iterator<Item = &'static str> {
    let mut last = "";
    KEYWORD_DOCS
        .iter()
        .filter(|doc| doc.kind == DocKind::Statement)
        .map(|doc| doc.words[0])
        .filter(move |word| {
            let new = *word != last;
            last = word;
            new
        })
}

/// The title of the one-word statement `word`, as hover shows it.
pub(crate) fn statement_title(word: &str) -> Option<&'static str> {
    KEYWORD_DOCS
        .iter()
        .find(|doc| {
            doc.kind == DocKind::Statement
                && doc.words.len() == 1
                && doc.words[0].eq_ignore_ascii_case(word)
        })
        .map(|doc| doc.title)
}

/// The statement keywords that are valid at a statement start in `place`.
pub(crate) fn statement_keywords(place: Place) -> Vec<Keyword> {
    let keep = |word: &str| -> bool {
        match place {
            Place::ProcedureFile => !CLASS_ONLY.contains(&word) && word != "END",
            Place::Routine => !CLASS_ONLY.contains(&word) && !FILE_ONLY.contains(&word),
            Place::ClassHeader => CLASS_HEADER.contains(&word),
            Place::ClassBody => CLASS_BODY.contains(&word),
            Place::InterfaceBody => INTERFACE_BODY.contains(&word),
            Place::EnumBody => ENUM_BODY.contains(&word),
        }
    };
    statement_words()
        .filter(|word| keep(word))
        .map(|word| Keyword {
            word,
            title: statement_title(word),
        })
        .collect()
}

/// A modifier that may come between `DEFINE` and what it defines.
pub(crate) fn is_define_modifier(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::New
            | Kind::Global
            | Kind::Shared
            | Kind::Private
            | Kind::Protected
            | Kind::Public
            | Kind::PackagePrivate
            | Kind::PackageProtected
            | Kind::KwStatic
            | Kind::Abstract
            | Kind::Override
            | Kind::Serializable
            | Kind::NonSerializable
            | Kind::Input
            | Kind::Output
            | Kind::InputOutput
            | Kind::KwReturn
    )
}

const ACCESS: &[Kind] = &[
    Kind::Private,
    Kind::Protected,
    Kind::Public,
    Kind::PackagePrivate,
    Kind::PackageProtected,
];

const DIRECTIONS: &[Kind] = &[Kind::Input, Kind::Output, Kind::InputOutput, Kind::KwReturn];

/// The words of `DEFINE ...` that may follow the modifiers in `seen`, in
/// `place`: the modifiers that may still come, in their legal order, and what
/// `DEFINE` defines. `None` when the modifiers cannot stand in `place`.
pub(crate) fn define_keywords(place: Place, seen: &[Kind]) -> Option<Vec<Keyword>> {
    let in_class = matches!(place, Place::ClassBody | Place::InterfaceBody);
    let has = |kinds: &[Kind]| seen.iter().any(|kind| kinds.contains(kind));
    let shared = has(&[Kind::New, Kind::Global, Kind::Shared]);
    let access = has(ACCESS);
    let class_modifiers = has(&[
        Kind::KwStatic,
        Kind::Abstract,
        Kind::Override,
        Kind::Serializable,
        Kind::NonSerializable,
    ]);
    let direction = has(DIRECTIONS);
    if (in_class && (shared || direction))
        || (!in_class && (access || class_modifiers))
        || place == Place::ClassHeader
    {
        return None;
    }
    let last = seen.last().copied();
    let mut words: Vec<&'static str> = Vec::new();
    // Modifiers.
    if in_class {
        if !access && !class_modifiers {
            words.extend([
                "PRIVATE",
                "PROTECTED",
                "PUBLIC",
                "PACKAGE-PRIVATE",
                "PACKAGE-PROTECTED",
            ]);
        }
        if !has(&[Kind::KwStatic]) && !has(&[Kind::Abstract, Kind::Override]) {
            words.push("STATIC");
        }
        if place == Place::ClassBody && !has(&[Kind::Abstract, Kind::KwStatic]) {
            words.push("ABSTRACT");
        }
        if place == Place::ClassBody && !has(&[Kind::Override, Kind::KwStatic]) {
            words.push("OVERRIDE");
        }
        if place == Place::ClassBody && !has(&[Kind::Serializable, Kind::NonSerializable]) {
            words.extend(["SERIALIZABLE", "NON-SERIALIZABLE"]);
        }
    } else if place != Place::EnumBody {
        if seen.is_empty() {
            words.push("NEW");
            if matches!(place, Place::ProcedureFile | Place::Routine) {
                words.extend(["INPUT", "OUTPUT", "INPUT-OUTPUT", "RETURN"]);
            }
        }
        if last == Some(Kind::New) {
            words.push("GLOBAL");
        }
        if !has(&[Kind::Shared]) && seen.iter().all(|k| matches!(k, Kind::New | Kind::Global)) {
            words.push("SHARED");
        }
        if has(&[Kind::New, Kind::Global]) && !has(&[Kind::Shared]) {
            // `NEW` and `NEW GLOBAL` need `SHARED` before the word.
            return Some(keywords(words));
        }
    }
    // What `DEFINE` defines.
    for word in define_words() {
        let fits = match word {
            "ENUM" => place == Place::EnumBody,
            "PROPERTY" | "EVENT" => in_class,
            "PARAMETER" => matches!(place, Place::ProcedureFile | Place::Routine),
            _ => !matches!(place, Place::EnumBody | Place::InterfaceBody),
        };
        let fits = fits
            && match word {
                "PARAMETER" => !shared,
                _ => !direction,
            };
        if fits {
            words.push(word);
        }
    }
    Some(keywords(words))
}

/// The second words of the `DEFINE ...` entries of the documentation index.
fn define_words() -> impl Iterator<Item = &'static str> {
    KEYWORD_DOCS
        .iter()
        .filter(|doc| doc.kind == DocKind::Statement && doc.words.len() == 2)
        .filter(|doc| doc.words[0] == "DEFINE")
        .map(|doc| doc.words[1])
}

fn keywords(words: Vec<&'static str>) -> Vec<Keyword> {
    words
        .into_iter()
        .map(|word| Keyword {
            word,
            title: KEYWORD_DOCS
                .iter()
                .find(|doc| doc.words == ["DEFINE", word])
                .map(|doc| doc.title),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn words(keywords: &[Keyword]) -> Vec<&str> {
        keywords.iter().map(|k| k.word).collect()
    }

    #[test]
    fn a_procedure_file_has_no_class_statements() {
        let found = statement_keywords(Place::ProcedureFile);
        let found = words(&found);
        for word in ["DEFINE", "FIND", "FOR", "PROCEDURE", "FUNCTION", "USING"] {
            assert!(found.contains(&word), "{word}");
        }
        for word in ["METHOD", "CLASS", "CONSTRUCTOR", "WHEN", "OTHERWISE", "END"] {
            assert!(!found.contains(&word), "{word}");
        }
        assert_eq!(
            found.len(),
            found.iter().collect::<std::collections::HashSet<_>>().len(),
            "each word once"
        );
    }

    #[test]
    fn the_modifiers_of_define_follow_their_order() {
        let found = define_keywords(Place::ProcedureFile, &[]).unwrap();
        let found = words(&found);
        for word in ["NEW", "SHARED", "INPUT", "VARIABLE", "BUFFER", "PARAMETER"] {
            assert!(found.contains(&word), "{word}");
        }
        assert!(!found.contains(&"PROPERTY"));
        let after_new = define_keywords(Place::ProcedureFile, &[Kind::New]).unwrap();
        assert_eq!(words(&after_new), vec!["GLOBAL", "SHARED"]);
        let shared = define_keywords(Place::Routine, &[Kind::New, Kind::Shared]).unwrap();
        let shared = words(&shared);
        assert!(shared.contains(&"VARIABLE"));
        assert!(!shared.contains(&"NEW") && !shared.contains(&"PARAMETER"));
        let input = define_keywords(Place::Routine, &[Kind::Input]).unwrap();
        assert_eq!(words(&input), vec!["PARAMETER"]);
        let class = define_keywords(Place::ClassBody, &[Kind::Public]).unwrap();
        let class = words(&class);
        assert!(class.contains(&"PROPERTY") && class.contains(&"STATIC"));
        assert!(!class.contains(&"PUBLIC") && !class.contains(&"PARAMETER"));
        assert!(define_keywords(Place::ClassBody, &[Kind::Shared]).is_none());
        assert_eq!(
            words(&define_keywords(Place::EnumBody, &[]).unwrap()),
            vec!["ENUM"]
        );
    }
}
