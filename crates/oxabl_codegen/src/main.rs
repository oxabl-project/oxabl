#[allow(unused_imports)]
use scraper::{Html, Selector};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

// Target file paths relative to workspace root
const KIND_RS_PATH: &str = "crates/oxabl_lexer/src/kind.rs";
const BUILD_RS_PATH: &str = "crates/oxabl_lexer/build.rs";
const CALLABLE_RS_PATH: &str = "crates/oxabl_lexer/src/callable.rs";

// =============================================================================
// TOML Override Structures
// =============================================================================

#[derive(Debug, Clone, Deserialize)]
pub struct OverrideFile {
    #[serde(default)]
    pub add: Vec<AddKeyword>,
    #[serde(default)]
    pub r#override: Vec<OverrideKeyword>,
    #[serde(default)]
    pub remove: Vec<RemoveKeyword>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct AddKeyword {
    pub name: String,
    #[serde(default)]
    pub reserved: bool,
    pub keyword_type: Option<String>,
    pub min_abbreviation: Option<String>,
    pub doc_url: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct OverrideKeyword {
    pub name: String,
    pub reserved: Option<bool>,
    pub keyword_type: Option<String>,
    pub min_abbreviation: Option<String>,
    pub doc_url: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RemoveKeyword {
    pub name: String,
}

/// Keyword info extracted from HTML
#[derive(Debug, Clone)]
pub struct HtmlKeyword {
    pub name: String,
    pub reserved: bool,
    pub min_abbreviation: Option<String>,
}

/// Entry from the JSON documentation index
#[derive(Debug, Clone, Deserialize)]
pub struct JsonEntry {
    pub bundle_id: String,
    #[serde(rename = "childEntries")]
    pub child_entries: Vec<JsonEntry>,
    pub id: Option<String>,
    pub nav_path: String,
    pub outputclasses: Vec<String>,
    pub title: String,
    pub url: String,
}

/// Keyword type extracted from JSON title
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KeywordType {
    Function,
    Statement,
    Method,
    Property,
    Attribute,
    Event,
    Option,
    Phrase,
    Widget,
    Type,
    Operator,
    Handle,
    System,
    Preprocessor,
    Other(String),
}

impl KeywordType {
    fn from_title(title: &str) -> Option<(String, Self)> {
        let title = title.trim();

        // Order matters! More specific suffixes must come before less specific ones
        // e.g., " system handle" before " handle", " preprocessor directive" before " preprocessor"
        let suffixes = [
            (" function", KeywordType::Function),
            (" statement", KeywordType::Statement),
            (" method", KeywordType::Method),
            (" property", KeywordType::Property),
            (" attribute", KeywordType::Attribute),
            (" event", KeywordType::Event),
            (" option", KeywordType::Option),
            (" phrase", KeywordType::Phrase),
            (" widget", KeywordType::Widget),
            (" type", KeywordType::Type),
            (" operator", KeywordType::Operator),
            (" system handle", KeywordType::Handle), // Before " handle"
            (" handle", KeywordType::Handle),
            (" preprocessor directive", KeywordType::Preprocessor), // Before " preprocessor"
            (" preprocessor", KeywordType::Preprocessor),
        ];

        for (suffix, kw_type) in suffixes {
            if let Some(name) = title.strip_suffix(suffix) {
                return Some((name.to_uppercase(), kw_type));
            }
        }

        None
    }

    fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "function" => Some(KeywordType::Function),
            "statement" => Some(KeywordType::Statement),
            "method" => Some(KeywordType::Method),
            "property" => Some(KeywordType::Property),
            "attribute" => Some(KeywordType::Attribute),
            "event" => Some(KeywordType::Event),
            "option" => Some(KeywordType::Option),
            "phrase" => Some(KeywordType::Phrase),
            "widget" => Some(KeywordType::Widget),
            "type" => Some(KeywordType::Type),
            "operator" => Some(KeywordType::Operator),
            "handle" => Some(KeywordType::Handle),
            "system" => Some(KeywordType::System),
            "preprocessor" => Some(KeywordType::Preprocessor),
            _ => Some(KeywordType::Other(s.to_string())),
        }
    }
}

/// Combined keyword information from both sources
#[derive(Debug, Clone)]
pub struct Keyword {
    pub name: String,
    pub reserved: bool,
    pub min_abbreviation: Option<String>,
    pub keyword_type: Option<KeywordType>,
    pub doc_url: Option<String>,
}

impl Keyword {
    /// Convert keyword name to a valid Rust PascalCase identifier
    fn to_variant_name(&self) -> String {
        let name = &self.name;

        // Handle special single-character operators/punctuation
        match name.as_str() {
            "+" => return "Plus".to_string(),
            "-" => return "Minus".to_string(),
            "*" => return "Star".to_string(),
            "/" => return "Slash".to_string(),
            "%" => return "Percent".to_string(),
            "=" => return "Equals".to_string(),
            "<" => return "LessThan".to_string(),
            ">" => return "GreaterThan".to_string(),
            "<=" => return "LessThanOrEqual".to_string(),
            ">=" => return "GreaterThanOrEqual".to_string(),
            "<>" => return "NotEqual".to_string(),
            "+=" => return "PlusEquals".to_string(),
            "-=" => return "MinusEquals".to_string(),
            "*=" => return "StarEquals".to_string(),
            "/=" => return "SlashEquals".to_string(),
            ":" => return "Colon".to_string(),
            "::" => return "DoubleColon".to_string(),
            "." => return "Period".to_string(),
            "?" => return "Question".to_string(),
            "@" => return "At".to_string(),
            "[" => return "LeftBracket".to_string(),
            "]" => return "RightBracket".to_string(),
            "{" => return "LeftBrace".to_string(),
            "}" => return "RightBrace".to_string(),
            "(" => return "LeftParen".to_string(),
            ")" => return "RightParen".to_string(),
            "," => return "Comma".to_string(),
            "^" => return "Caret".to_string(),
            "~" => return "Tilde".to_string(),
            "//" => return "LineComment".to_string(),
            "/*" => return "BlockCommentStart".to_string(),
            "*/" => return "BlockCommentEnd".to_string(),
            _ => {}
        }

        // Convert ABL keyword to PascalCase
        // e.g., "DEFINE" -> "Define", "AUTO-RETURN" -> "AutoReturn"
        let mut result = String::new();
        let mut capitalize_next = true;

        for ch in name.chars() {
            if ch == '-' || ch == '_' || ch == ' ' {
                capitalize_next = true;
            } else if ch == '&' {
                // Preprocessor directives start with &
                result.push_str("Preproc");
                capitalize_next = true;
            } else if ch.is_alphanumeric() {
                if capitalize_next {
                    result.push(ch.to_ascii_uppercase());
                    capitalize_next = false;
                } else {
                    result.push(ch.to_ascii_lowercase());
                }
            } else {
                // Skip other special characters
                capitalize_next = true;
            }
        }

        // Handle edge cases where result might start with a digit
        if result
            .chars()
            .next()
            .map(|c| c.is_ascii_digit())
            .unwrap_or(false)
        {
            result = format!("Kw{}", result);
        }

        if result.is_empty() {
            return "Unknown".to_string();
        }

        // Handle Rust reserved keywords by adding "Kw" prefix
        let rust_keywords = [
            "As", "Async", "Await", "Box", "Break", "Const", "Continue", "Crate", "Dyn", "Else",
            "Enum", "Extern", "False", "Fn", "For", "If", "Impl", "In", "Let", "Loop", "Match",
            "Mod", "Move", "Mut", "Pub", "Ref", "Return", "Self", "Static", "Struct", "Super",
            "Trait", "True", "Type", "Unsafe", "Use", "Where", "While", "Yield",
        ];

        if rust_keywords.contains(&result.as_str()) {
            format!("Kw{}", result)
        } else {
            result
        }
    }

    /// Check if this is a punctuation/operator (single special char or symbol combo)
    fn is_punctuation(&self) -> bool {
        let name = &self.name;
        matches!(
            name.as_str(),
            "+" | "-"
                | "*"
                | "/"
                | "%"
                | "="
                | "<"
                | ">"
                | "<="
                | ">="
                | "<>"
                | "+="
                | "-="
                | "*="
                | "/="
                | ":"
                | "::"
                | "."
                | "?"
                | "@"
                | "["
                | "]"
                | "{"
                | "}"
                | "("
                | ")"
                | ","
                | "^"
                | "~"
                | "//"
                | "/*"
                | "*/"
        )
    }

    /// Check if this is a preprocessor directive
    fn is_preprocessor(&self) -> bool {
        self.name.starts_with('&') || self.name.starts_with("{&")
    }
}

/// Parse the Progress reserved keywords file
/// Format: "  KEYWORD NAME                    ABBREV"
/// - Header line is skipped
/// - Keywords and abbreviations can contain single spaces
/// - Columns are separated by 2+ consecutive spaces
/// - All keywords in this file are reserved
pub fn parse_reserved_keywords(path: &Path) -> Vec<Keyword> {
    let content = fs::read_to_string(path).expect("Failed to read reserved keywords file");
    let mut keywords = Vec::new();

    for line in content.lines().skip(1) {
        // Skip header line
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Split on 2+ consecutive spaces to handle keywords with single spaces
        // e.g., "ACTIVE FORM                     " -> ["ACTIVE FORM"]
        // e.g., "AUTO RETURN                     AUTO RET" -> ["AUTO RETURN", "AUTO RET"]
        let parts: Vec<&str> = line.splitn(2, "  ").collect();

        let name = parts
            .first()
            .map(|s| s.trim().to_string())
            .unwrap_or_default();
        let min_abbreviation = parts
            .get(1)
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string());

        if !name.is_empty() {
            keywords.push(Keyword {
                name,
                reserved: true, // All keywords in this file are reserved
                min_abbreviation,
                keyword_type: None, // Will be enriched from JSON
                doc_url: None,      // Will be enriched from JSON
            });
        }
    }

    keywords
}

/// Parse JSON file to extract keyword types and URLs
pub fn parse_json(path: &Path) -> HashMap<String, (KeywordType, String)> {
    let content = fs::read_to_string(path).expect("Failed to read JSON file");
    let entries: Vec<JsonEntry> = serde_json::from_str(&content).expect("Failed to parse JSON");

    let mut keyword_info = HashMap::new();

    fn process_entries(entries: &[JsonEntry], info: &mut HashMap<String, (KeywordType, String)>) {
        for entry in entries {
            if let Some((name, kw_type)) = KeywordType::from_title(&entry.title) {
                info.insert(name, (kw_type, entry.url.clone()));
            }
            process_entries(&entry.child_entries, info);
        }
    }

    process_entries(&entries, &mut keyword_info);
    keyword_info
}

/// Enrich reserved keywords with type info from JSON
pub fn enrich_keywords(
    keywords: Vec<Keyword>,
    json_info: &HashMap<String, (KeywordType, String)>,
) -> Vec<Keyword> {
    keywords
        .into_iter()
        .map(|mut kw| {
            let name_upper = kw.name.to_uppercase();
            if let Some((kt, url)) = json_info.get(&name_upper) {
                kw.keyword_type = Some(kt.clone());
                kw.doc_url = Some(url.clone());
            }
            kw
        })
        .collect()
}

/// Find alternate forms (space ↔ hyphen) for keywords by checking if JSON has the other form
/// Returns a map from keyword name (uppercase) to its alternate form (if exists in JSON)
pub fn find_alternate_forms(
    keywords: &[Keyword],
    json_info: &HashMap<String, (KeywordType, String)>,
) -> HashMap<String, String> {
    let mut alternates = HashMap::new();

    // Build a set of all JSON keyword names for quick lookup
    let json_names: HashSet<String> = json_info.keys().cloned().collect();

    for kw in keywords {
        let name_upper = kw.name.to_uppercase();

        // Check if this keyword has a space that could be a hyphen in JSON
        if name_upper.contains(' ') {
            let hyphenated = name_upper.replace(' ', "-");
            if json_names.contains(&hyphenated) {
                alternates.insert(name_upper.clone(), hyphenated);
            }
        }
        // Check if this keyword has a hyphen that could be a space in JSON
        else if name_upper.contains('-') {
            let spaced = name_upper.replace('-', " ");
            if json_names.contains(&spaced) {
                alternates.insert(name_upper.clone(), spaced);
            }
        }
    }

    alternates
}

/// Load overrides from TOML file
pub fn load_overrides(path: &Path) -> Option<OverrideFile> {
    if !path.exists() {
        return None;
    }

    let content = fs::read_to_string(path).ok()?;
    toml::from_str(&content).ok()
}

/// Apply overrides to keyword list
pub fn apply_overrides(keywords: &mut Vec<Keyword>, overrides: &OverrideFile) {
    // Build a map for quick lookup by uppercase name
    let mut keyword_map: HashMap<String, usize> = keywords
        .iter()
        .enumerate()
        .map(|(i, kw)| (kw.name.to_uppercase(), i))
        .collect();

    // Process removals first
    for remove in &overrides.remove {
        let name_upper = remove.name.to_uppercase();
        if let Some(&idx) = keyword_map.get(&name_upper) {
            // Mark for removal by setting name to empty (we'll filter later)
            keywords[idx].name.clear();
        }
    }

    // Process overrides
    for ov in &overrides.r#override {
        let name_upper = ov.name.to_uppercase();
        if let Some(&idx) = keyword_map.get(&name_upper) {
            let kw = &mut keywords[idx];
            if let Some(reserved) = ov.reserved {
                kw.reserved = reserved;
            }
            if let Some(ref kt_str) = ov.keyword_type {
                kw.keyword_type = KeywordType::from_str(kt_str);
            }
            if let Some(ref abbrev) = ov.min_abbreviation {
                kw.min_abbreviation = if abbrev.is_empty() {
                    None
                } else {
                    Some(abbrev.clone())
                };
            }
            if let Some(ref url) = ov.doc_url {
                kw.doc_url = Some(url.clone());
            }
        }
    }

    // Process additions
    for add in &overrides.add {
        let name_upper = add.name.to_uppercase();

        // Skip if already exists
        if keyword_map.contains_key(&name_upper) {
            continue;
        }

        let keyword = Keyword {
            name: add.name.clone(),
            reserved: add.reserved,
            min_abbreviation: if add
                .min_abbreviation
                .as_ref()
                .map(|s| s.is_empty())
                .unwrap_or(true)
            {
                None
            } else {
                add.min_abbreviation.clone()
            },
            keyword_type: add
                .keyword_type
                .as_ref()
                .and_then(|s| KeywordType::from_str(s)),
            doc_url: add.doc_url.clone(),
        };

        keyword_map.insert(name_upper, keywords.len());
        keywords.push(keyword);
    }

    // Remove entries marked for deletion (empty names)
    keywords.retain(|kw| !kw.name.is_empty());
}

/// Generate the Kind enum for the lexer
pub fn generate_kind_enum(keywords: &[Keyword]) -> String {
    let mut output = String::new();

    output.push_str("/// Token types for the ABL lexer\n");
    output.push_str("/// Generated by oxabl_codegen\n");
    output.push_str("#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]\n");
    output.push_str("pub enum Kind {\n");

    // Special tokens first
    output.push_str("    // Special tokens\n");
    output.push_str("    Eof,\n");
    output.push_str("    Invalid,\n");
    output.push_str("    Identifier,\n");
    output.push_str("    Comment,\n");
    output.push('\n');

    // Literals (suffixed with "Literal" to avoid collision with ABL type keywords)
    output.push_str("    // Literals\n");
    output.push_str("    IntegerLiteral,\n");
    output.push_str("    BigIntLiteral,\n");
    output.push_str("    DecimalLiteral,\n");
    output.push_str("    StringLiteral,\n");
    output.push('\n');

    // Collect keywords by category, tracking seen variant names to avoid duplicates
    let mut seen_variants: HashSet<String> = HashSet::new();

    // Add special tokens we've already defined
    for name in [
        "Eof",
        "Invalid",
        "Identifier",
        "Comment",
        "IntegerLiteral",
        "BigIntLiteral",
        "DecimalLiteral",
        "StringLiteral",
    ] {
        seen_variants.insert(name.to_string());
    }

    let mut punctuation = Vec::new();
    let mut preprocessor = Vec::new();
    let mut functions = Vec::new();
    let mut statements = Vec::new();
    let mut methods = Vec::new();
    let mut properties = Vec::new();
    let mut attributes = Vec::new();
    let mut events = Vec::new();
    let mut options = Vec::new();
    let mut phrases = Vec::new();
    let mut widgets = Vec::new();
    let mut types = Vec::new();
    let mut operators = Vec::new();
    let mut handles = Vec::new();
    let mut other_keywords = Vec::new();

    for kw in keywords {
        let variant = kw.to_variant_name();

        if seen_variants.contains(&variant) {
            continue;
        }
        seen_variants.insert(variant.clone());

        let entry = format!("    {},\n", variant);

        if kw.is_punctuation() {
            punctuation.push(entry);
        } else if kw.is_preprocessor() {
            preprocessor.push(entry);
        } else if let Some(ref kt) = kw.keyword_type {
            match kt {
                KeywordType::Function => functions.push(entry),
                KeywordType::Statement => statements.push(entry),
                KeywordType::Method => methods.push(entry),
                KeywordType::Property => properties.push(entry),
                KeywordType::Attribute => attributes.push(entry),
                KeywordType::Event => events.push(entry),
                KeywordType::Option => options.push(entry),
                KeywordType::Phrase => phrases.push(entry),
                KeywordType::Widget => widgets.push(entry),
                KeywordType::Type => types.push(entry),
                KeywordType::Operator => operators.push(entry),
                KeywordType::Handle => handles.push(entry),
                _ => other_keywords.push(entry),
            }
        } else {
            other_keywords.push(entry);
        }
    }

    // Write each category
    let categories = [
        ("Punctuation", &punctuation),
        ("Operators", &operators),
        ("Preprocessor directives", &preprocessor),
        ("Statements", &statements),
        ("Functions", &functions),
        ("Methods", &methods),
        ("Properties", &properties),
        ("Attributes", &attributes),
        ("Events", &events),
        ("Options", &options),
        ("Phrases", &phrases),
        ("Widgets", &widgets),
        ("Types", &types),
        ("Handles", &handles),
        ("Other keywords", &other_keywords),
    ];

    for (name, items) in categories {
        if !items.is_empty() {
            output.push_str(&format!("    // {}\n", name));
            for item in items {
                output.push_str(item);
            }
            output.push('\n');
        }
    }

    output.push_str("}\n");
    output
}

/// Generate the complete build.rs file with atom list
pub fn generate_build_rs(keywords: &[Keyword]) -> String {
    let mut output = String::new();

    // File header
    output.push_str("// Generated by oxabl_codegen - DO NOT EDIT\n");
    output.push_str("use std::env;\n");
    output.push_str("use std::path::Path;\n\n");
    output.push_str("fn main() {\n");

    // Atom list content
    output.push_str("    string_cache_codegen::AtomType::new(\"oxabl_atom::OxablAtom\", \"atom!\")\n");
    output.push_str("        .atoms(&[\n");

    // Collect unique strings - lowercase versions of keywords for case-insensitive matching
    let mut atoms: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();

    for kw in keywords {
        // Add lowercase version for matching
        let lower = kw.name.to_lowercase();
        if !seen.contains(&lower) {
            seen.insert(lower.clone());
            atoms.push(lower);
        }

        // Also add the abbreviation if present
        if let Some(ref abbrev) = kw.min_abbreviation {
            let abbrev_lower = abbrev.to_lowercase();
            if !seen.contains(&abbrev_lower) {
                seen.insert(abbrev_lower.clone());
                atoms.push(abbrev_lower);
            }
        }
    }

    // Sort for consistent output
    atoms.sort();

    // Write in chunks of reasonable line length
    for atom in &atoms {
        // Escape the string for Rust
        let escaped = atom.replace('\\', "\\\\").replace('"', "\\\"");
        output.push_str(&format!("            \"{}\",\n", escaped));
    }

    output.push_str("        ])\n");
    output.push_str(
        "        .write_to_file(&Path::new(&env::var(\"OUT_DIR\").unwrap()).join(\"oxabl_atom.rs\"))\n",
    );
    output.push_str("        .unwrap();\n");
    output.push_str("}\n");

    output
}

/// Generate a keyword matching function
/// The `alternates` map contains keywords that have both space and hyphen forms
/// (e.g., "ACTIVE WINDOW" -> "ACTIVE-WINDOW" if both exist in sources)
pub fn generate_keyword_match(
    keywords: &[Keyword],
    alternates: &HashMap<String, String>,
) -> String {
    let mut output = String::new();

    output.push_str("/// Match a string to a keyword Kind\n");
    output.push_str("/// Handles ABL prefix abbreviations and case-insensitive matching\n");
    output.push_str("/// e.g., \"def\", \"defi\", \"defin\", \"define\" all match Kind::Define\n");
    output.push_str("pub fn match_keyword(s: &str) -> Option<Kind> {\n");
    output.push_str("    let lower = s.to_lowercase();\n");
    output.push_str("    match lower.as_str() {\n");

    // Track all string -> Kind mappings, detecting collisions
    let mut match_to_kind: HashMap<String, String> = HashMap::new();
    let mut collisions: Vec<(String, String, String)> = Vec::new();

    // Helper to add a match string, checking for collisions
    let mut add_match = |match_str: String, variant: &str| {
        if let Some(existing) = match_to_kind.get(&match_str) {
            if existing != variant {
                collisions.push((match_str, existing.clone(), variant.to_string()));
            }
        } else {
            match_to_kind.insert(match_str, variant.to_string());
        }
    };

    for kw in keywords {
        // Skip punctuation - handled separately in lexer
        if kw.is_punctuation() {
            continue;
        }

        let variant = kw.to_variant_name();
        let lower = kw.name.to_lowercase();
        let name_upper = kw.name.to_uppercase();

        // Determine the minimum length for prefix matching
        let min_len = if let Some(ref abbrev) = kw.min_abbreviation {
            abbrev.len()
        } else {
            // No abbreviation means exact match only
            lower.len()
        };

        // Generate all valid prefixes from min_len to full length
        for len in min_len..=lower.len() {
            let prefix = &lower[..len];
            add_match(prefix.to_string(), &variant);
        }

        // If this keyword has an alternate form (space ↔ hyphen), generate those prefixes too
        if let Some(alternate_upper) = alternates.get(&name_upper) {
            let alternate_lower = alternate_upper.to_lowercase();

            // Use the same min_len logic for the alternate form
            // The abbreviation applies to both forms
            for len in min_len..=alternate_lower.len() {
                // Make sure we don't go past the string length
                if len <= alternate_lower.len() {
                    let prefix = &alternate_lower[..len];
                    add_match(prefix.to_string(), &variant);
                }
            }
        }
    }

    // Report collisions to stderr during codegen
    for (prefix, kind1, kind2) in &collisions {
        eprintln!(
            "Warning: abbreviation collision for \"{}\": {} vs {}",
            prefix, kind1, kind2
        );
    }

    // Group matches by Kind for cleaner output with | patterns
    let mut kind_to_matches: HashMap<String, Vec<String>> = HashMap::new();
    for (match_str, kind) in &match_to_kind {
        kind_to_matches
            .entry(kind.clone())
            .or_default()
            .push(match_str.clone());
    }

    // Sort kinds for consistent output
    let mut kinds: Vec<_> = kind_to_matches.keys().cloned().collect();
    kinds.sort();

    for kind in kinds {
        let mut matches = kind_to_matches.remove(&kind).unwrap();
        matches.sort_by(|a, b| {
            // Sort by length first (shorter first), then alphabetically
            a.len().cmp(&b.len()).then(a.cmp(b))
        });

        if matches.len() == 1 {
            output.push_str(&format!(
                "        \"{}\" => Some(Kind::{}),\n",
                matches[0], kind
            ));
        } else {
            // Use | pattern for multiple matches
            let pattern = matches
                .iter()
                .map(|s| format!("\"{}\"", s))
                .collect::<Vec<_>>()
                .join(" | ");
            output.push_str(&format!("        {} => Some(Kind::{}),\n", pattern, kind));
        }
    }

    output.push_str("        _ => None,\n");
    output.push_str("    }\n");
    output.push_str("}\n");

    output
}

/// Generate the callable.rs file with is_callable_kind function
/// This function returns true for Kind::Identifier and all built-in function keywords
pub fn generate_callable_rs(keywords: &[Keyword]) -> String {
    let mut output = String::new();

    // File header
    output.push_str("// Generated by oxabl_codegen - DO NOT EDIT\n\n");
    output.push_str("use crate::Kind;\n\n");

    // Collect all function keywords
    let mut function_variants: Vec<String> = keywords
        .iter()
        .filter(|kw| matches!(kw.keyword_type, Some(KeywordType::Function)))
        .map(|kw| kw.to_variant_name())
        .collect();

    // Sort for consistent output
    function_variants.sort();
    function_variants.dedup();

    // Generate the function
    output.push_str("/// Returns true if the given Kind can be used as a callable (function call position).\n");
    output.push_str("/// This includes user-defined identifiers and all built-in ABL functions.\n");
    output.push_str("///\n");
    output.push_str("/// # Example\n");
    output.push_str("/// ```ignore\n");
    output.push_str("/// // These are callable:\n");
    output.push_str("/// // - myFunction()     -> Kind::Identifier\n");
    output.push_str("/// // - NOW()            -> Kind::Now\n");
    output.push_str("/// // - TRIM(x)          -> Kind::Trim\n");
    output.push_str("/// ```\n");
    output.push_str("#[inline]\n");
    output.push_str("pub fn is_callable_kind(kind: Kind) -> bool {\n");
    output.push_str("    matches!(\n");
    output.push_str("        kind,\n");
    output.push_str("        Kind::Identifier\n");

    for variant in &function_variants {
        output.push_str(&format!("            | Kind::{}\n", variant));
    }

    output.push_str("    )\n");
    output.push_str("}\n\n");

    // Also generate a list of callable kinds for reference/debugging
    output.push_str("/// List of all callable Kind variants (excluding Identifier).\n");
    output.push_str("/// Useful for documentation and testing.\n");
    output.push_str("pub const CALLABLE_FUNCTION_KINDS: &[Kind] = &[\n");

    for variant in &function_variants {
        output.push_str(&format!("    Kind::{},\n", variant));
    }

    output.push_str("];\n");

    output
}

/// Generate the complete kind.rs file (enum + match function)
pub fn generate_kind_rs(keywords: &[Keyword], alternates: &HashMap<String, String>) -> String {
    let mut output = String::new();

    // File header
    output.push_str("// Generated by oxabl_codegen - DO NOT EDIT\n\n");

    // Kind enum
    output.push_str(&generate_kind_enum(keywords));

    // Match function
    output.push('\n');
    output.push_str(&generate_keyword_match(keywords, alternates));

    output
}

/// Write generated content to a file
fn write_generated_file(relative_path: &str, content: &str) -> std::io::Result<()> {
    // Resolve path relative to workspace root
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let workspace_root = Path::new(manifest_dir)
        .parent()
        .unwrap() // crates/
        .parent()
        .unwrap(); // workspace root

    let target_path = workspace_root.join(relative_path);

    eprintln!("Writing {} ({} bytes)", relative_path, content.len());
    fs::write(&target_path, content)?;
    eprintln!("  Done: {}", target_path.display());

    Ok(())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let resources_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources");

    let reserved_path = resources_dir.join("abl_reserved_keywords.txt");
    let json_path = resources_dir.join("abl_keyword_index.json");
    let overrides_path = resources_dir.join("keyword_overrides.toml");

    // Parse reserved keywords from the official Progress keyword index
    eprintln!(
        "Parsing reserved keywords from: {}",
        reserved_path.display()
    );
    let reserved_keywords = parse_reserved_keywords(&reserved_path);
    eprintln!(
        "Found {} reserved keywords in official index",
        reserved_keywords.len()
    );

    // Parse JSON for type/URL enrichment (secondary source)
    eprintln!("Parsing JSON for type info from: {}", json_path.display());
    let json_info = parse_json(&json_path);
    eprintln!("Found {} typed entries in JSON", json_info.len());

    // Enrich reserved keywords with type info from JSON where available
    let mut keywords = enrich_keywords(reserved_keywords, &json_info);

    // Find alternate forms (space ↔ hyphen) by checking if JSON has the other form
    let alternates = find_alternate_forms(&keywords, &json_info);
    eprintln!(
        "Found {} keywords with alternate forms (space/hyphen variants)",
        alternates.len()
    );

    // Load and apply TOML overrides (operators, punctuation, etc.)
    if let Some(overrides) = load_overrides(&overrides_path) {
        let add_count = overrides.add.len();
        let override_count = overrides.r#override.len();
        let remove_count = overrides.remove.len();
        eprintln!(
            "Applying overrides: {} additions, {} modifications, {} removals",
            add_count, override_count, remove_count
        );
        apply_overrides(&mut keywords, &overrides);
    } else {
        eprintln!("No overrides file found at: {}", overrides_path.display());
    }

    // Check for command line arguments
    let command = args.get(1).map(|s| s.as_str()).unwrap_or("all");

    match command {
        "summary" => {
            // Print stats only, no file writing
            let reserved_count = keywords.iter().filter(|k| k.reserved).count();
            let with_abbrev = keywords
                .iter()
                .filter(|k| k.min_abbreviation.is_some())
                .count();
            let with_type = keywords.iter().filter(|k| k.keyword_type.is_some()).count();
            let function_count = keywords
                .iter()
                .filter(|k| matches!(k.keyword_type, Some(KeywordType::Function)))
                .count();

            println!("=== ABL Keyword Summary ===");
            println!("Total keywords: {}", keywords.len());
            println!("Reserved: {}", reserved_count);
            println!("With abbreviations: {}", with_abbrev);
            println!("With type info: {}", with_type);
            println!("Functions (callable): {}", function_count);
        }
        "kind" => {
            let content = generate_kind_rs(&keywords, &alternates);
            write_generated_file(KIND_RS_PATH, &content).expect("Failed to write kind.rs");
        }
        "atoms" => {
            let content = generate_build_rs(&keywords);
            write_generated_file(BUILD_RS_PATH, &content).expect("Failed to write build.rs");
        }
        "callable" => {
            let content = generate_callable_rs(&keywords);
            write_generated_file(CALLABLE_RS_PATH, &content).expect("Failed to write callable.rs");
        }
        "all" | "" => {
            // Default: write all generated files
            let kind_content = generate_kind_rs(&keywords, &alternates);
            write_generated_file(KIND_RS_PATH, &kind_content).expect("Failed to write kind.rs");

            let atoms_content = generate_build_rs(&keywords);
            write_generated_file(BUILD_RS_PATH, &atoms_content).expect("Failed to write build.rs");

            let callable_content = generate_callable_rs(&keywords);
            write_generated_file(CALLABLE_RS_PATH, &callable_content)
                .expect("Failed to write callable.rs");

            eprintln!("\nAll files regenerated successfully.");
        }
        "match" => {
            // Deprecated - now part of "kind"
            eprintln!("Note: 'match' is now combined with 'kind'. Use 'kind' or 'all' instead.");
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            eprintln!("Usage: cargo run -p oxabl_codegen [command]");
            eprintln!("Commands:");
            eprintln!("  all      - Write all generated files (default)");
            eprintln!("  kind     - Write kind.rs (enum + match function)");
            eprintln!("  atoms    - Write build.rs (atom list)");
            eprintln!("  callable - Write callable.rs (is_callable_kind function)");
            eprintln!("  summary  - Show statistics without writing files");
            std::process::exit(1);
        }
    }
}
