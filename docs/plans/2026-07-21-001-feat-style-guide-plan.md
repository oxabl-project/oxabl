# ABL Style Guide — Plan (Revision 2)

## Goal

Create a Rust library crate (`oxabl_style`) that provides a comprehensive,
typed ABL style guide cataloging all rules from two reference standards
(oestandards, consultingwerk). Each rule carries a `Scope` tag identifying
whether it can be mechanically enforced by a formatter (`Formatting`) or
requires semantic analysis (`Diagnostic`). The library serves as the single
source of truth for both the future formatter and the existing linter.

## Scope Classification

Rules are annotated with one of two scopes:

- **Formatting**: A formatter can mechanically enforce this from the token
  stream or AST. Changes to formatting never alter program semantics.
- **Diagnostic**: Requires semantic analysis — a lint tool must check this.
  These rules involve renaming, inserting/removing constructs, or reasoning
  about runtime behavior.

The `StyleGuide` struct holds ALL rules regardless of scope. Consumers
filter by scope at use sites: the formatter reads `Formatting` rules;
the linter reads `Diagnostic` rules.

## Complete Rule Catalog

Rules are numbered with F=Formatting, D=Diagnostic.

### Keywords & Operators

| # | Rule | Scope | oestandards | consultingwerk |
|---|---|---|---|---|
| F1 | keyword_case | Formatting | Uppercase | Uppercase |
| F2 | keyword_abbreviation | Formatting | AbbreviateNothing | AbbreviateNothing |
| D3 | require_symbolic_operators | Diagnostic | true | false |

### Layout & Whitespace

| # | Rule | Scope | oestandards | consultingwerk |
|---|---|---|---|---|
| F4 | indent_size | Formatting | 4 | 4 |
| F5 | indent_style | Formatting | Spaces | Spaces |
| F6 | do_placement | Formatting | SameLine | SameLine |
| F7 | dot_colon_same_line | Formatting | true | false |
| F8 | period_placement | Formatting | SameLine | SameLine |
| F9 | where_placement | Formatting | NextLine | NextLine |
| F10 | and_or_placement | Formatting | EndOfLine | EndOfLine |
| F11 | first_param_same_line | Formatting | true | true |
| F12 | multi_param_threshold | Formatting | 3 | 3 |
| F13 | max_line_length | Formatting | 120 | 120 |
| F14 | wrap_long_lines | Formatting | true | true |
| F15 | blank_lines_between_sections | Formatting | true | true |

### Block & Statement Structure

| # | Rule | Scope | oestandards | consultingwerk |
|---|---|---|---|---|
| F16 | end_with_type | Formatting | true | true |
| F17 | using_sort | Formatting | false | true |
| D18 | require_block_labels | Diagnostic | true | false |
| D19 | disallow_unnecessary_blocks | Diagnostic | true | false |
| D20 | require_assign_statement | Diagnostic | true | false |
| D21 | prefer_substitute | Diagnostic | Always | ThreeOrMore |
| D22 | run_in_this_procedure | Diagnostic | true | false |
| D23 | require_if_parentheses | Diagnostic | true | false |

### Naming Conventions (all Diagnostic)

| # | Rule | Scope | oestandards | consultingwerk |
|---|---|---|---|---|
| D24 | variable_case | Diagnostic | CamelCase | LowerCase |
| D25 | method_case | Diagnostic | Unspecified | PascalCase |
| D26 | variable_type_prefix | Diagnostic | true | true |
| D27 | global_prefix | Diagnostic | Some('g') | None |
| D28 | parameter_prefix | Diagnostic | IpOpIop | PLowercase |
| D29 | buffer_naming | Diagnostic | BPrefix | RoleTable |
| D30 | temp_table_prefix | Diagnostic | TtBiPdsDs | TtE |
| D31 | class_naming | Diagnostic | Unspecified | SuffixBaseClass |
| D32 | interface_prefix | Diagnostic | None | Some('I') |
| D33 | file_name_casing | Diagnostic | Unspecified | LowerCamelCase |

### Required Constructs (all Diagnostic)

| # | Rule | Scope | oestandards | consultingwerk |
|---|---|---|---|---|
| D34 | require_no_undo | Diagnostic | true | true |
| D35 | require_this_object | Diagnostic | false | true |
| D36 | static_member_ref | Diagnostic | Unspecified | ClassName |
| D37 | require_file_headers | Diagnostic | true | true |
| D38 | require_widget_pool | Diagnostic | false | true |
| D39 | temp_table_in_include | Diagnostic | false | true |
| D40 | named_events_on_prefix | Diagnostic | false | true |

### Code Quality (all Diagnostic)

| # | Rule | Scope | oestandards | consultingwerk |
|---|---|---|---|---|
| D41 | disallow_commented_code | Diagnostic | true | false |
| D42 | variable_decl_alignment | Diagnostic | None | Tabular |

### File & Comment Structure

| # | Rule | Scope | oestandards | consultingwerk |
|---|---|---|---|---|
| F43 | comment_style | Formatting | BlockComment | BlockComment |
| F44 | class_structure_order | Formatting | Specified | Unspecified |
| F45 | procedure_structure_order | Formatting | Specified | Unspecified |

### Data Access & Performance (not in StyleGuide — oxabl_lint config)

These are pure semantic conventions with no layout component.
They map to lint rule configurations, not style guide rules.

| Rule | oestandards | consultingwerk | Lint |
|---|---|---|---|
| prefer_no_share_lock | true | true (with explanation) | new rule |
| prefer_can_find | true | false | LINT000? |
| prefer_rowid | true | false | new rule |
| require_table_name_qualification | true | false | LINT0003 |
| prefer_for_first_last | true | false | new rule |
| require_define_buffer | true | false | new rule |
| require_by_reference | true | false | new rule |
| allow_like_on_db | false | false | new rule |
| prefer_forward_slash | true | true | new rule |
| cleanup_in_finally | true | true | new rule |
| require_catch_throw | true | true | new rule |
| error_handling_mode | BlockLevel | Either | new rule |
| delete_dynamic_objects | true | true | new rule |
| memptr_deallocate | true | true | new rule |

## Architecture

### Crate: `oxabl_style`

```
crates/oxabl_style/
├── Cargo.toml
├── src/
│   ├── lib.rs           # Library entry point, re-exports
│   ├── rules.rs         # All rule enum definitions (Formatting + Diagnostic)
│   ├── style_guide.rs   # StyleGuide struct with ALL rules
│   └── preset.rs        # Named presets (oestandards, consultingwerk)
└── tests/
    └── presets.rs       # Preset correctness tests
```

### Design Decisions (addressing Claude Fable revision 2 feedback)

1. **No codegen** — `StyleGuide` deserializes TOML at runtime via serde.
   `default_base()` is defined in Rust (single source of truth). TOML
   deserialization is for user customizations, not for defining defaults.
   No duplicate TOML file.

2. **All rules in StyleGuide with Scope tags** — The `StyleGuide` struct
   holds ALL rules (Formatting + Diagnostic). Each rule field carries a
   `Scope` classification. Consumers filter by scope: the formatter reads
   `Scope::Formatting` rules; the linter reads `Scope::Diagnostic` rules.
   This preserves the comprehensive catalog the user requested while
   correctly separating enforcement responsibility.

3. **Two named presets** — `StyleGuide::oestandards()` and
   `StyleGuide::consultingwerk()` are first-class constructors.

4. **No duplicate TOML** — `default_base()` is the single source of truth.
   User TOML files are overrides, not a parallel definition.

5. **Resources: crate-local** — No top-level `resources/` file; the crate
   is self-contained. If shared TOML is needed later, it can be added.

6. **CLI: positional subcommands** — Matches `oxabl_codegen` style.

### Rule Enums

```rust
/// Whether a rule can be mechanically enforced by a formatter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Scope {
    /// Formatter can enforce from tokens/AST without semantic analysis.
    Formatting,
    /// Requires semantic analysis — lint tool territory.
    Diagnostic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum KeywordCase { Uppercase, Lowercase, Preserve }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum KeywordAbbreviation { KeepAbbreviations, AbbreviateEverything, AbbreviateNothing }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IndentStyle { Spaces, Tabs }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Placement { SameLine, NewLine }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PeriodPlacement { SameLine, NewLine }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SubstitutePolicy { Never, ThreeOrMore, Always }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VariableCase { Unspecified, CamelCase, LowerCase, SnakeCase }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MethodCase { Unspecified, PascalCase, CamelCase }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ParameterPrefix { IpOpIop, PLowercase, None }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BufferNaming { BPrefix, RoleTable, Unspecified }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TempTablePrefix { TtBiPdsDs, TtE, Unspecified }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ClassNaming { Unspecified, SuffixBaseClass }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StaticMemberRef { ShortName, ClassName, Unspecified }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CommentStyle { BlockComment, SlashSlash, Either }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FileNameCasing { Unspecified, LowerCamelCase }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AndOrPlacement { EndOfLine, BeginningOfLine }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VariableDeclAlignment { None, Tabular }

```

### StyleGuide Struct

All rules live in `StyleGuide`. The struct uses `#[serde(default)]` which
fills missing TOML fields via `Default::default()`. `Default` delegates to
`default_base()` so partial TOML overrides "fall back" to the base.

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)] // missing fields → Default::default() = default_base()
pub struct StyleGuide {
    // Keywords & Operators
    pub keyword_case: KeywordCase,
    pub keyword_abbreviation: KeywordAbbreviation,
    pub require_symbolic_operators: bool,

    // Layout & Whitespace
    pub indent_size: usize,
    pub indent_style: IndentStyle,
    pub do_placement: Placement,
    pub dot_colon_same_line: bool,
    pub period_placement: PeriodPlacement,
    pub where_placement: Placement,
    pub and_or_placement: AndOrPlacement,
    pub first_param_same_line: bool,
    pub multi_param_threshold: usize,
    pub max_line_length: usize,
    pub wrap_long_lines: bool,
    pub blank_lines_between_sections: bool,

    // Block & Statement Structure
    pub end_with_type: bool,
    pub using_sort: bool,
    pub require_block_labels: bool,
    pub disallow_unnecessary_blocks: bool,
    pub require_assign_statement: bool,
    pub prefer_substitute: SubstitutePolicy,
    pub run_in_this_procedure: bool,
    pub require_if_parentheses: bool,

    // Naming Conventions
    pub variable_case: VariableCase,
    pub method_case: MethodCase,
    pub variable_type_prefix: bool,
    pub global_prefix: Option<char>,
    pub parameter_prefix: ParameterPrefix,
    pub buffer_naming: BufferNaming,
    pub temp_table_prefix: TempTablePrefix,
    pub class_naming: ClassNaming,
    pub interface_prefix: Option<char>,
    pub file_name_casing: FileNameCasing,

    // Required Constructs
    pub require_no_undo: bool,
    pub require_this_object: bool,
    pub static_member_ref: StaticMemberRef,
    pub require_file_headers: bool,
    pub require_widget_pool: bool,
    pub temp_table_in_include: bool,
    pub named_events_on_prefix: bool,

    // Code Quality
    pub disallow_commented_code: bool,
    pub variable_decl_alignment: VariableDeclAlignment,

    // File & Comment Structure
    pub comment_style: CommentStyle,
    pub class_structure_order: Vec<String>,
    pub procedure_structure_order: Vec<String>,
}
```

### Scope Lookup

```rust
impl StyleGuide {
    /// Returns the Scope of a rule by field name, or None if unknown.
    pub fn scope(field_name: &str) -> Option<Scope> {
        match field_name {
            // Formatting rules — mechanically enforceable
            "keyword_case" | "keyword_abbreviation"
            | "indent_size" | "indent_style"
            | "do_placement" | "dot_colon_same_line" | "period_placement"
            | "where_placement" | "and_or_placement"
            | "first_param_same_line" | "multi_param_threshold"
            | "max_line_length" | "wrap_long_lines"
            | "blank_lines_between_sections"
            | "end_with_type" | "using_sort"
            | "comment_style"
            | "class_structure_order" | "procedure_structure_order"
                => Some(Scope::Formatting),

            // Diagnostic rules — require semantic analysis (explicit list)
            "require_symbolic_operators"
            | "require_block_labels"
            | "disallow_unnecessary_blocks"
            | "require_assign_statement"
            | "prefer_substitute"
            | "run_in_this_procedure"
            | "require_if_parentheses"
            | "variable_case" | "method_case" | "variable_type_prefix"
            | "global_prefix" | "parameter_prefix" | "buffer_naming"
            | "temp_table_prefix" | "class_naming" | "interface_prefix"
            | "file_name_casing"
            | "require_no_undo" | "require_this_object" | "static_member_ref"
            | "require_file_headers" | "require_widget_pool"
            | "temp_table_in_include" | "named_events_on_prefix"
            | "disallow_commented_code" | "variable_decl_alignment"
                => Some(Scope::Diagnostic),

            _ => None,
        }
    }
}
```

A test enumerates every `StyleGuide` struct field and asserts `scope()` returns
`Some(_)` — so if a field is added without updating the match, the test fails.

### Preset Constructors

```rust
impl StyleGuide {
    /// Shared base — rules both standards agree on.
    pub fn default_base() -> Self { ... }

    /// alextrs/oestandards profile.
    pub fn oestandards() -> Self { ... }

    /// consultingwerk/ABL-Coding-Standards profile.
    pub fn consultingwerk() -> Self { ... }

    /// Load from a TOML string for user customization.
    /// Partial TOML files work: any missing field falls back to
    /// `default_base()` via `#[serde(default)]`.
    pub fn from_toml(toml: &str) -> Result<Self, toml::de::Error> { ... }

    /// Serialize to TOML for round-tripping/inspection.
    pub fn to_toml(&self) -> Result<String, toml::ser::Error> { ... }
}

// NOTE: Default delegates to default_base() so #[serde(default)]
// fills missing TOML fields from the shared base, not from #[derive(Default)].
impl Default for StyleGuide {
    fn default() -> Self { Self::default_base() }
}
```

### CLI Interface (positional subcommands, matching oxabl_codegen)

```
cargo run -p oxabl_style preset oestandards     # Print preset as TOML
cargo run -p oxabl_style preset consultingwerk   # Print preset as TOML
cargo run -p oxabl_style validate path/to/config.toml  # Validate TOML
cargo run -p oxabl_style diff ~/.oxabl/style.toml    # Diff config against base
```

### TOML Format

```toml
# All fields at top level for ergonomic user-authoring.
# All fields are optional — unset fields fall back to default_base().
keyword_case = "Uppercase"
indent_size = 4
max_line_length = 120
variable_case = "CamelCase"
method_case = "PascalCase"
require_no_undo = true
# ...
```

## Implementation Steps

1. Create `crates/oxabl_style/Cargo.toml` with full metadata
2. Add `crates/oxabl_style` to workspace `Cargo.toml`
3. Implement `rules.rs` — all enum definitions with serde derive + Scope tag
4. Implement `style_guide.rs` — struct, `default_base()`, `from_toml()`, `to_toml()`, `scope()`
5. Implement `preset.rs` — `oestandards()`, `consultingwerk()` constructors
6. Implement `lib.rs` — re-exports
7. Implement `src/main.rs` — CLI binary with subcommands
8. Implement `tests/presets.rs` — verify preset values against reference docs;
   include a `scope()` coverage test that serializes a synthetic all-`Some`
   instance and asserts every key resolves to `Some(_)` (avoids the
   serialization-Option-gap where `None` fields get dropped from TOML
   output); include a round-trip test `default_base() → to_toml →
   from_toml == default_base()`.
9. Run `cargo check`, `cargo clippy -D warnings`, `cargo fmt --check`, `cargo test`

## Files to Create/Modify

- `Cargo.toml` (workspace) — add `crates/oxabl_style`
- `crates/oxabl_style/Cargo.toml` — new crate ([bin] oxabl-style, edition 2024, MIT, serde+toml deps, version 0.1.0)
- `crates/oxabl_style/src/lib.rs` — library entry
- `crates/oxabl_style/src/rules.rs` — rule enum definitions
- `crates/oxabl_style/src/style_guide.rs` — StyleGuide struct + constructors + scope
- `crates/oxabl_style/src/preset.rs` — preset overrides
- `crates/oxabl_style/src/main.rs` — CLI binary
- `crates/oxabl_style/tests/presets.rs` — integration tests
- `docs/design/style-guide.md` — design document
