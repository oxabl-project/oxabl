# ABL Style Guide

The `oxabl_style` crate provides a typed, configurable ABL style guide that
serves as the single source of truth for both the future oxabl formatter and
the existing `oxabl_lint` linter.

## Rules Model

Every style rule lives in the [`StyleGuide`] struct. Each rule is tagged with
one of two [`Scope`] values:

- **`Formatting`** — can be mechanically enforced from the token stream or
  AST. Changes to formatting never alter program semantics.
- **`Diagnostic`** — requires semantic analysis. A lint tool must check these.
  These rules involve renaming, inserting/removing constructs, or reasoning
  about runtime behavior.

The `StyleGuide` struct holds ALL rules regardless of scope. Consumers
filter by scope at use sites.

## Named Presets

Two reference standards ship as first-class profiles:

| Preset | Source |
|---|---|
| `StyleGuide::oestandards()` | alextrs/oestandards |
| `StyleGuide::consultingwerk()` | consultingwerk/ABL-Coding-Standards |

Both inherit from `StyleGuide::default_base()` which captures rules the
two standards agree on.

## Rule Catalog

### Formatting Rules (19 rules)

Rules a formatter can mechanically enforce.

| Rule | oestandards | consultingwerk |
|---|---|---|
| `keyword_case` | Uppercase | Uppercase |
| `keyword_abbreviation` | AbbreviateNothing | AbbreviateNothing |
| `indent_size` | 4 | 4 |
| `indent_style` | Spaces | Spaces |
| `do_placement` | SameLine | SameLine |
| `dot_colon_same_line` | true | false |
| `period_placement` | SameLine | SameLine |
| `where_placement` | NextLine | NextLine |
| `and_or_placement` | EndOfLine | EndOfLine |
| `first_param_same_line` | true | true |
| `multi_param_threshold` | 3 | 3 |
| `max_line_length` | 120 | 120 |
| `wrap_long_lines` | true | true |
| `blank_lines_between_sections` | true | true |
| `end_with_type` | true | true |
| `using_sort` | false | true |
| `comment_style` | BlockComment | BlockComment |
| `class_structure_order` | Specified | Unspecified |
| `procedure_structure_order` | Specified | Unspecified |

### Diagnostic Rules (26 rules)

Rules requiring semantic analysis — lint tool territory.

| Rule | oestandards | consultingwerk |
|---|---|---|
| `require_symbolic_operators` | true | false |
| `require_block_labels` | true | false |
| `disallow_unnecessary_blocks` | true | false |
| `require_assign_statement` | true | false |
| `prefer_substitute` | Always | ThreeOrMore |
| `run_in_this_procedure` | true | false |
| `require_if_parentheses` | true | false |
| `variable_case` | CamelCase | LowerCase |
| `method_case` | Unspecified | PascalCase |
| `variable_type_prefix` | true | true |
| `global_prefix` | Some('g') | None |
| `parameter_prefix` | IpOpIop | PLowercase |
| `buffer_naming` | BPrefix | RoleTable |
| `temp_table_prefix` | TtBiPdsDs | TtE |
| `class_naming` | Unspecified | SuffixBaseClass |
| `interface_prefix` | None | Some('I') |
| `file_name_casing` | Unspecified | LowerCamelCase |
| `require_no_undo` | true | true |
| `require_this_object` | false | true |
| `static_member_ref` | Unspecified | ClassName |
| `require_file_headers` | true | true |
| `require_widget_pool` | false | true |
| `temp_table_in_include` | false | true |
| `named_events_on_prefix` | false | true |
| `disallow_commented_code` | true | false |
| `variable_decl_alignment` | None | Tabular |

### Data-Access & Performance Rules (not in StyleGuide)

These are pure semantic conventions with no layout component. They map to
`oxabl_lint` rule configurations in a follow-up.

| Rule | oestandards | consultingwerk |
|---|---|---|
| prefer_no_share_lock | true | true |
| prefer_can_find | true | false |
| prefer_rowid | true | false |
| require_table_name_qualification | true | false |
| prefer_for_first_last | true | false |
| require_define_buffer | true | false |
| require_by_reference | true | false |
| allow_like_on_db | false | false |
| prefer_forward_slash | true | true |
| cleanup_in_finally | true | true |
| require_catch_throw | true | true |
| error_handling_mode | BlockLevel | Either |
| delete_dynamic_objects | true | true |
| memptr_deallocate | true | true |

## API

```rust
use oxabl_style::{StyleGuide, Scope, KeywordCase};

// Presets
let oes = StyleGuide::oestandards();
let cw = StyleGuide::consultingwerk();

// TOML round-trip
let toml = oes.to_toml()?;
let parsed = StyleGuide::from_toml(&toml)?;

// Partial TOML overrides fall back to default_base()
let partial = r#"keyword_case = "Lowercase""#;
let guide = StyleGuide::from_toml(partial)?;
assert_eq!(guide.keyword_case, KeywordCase::Lowercase);
assert_eq!(guide.indent_size, 4); // from default_base()

// Scope lookup
assert_eq!(StyleGuide::scope("keyword_case"), Some(Scope::Formatting));
assert_eq!(StyleGuide::scope("variable_case"), Some(Scope::Diagnostic));
assert_eq!(StyleGuide::scope("nonexistent"), None);
```

## CLI

```
$ oxabl-style preset oestandards     # Print oestandards preset as TOML
$ oxabl-style preset consultingwerk  # Print consultingwerk preset as TOML
$ oxabl-style validate path/to/custom.toml  # Validate TOML config
$ oxabl-style diff path/to/custom.toml      # Diff against default_base()
```
