use crate::rules::*;
use serde::{Deserialize, Serialize};

/// Whether a rule can be mechanically enforced by a formatter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Scope {
    /// Formatter can enforce from tokens/AST without semantic analysis.
    Formatting,
    /// Requires semantic analysis — lint tool territory.
    Diagnostic,
}

/// Comprehensive ABL style guide holding all formatting and diagnostic rules.
///
/// The struct uses `#[serde(default)]` so partial TOML files work: any missing
/// field falls back to `Default::default()` which delegates to
/// [`StyleGuide::default_base`].
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct StyleGuide {
    // Keywords & Operators ------------------------------------------------
    pub keyword_case: KeywordCase,
    pub keyword_abbreviation: KeywordAbbreviation,
    pub require_symbolic_operators: bool,

    // Layout & Whitespace --------------------------------------------------
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

    // Block & Statement Structure ------------------------------------------
    pub end_with_type: bool,
    pub using_sort: bool,
    pub require_block_labels: bool,
    pub disallow_unnecessary_blocks: bool,
    pub require_assign_statement: bool,
    pub prefer_substitute: SubstitutePolicy,
    pub run_in_this_procedure: bool,
    pub require_if_parentheses: bool,

    // Naming Conventions ---------------------------------------------------
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

    // Required Constructs --------------------------------------------------
    pub require_no_undo: bool,
    pub require_this_object: bool,
    pub static_member_ref: StaticMemberRef,
    pub require_file_headers: bool,
    pub require_widget_pool: bool,
    pub temp_table_in_include: bool,
    pub named_events_on_prefix: bool,

    // Code Quality ---------------------------------------------------------
    pub disallow_commented_code: bool,
    pub variable_decl_alignment: VariableDeclAlignment,

    // File & Comment Structure ---------------------------------------------
    pub comment_style: CommentStyle,
    pub class_structure_order: Vec<String>,
    pub procedure_structure_order: Vec<String>,
}

impl StyleGuide {
    /// Safe, non-mangling default — the baseline a user gets with no config.
    ///
    /// The guiding principle is **preserve what the author already wrote** for
    /// anything that touches their identifiers, keywords, or intent, and only
    /// fix pure layout (whitespace, indentation, line length). So keyword
    /// casing and abbreviations are [`KeywordCase::Preserve`] /
    /// [`KeywordAbbreviation::KeepAbbreviations`], comment style is
    /// [`CommentStyle::Either`], and opinionated "required construct" rules
    /// (`require_no_undo`, `require_file_headers`, `end_with_type`,
    /// `variable_type_prefix`, `blank_lines_between_sections`) default to
    /// off/false. A first pass over an existing codebase must not rewrite
    /// keywords or inject constructs — it just tidies layout.
    ///
    /// The named presets ([`StyleGuide::oestandards`],
    /// [`StyleGuide::consultingwerk`]) build on [`StyleGuide::strict_base`],
    /// which layers the opinionated values both standards agree on back on
    /// top of this safe baseline.
    pub fn default_base() -> Self {
        Self {
            // Keywords & Operators — preserve the author's keywords as written
            keyword_case: KeywordCase::Preserve,
            keyword_abbreviation: KeywordAbbreviation::KeepAbbreviations,
            require_symbolic_operators: false,

            // Layout & Whitespace — the only things a safe first pass fixes
            indent_size: 4,
            indent_style: IndentStyle::Spaces,
            do_placement: Placement::SameLine,
            dot_colon_same_line: false,
            period_placement: PeriodPlacement::SameLine,
            where_placement: Placement::NewLine,
            and_or_placement: AndOrPlacement::EndOfLine,
            first_param_same_line: true,
            multi_param_threshold: 3,
            max_line_length: 120,
            wrap_long_lines: true,
            blank_lines_between_sections: false,

            // Block & Statement Structure
            end_with_type: false,
            using_sort: false,
            require_block_labels: false,
            disallow_unnecessary_blocks: false,
            require_assign_statement: false,
            prefer_substitute: SubstitutePolicy::ThreeOrMore,
            run_in_this_procedure: false,
            require_if_parentheses: false,

            // Naming Conventions — all unspecified: never rename the author's code
            variable_case: VariableCase::Unspecified,
            method_case: MethodCase::Unspecified,
            variable_type_prefix: false,
            global_prefix: None,
            parameter_prefix: ParameterPrefix::None,
            buffer_naming: BufferNaming::Unspecified,
            temp_table_prefix: TempTablePrefix::Unspecified,
            class_naming: ClassNaming::Unspecified,
            interface_prefix: None,
            file_name_casing: FileNameCasing::Unspecified,

            // Required Constructs — off by default: don't inject what isn't there
            require_no_undo: false,
            require_this_object: false,
            static_member_ref: StaticMemberRef::Unspecified,
            require_file_headers: false,
            require_widget_pool: false,
            temp_table_in_include: false,
            named_events_on_prefix: false,

            // Code Quality
            disallow_commented_code: false,
            variable_decl_alignment: VariableDeclAlignment::None,

            // File & Comment Structure — accept whatever comment style is present
            comment_style: CommentStyle::Either,
            class_structure_order: Vec::new(),
            procedure_structure_order: Vec::new(),
        }
    }

    /// Opinionated baseline shared by the named presets.
    ///
    /// Layers the values **both** the oestandards and consultingwerk standards
    /// agree on (uppercase unabbreviated keywords, `END <type>`, `NO-UNDO`,
    /// file headers, type-prefixed variables, block comments, section spacing)
    /// on top of [`StyleGuide::default_base`]. Presets extend this, not the
    /// safe default, so they keep their strictness while the user-facing
    /// default stays non-mangling.
    pub fn strict_base() -> Self {
        Self {
            keyword_case: KeywordCase::Uppercase,
            keyword_abbreviation: KeywordAbbreviation::AbbreviateNothing,
            end_with_type: true,
            blank_lines_between_sections: true,
            variable_type_prefix: true,
            require_no_undo: true,
            require_file_headers: true,
            comment_style: CommentStyle::BlockComment,
            ..Self::default_base()
        }
    }

    /// alextrs/oestandards profile.
    pub fn oestandards() -> Self {
        Self {
            // oestandards overrides
            dot_colon_same_line: true,
            require_block_labels: true,
            disallow_unnecessary_blocks: true,
            require_assign_statement: true,
            prefer_substitute: SubstitutePolicy::Always,
            run_in_this_procedure: true,
            require_if_parentheses: true,
            require_symbolic_operators: true,
            variable_case: VariableCase::CamelCase,
            method_case: MethodCase::Unspecified,
            global_prefix: Some('g'),
            parameter_prefix: ParameterPrefix::IpOpIop,
            buffer_naming: BufferNaming::BPrefix,
            temp_table_prefix: TempTablePrefix::TtBiPdsDs,
            disallow_commented_code: true,
            require_this_object: false,
            static_member_ref: StaticMemberRef::Unspecified,
            file_name_casing: FileNameCasing::Unspecified,
            interface_prefix: None,
            using_sort: false,
            require_widget_pool: false,
            temp_table_in_include: false,
            named_events_on_prefix: false,
            class_structure_order: vec![
                "Class Description".into(),
                "USING Statements".into(),
                "Routine Level".into(),
                "Preprocessor Definitions".into(),
                "Defines".into(),
                "Define Variables".into(),
                "Define Events".into(),
                "Define Properties".into(),
                "Constructors".into(),
                "Methods".into(),
                "Destructor".into(),
            ],
            procedure_structure_order: vec![
                "Procedure Description".into(),
                "USING Statements".into(),
                "Routine Level".into(),
                "Preprocessor Definitions".into(),
                "Define Input-Output Parameters".into(),
                "Includes".into(),
                "Define Local Variables".into(),
                "Define Functions".into(),
                "Main Block".into(),
                "Define Procedures".into(),
            ],
            ..Self::strict_base()
        }
    }

    /// consultingwerk/ABL-Coding-Standards profile.
    pub fn consultingwerk() -> Self {
        Self {
            // consultingwerk overrides
            variable_case: VariableCase::LowerCase,
            method_case: MethodCase::PascalCase,
            parameter_prefix: ParameterPrefix::PLowercase,
            buffer_naming: BufferNaming::RoleTable,
            temp_table_prefix: TempTablePrefix::TtE,
            class_naming: ClassNaming::SuffixBaseClass,
            interface_prefix: Some('I'),
            using_sort: true,
            file_name_casing: FileNameCasing::LowerCamelCase,
            require_this_object: true,
            static_member_ref: StaticMemberRef::ClassName,
            require_widget_pool: true,
            temp_table_in_include: true,
            named_events_on_prefix: true,
            variable_decl_alignment: VariableDeclAlignment::Tabular,
            ..Self::strict_base()
        }
    }

    /// Returns the [`Scope`] of a rule by field name, or `None` if unknown.
    pub fn scope(field_name: &str) -> Option<Scope> {
        match field_name {
            // Formatting rules — mechanically enforceable
            "keyword_case"
            | "keyword_abbreviation"
            | "indent_size"
            | "indent_style"
            | "do_placement"
            | "dot_colon_same_line"
            | "period_placement"
            | "where_placement"
            | "and_or_placement"
            | "first_param_same_line"
            | "multi_param_threshold"
            | "max_line_length"
            | "wrap_long_lines"
            | "blank_lines_between_sections"
            | "end_with_type"
            | "using_sort"
            | "comment_style"
            | "class_structure_order"
            | "procedure_structure_order" => Some(Scope::Formatting),

            // Diagnostic rules — require semantic analysis
            "require_symbolic_operators"
            | "require_block_labels"
            | "disallow_unnecessary_blocks"
            | "require_assign_statement"
            | "prefer_substitute"
            | "run_in_this_procedure"
            | "require_if_parentheses"
            | "variable_case"
            | "method_case"
            | "variable_type_prefix"
            | "global_prefix"
            | "parameter_prefix"
            | "buffer_naming"
            | "temp_table_prefix"
            | "class_naming"
            | "interface_prefix"
            | "file_name_casing"
            | "require_no_undo"
            | "require_this_object"
            | "static_member_ref"
            | "require_file_headers"
            | "require_widget_pool"
            | "temp_table_in_include"
            | "named_events_on_prefix"
            | "disallow_commented_code"
            | "variable_decl_alignment" => Some(Scope::Diagnostic),

            _ => None,
        }
    }

    /// Load a style guide from a TOML string.
    ///
    /// Partial TOML files are supported: any field not present in the TOML
    /// falls back to [`StyleGuide::default_base`] via `#[serde(default)]` +
    /// `Default`.
    pub fn from_toml(toml_str: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(toml_str)
    }

    /// Serialize this style guide to a TOML string.
    pub fn to_toml(&self) -> Result<String, toml::ser::Error> {
        toml::to_string_pretty(self)
    }
}

impl Default for StyleGuide {
    fn default() -> Self {
        Self::default_base()
    }
}
