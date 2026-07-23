use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// =============================================================================
// Enums defining the possible values for each style guide rule.
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum KeywordCase {
    #[default]
    Uppercase,
    Lowercase,
    Preserve,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum KeywordAbbreviation {
    KeepAbbreviations,
    AbbreviateEverything,
    #[default]
    AbbreviateNothing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum IndentStyle {
    #[default]
    Spaces,
    Tabs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum Placement {
    #[default]
    SameLine,
    NewLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum PeriodPlacement {
    #[default]
    SameLine,
    NewLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum SubstitutePolicy {
    Never,
    #[default]
    ThreeOrMore,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum VariableCase {
    #[default]
    Unspecified,
    CamelCase,
    LowerCase,
    SnakeCase,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum MethodCase {
    #[default]
    Unspecified,
    PascalCase,
    CamelCase,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum ParameterPrefix {
    #[default]
    None,
    IpOpIop,
    PLowercase,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum BufferNaming {
    #[default]
    Unspecified,
    BPrefix,
    RoleTable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum TempTablePrefix {
    #[default]
    Unspecified,
    TtBiPdsDs,
    TtE,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum ClassNaming {
    #[default]
    Unspecified,
    SuffixBaseClass,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum StaticMemberRef {
    #[default]
    Unspecified,
    ShortName,
    ClassName,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum CommentStyle {
    #[default]
    BlockComment,
    SlashSlash,
    Either,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum FileNameCasing {
    #[default]
    Unspecified,
    LowerCamelCase,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum AndOrPlacement {
    #[default]
    EndOfLine,
    BeginningOfLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default, JsonSchema)]
pub enum VariableDeclAlignment {
    #[default]
    None,
    Tabular,
}
