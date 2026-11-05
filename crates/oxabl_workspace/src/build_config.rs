//! The `[build]` tables of `oxabl.toml`: where `oxabl build` keeps its state and
//! outputs, how it compiles, and how it starts workers.
//!
//! Every table denies unknown keys, so a misspelling is an error and never a
//! silent default.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::Duration;

use schemars::JsonSchema;
use schemars::r#gen::SchemaGenerator;
use schemars::schema::{InstanceType, Metadata, Schema, SchemaObject};
use serde::{Deserialize, Deserializer};

/// The `[build]` table.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct BuildConfig {
    /// R-code output directory, under the workspace root. Required to compile.
    pub output: PathBuf,

    /// State directory, under the workspace root. Default `.oxabl/build`.
    pub state: PathBuf,

    /// Host directory that holds `state.db` alone, for a root on a network file
    /// system. Empty keeps it in `state`.
    pub store: PathBuf,

    /// Glob patterns, relative to the root, of files that are not compile
    /// units. `**` crosses directories; a pattern that matches a directory
    /// excludes every file under it.
    pub exclude: Vec<String>,

    /// Procedure libraries outside the root and outside `$DLC`: a logical name
    /// and the path of the library as the worker sees it.
    pub libraries: BTreeMap<String, String>,

    /// Class name prefix to the library entries that supply it.
    pub external_packages: BTreeMap<String, Vec<String>>,

    /// Directories for the optional compiler outputs.
    pub outputs: BuildOutputs,

    /// Options of the `COMPILE` statement.
    pub compile: BuildCompile,

    /// How workers start.
    pub workers: BuildWorkers,

    /// Verification of what a compile read.
    pub verify: BuildVerify,

    /// The compile caches.
    pub cache: BuildCache,
}

impl Default for BuildConfig {
    fn default() -> Self {
        BuildConfig {
            output: PathBuf::new(),
            state: PathBuf::from(".oxabl/build"),
            store: PathBuf::new(),
            exclude: Vec::new(),
            libraries: BTreeMap::new(),
            external_packages: BTreeMap::from([
                ("OpenEdge.".to_string(), vec!["$DLC".to_string()]),
                ("Progress.".to_string(), vec!["$DLC".to_string()]),
            ]),
            outputs: BuildOutputs::default(),
            compile: BuildCompile::default(),
            workers: BuildWorkers::default(),
            verify: BuildVerify::default(),
            cache: BuildCache::default(),
        }
    }
}

impl BuildConfig {
    /// A directory setting, or `None` when it is empty.
    pub fn directory(path: &Path) -> Option<&Path> {
        (!path.as_os_str().is_empty()).then_some(path)
    }
}

/// The form of the cross-reference output.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(rename_all = "lowercase")]
pub enum XrefFormat {
    /// `XREF-XML`.
    #[default]
    Xml,
    /// `XREF`.
    Text,
}

/// The `[build.outputs]` table. An empty directory turns the output off.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct BuildOutputs {
    pub xref: PathBuf,
    pub xref_format: XrefFormat,
    pub debug_list: PathBuf,
    pub listing: PathBuf,
}

/// The `[build.compile]` table.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct BuildCompile {
    pub min_size: bool,
    pub generate_md5: bool,
    pub stream_io: bool,
    pub v6frame: bool,
    pub attr_space: bool,
    pub languages: Vec<String>,
    pub options: Vec<String>,
}

/// The `[build.workers]` table.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct BuildWorkers {
    /// The argument list that starts one worker. Empty means unset.
    pub command: Vec<String>,

    /// The workspace root as the worker sees it. Default: the host root.
    pub root: Option<String>,

    /// Number of workers. Default: the CPU count, at most 8.
    pub count: Option<u32>,

    /// Time for one handshake. Default `60s`.
    pub start_timeout: TimeSpan,

    /// Time for one unit. Default `10m`.
    pub unit_timeout: TimeSpan,

    /// Time for busy workers after a cancel. Default `30s`.
    pub cancel_grace: TimeSpan,

    /// Extra environment variables for the command. Never a secret.
    pub env: BTreeMap<String, String>,
}

impl Default for BuildWorkers {
    fn default() -> Self {
        BuildWorkers {
            command: Vec::new(),
            root: None,
            count: None,
            start_timeout: TimeSpan(Duration::from_secs(60)),
            unit_timeout: TimeSpan(Duration::from_secs(10 * 60)),
            cancel_grace: TimeSpan(Duration::from_secs(30)),
            env: BTreeMap::new(),
        }
    }
}

/// The `[build.verify]` table.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct BuildVerify {
    /// Read the compiler's own list of opened files and check it. Without it no
    /// cache write happens.
    pub reads: bool,

    /// Units held for the schema check that release them to a cache. Default
    /// 500.
    pub batch_units: u32,

    /// Longest time a unit waits for the schema check. Default `60s`.
    pub batch_interval: TimeSpan,
}

impl Default for BuildVerify {
    fn default() -> Self {
        BuildVerify {
            reads: true,
            batch_units: 500,
            batch_interval: TimeSpan(Duration::from_secs(60)),
        }
    }
}

/// The `[build.cache]` table.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(default, deny_unknown_fields)]
pub struct BuildCache {
    /// Both caches. Default `true`.
    pub enabled: bool,

    /// Use the remote cache. Default: whether `.oxabl/cache.toml` exists.
    pub remote: Option<bool>,

    /// A fetch with no progress for this long ends. Default `30s`.
    pub idle_timeout: TimeSpan,

    /// Host path of the shared local cache. Empty means the user cache
    /// directory.
    pub local: PathBuf,

    /// The per-job local cache directory of the `ci` and `read-only` modes.
    pub local_job_dir: PathBuf,

    /// Eviction limit of the local cache. Default `20GiB`.
    pub local_max_size: Size,
}

impl Default for BuildCache {
    fn default() -> Self {
        BuildCache {
            enabled: true,
            remote: None,
            idle_timeout: TimeSpan(Duration::from_secs(30)),
            local: PathBuf::new(),
            local_job_dir: PathBuf::new(),
            local_max_size: Size(20 << 30),
        }
    }
}

/// A duration written as an integer and a unit: `"90s"`, `"10m"`, `"1h"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimeSpan(pub Duration);

/// A size written as an integer and a unit: `"512MiB"`, `"20GiB"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Size(pub u64);

/// Split `text` into its leading integer and the unit after it.
fn number_and_unit(text: &str) -> Result<(u64, &str), String> {
    let split = text
        .find(|c: char| !c.is_ascii_digit())
        .ok_or_else(|| format!("`{text}` has no unit"))?;
    let number = text[..split]
        .parse()
        .map_err(|_| format!("`{text}` does not start with an integer"))?;
    Ok((number, text[split..].trim()))
}

impl TimeSpan {
    /// Parse `"90s"`, `"10m"` or `"1h"`.
    pub fn parse(text: &str) -> Result<Self, String> {
        let (number, unit) = number_and_unit(text.trim())?;
        let seconds = match unit {
            "s" => 1,
            "m" => 60,
            "h" => 3600,
            _ => return Err(format!("`{text}`: the unit must be s, m or h")),
        };
        number
            .checked_mul(seconds)
            .map(|seconds| TimeSpan(Duration::from_secs(seconds)))
            .ok_or_else(|| format!("`{text}` is too large"))
    }
}

impl Size {
    /// Parse `"512MiB"`, `"20GiB"` (`KiB`, `MiB`, `GiB`, `TiB`).
    pub fn parse(text: &str) -> Result<Self, String> {
        let (number, unit) = number_and_unit(text.trim())?;
        let shift = match unit {
            "KiB" => 10,
            "MiB" => 20,
            "GiB" => 30,
            "TiB" => 40,
            _ => return Err(format!("`{text}`: the unit must be KiB, MiB, GiB or TiB")),
        };
        number
            .checked_mul(1 << shift)
            .map(Size)
            .ok_or_else(|| format!("`{text}` is too large"))
    }
}

impl<'de> Deserialize<'de> for TimeSpan {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let text = String::deserialize(deserializer)?;
        TimeSpan::parse(&text).map_err(serde::de::Error::custom)
    }
}

impl<'de> Deserialize<'de> for Size {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let text = String::deserialize(deserializer)?;
        Size::parse(&text).map_err(serde::de::Error::custom)
    }
}

fn text_schema(description: &str, example: &str) -> Schema {
    SchemaObject {
        metadata: Some(Box::new(Metadata {
            description: Some(description.to_string()),
            examples: vec![serde_json::Value::String(example.to_string())],
            ..Default::default()
        })),
        instance_type: Some(InstanceType::String.into()),
        ..Default::default()
    }
    .into()
}

impl JsonSchema for TimeSpan {
    fn schema_name() -> String {
        "TimeSpan".to_string()
    }

    fn json_schema(_: &mut SchemaGenerator) -> Schema {
        text_schema("An integer and a unit: `s`, `m` or `h`.", "90s")
    }
}

impl JsonSchema for Size {
    fn schema_name() -> String {
        "Size".to_string()
    }

    fn json_schema(_: &mut SchemaGenerator) -> Schema {
        text_schema(
            "An integer and a unit: `KiB`, `MiB`, `GiB` or `TiB`.",
            "20GiB",
        )
    }
}
