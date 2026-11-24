//! What the AVM session brings to a compile, and its identity.

use std::collections::BTreeMap;

use super::digest::FileDigest;
use super::encode::{ENVIRONMENT_CONTEXT, Encoder, derive};
use super::parameters::Parameter;
use super::schema_image::SchemaImage;

/// The `ENV` field names, in the order of the protocol document.
pub const FIELD_NAMES: [&str; 19] = [
    "program",
    "dlc",
    "dlc_version",
    "proversion",
    "opsys",
    "window_system",
    "batch_mode",
    "process_architecture",
    "cpinternal",
    "cpstream",
    "cpcoll",
    "cpcase",
    "date_format",
    "year_offset",
    "numeric_decimal_point",
    "numeric_separator",
    "startup_parameters",
    "propath",
    "cwd",
];

/// One `DB` line.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DatabaseInfo {
    pub number: u32,
    pub ldb: String,
    pub dbtype: String,
    pub aliases: Vec<String>,
}

/// The fields of the `ENV` and `DB` lines.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Facts {
    fields: BTreeMap<&'static str, String>,
    pub databases: Vec<DatabaseInfo>,
}

impl Facts {
    /// Checks that every field is present once and that nothing else is.
    pub fn new(
        lines: Vec<(String, String)>,
        databases: Vec<DatabaseInfo>,
    ) -> Result<Facts, String> {
        let mut fields: BTreeMap<&'static str, String> = BTreeMap::new();
        for (name, value) in lines {
            let Some(known) = FIELD_NAMES.iter().find(|known| **known == name) else {
                return Err(format!("unknown environment field `{name}`"));
            };
            if fields.insert(known, value).is_some() {
                return Err(format!("environment field `{name}` appears twice"));
            }
        }
        if let Some(missing) = FIELD_NAMES.iter().find(|name| !fields.contains_key(**name)) {
            return Err(format!("environment field `{missing}` is missing"));
        }
        for (position, database) in databases.iter().enumerate() {
            if database.number as usize != position + 1 {
                return Err(format!(
                    "database `{}` has number {}, expected {}",
                    database.ldb,
                    database.number,
                    position + 1
                ));
            }
        }
        Ok(Facts { fields, databases })
    }

    pub fn get(&self, name: &str) -> &str {
        self.fields
            .get(name)
            .map(String::as_str)
            .unwrap_or_default()
    }

    /// Every field in protocol order, with the value as reported.
    pub fn fields(&self) -> impl Iterator<Item = (&'static str, &str)> {
        FIELD_NAMES.iter().map(|name| (*name, self.get(name)))
    }
}

/// A library entry of the worker's own startup PROPATH, with its digest.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Library {
    pub entry: String,
    pub digest: FileDigest,
}

/// What a worker reports about a database cheaply: the database's GUID and
/// the stamp of its last schema change, `?` when unavailable. The stamp has a
/// resolution of one second and not every schema change moves it, so it only
/// lets workers check that they see the same database; it never stands in
/// for the schema image.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fingerprint {
    pub ldb: String,
    pub guid: String,
    pub stamp: String,
}

/// A connected database with its fingerprint and schema image.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Database {
    pub info: DatabaseInfo,
    pub fingerprint: Fingerprint,
    pub schema: SchemaImage,
}

/// Everything one worker reports in its handshake except the schema images.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Profile {
    pub facts: Facts,
    pub parameters: Vec<Parameter>,
    pub libraries: Vec<Library>,
    /// Why keys cannot rest on this environment, for example
    /// `unhashable-parameter: -assemblies`.
    pub unknown: Vec<String>,
    /// One fingerprint per database, in connection order.
    pub fingerprints: Vec<Fingerprint>,
}

fn database_key(info: &DatabaseInfo) -> (String, &str, Vec<String>) {
    let mut aliases: Vec<String> = info
        .aliases
        .iter()
        .map(|a| a.to_ascii_lowercase())
        .collect();
    aliases.sort();
    (info.ldb.to_ascii_lowercase(), info.dbtype.as_str(), aliases)
}

impl Profile {
    fn dlc_version(&self) -> &str {
        self.facts.get("dlc_version").trim_end()
    }

    /// The values that all workers of a pool must share, as `(name, value)`
    /// pairs a person can read.
    fn comparable(&self) -> Vec<(String, String)> {
        let mut pairs: Vec<(String, String)> =
            vec![("dlc_version".into(), self.dlc_version().into())];
        for (name, value) in self.facts.fields() {
            let relevant = !matches!(
                name,
                "program" | "dlc" | "dlc_version" | "startup_parameters" | "propath" | "cwd"
            );
            if relevant {
                pairs.push((name.into(), value.into()));
            }
        }
        pairs.push((
            "parameters".into(),
            self.parameters
                .iter()
                .map(|p| format!("{} {}", p.name, p.value).trim_end().to_string())
                .collect::<Vec<_>>()
                .join(", "),
        ));
        pairs.push((
            "databases".into(),
            self.facts
                .databases
                .iter()
                .map(|d| {
                    let (ldb, dbtype, aliases) = database_key(d);
                    format!("{ldb}:{dbtype}:{}", aliases.join("+"))
                })
                .collect::<Vec<_>>()
                .join(", "),
        ));
        for fingerprint in &self.fingerprints {
            pairs.push((
                format!("fingerprint {}", fingerprint.ldb.to_ascii_lowercase()),
                format!("{} {}", fingerprint.guid, fingerprint.stamp),
            ));
        }
        for library in &self.libraries {
            pairs.push((
                format!("library {}", library.entry),
                library.digest.word().into(),
            ));
        }
        pairs.push(("unknown".into(), self.unknown.join(", ")));
        pairs
    }

    /// The fields in which two profiles differ, each as
    /// `name: <this> vs <other>`. Empty when they agree.
    pub fn differences(&self, other: &Profile) -> Vec<String> {
        let ours: BTreeMap<_, _> = self.comparable().into_iter().collect();
        let theirs: BTreeMap<_, _> = other.comparable().into_iter().collect();
        let mut names: Vec<&String> = ours.keys().chain(theirs.keys()).collect();
        names.sort();
        names.dedup();
        names
            .into_iter()
            .filter_map(|name| {
                let a = ours.get(name).map(String::as_str).unwrap_or("(absent)");
                let b = theirs.get(name).map(String::as_str).unwrap_or("(absent)");
                (a != b).then(|| format!("{name}: {a} vs {b}"))
            })
            .collect()
    }
}

/// The environment of a pool, canonicalised, with the schema images.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    pub facts: Facts,
    pub parameters: Vec<Parameter>,
    pub databases: Vec<Database>,
    pub libraries: Vec<Library>,
    pub unknown: Vec<String>,
}

impl Environment {
    /// Joins a profile with one schema image per database, in connection order.
    pub fn assemble(profile: Profile, schemas: Vec<SchemaImage>) -> Environment {
        let Profile {
            facts,
            parameters,
            libraries,
            unknown,
            fingerprints,
        } = profile;
        let databases = facts
            .databases
            .iter()
            .cloned()
            .zip(fingerprints)
            .zip(schemas)
            .map(|((info, fingerprint), schema)| Database {
                info,
                fingerprint,
                schema,
            })
            .collect();
        Environment {
            facts,
            parameters,
            databases,
            libraries,
            unknown,
        }
    }

    /// The hash of everything the compile reads from the session.
    pub fn identity(&self) -> blake3::Hash {
        let facts = &self.facts;
        let mut e = Encoder::new();
        e.section("openedge")
            .str(facts.get("dlc_version").trim_end())
            .str(facts.get("proversion"));
        e.section("platform")
            .str(facts.get("opsys"))
            .str(facts.get("window_system"))
            .str(facts.get("batch_mode"))
            .str(facts.get("process_architecture"));
        e.section("session");
        for name in [
            "cpinternal",
            "cpstream",
            "cpcoll",
            "cpcase",
            "date_format",
            "year_offset",
            "numeric_decimal_point",
            "numeric_separator",
        ] {
            e.str(facts.get(name));
        }
        e.section("parameters").u64(self.parameters.len() as u64);
        for parameter in &self.parameters {
            e.str(&parameter.name).str(&parameter.value);
        }
        e.section("databases").u64(self.databases.len() as u64);
        for database in &self.databases {
            let (ldb, dbtype, aliases) = database_key(&database.info);
            e.str(&ldb).str(dbtype).u64(aliases.len() as u64);
            for alias in &aliases {
                e.str(alias);
            }
            e.hash(&database.schema.identity());
        }
        derive(ENVIRONMENT_CONTEXT, &e.finish())
    }
}
