//! The handshake of one worker: environment, schema images and hashes.

use std::time::Instant;

use oxabl_workspace::PreprocessorConfig;

use super::codec::Codec;
use super::error::WorkerError;
use super::frame::{Reply, Request};
use super::session::Session;
use crate::key::predefined::{self, Disagreement};
use crate::key::{DatabaseInfo, Facts, FileDigest, Library, Parameter, Profile, parameters};

/// What the handshake checks the worker against.
pub struct Context<'a> {
    /// The workspace root as the worker sees it, without a trailing `/`.
    pub worker_root: &'a str,
    pub preprocessor: &'a PreprocessorConfig,
    /// The logical names the `[workspace.schema]` files stand for.
    pub schema_names: &'a [String],
}

/// Why a handshake failed.
#[derive(Debug, PartialEq, Eq)]
pub enum Failure {
    /// The worker is fine but does not match the configuration.
    Config(String),
    Worker(WorkerError),
}

impl From<WorkerError> for Failure {
    fn from(error: WorkerError) -> Failure {
        Failure::Worker(error)
    }
}

fn protocol(message: impl Into<String>) -> Failure {
    Failure::Worker(WorkerError::Protocol(message.into()))
}

/// The reply to `HELLO` must echo oxabl's version and token.
fn expect_hello(session: &mut Session, token: &str, deadline: Instant) -> Result<(), Failure> {
    match session.recv(deadline)? {
        Reply::Hello {
            version,
            token: echoed,
        } => {
            if version != "1" {
                return Err(protocol(format!(
                    "the worker speaks protocol version {version}, oxabl speaks 1"
                )));
            }
            if echoed != token {
                return Err(protocol("the worker echoed a different session token"));
            }
            Ok(())
        }
        other => Err(protocol(format!("expected HELLO, got {other:?}"))),
    }
}

/// One `DB` line, with text fields still as bytes.
struct RawDatabase {
    number: String,
    ldb: Vec<u8>,
    dbtype: Vec<u8>,
    aliases: Vec<u8>,
}

/// The raw `ENV` and `DB` lines, up to `ENV-END`.
struct RawLines {
    env: Vec<(String, Vec<u8>)>,
    databases: Vec<RawDatabase>,
}

fn read_environment(session: &mut Session, deadline: Instant) -> Result<RawLines, Failure> {
    let mut raw = RawLines {
        env: Vec::new(),
        databases: Vec::new(),
    };
    loop {
        match session.recv(deadline)? {
            Reply::Env { name, value } => raw.env.push((name, value)),
            Reply::Db {
                number,
                ldb,
                dbtype,
                aliases,
            } => raw.databases.push(RawDatabase {
                number,
                ldb,
                dbtype,
                aliases,
            }),
            Reply::EnvEnd { count } => {
                let seen = raw.env.len() + raw.databases.len();
                if count != seen.to_string() {
                    return Err(protocol(format!(
                        "ENV-END says {count} lines, oxabl read {seen}"
                    )));
                }
                return Ok(raw);
            }
            other => return Err(protocol(format!("unexpected {other:?} in the environment"))),
        }
    }
}

fn facts(session: &mut Session, raw: RawLines) -> Result<Facts, Failure> {
    let cpinternal = raw
        .env
        .iter()
        .find(|(name, _)| name == "cpinternal")
        .map(|(_, value)| String::from_utf8_lossy(value).into_owned())
        .ok_or_else(|| protocol("environment field `cpinternal` is missing"))?;
    let codec = Codec::for_name(&cpinternal).map_err(protocol)?;
    session.codec = codec;
    let decode = |bytes: &[u8]| codec.decode(bytes).map_err(protocol);
    let env = raw
        .env
        .iter()
        .map(|(name, value)| Ok((name.clone(), decode(value)?)))
        .collect::<Result<Vec<_>, Failure>>()?;
    let databases = raw
        .databases
        .iter()
        .map(|database| {
            let number = &database.number;
            Ok(DatabaseInfo {
                number: number
                    .parse()
                    .map_err(|_| protocol(format!("bad database number `{number}`")))?,
                ldb: decode(&database.ldb)?,
                dbtype: decode(&database.dbtype)?,
                aliases: decode(&database.aliases)?
                    .split(',')
                    .filter(|alias| !alias.is_empty())
                    .map(str::to_string)
                    .collect(),
            })
        })
        .collect::<Result<Vec<_>, Failure>>()?;
    Facts::new(env, databases).map_err(protocol)
}

fn check_facts(facts: &Facts, context: &Context<'_>) -> Result<(), Failure> {
    if facts.get("program") != "1" {
        return Err(protocol(format!(
            "the worker program is version {}, oxabl needs 1",
            facts.get("program")
        )));
    }
    let cwd = facts.get("cwd");
    let cwd = if cwd == "/" {
        cwd
    } else {
        cwd.trim_end_matches('/')
    };
    if cwd != context.worker_root {
        return Err(Failure::Config(format!(
            "the worker runs in `{cwd}`, not in the worker root `{}`; start the command in the root (for a container, `-w {{root}}`)",
            context.worker_root
        )));
    }
    match predefined::check(facts, context.preprocessor, context.schema_names) {
        Ok(()) => Ok(()),
        Err(Disagreement::ProversionUnset) => Err(Failure::Config(
            "[workspace.preprocessor].proversion is not set; `oxabl build` needs the OpenEdge version the workers compile with".into(),
        )),
        Err(Disagreement::Differences(list)) => Err(Failure::Config(format!(
            "the analysis and the worker disagree on predefined names:\n  {}",
            list.join("\n  ")
        ))),
    }
}

/// Canonical startup parameters. A value that names a file counts by its
/// digest. A directory makes the environment unknown.
fn canonical_parameters(
    session: &mut Session,
    raw: &str,
    deadline: Instant,
    unknown: &mut Vec<String>,
) -> Result<Vec<Parameter>, Failure> {
    let mut parameters = parameters::parse(raw);
    for parameter in &mut parameters {
        if parameter.value.is_empty() || session.codec.encode(&parameter.value).is_err() {
            continue;
        }
        match session.hash(&parameter.value, deadline)? {
            FileDigest::File { sha256, .. } => parameter.value = format!("sha256:{sha256}"),
            FileDigest::Dir => unknown.push(format!("unhashable-parameter: {}", parameter.name)),
            FileDigest::Absent => {}
        }
    }
    Ok(parameters)
}

/// The library entries of the worker's own startup PROPATH that sit under
/// `$DLC`, hashed; the others are named in `warnings` and dropped.
fn dlc_libraries(
    session: &mut Session,
    facts: &Facts,
    deadline: Instant,
    warnings: &mut Vec<String>,
) -> Result<Vec<Library>, Failure> {
    let dlc = facts.get("dlc").trim_end_matches('/');
    let mut libraries = Vec::new();
    let mut outside = Vec::new();
    for entry in facts
        .get("propath")
        .split(',')
        .filter(|entry| !entry.is_empty())
    {
        let under = if dlc.is_empty() {
            None
        } else if entry == dlc {
            Some("$DLC".to_string())
        } else {
            entry
                .strip_prefix(&format!("{dlc}/"))
                .map(|rest| format!("$DLC/{rest}"))
        };
        match under {
            Some(entry) => {
                let digest = session.hash(&entry, deadline)?;
                libraries.push(Library { entry, digest });
            }
            None => outside.push(entry.to_string()),
        }
    }
    if !outside.is_empty() {
        warnings.push(format!(
            "worker startup PROPATH entries outside $DLC are not on the compile PROPATH: {}",
            outside.join(", ")
        ));
    }
    Ok(libraries)
}

/// Runs the whole handshake, then asks for the fingerprints and the hashes.
/// The schema images are not part of it: the pool asks one worker for each.
/// Returns the profile and the warnings the worker caused.
pub fn run(
    session: &mut Session,
    context: &Context<'_>,
    token: &str,
    deadline: Instant,
) -> Result<(Profile, Vec<String>), Failure> {
    session.send(&Request::Hello { token })?;
    expect_hello(session, token, deadline)?;
    let raw = read_environment(session, deadline)?;
    let facts = facts(session, raw)?;
    check_facts(&facts, context)?;
    let mut fingerprints = Vec::new();
    for info in &facts.databases {
        fingerprints.push(session.fingerprint(&info.ldb, deadline)?);
    }
    let mut unknown = Vec::new();
    let mut warnings = Vec::new();
    let parameters = canonical_parameters(
        session,
        facts.get("startup_parameters"),
        deadline,
        &mut unknown,
    )?;
    let libraries = dlc_libraries(session, &facts, deadline, &mut warnings)?;
    let profile = Profile {
        facts,
        parameters,
        libraries,
        unknown,
        fingerprints,
    };
    Ok((profile, warnings))
}
