//! The schema images at the start of a build: a raw pass tells, for each
//! database, whether the image that the last build stored still holds; the
//! databases whose image does not are imaged.

use std::path::Path;
use std::time::{Duration, Instant};

use super::error::WorkerError;
use super::imaging::{image_databases, raw_digests};
use super::session::Session;
use crate::key::image_store::{self, Binding, StoredImage};
use crate::key::{RawDigest, SchemaImage};

/// How the images of a build start came about.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Origin {
    /// For each database, the raw digest that pins its image; `None` when no
    /// raw pass before and after the imaging agreed. The schema checks of
    /// the build make a raw pass only when every database has one.
    pub raw: Vec<Option<String>>,
    /// The databases whose stored image was reused.
    pub reused: Vec<String>,
    /// The databases that were imaged.
    pub imaged: Vec<String>,
}

/// The images of a build start and how they came about.
pub struct Schemas {
    /// One image per database, in connection order.
    pub images: Vec<SchemaImage>,
    pub origin: Origin,
    /// Both raw passes together.
    pub raw_ms: u64,
    pub imaging_ms: u64,
    /// The sessions, by place, that failed a raw pass and could not be
    /// replaced, with why. They are out of step and must not be used.
    pub lost: Vec<(usize, String)>,
    pub warnings: Vec<String>,
}

/// Binds a database of this build: its logical and its physical name.
pub type BindingOf<'a> = dyn Fn(&str, &str) -> Binding + 'a;

/// Starts a new session for the one at a place that failed a raw pass.
pub type Restart<'a> = dyn Fn(usize) -> Result<Session, String> + 'a;

/// A raw pass over every database; an equal digest with an equal binding
/// reuses the stored image. Every other database (no stored image, another
/// digest or binding, a failed pass) is imaged, then a second raw pass runs,
/// and the image is stored only when both passes agree: a schema change
/// during the imaging leaves nothing behind. An error is never "unchanged".
pub fn start(
    sessions: &mut [&mut Session],
    databases: &[String],
    binding_of: &BindingOf<'_>,
    restart: &Restart<'_>,
    store: &Path,
    timeout: Duration,
) -> Result<Schemas, WorkerError> {
    let mut schemas = Schemas {
        images: Vec::new(),
        origin: Origin {
            raw: vec![None; databases.len()],
            ..Origin::default()
        },
        raw_ms: 0,
        imaging_ms: 0,
        lost: Vec::new(),
        warnings: Vec::new(),
    };
    let mut live: Vec<usize> = (0..sessions.len()).collect();
    let before = pass(
        sessions,
        &mut live,
        databases,
        restart,
        timeout,
        &mut schemas,
    )?;
    let before: Option<Vec<RawDigest>> = match before {
        Ok(before) => Some(before),
        Err(reason) => {
            schemas.warnings.push(format!(
                "the schema images are made again: the raw digest failed: {reason}"
            ));
            None
        }
    };
    let stored = image_store::load(store);
    let mut images: Vec<Option<SchemaImage>> = vec![None; databases.len()];
    let mut keep: Vec<StoredImage> = Vec::new();
    for (at, ldb) in databases.iter().enumerate() {
        let Some(raw) = before.as_ref().map(|before| &before[at]) else {
            continue;
        };
        let binding = binding_of(ldb, &raw.physical);
        if let Some(image) = image_store::find(&stored, &binding, &raw.digest) {
            schemas.origin.raw[at] = Some(raw.digest.clone());
            schemas.origin.reused.push(ldb.clone());
            keep.push(StoredImage {
                binding,
                raw: raw.digest.clone(),
                image: image.clone(),
            });
            images[at] = Some(image);
        }
    }
    let need: Vec<usize> = (0..databases.len())
        .filter(|at| images[*at].is_none())
        .collect();
    if !need.is_empty() {
        let names: Vec<String> = need.iter().map(|at| databases[*at].clone()).collect();
        let imaging = Instant::now();
        let made = {
            let mut alive = subset(sessions, &live);
            if alive.is_empty() {
                return Err(WorkerError::Lost(
                    "no worker is left to image the databases".into(),
                ));
            }
            image_databases(&mut alive, &names, timeout)?
        };
        schemas.imaging_ms = imaging.elapsed().as_millis() as u64;
        let after = pass(sessions, &mut live, &names, restart, timeout, &mut schemas)?.ok();
        for ((at, ldb), image) in need.iter().zip(&names).zip(made) {
            let pinned = before
                .as_ref()
                .zip(after.as_ref())
                .and_then(|(before, after)| {
                    let (first, second) = (&before[*at], after.iter().find(|raw| raw.ldb == *ldb)?);
                    (first.digest == second.digest && first.physical == second.physical)
                        .then_some(first)
                });
            if let Some(raw) = pinned {
                schemas.origin.raw[*at] = Some(raw.digest.clone());
                keep.push(StoredImage {
                    binding: binding_of(ldb, &raw.physical),
                    raw: raw.digest.clone(),
                    image: image.clone(),
                });
            }
            schemas.origin.imaged.push(ldb.clone());
            images[*at] = Some(image);
        }
        if let Err(error) = image_store::save(store, &keep) {
            schemas.warnings.push(format!(
                "cannot keep the schema images in {}: {error}",
                store.display()
            ));
        }
    }
    schemas.images = images
        .into_iter()
        .map(|image| image.expect("every database has an image"))
        .collect();
    Ok(schemas)
}

/// The sessions at the places in `live`.
fn subset<'a>(sessions: &'a mut [&mut Session], live: &[usize]) -> Vec<&'a mut Session> {
    sessions
        .iter_mut()
        .enumerate()
        .filter(|(at, _)| live.contains(at))
        .map(|(_, session)| &mut **session)
        .collect()
}

/// One raw pass over `databases` on the live sessions. A session that fails
/// is out of step: it is replaced, or left out from then on. Only a cancel
/// stops the start.
fn pass(
    sessions: &mut [&mut Session],
    live: &mut Vec<usize>,
    databases: &[String],
    restart: &Restart<'_>,
    timeout: Duration,
    schemas: &mut Schemas,
) -> Result<Result<Vec<RawDigest>, String>, WorkerError> {
    let passing = Instant::now();
    let (digests, failed) = {
        let mut alive = subset(sessions, live);
        raw_digests(&mut alive, databases, timeout)
    };
    schemas.raw_ms += passing.elapsed().as_millis() as u64;
    let mut gone = Vec::new();
    for (at, error) in failed {
        if matches!(error, WorkerError::Cancelled) {
            return Err(WorkerError::Cancelled);
        }
        let place = live[at];
        sessions[place].kill();
        match restart(place) {
            Ok(session) => {
                *sessions[place] = session;
                schemas.warnings.push(format!(
                    "a worker failed the raw digest pass and was replaced: {error}"
                ));
            }
            Err(why) => {
                schemas
                    .lost
                    .push((place, format!("{error}; no replacement started: {why}")));
                gone.push(place);
            }
        }
    }
    live.retain(|place| !gone.contains(place));
    Ok(digests)
}
