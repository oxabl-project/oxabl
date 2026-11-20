//! The schema image of each database that a build made, kept with the raw
//! digest that was read before and after the imaging, for the next build.

use std::path::Path;

use serde_json::{Value, json};

use super::encode::SCHEMA_CONTEXT;
use super::raw_digest::form;
use super::schema_image::{SchemaEntry, SchemaImage};

/// The file in the state directory.
pub const FILE: &str = "schema-images.json";
const FORMAT: u64 = 1;

/// What a stored image is bound to. A stored image whose binding differs in
/// any field from the build's is void.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding {
    pub ldb: String,
    /// The physical name of the database (`PDBNAME`).
    pub physical: String,
    /// `_Db._Db-guid`.
    pub guid: String,
    /// The BLAKE3 hash of the worker program, in hex.
    pub worker: String,
    /// The context string of the schema identity.
    pub image_version: String,
    /// The form of the raw digest.
    pub form: String,
    /// The first line of `$DLC/version`.
    pub openedge: String,
}

impl Binding {
    /// The binding of database `ldb` in this build.
    pub fn new(ldb: &str, physical: &str, guid: &str, worker: &str, openedge: &str) -> Binding {
        Binding {
            ldb: ldb.to_string(),
            physical: physical.to_string(),
            guid: guid.to_string(),
            worker: worker.to_string(),
            image_version: SCHEMA_CONTEXT.to_string(),
            form: form(),
            openedge: openedge.trim_end().to_string(),
        }
    }
}

/// An image with the raw digest that pins it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StoredImage {
    pub binding: Binding,
    pub raw: String,
    pub image: SchemaImage,
}

fn text<'a>(value: &'a Value, name: &str) -> Option<&'a str> {
    value.get(name)?.as_str()
}

fn parse(value: &Value) -> Option<StoredImage> {
    let binding = Binding {
        ldb: text(value, "ldb")?.to_string(),
        physical: text(value, "physical")?.to_string(),
        guid: text(value, "guid")?.to_string(),
        worker: text(value, "worker")?.to_string(),
        image_version: text(value, "image_version")?.to_string(),
        form: text(value, "form")?.to_string(),
        openedge: text(value, "openedge")?.to_string(),
    };
    let entries = value
        .get("entries")?
        .as_array()?
        .iter()
        .map(|entry| {
            Some(SchemaEntry {
                kind: text(entry, "kind")?.to_string(),
                name: text(entry, "name")?.to_string(),
                records: entry.get("records")?.as_u64()?,
                sha256: text(entry, "sha256")?.to_string(),
            })
        })
        .collect::<Option<Vec<_>>>()?;
    let image = SchemaImage::new(binding.ldb.clone(), entries);
    // A file that was changed by hand or cut short gives another identity.
    if image.identity().to_hex().as_str() != text(value, "identity")? {
        return None;
    }
    Some(StoredImage {
        binding,
        raw: text(value, "raw")?.to_string(),
        image,
    })
}

/// The stored images; none when the file is absent, unreadable or of
/// another format, or an entry does not parse or hash to its identity.
pub fn load(path: &Path) -> Vec<StoredImage> {
    let Ok(bytes) = std::fs::read(path) else {
        return Vec::new();
    };
    let Ok(document) = serde_json::from_slice::<Value>(&bytes) else {
        return Vec::new();
    };
    if document.get("format").and_then(Value::as_u64) != Some(FORMAT) {
        return Vec::new();
    }
    let Some(databases) = document.get("databases").and_then(Value::as_array) else {
        return Vec::new();
    };
    databases.iter().filter_map(parse).collect()
}

/// The stored image of the database with `binding` whose raw digest is
/// `raw`, if there is one.
pub fn find(stored: &[StoredImage], binding: &Binding, raw: &str) -> Option<SchemaImage> {
    stored
        .iter()
        .find(|stored| stored.binding == *binding && stored.raw == raw)
        .map(|stored| stored.image.clone())
}

/// Replaces the file with `images`, by a temporary file and a rename.
pub fn save(path: &Path, images: &[StoredImage]) -> std::io::Result<()> {
    let databases: Vec<Value> = images
        .iter()
        .map(|stored| {
            let b = &stored.binding;
            json!({
                "ldb": b.ldb,
                "physical": b.physical,
                "guid": b.guid,
                "worker": b.worker,
                "image_version": b.image_version,
                "form": b.form,
                "openedge": b.openedge,
                "raw": stored.raw,
                "identity": stored.image.identity().to_hex().to_string(),
                "entries": stored.image.entries.iter().map(|entry| json!({
                    "kind": entry.kind,
                    "name": entry.name,
                    "records": entry.records,
                    "sha256": entry.sha256,
                })).collect::<Vec<_>>(),
            })
        })
        .collect();
    let document = json!({ "format": FORMAT, "databases": databases });
    let temp = path.with_extension(format!("json.tmp-{}", std::process::id()));
    std::fs::write(&temp, document.to_string())?;
    std::fs::rename(&temp, path)
}
