//! The compile key as `docs/design/build-tool/compile-key.md` specifies it,
//! written again from the document so that a test can compare it with what
//! `oxabl build --plan --json` prints. Nothing here calls oxabl.

#![allow(dead_code)]

use serde_json::Value;

const KEY_CONTEXT: &str = "oxabl 2026-11 compile key v1";
const ENVIRONMENT_CONTEXT: &str = "oxabl 2026-11 compile environment v1";

pub fn u64_le(out: &mut Vec<u8>, value: usize) {
    out.extend_from_slice(&(value as u64).to_le_bytes());
}

pub fn str_(out: &mut Vec<u8>, text: &str) {
    u64_le(out, text.len());
    out.extend_from_slice(text.as_bytes());
}

pub fn derive(context: &str, material: &[u8]) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new_derive_key(context);
    hasher.update(material);
    *hasher.finalize().as_bytes()
}

pub fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

pub fn unhex(text: &str) -> Vec<u8> {
    (0..text.len() / 2)
        .map(|at| u8::from_str_radix(&text[at * 2..at * 2 + 2], 16).unwrap())
        .collect()
}

fn strings(value: &Value) -> Vec<String> {
    value
        .as_array()
        .unwrap()
        .iter()
        .map(|item| item.as_str().unwrap().to_string())
        .collect()
}

/// The environment identity of the `expected` object of a vector: the
/// sections `openedge`, `platform`, `session`, `parameters` and `databases`.
/// `schema_identity` gives the identity of one database's schema records.
pub fn environment_identity(
    expected: &Value,
    schema_identity: impl Fn(&Value) -> String,
) -> [u8; 32] {
    let mut m = Vec::new();
    for (section, key) in [
        ("openedge", "openedge"),
        ("platform", "platform"),
        ("session", "session"),
    ] {
        str_(&mut m, section);
        for field in strings(&expected[key]) {
            str_(&mut m, &field);
        }
    }
    str_(&mut m, "parameters");
    let parameters = expected["parameters"].as_array().unwrap();
    u64_le(&mut m, parameters.len());
    for pair in parameters {
        str_(&mut m, pair[0].as_str().unwrap());
        str_(&mut m, pair[1].as_str().unwrap());
    }
    str_(&mut m, "databases");
    let databases = expected["databases"].as_array().unwrap();
    u64_le(&mut m, databases.len());
    for database in databases {
        str_(&mut m, database["name"].as_str().unwrap());
        str_(&mut m, database["dbtype"].as_str().unwrap());
        let aliases = strings(&database["aliases"]);
        u64_le(&mut m, aliases.len());
        for alias in &aliases {
            str_(&mut m, alias);
        }
        m.extend_from_slice(&unhex(&schema_identity(&database["schema"])));
    }
    derive(ENVIRONMENT_CONTEXT, &m)
}

/// Every section of the key material except the last, `worker`.
pub fn material_without_worker(expected: &Value, unit: &Value, environment: &[u8; 32]) -> Vec<u8> {
    let mut m = Vec::new();
    str_(&mut m, "unit");
    str_(&mut m, unit["path"].as_str().unwrap());
    str_(&mut m, unit["compile_name"].as_str().unwrap());
    m.extend_from_slice(&unhex(unit["content"].as_str().unwrap()));
    str_(&mut m, "closure");
    let closure = expected["closure"].as_array().unwrap();
    u64_le(&mut m, closure.len());
    for entry in closure {
        str_(&mut m, entry[0].as_str().unwrap());
        str_(&mut m, entry[1].as_str().unwrap());
        m.extend_from_slice(&unhex(entry[2].as_str().unwrap()));
    }
    str_(&mut m, "propath");
    let propath = expected["propath"].as_array().unwrap();
    u64_le(&mut m, propath.len());
    for entry in propath {
        str_(&mut m, entry[0].as_str().unwrap());
        str_(&mut m, entry[1].as_str().unwrap());
    }
    str_(&mut m, "environment");
    m.extend_from_slice(environment);
    str_(&mut m, "options");
    let options = expected["options"].as_array().unwrap();
    u64_le(&mut m, options.len());
    for pair in options {
        str_(&mut m, pair[0].as_str().unwrap());
        str_(&mut m, pair[1].as_str().unwrap());
    }
    str_(&mut m, "outputs");
    let outputs = strings(&expected["outputs"]);
    u64_le(&mut m, outputs.len());
    for kind in &outputs {
        str_(&mut m, kind);
    }
    m
}

/// The key over `material` (all sections but the last) and a worker program
/// whose BLAKE3 hash is `worker`.
pub fn key_with_worker(material: &[u8], worker: &[u8; 32]) -> String {
    let mut full = material.to_vec();
    str_(&mut full, "worker");
    full.extend_from_slice(worker);
    hex(&derive(KEY_CONTEXT, &full))
}
