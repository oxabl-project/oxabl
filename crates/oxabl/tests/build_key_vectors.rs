//! The golden compile keys of `crates/oxabl_build/tests/key_vectors.json`.
//!
//! Each vector holds a workspace, the environment the fake AVM reports, and
//! the key material as the specification builds it, in hex. The material is
//! checked against the files and against an encoder written from the
//! specification (`common::key_spec`). The key that `oxabl build --plan
//! --probe --json` prints must equal the key over that material and the hash
//! of the worker program that ships in `oxabl_build`.

mod common;

use std::fs;

use common::key_spec::{
    derive, environment_identity, hex, key_with_worker, material_without_worker, unhex,
};
use common::keyed::Keyed;
use serde_json::Value;
use sha2::{Digest, Sha256};

fn document() -> Value {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../oxabl_build/tests/key_vectors.json"
    );
    serde_json::from_str(&fs::read_to_string(path).expect("key_vectors.json")).unwrap()
}

fn worker_program_hash() -> [u8; 32] {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../oxabl_build/resources/oxabl_worker.p"
    );
    *blake3::hash(&fs::read(path).expect("the worker program")).as_bytes()
}

fn schema_identity(records: &Value) -> String {
    let records: Vec<Vec<String>> = records
        .as_array()
        .unwrap()
        .iter()
        .map(|record| {
            record
                .as_array()
                .unwrap()
                .iter()
                .map(|field| field.as_str().unwrap().to_string())
                .collect()
        })
        .collect();
    common::schema_identity(&records)
}

fn material_of(vector: &Value) -> Vec<u8> {
    let expected = &vector["expected"];
    let environment = environment_identity(expected, schema_identity);
    assert_eq!(
        hex(&environment),
        vector["environment_identity"].as_str().unwrap(),
        "{}: the environment identity",
        vector["name"]
    );
    let unit = serde_json::json!({
        "path": vector["unit"],
        "compile_name": vector["compile_name"],
        "content": vector["unit_content"],
    });
    material_without_worker(expected, &unit, &environment)
}

fn workspace_of(vector: &Value) -> Keyed {
    let workspace = Keyed::new(
        vector["sources"].as_str().unwrap(),
        vector["extra_config"].as_str().unwrap(),
    );
    workspace.set_preprocessor(vector["preprocessor"].as_str().unwrap());
    for (path, text) in vector["files"].as_object().unwrap() {
        workspace.write(path, text.as_str().unwrap());
    }
    for (path, text) in vector["dlc_files"].as_object().unwrap() {
        workspace.write(&format!("dlc/{path}"), text.as_str().unwrap());
    }
    workspace.set_environment(&vector["env"]);
    workspace
}

#[test]
fn the_vector_material_is_what_the_files_and_the_specification_give() {
    for vector in document()["vectors"].as_array().unwrap() {
        let name = vector["name"].as_str().unwrap();
        let unit = vector["unit"].as_str().unwrap();
        let unit_bytes = vector["files"][unit].as_str().unwrap().as_bytes();
        assert_eq!(
            hex(blake3::hash(unit_bytes).as_bytes()),
            vector["unit_content"].as_str().unwrap(),
            "{name}: the unit bytes"
        );
        for entry in vector["expected"]["closure"].as_array().unwrap() {
            let (path, algorithm, hash) = (
                entry[0].as_str().unwrap(),
                entry[1].as_str().unwrap(),
                entry[2].as_str().unwrap(),
            );
            let bytes = match path.strip_prefix("$DLC/") {
                Some(rest) => vector["dlc_files"][rest].as_str(),
                None => vector["files"][path].as_str(),
            }
            .unwrap_or_else(|| panic!("{name}: no file {path}"))
            .as_bytes();
            let computed = match algorithm {
                "sha256" => hex(&Sha256::digest(bytes)),
                "blake3" => hex(blake3::hash(bytes).as_bytes()),
                other => panic!("{name}: algorithm {other}"),
            };
            assert_eq!(computed, hash, "{name}: the hash of {path}");
        }
        assert_eq!(
            hex(&material_of(vector)),
            vector["material_hex"].as_str().unwrap(),
            "{name}: the key material"
        );
    }
}

#[test]
fn the_worked_example_of_the_specification_holds() {
    let document = document();
    let example = &document["prd_example"];
    let vector = document["vectors"]
        .as_array()
        .unwrap()
        .iter()
        .find(|v| v["name"] == example["vector"])
        .unwrap();
    let material = material_of(vector);

    let worker = blake3::hash(example["worker_program"].as_str().unwrap().as_bytes());
    assert_eq!(hex(worker.as_bytes()), example["worker_program_hash"]);
    assert_eq!(
        material.len() + 8 + "worker".len() + 32,
        example["key_material_length"].as_u64().unwrap() as usize
    );
    assert_eq!(
        vector["unit_content"].as_str().unwrap(),
        example["unit_content"].as_str().unwrap()
    );
    assert_eq!(
        vector["environment_identity"].as_str().unwrap(),
        example["environment_identity"].as_str().unwrap()
    );
    assert_eq!(
        schema_identity(&vector["expected"]["databases"][0]["schema"]),
        example["schema_identity"].as_str().unwrap()
    );
    assert_eq!(
        key_with_worker(&material, worker.as_bytes()),
        example["key"].as_str().unwrap()
    );
}

#[test]
fn every_golden_vector_is_the_key_oxabl_prints() {
    let worker = worker_program_hash();
    for vector in document()["vectors"].as_array().unwrap() {
        let name = vector["name"].as_str().unwrap();
        let expected_key = key_with_worker(&unhex_material(vector), &worker);

        let plan = workspace_of(vector).plan();

        let unit = vector["unit"].as_str().unwrap();
        assert_eq!(plan.key_of(unit), expected_key, "{name}");
        assert_eq!(
            plan.unit(unit)["compile_name"],
            vector["compile_name"],
            "{name}"
        );
        assert_eq!(plan.identity(), vector["environment_identity"], "{name}");
        let propath: Vec<(String, String)> = vector["expected"]["propath"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| (e[0].as_str().unwrap().into(), e[1].as_str().unwrap().into()))
            .collect();
        assert_eq!(plan.propath(unit), propath, "{name}");
    }
}

fn unhex_material(vector: &Value) -> Vec<u8> {
    unhex(vector["material_hex"].as_str().unwrap())
}

#[test]
fn the_key_is_the_derive_key_hash_of_the_material() {
    let document = document();
    let vector = &document["vectors"][0];
    let material = unhex_material(vector);
    let mut full = material.clone();
    full.extend_from_slice(&[6, 0, 0, 0, 0, 0, 0, 0]);
    full.extend_from_slice(b"worker");
    full.extend_from_slice(&worker_program_hash());
    let plain = blake3::hash(&full);
    let keyed = derive("oxabl 2026-11 compile key v1", &full);
    assert_ne!(plain.as_bytes(), &keyed, "a plain hash is not the key");
    assert_eq!(
        hex(&keyed),
        key_with_worker(&material, &worker_program_hash())
    );
}
