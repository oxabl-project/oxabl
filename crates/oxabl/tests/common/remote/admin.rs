//! The operator command line of the cache server.

use std::process::Command;

use serde_json::Value;

use super::binaries;
use super::server::Server;

fn run(server: &Server, arguments: &[&str]) -> Value {
    let output = Command::new(binaries::admin())
        .args([
            "--server",
            &format!("http://{}", server.admin),
            "--token-file",
        ])
        .arg(server.token_file())
        .arg("--json")
        .args(arguments)
        .output()
        .expect("run the admin tool");
    assert!(
        output.status.success(),
        "oxabl-cache-admin {arguments:?}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).unwrap_or_else(|e| {
        panic!(
            "oxabl-cache-admin {arguments:?} printed no JSON ({e}): {}",
            String::from_utf8_lossy(&output.stdout)
        )
    })
}

/// A new tenant and a `tenant:admin` token for it.
pub fn tenant(server: &Server, name: &str) -> (String, String) {
    let created = run(server, &["tenant", "create", name]);
    let id = created["id"].as_str().expect("a tenant id").to_string();
    let token = run(server, &["token", "create", "--tenant", &id]);
    (id, token["token"].as_str().expect("a token").to_string())
}
