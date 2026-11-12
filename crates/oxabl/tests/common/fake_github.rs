//! A fake GitHub on a loopback port: the tag list, the release and the
//! assets that `oxabl self-update` and `scripts/install.sh` read.

#![allow(dead_code)]

use std::collections::HashMap;
use std::io::{Read, Write};
use std::net::TcpListener;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use sha2::{Digest, Sha256};

pub const ASSET: &str = "oxabl-x86_64-unknown-linux-musl";
pub const INSTALLED: &str = env!("CARGO_PKG_VERSION");

pub enum Reply {
    Ok(Vec<u8>),
    Status(u16),
    /// A body and a `Link` header.
    Linked(Vec<u8>, String),
    /// Waits, then answers 500.
    Stall(Duration),
}

pub type Routes = Arc<Mutex<HashMap<String, Reply>>>;

pub struct Server {
    pub port: u16,
    routes: Routes,
    requests: Arc<Mutex<Vec<String>>>,
}

impl Server {
    pub fn start() -> Server {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let routes: Routes = Arc::default();
        let requests: Arc<Mutex<Vec<String>>> = Arc::default();
        let (served, log) = (routes.clone(), requests.clone());
        std::thread::spawn(move || {
            for stream in listener.incoming().flatten() {
                let (served, log) = (served.clone(), log.clone());
                std::thread::spawn(move || answer(stream, &served, &log));
            }
        });
        Server {
            port,
            routes,
            requests,
        }
    }

    pub fn url(&self) -> String {
        format!("http://127.0.0.1:{}", self.port)
    }

    pub fn route(&self, path: &str, reply: Reply) {
        self.routes.lock().unwrap().insert(path.to_string(), reply);
    }

    pub fn requests(&self) -> Vec<String> {
        self.requests.lock().unwrap().clone()
    }

    /// A complete release `version` whose binary is `body`; `digest` replaces
    /// the true SHA-256 when it is given.
    pub fn release(&self, version: &str, body: &[u8], digest: Option<&str>) {
        self.tags(&[
            &format!("refs/tags/oxabl-v{INSTALLED}"),
            &format!("refs/tags/oxabl-v{version}"),
        ]);
        let digest = digest.map_or_else(|| hex(body), str::to_string);
        self.route(&format!("/dl/{ASSET}"), Reply::Ok(body.to_vec()));
        self.route(
            &format!("/dl/{ASSET}.sha256"),
            Reply::Ok(format!("{digest}  {ASSET}\n").into_bytes()),
        );
        self.release_json(version, false, false, &[ASSET, &format!("{ASSET}.sha256")]);
    }

    pub fn tags(&self, refs: &[&str]) {
        let list: Vec<String> = refs
            .iter()
            .map(|r| format!("{{\"ref\":\"{r}\"}}"))
            .collect();
        self.route(
            "/repos/oxabl-project/oxabl/git/matching-refs/tags/oxabl-v",
            Reply::Ok(format!("[{}]", list.join(",")).into_bytes()),
        );
    }

    pub fn release_json(&self, version: &str, draft: bool, prerelease: bool, assets: &[&str]) {
        let assets: Vec<String> = assets
            .iter()
            .map(|name| {
                format!(
                    "{{\"name\":\"{name}\",\"browser_download_url\":\"{}/dl/{name}\"}}",
                    self.url()
                )
            })
            .collect();
        self.route(
            &format!("/repos/oxabl-project/oxabl/releases/tags/oxabl-v{version}"),
            Reply::Ok(
                format!(
                    "{{\"draft\":{draft},\"prerelease\":{prerelease},\"assets\":[{}]}}",
                    assets.join(",")
                )
                .into_bytes(),
            ),
        );
    }
}

pub fn hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}

fn answer(mut stream: std::net::TcpStream, routes: &Routes, log: &Arc<Mutex<Vec<String>>>) {
    let mut request = Vec::new();
    let mut buffer = [0u8; 1024];
    while !request.windows(4).any(|w| w == b"\r\n\r\n") {
        match stream.read(&mut buffer) {
            Ok(0) | Err(_) => return,
            Ok(n) => request.extend_from_slice(&buffer[..n]),
        }
    }
    let text = String::from_utf8_lossy(&request).into_owned();
    let target = text.split_whitespace().nth(1).unwrap_or("/");
    let path = target.split('?').next().unwrap_or(target).to_string();
    log.lock().unwrap().push(path.clone());
    let stall = match routes.lock().unwrap().get(&path) {
        Some(Reply::Stall(wait)) => Some(*wait),
        _ => None,
    };
    if let Some(wait) = stall {
        std::thread::sleep(wait);
    }
    let (status, body) = match routes.lock().unwrap().get(&path) {
        Some(Reply::Ok(body) | Reply::Linked(body, _)) => (200, body.clone()),
        Some(Reply::Status(code)) => (*code, Vec::new()),
        Some(Reply::Stall(_)) => (500, Vec::new()),
        None => (404, Vec::new()),
    };
    let link = match routes.lock().unwrap().get(&path) {
        Some(Reply::Linked(_, link)) => format!("Link: {link}\r\n"),
        _ => String::new(),
    };
    let head = format!(
        "HTTP/1.1 {status} X\r\nContent-Length: {}\r\n{link}Connection: close\r\n\r\n",
        body.len()
    );
    let _ = stream.write_all(head.as_bytes());
    let _ = stream.write_all(&body);
}
