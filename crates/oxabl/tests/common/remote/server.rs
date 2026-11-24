//! The real `oxabl-cache-server` on a fresh data directory.

use std::fs;
use std::io::{BufRead, BufReader};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

use tempfile::TempDir;

use super::binaries;

pub struct Server {
    child: Option<Child>,
    dir: TempDir,
    pub public: String,
    pub admin: String,
}

impl Server {
    /// Starts a server whose object URLs name `public_url`, or itself when
    /// there is none.
    pub fn start(public_url: Option<&str>) -> Server {
        let dir = TempDir::new().expect("a server directory");
        let shown = dir.path().display();
        let public_url =
            public_url.map_or_else(String::new, |url| format!("public_url = \"{url}\"\n"));
        let config = dir.path().join("server.toml");
        fs::write(
            &config,
            format!(
                "[server]\nlisten = \"127.0.0.1:0\"\nadmin_listen = \"127.0.0.1:0\"\n\
                 metrics_listen = \"127.0.0.1:0\"\n{public_url}shutdown_timeout = \"1s\"\n\
                 [metadata]\nurl = \"sqlite://{shown}/meta.db\"\n\
                 [store]\nkind = \"fs\"\nroot = \"{shown}/objects\"\n\
                 [keys]\nurl_key_file = \"{shown}/url.key\"\n"
            ),
        )
        .expect("server configuration");
        let binary = binaries::server();
        let init = Command::new(&binary)
            .arg("-c")
            .arg(&config)
            .arg("init")
            .output()
            .expect("run the server");
        assert!(
            init.status.success(),
            "the server's init failed: {}",
            String::from_utf8_lossy(&init.stderr)
        );
        let token_file = dir.path().join("operator.token");
        fs::write(&token_file, &init.stdout).expect("operator token");
        fs::set_permissions(&token_file, fs::Permissions::from_mode(0o600)).expect("mode");
        let mut child = Command::new(&binary)
            .arg("-c")
            .arg(&config)
            .arg("serve")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(fs::File::create(dir.path().join("server.log")).expect("log"))
            .spawn()
            .expect("start the server");
        let mut reader = BufReader::new(child.stdout.take().expect("stdout"));
        let mut line = String::new();
        reader.read_line(&mut line).expect("the ready line");
        let ready: serde_json::Value = serde_json::from_str(&line)
            .unwrap_or_else(|e| panic!("the server's first line is not JSON ({e}): {line}"));
        std::thread::spawn(move || {
            let mut sink = String::new();
            while reader.read_line(&mut sink).unwrap_or(0) > 0 {
                sink.clear();
            }
        });
        let address = |key: &str| {
            ready["ready"][key]
                .as_str()
                .expect("an address")
                .to_string()
        };
        Server {
            child: Some(child),
            public: address("listen"),
            admin: address("admin_listen"),
            dir,
        }
    }

    pub fn token_file(&self) -> PathBuf {
        self.dir.path().join("operator.token")
    }

    pub fn dir(&self) -> &Path {
        self.dir.path()
    }

    /// Ends the server: every later connection is refused.
    pub fn stop(&mut self) {
        if let Some(mut child) = self.child.take() {
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        self.stop();
    }
}
