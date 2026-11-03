//! A proxy in front of the cache server that can damage, slow or stall the
//! answers to the requests whose path names a marker.

use std::io::Write;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use super::http::{read_message, response_head};

/// What the proxy does with the answers to requests whose path contains
/// `marker`: `/lookup` for the lookup, `/objects/` for the packs.
#[derive(Clone, Copy)]
pub enum Mode {
    Pass,
    /// The body is replaced by noise of the same length.
    Garbage {
        marker: &'static str,
    },
    /// The body arrives in pieces of `piece` bytes with `gap` between them.
    Slow {
        marker: &'static str,
        piece: usize,
        gap: Duration,
    },
    /// `sent` bytes of the body arrive, then nothing, and the connection stays open.
    Stall {
        marker: &'static str,
        sent: usize,
    },
}

struct Shared {
    mode: Mutex<Mode>,
    requests: Mutex<Vec<String>>,
    upstream: String,
}

pub struct Proxy {
    shared: Arc<Shared>,
    address: String,
}

fn noise(length: usize) -> Vec<u8> {
    let mut state = 0x9E37_79B9_7F4A_7C15_u64;
    (0..length)
        .map(|_| {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            (state >> 24) as u8
        })
        .collect()
}

fn forward(shared: &Shared, mut client: TcpStream) -> std::io::Result<()> {
    let request = read_message(&mut client)?;
    shared.requests.lock().unwrap().push(format!(
        "{} {}",
        request.start.split(' ').next().unwrap_or(""),
        request.path()
    ));
    let mut upstream = TcpStream::connect(&shared.upstream)?;
    let mut head = format!(
        "{}\r\nhost: {}\r\nconnection: close\r\n",
        request.start, shared.upstream
    );
    for (key, value) in &request.headers {
        let lower = key.to_ascii_lowercase();
        if matches!(
            lower.as_str(),
            "host" | "connection" | "content-length" | "transfer-encoding"
        ) {
            continue;
        }
        head.push_str(&format!("{key}: {value}\r\n"));
    }
    head.push_str(&format!("content-length: {}\r\n\r\n", request.body.len()));
    upstream.write_all(head.as_bytes())?;
    upstream.write_all(&request.body)?;
    let response = read_message(&mut upstream)?;
    let mode = *shared.mode.lock().unwrap();
    let path = request.path();
    let mut body = response.body;
    let head_of = |length: usize| response_head(&response.start, &response.headers, length);
    match mode {
        Mode::Garbage { marker } if path.contains(marker) => body = noise(body.len()),
        Mode::Slow { marker, piece, gap } if path.contains(marker) => {
            client.write_all(&head_of(body.len()))?;
            for part in body.chunks(piece) {
                client.write_all(part)?;
                client.flush()?;
                std::thread::sleep(gap);
            }
            return Ok(());
        }
        Mode::Stall { marker, sent } if path.contains(marker) => {
            client.write_all(&head_of(body.len().max(sent + 1)))?;
            client.write_all(&body[..sent.min(body.len())])?;
            client.flush()?;
            let mut rest = [0u8; 1];
            let _ = std::io::Read::read(&mut client, &mut rest);
            return Ok(());
        }
        _ => {}
    }
    client.write_all(&head_of(body.len()))?;
    client.write_all(&body)
}

impl Proxy {
    /// Serves on `listener` and forwards to `upstream` (`host:port`).
    pub fn start(listener: TcpListener, upstream: &str) -> Proxy {
        let address = listener.local_addr().expect("an address").to_string();
        let shared = Arc::new(Shared {
            mode: Mutex::new(Mode::Pass),
            requests: Mutex::new(Vec::new()),
            upstream: upstream.to_string(),
        });
        let serving = shared.clone();
        std::thread::spawn(move || {
            for client in listener.incoming().flatten() {
                let shared = serving.clone();
                std::thread::spawn(move || {
                    let _ = forward(&shared, client);
                });
            }
        });
        Proxy { shared, address }
    }

    pub fn url(&self) -> String {
        format!("http://{}", self.address)
    }

    pub fn set(&self, mode: Mode) {
        *self.shared.mode.lock().unwrap() = mode;
    }

    /// `METHOD /path` of every request so far.
    pub fn requests(&self) -> Vec<String> {
        self.shared.requests.lock().unwrap().clone()
    }

    pub fn clear(&self) {
        self.shared.requests.lock().unwrap().clear();
    }
}
