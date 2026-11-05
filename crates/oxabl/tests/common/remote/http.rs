//! A small HTTP/1.1 client and the pieces of one that the proxy needs.

use std::io::{Read, Write};
use std::net::TcpStream;

/// One parsed message: its start line, headers and body.
pub struct Message {
    pub start: String,
    pub headers: Vec<(String, String)>,
    pub body: Vec<u8>,
}

impl Message {
    pub fn header(&self, name: &str) -> Option<&str> {
        self.headers
            .iter()
            .find(|(key, _)| key.eq_ignore_ascii_case(name))
            .map(|(_, value)| value.as_str())
    }

    pub fn status(&self) -> u16 {
        self.start
            .split_whitespace()
            .nth(1)
            .and_then(|code| code.parse().ok())
            .unwrap_or_else(|| panic!("no status in {:?}", self.start))
    }

    pub fn path(&self) -> &str {
        self.start.split_whitespace().nth(1).unwrap_or("/")
    }
}

fn head_end(buffer: &[u8]) -> Option<usize> {
    buffer
        .windows(4)
        .position(|w| w == b"\r\n\r\n")
        .map(|at| at + 4)
}

fn parse_head(head: &[u8]) -> (String, Vec<(String, String)>) {
    let text = String::from_utf8_lossy(head);
    let mut lines = text.split("\r\n");
    let start = lines.next().unwrap_or_default().to_string();
    let headers = lines
        .filter_map(|line| line.split_once(':'))
        .map(|(key, value)| (key.trim().to_string(), value.trim().to_string()))
        .collect();
    (start, headers)
}

fn dechunk(mut raw: &[u8]) -> Vec<u8> {
    let mut body = Vec::new();
    loop {
        let Some(line_end) = raw.windows(2).position(|w| w == b"\r\n") else {
            return body;
        };
        let size = usize::from_str_radix(
            std::str::from_utf8(&raw[..line_end])
                .unwrap_or("0")
                .split(';')
                .next()
                .unwrap_or("0")
                .trim(),
            16,
        )
        .unwrap_or(0);
        raw = &raw[line_end + 2..];
        if size == 0 || raw.len() < size {
            return body;
        }
        body.extend_from_slice(&raw[..size]);
        raw = raw.get(size + 2..).unwrap_or_default();
    }
}

/// Reads one message from `stream`: until the end of the body that its headers
/// announce, or until the peer closes.
pub fn read_message(stream: &mut TcpStream) -> std::io::Result<Message> {
    let mut buffer = Vec::new();
    let mut chunk = [0u8; 16 * 1024];
    let head_len = loop {
        if let Some(end) = head_end(&buffer) {
            break end;
        }
        let read = stream.read(&mut chunk)?;
        if read == 0 {
            return Err(std::io::ErrorKind::UnexpectedEof.into());
        }
        buffer.extend_from_slice(&chunk[..read]);
    };
    let (start, headers) = parse_head(&buffer[..head_len - 4]);
    let mut message = Message {
        start,
        headers,
        body: Vec::new(),
    };
    let rest = buffer[head_len..].to_vec();
    let is_response = message.start.starts_with("HTTP/");
    let chunked = message
        .header("transfer-encoding")
        .is_some_and(|value| value.eq_ignore_ascii_case("chunked"));
    let length = message
        .header("content-length")
        .and_then(|value| value.parse::<usize>().ok());
    let mut body = rest;
    if chunked {
        loop {
            if body.ends_with(b"0\r\n\r\n") {
                break;
            }
            let read = stream.read(&mut chunk)?;
            if read == 0 {
                break;
            }
            body.extend_from_slice(&chunk[..read]);
        }
        message.body = dechunk(&body);
    } else if let Some(length) = length {
        while body.len() < length {
            let read = stream.read(&mut chunk)?;
            if read == 0 {
                return Err(std::io::ErrorKind::UnexpectedEof.into());
            }
            body.extend_from_slice(&chunk[..read]);
        }
        body.truncate(length);
        message.body = body;
    } else if is_response && message.status() != 204 && message.status() != 304 {
        loop {
            let read = stream.read(&mut chunk)?;
            if read == 0 {
                break;
            }
            body.extend_from_slice(&chunk[..read]);
        }
        message.body = body;
    }
    Ok(message)
}

/// The head of a response whose body has `length` bytes, ending the connection.
pub fn response_head(start: &str, headers: &[(String, String)], length: usize) -> Vec<u8> {
    let mut head = format!("{start}\r\n");
    for (key, value) in headers {
        let lower = key.to_ascii_lowercase();
        if matches!(
            lower.as_str(),
            "content-length" | "transfer-encoding" | "connection"
        ) {
            continue;
        }
        head.push_str(&format!("{key}: {value}\r\n"));
    }
    head.push_str(&format!(
        "content-length: {length}\r\nconnection: close\r\n\r\n"
    ));
    head.into_bytes()
}

/// One request to `address` (`host:port`) and its answer.
pub fn request(
    address: &str,
    method: &str,
    path: &str,
    headers: &[(&str, String)],
    body: &[u8],
) -> (u16, Vec<u8>) {
    let mut stream = TcpStream::connect(address).unwrap_or_else(|e| panic!("{address}: {e}"));
    let mut head = format!("{method} {path} HTTP/1.1\r\nhost: {address}\r\nconnection: close\r\n");
    for (key, value) in headers {
        head.push_str(&format!("{key}: {value}\r\n"));
    }
    head.push_str(&format!("content-length: {}\r\n\r\n", body.len()));
    stream.write_all(head.as_bytes()).unwrap();
    stream.write_all(body).unwrap();
    let message = read_message(&mut stream).unwrap_or_else(|e| panic!("{method} {path}: {e}"));
    (message.status(), message.body)
}

/// A JSON request with a bearer token; the answer must have `expected` as its status.
pub fn json(
    address: &str,
    method: &str,
    path: &str,
    token: &str,
    body: &serde_json::Value,
    expected: u16,
) -> serde_json::Value {
    let (status, answer) = request(
        address,
        method,
        path,
        &[
            ("authorization", format!("Bearer {token}")),
            ("content-type", "application/json".to_string()),
        ],
        body.to_string().as_bytes(),
    );
    assert_eq!(
        status,
        expected,
        "{method} {path}: {}",
        String::from_utf8_lossy(&answer)
    );
    serde_json::from_slice(&answer).unwrap_or(serde_json::Value::Null)
}
