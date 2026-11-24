//! The thread that reads a worker's stdout.

use std::fs::File;
use std::io::{BufRead, BufReader, Read, Write};
use std::sync::mpsc::Sender;
use std::thread::JoinHandle;

use super::frame::{MAX_LINE, PREFIX};

/// What the reader reports.
pub enum Event {
    /// A protocol line, without its line end.
    Line(Vec<u8>),
    /// A protocol line longer than the limit.
    TooLong,
    /// Stdout ended.
    Eof,
    Failed(String),
}

/// Longest noise line kept in the log, in bytes.
const NOISE_KEPT: usize = 2_000;

/// Reads lines from `stdout`. Lines that are not protocol lines are noise:
/// they go to `noise` and are never parsed.
pub fn spawn(
    stdout: impl Read + Send + 'static,
    mut noise: File,
    events: Sender<Event>,
) -> JoinHandle<()> {
    std::thread::spawn(move || {
        let mut reader = BufReader::new(stdout);
        let mut line: Vec<u8> = Vec::new();
        let mut overflow = false;
        loop {
            let chunk = match reader.fill_buf() {
                Ok([]) => {
                    if !line.is_empty() && finish(&mut line, overflow, &mut noise, &events).is_err()
                    {
                        return;
                    }
                    let _ = events.send(Event::Eof);
                    return;
                }
                Ok(chunk) => chunk,
                Err(error) => {
                    let _ = events.send(Event::Failed(error.to_string()));
                    return;
                }
            };
            let (take, ended) = match chunk.iter().position(|byte| *byte == b'\n') {
                Some(at) => (at, true),
                None => (chunk.len(), false),
            };
            let room = (MAX_LINE + 2).saturating_sub(line.len());
            line.extend_from_slice(&chunk[..take.min(room)]);
            overflow |= take > room;
            reader.consume(take + usize::from(ended));
            if ended {
                if finish(&mut line, overflow, &mut noise, &events).is_err() {
                    return;
                }
                overflow = false;
            }
        }
    })
}

fn finish(
    line: &mut Vec<u8>,
    overflow: bool,
    noise: &mut File,
    events: &Sender<Event>,
) -> Result<(), ()> {
    let mut text = std::mem::take(line);
    if text.last() == Some(&b'\r') {
        text.pop();
    }
    if text.starts_with(PREFIX) {
        let event = if overflow || text.len() > MAX_LINE {
            Event::TooLong
        } else {
            Event::Line(text)
        };
        return events.send(event).map_err(|_| ());
    }
    text.truncate(NOISE_KEPT);
    text.push(b'\n');
    let _ = noise.write_all(&text);
    Ok(())
}
