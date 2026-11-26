//! The framing threads of one connection: a reader and a writer over a byte stream.
//!
//! # No direction waits on the other
//!
//! The reader hands each frame it reads to an unbounded queue, so it never stops
//! reading the peer because the consumer is busy or is itself waiting to write. A
//! bounded inbound channel lets a deadlock form across two processes: the consumer
//! waits for its peer to read a response, the peer waits for this side to read a
//! request, and both socket buffers are full. The outbound channel stays a
//! rendezvous, so the writer takes the next frame only after it wrote the one before
//! and a slow reader slows its own responses, not the reading of its requests.
//!
//! # What bounds the queue
//!
//! The queue holds frames the peer sent and the consumer has not taken. A peer that
//! reads its answers keeps it short, because the consumer then never waits to write.
//! A peer that keeps writing and stops reading fills it, so the reader counts the
//! bytes still in the queue and closes the connection when they pass
//! [`HIGH_WATER_BYTES`]. A test build reads a lower mark from
//! [`TEST_HIGH_WATER_VARIABLE`].

use std::collections::VecDeque;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::thread;

use crossbeam_channel::{Sender, bounded, unbounded};
use lsp_server::{Connection, Message};

/// The bytes of read frames that may wait for the consumer before the reader closes
/// the connection.
pub const HIGH_WATER_BYTES: usize = 256 << 20;

/// The variable that sets another high-water mark, in bytes, in test builds only.
pub const TEST_HIGH_WATER_VARIABLE: &str = "OXABL_TEST_QUEUE_LIMIT";

fn high_water() -> usize {
    if oxabl_common::INJECTION_ENABLED
        && let Some(limit) = std::env::var(TEST_HIGH_WATER_VARIABLE)
            .ok()
            .and_then(|value| value.parse().ok())
    {
        return limit;
    }
    HIGH_WATER_BYTES
}

/// The framing threads of one connection.
pub struct Threads {
    pub reader: thread::JoinHandle<()>,
    pub writer: thread::JoinHandle<()>,
}

impl Threads {
    /// Wait for both threads, so a response is not cut off by an early return.
    pub fn join(self) {
        let _ = self.reader.join();
        let _ = self.writer.join();
    }
}

/// Start the reader and the writer of a connection over `input` and `output`.
///
/// `close` runs when the reader stops because its queue passed the high-water mark.
/// It must make the peer and the writer see the disconnect (for a socket, a shutdown
/// of both directions).
pub fn spawn<R, W, C>(input: R, output: W, close: C) -> io::Result<(Connection, Threads)>
where
    R: Read + Send + 'static,
    W: Write + Send + 'static,
    C: FnOnce() + Send + 'static,
{
    let (inbound_tx, inbound_rx) = unbounded();
    let reader = thread::Builder::new()
        .name("oxabl-conn-read".to_string())
        .spawn(move || read_frames(BufReader::new(input), &inbound_tx, close))?;

    let (outbound_tx, outbound_rx) = bounded::<Message>(0);
    let writer = thread::Builder::new()
        .name("oxabl-conn-write".to_string())
        .spawn(move || {
            let mut output = output;
            for message in outbound_rx {
                // A write failure means the peer is gone. The reader ends too.
                if message.write(&mut output).is_err() {
                    break;
                }
            }
        })?;

    Ok((
        Connection {
            sender: outbound_tx,
            receiver: inbound_rx,
        },
        Threads { reader, writer },
    ))
}

/// Read frames into `queue` until the peer closes, sends `exit`, sends a frame the
/// framing cannot read, or fills the queue past the high-water mark.
fn read_frames<R: BufRead, C: FnOnce()>(input: R, queue: &Sender<Message>, close: C) {
    let mut input = Counting {
        inner: input,
        read: 0,
    };
    // The size of each frame still in the queue, oldest first. The queue is FIFO, so
    // the frames the consumer took are the oldest ones.
    let mut sizes = VecDeque::new();
    let mut queued = 0usize;
    let limit = high_water();
    while let Ok(Some(message)) = Message::read(&mut input) {
        let is_exit = matches!(&message, Message::Notification(n) if n.method == "exit");
        while sizes.len() > queue.len() {
            queued -= sizes.pop_front().unwrap_or(0);
        }
        let size = std::mem::take(&mut input.read);
        if queued + size > limit {
            eprintln!(
                "oxabl: closing a connection whose peer sent {} bytes that were not \
                 handled, and does not read the answers",
                queued + size
            );
            close();
            return;
        }
        sizes.push_back(size);
        queued += size;
        if queue.send(message).is_err() || is_exit {
            return;
        }
    }
}

/// A reader that counts the bytes taken from it.
struct Counting<R> {
    inner: R,
    read: usize,
}

impl<R: BufRead> Read for Counting<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = self.inner.read(buf)?;
        self.read += n;
        Ok(n)
    }
}

impl<R: BufRead> BufRead for Counting<R> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        self.inner.fill_buf()
    }

    fn consume(&mut self, amount: usize) {
        self.read += amount;
        self.inner.consume(amount);
    }
}

/// A connection over this process's stdin and stdout.
///
/// It replaces `Connection::stdio`, whose reader waits on the consumer for each
/// frame. The reader is not joined on an early return: it may wait on an input pipe
/// the peer keeps open.
pub fn stdio() -> io::Result<(Connection, Threads)> {
    spawn(io::stdin(), io::stdout(), || {})
}
