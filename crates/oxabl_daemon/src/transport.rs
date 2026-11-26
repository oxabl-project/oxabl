//! The framing threads of one connection: a reader and a writer over a byte stream.
//!
//! # No direction waits on the other
//!
//! The reader hands each frame it reads to an unbounded queue, so it never stops
//! reading the peer because the consumer is busy or is itself waiting to write. A
//! bounded inbound channel lets a deadlock form across two processes: the consumer
//! waits for its peer to read a response, the peer waits for this side to read a
//! request, and both socket buffers are full.
//!
//! # No client waits on the writer of another
//!
//! A socket connection ([`Outbound::Queued`]) also queues the frames it writes, so a
//! send never waits for the peer. In the daemon, a worker can hold a database handle
//! while it sends, and a write to the shared database waits for every handle. A send
//! that waited on a client that does not read would then stop every client.
//!
//! The stdio of `oxabl lsp` keeps a rendezvous writer ([`Outbound::Rendezvous`]): a
//! send returns when the writer has taken the frame, after it wrote the one before.
//! Only one thread sends there, so a slow editor slows only its own frames.
//!
//! # What bounds the queues
//!
//! The inbound queue holds frames the peer sent and the consumer has not taken. The
//! outbound queue holds frames this side sent and the peer has not read. A peer that
//! reads its answers keeps both short. A peer that stops reading fills one of them,
//! so each side counts the bytes of the frames that wait (as they are on the wire,
//! not their size in memory) and closes the connection when they pass
//! [`HIGH_WATER_BYTES`]. A test build reads a lower mark from
//! [`TEST_HIGH_WATER_VARIABLE`].
//!
//! A failed write ends the connection too: the peer is gone or does not read. The
//! connection is closed, so the reader stops and the consumer sees the input end.
//! Frames that the consumer still sends are dropped, so nothing collects.

use std::collections::VecDeque;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, PoisonError};
use std::thread;
use std::time::{Duration, Instant};

use crossbeam_channel::{Receiver, Select, Sender, TryRecvError, bounded, unbounded};
use lsp_server::{Connection, Message};

/// The bytes of frames that may wait in one queue of a connection before the
/// connection is closed.
pub const HIGH_WATER_BYTES: usize = 256 << 20;

/// The variable that sets another high-water mark, in bytes, in test builds only.
pub const TEST_HIGH_WATER_VARIABLE: &str = "OXABL_TEST_QUEUE_LIMIT";

/// How long [`Threads::finish`] lets the writer write the frames already sent.
const WRITE_GRACE: Duration = Duration::from_secs(2);

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

/// How a connection writes the frames sent to it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outbound {
    /// A send waits until the writer takes the frame.
    Rendezvous,
    /// A send never waits; the frames wait in a queue with a high-water mark.
    Queued,
}

/// Ends a connection: makes the peer, the reader and the writer see the disconnect.
pub type Close = Arc<dyn Fn() + Send + Sync>;

/// The framing threads of one connection.
pub struct Threads {
    reader: Option<thread::JoinHandle<()>>,
    writer: Option<thread::JoinHandle<()>>,
    queue: Option<QueueThreads>,
    /// Set when this side closed the connection because a queue passed the mark.
    over_limit: Arc<AtomicBool>,
}

/// The extra state of an [`Outbound::Queued`] connection.
struct QueueThreads {
    forwarder: thread::JoinHandle<()>,
    /// Sees whether frames still wait to go to the forwarder.
    sent: Receiver<Message>,
    progress: Arc<Progress>,
}

/// What the forwarder and the writer of a queued connection have not finished.
#[derive(Default)]
struct Progress {
    /// Bytes of frames the forwarder made and the writer has not written.
    waiting: AtomicUsize,
    /// Held while the forwarder takes a frame from the channel and counts it, so
    /// that [`Threads::finish`] never looks between the two.
    gate: Mutex<()>,
}

impl Threads {
    /// Threads that have already ended.
    pub fn ended() -> Threads {
        Threads {
            reader: None,
            writer: None,
            queue: None,
            over_limit: Arc::default(),
        }
    }

    /// Whether this side closed the connection because a queue passed the mark. The
    /// flag is shared, so it can be read after the threads have moved.
    pub fn over_limit(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.over_limit)
    }

    /// Wait for every thread, so a response is not cut off by an early return.
    pub fn join(self) {
        if let Some(queue) = self.queue {
            let _ = queue.forwarder.join();
        }
        for handle in [self.writer, self.reader].into_iter().flatten() {
            let _ = handle.join();
        }
    }

    /// Wait, for [`WRITE_GRACE`] at most, until the frames already sent are written
    /// or the writer has stopped. A rendezvous writer has taken every frame already.
    pub fn finish(&self) {
        let Some(queue) = &self.queue else { return };
        let deadline = Instant::now() + WRITE_GRACE;
        while Instant::now() < deadline && !self.writer.as_ref().is_some_and(|w| w.is_finished()) {
            {
                let _gate = queue
                    .progress
                    .gate
                    .lock()
                    .unwrap_or_else(PoisonError::into_inner);
                if queue.sent.is_empty() && queue.progress.waiting.load(Ordering::SeqCst) == 0 {
                    return;
                }
            }
            thread::sleep(Duration::from_millis(5));
        }
    }
}

/// Start the reader and the writer of a connection over `input` and `output`.
///
/// `close` runs when a queue passes the high-water mark. `peer` names the other side
/// in the log line that says so.
pub fn spawn<R, W>(
    input: R,
    output: W,
    outbound: Outbound,
    close: Close,
    peer: String,
) -> io::Result<(Connection, Threads)>
where
    R: Read + Send + 'static,
    W: Write + Send + 'static,
{
    let limit = high_water();
    let over_limit = Arc::new(AtomicBool::new(false));
    // What a queue past the mark runs: it also tells the owner that this side did it.
    let close_over_limit: Close = {
        let close = Arc::clone(&close);
        let over_limit = Arc::clone(&over_limit);
        Arc::new(move || {
            over_limit.store(true, Ordering::SeqCst);
            close();
        })
    };
    let (inbound_tx, inbound_rx) = unbounded();
    let reader = thread::Builder::new()
        .name("oxabl-conn-read".to_string())
        .spawn({
            let close = Arc::clone(&close_over_limit);
            let peer = peer.clone();
            move || read_frames(BufReader::new(input), &inbound_tx, limit, &*close, &peer)
        })?;

    let (outbound_tx, writer, queue) = match outbound {
        Outbound::Rendezvous => {
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
            (outbound_tx, writer, None)
        }
        Outbound::Queued => {
            let (outbound_tx, outbound_rx) = unbounded::<Message>();
            let (bytes_tx, bytes_rx) = unbounded::<Vec<u8>>();
            let progress = Arc::new(Progress::default());
            let writer = thread::Builder::new()
                .name("oxabl-conn-write".to_string())
                .spawn({
                    let progress = Arc::clone(&progress);
                    let close = Arc::clone(&close);
                    let unhandled = inbound_rx.clone();
                    move || write_bytes(output, &bytes_rx, &progress, &*close, &unhandled)
                })?;
            let forwarder = thread::Builder::new()
                .name("oxabl-conn-queue".to_string())
                .spawn({
                    let outbound_rx = outbound_rx.clone();
                    let progress = Arc::clone(&progress);
                    move || {
                        forward_frames(
                            &outbound_rx,
                            &bytes_tx,
                            &progress,
                            limit,
                            &*close_over_limit,
                            &peer,
                        )
                    }
                })?;
            let queue = QueueThreads {
                forwarder,
                sent: outbound_rx,
                progress,
            };
            (outbound_tx, writer, Some(queue))
        }
    };

    Ok((
        Connection {
            sender: outbound_tx,
            receiver: inbound_rx,
        },
        Threads {
            reader: Some(reader),
            writer: Some(writer),
            queue,
            over_limit,
        },
    ))
}

/// Write a log line of the daemon. The daemon's stderr may be a pipe whose reader
/// has gone (see `crate::stderr`), so a failed write is ignored.
fn log(line: std::fmt::Arguments<'_>) {
    let _ = writeln!(io::stderr(), "oxabl: {line}");
}

/// Read frames into `queue` until the peer closes, sends `exit`, sends a frame the
/// framing cannot read, or lets the frames that wait pass `limit`.
fn read_frames<R: BufRead>(
    input: R,
    queue: &Sender<Message>,
    limit: usize,
    close: &dyn Fn(),
    peer: &str,
) {
    let mut input = Counting {
        inner: input,
        read: 0,
    };
    // The size of each frame still in the queue, oldest first. The queue is FIFO, so
    // the frames the consumer took are the oldest ones.
    let mut sizes = VecDeque::new();
    let mut queued = 0usize;
    while let Ok(Some(message)) = Message::read(&mut input) {
        let is_exit = matches!(&message, Message::Notification(n) if n.method == "exit");
        while sizes.len() > queue.len() {
            queued -= sizes.pop_front().unwrap_or(0);
        }
        // Only frames that wait count. One large frame on an empty queue is already
        // read, so refusing it saves nothing.
        if queued > limit {
            close();
            log(format_args!(
                "closing the connection of {peer}: {queued} bytes of frames from it were \
                 not handled in time"
            ));
            return;
        }
        let size = std::mem::take(&mut input.read);
        sizes.push_back(size);
        queued += size;
        if queue.send(message).is_err() || is_exit {
            return;
        }
    }
}

/// Turn each frame sent into bytes for the writer, until every sender is gone or the
/// bytes that wait pass `limit`. After the writer or the mark ended the work, the
/// frames that are still sent are dropped, so they do not collect.
fn forward_frames(
    frames: &Receiver<Message>,
    bytes: &Sender<Vec<u8>>,
    progress: &Progress,
    limit: usize,
    close: &dyn Fn(),
    peer: &str,
) {
    forward(frames, bytes, progress, limit, close, peer);
    // The workers still in flight send their answers here until teardown. Drop them,
    // so they do not collect.
    for _dropped in frames {}
}

fn forward(
    frames: &Receiver<Message>,
    bytes: &Sender<Vec<u8>>,
    progress: &Progress,
    limit: usize,
    close: &dyn Fn(),
    peer: &str,
) {
    let mut select = Select::new();
    select.recv(frames);
    loop {
        // Wait without taking a frame, so the gate covers the take and the count.
        select.ready();
        let over_limit = {
            let _gate = progress.gate.lock().unwrap_or_else(PoisonError::into_inner);
            let message = match frames.try_recv() {
                Ok(message) => message,
                Err(TryRecvError::Empty) => continue,
                Err(TryRecvError::Disconnected) => return,
            };
            let waiting = progress.waiting.load(Ordering::SeqCst);
            if waiting > limit {
                Some(waiting)
            } else {
                let mut frame = Vec::new();
                if message.write(&mut frame).is_err() {
                    continue;
                }
                progress.waiting.fetch_add(frame.len(), Ordering::SeqCst);
                if bytes.send(frame).is_err() {
                    return;
                }
                None
            }
        };
        // Outside the gate: a log write to a full stderr pipe can block, and
        // `Threads::finish` must not wait on the gate for longer than its grace.
        if let Some(waiting) = over_limit {
            close();
            log(format_args!(
                "closing the connection of {peer}: {waiting} bytes of frames for it were \
                 not written, because it does not read them"
            ));
            return;
        }
    }
}

/// Write each frame's bytes until the forwarder ends or a write fails. A failed
/// write ends the connection: the peer is gone, so the requests that wait for the
/// consumer are dropped until the reader ends, and no answer is made for them.
fn write_bytes<W: Write>(
    mut output: W,
    bytes: &Receiver<Vec<u8>>,
    progress: &Progress,
    close: &dyn Fn(),
    unhandled: &Receiver<Message>,
) {
    for frame in bytes {
        if output
            .write_all(&frame)
            .and_then(|()| output.flush())
            .is_err()
        {
            close();
            for _dropped in unhandled {}
            return;
        }
        progress.waiting.fetch_sub(frame.len(), Ordering::SeqCst);
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

/// A connection over this process's stdin and stdout, with a rendezvous writer.
///
/// It replaces `Connection::stdio`, whose reader waits on the consumer for each
/// frame. When the inbound queue passes the high-water mark, stdin becomes
/// `/dev/null`: the peer's next write fails, and the consumer sees the input end.
pub fn stdio() -> io::Result<(Connection, Threads)> {
    let close: Close = Arc::new(|| {
        #[cfg(unix)]
        if let Ok(null) = std::fs::File::open("/dev/null") {
            let _ = rustix::stdio::dup2_stdin(&null);
        }
    });
    spawn(
        io::stdin(),
        io::stdout(),
        Outbound::Rendezvous,
        close,
        "the peer on stdin".to_string(),
    )
}
