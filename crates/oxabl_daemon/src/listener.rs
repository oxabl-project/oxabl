//! The Unix socket listener and its own accept loop (KTD17, KTD18).
//!
//! # Why the daemon owns the loop
//!
//! The transport crate's `Connection::listen` binds and accepts **exactly once**,
//! and its socket transport is crate-private and TCP-only. What *is* public is what
//! matters: `Message::read`, `Message::write`, and `Connection`'s `sender` and
//! `receiver` fields. So the daemon keeps the LSP framing and the request ids and
//! owns the listener, one reader/writer thread pair per accepted client.
//!
//! # Why a Unix socket
//!
//! No port to allocate, and filesystem permissions do the access control: the
//! socket is bound inside the 0700 registration directory and chmodded 0600, so it
//! is reachable only by the uid that owns it. See `registry` for why the directory
//! rather than the file mode is the load-bearing half. Windows named pipes are
//! deferred; the desktop app targets Linux first.
//!
//! # One client's slowness is one client's slowness
//!
//! A thread per client, and the session lock is held only to write or to clone a
//! snapshot — never across a query. A handler that respects that leaves every other
//! client free; one that queries under the lock serialises the daemon, which is why
//! the rule is stated on [`SessionHost`] rather than left to habit.

use std::io::{self, BufReader};
use std::net::Shutdown;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;

use crossbeam_channel::{Receiver, Sender, bounded};
use lsp_server::{Connection, Message};

use crate::dispatch::Dispatch;
use crate::registry;
use crate::server;
use crate::session::SessionHost;

/// One routed client service behind the shared listener.
pub type ConnectionService = dyn Fn(&Connection, &SessionHost) + Send + Sync;

/// A bound listener for one workspace root, with its registration written.
pub struct Listener {
    listener: UnixListener,
    socket_path: PathBuf,
    workspace_root: PathBuf,
    /// Set when the loop should stop accepting, so a test — or a shutdown request —
    /// can end the loop without killing the process.
    stopping: Arc<AtomicBool>,
    /// Exclusive ownership of this root, held for exactly as long as the listener.
    ///
    /// Never read. It exists to be dropped: releasing it is what lets the next
    /// daemon start, and the kernel does that for us if this process is killed.
    _lock: registry::RootLock,
}

impl Listener {
    /// Bind the socket for `workspace_root` and register this process as its daemon.
    ///
    /// A leftover socket file is removed first, but **only after the socket itself
    /// says nobody is listening on it**: a socket whose owner is alive belongs to that
    /// daemon, and unlinking it would leave the running daemon unreachable while this
    /// one served a second index over the same workspace — exactly the duplication
    /// the daemon exists to prevent. So a socket that answers is refused rather than
    /// stolen, and the check is made *after* the lock is held, because the lock is
    /// what stops a rival from binding between the probe and the bind.
    pub fn bind(workspace_root: impl AsRef<Path>) -> io::Result<Self> {
        let workspace_root = workspace_root.as_ref().to_path_buf();

        // The lock, before anything observable is touched. `discover` alone is a
        // check without a claim: two daemons could both see `Absent`, both unlink
        // the socket path, and — because unlinking makes `EADDRINUSE` impossible —
        // both bind. One would then be orphaned, and on exit its unconditional
        // cleanup would delete the winner's registration and socket, leaving a live
        // daemon nothing could reach and a client that starts a third.
        //
        // Holding the lock for the listener's lifetime makes the rest of this
        // function safe: no rival can be between the unlink and the bind.
        // A lock this call could not *open* is a different failure from a lock
        // somebody holds, and `acquire_root_lock` keeps them apart: only the second
        // is `Ok(None)`. So the refusal below is never printed over a transient lock
        // failure — that one propagates with its own cause.
        let Some(lock) = registry::acquire_root_lock(&workspace_root)? else {
            // The lock says somebody holds the root. It does not say they are serving
            // yet, and the two are different sentences: a refusal that claims a daemon
            // already serves the workspace sends its reader looking for a daemon that
            // may still be binding. So the state is probed and reported as observed.
            let refusal = match registry::daemon_state(&workspace_root) {
                registry::DaemonState::Running(existing)
                | registry::DaemonState::Saturated(existing)
                | registry::DaemonState::Undecided(existing) => format!(
                    "a daemon (pid {}) already serves {}",
                    existing.pid,
                    workspace_root.display()
                ),
                registry::DaemonState::Starting(existing) => format!(
                    "a daemon (pid {}) is starting for {} and holds the start lock. \
                     Wait for it rather than starting a second one.",
                    existing.pid,
                    workspace_root.display()
                ),
                // The holder has the lock and has registered nothing readable yet, so
                // there is no pid to name and claiming one would be a guess.
                registry::DaemonState::Crashed(_) | registry::DaemonState::Absent => format!(
                    "another process holds the start lock for {} and has not \
                     registered a daemon yet. Wait for it, or remove {} if you are \
                     sure nothing is starting.",
                    workspace_root.display(),
                    registry::lock_path_for(&workspace_root).display()
                ),
            };
            return Err(io::Error::new(io::ErrorKind::AddrInUse, refusal));
        };

        let socket_path = registry::socket_path_for(&workspace_root);
        // The naming rule budgets for `sun_path`, so a failure here means the
        // registration directory itself is too long — which no name can rescue.
        // Reported with the limit and the path, rather than as a bare
        // `ENAMETOOLONG` from `bind` with neither.
        oxabl_daemon_protocol::check_socket_path_fits(&socket_path)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidInput, error))?;
        // A 0700 directory is the access control, and it is the race-free half:
        // the socket is unreachable from the moment it exists, because nobody else
        // can traverse into the directory to reach it.
        registry::ensure_registration_dir()?;
        // The lock is ours, and it is still not a licence to unlink: between a
        // client's probe and its lock another process can take the lock, bind, start
        // serving and release nothing — and a socket bound by anything that skipped
        // the lock answers too. Unlinking one that answers would leave a live server
        // unreachable, so the socket is probed once more and a live one is refused.
        if registry::socket_is_answering(&socket_path) {
            return Err(io::Error::new(
                io::ErrorKind::AddrInUse,
                format!(
                    "something is already listening on {}, although no daemon holds \
                     the start lock for {}. Refusing to unlink a socket that answers: \
                     stop whatever owns it first.",
                    socket_path.display(),
                    workspace_root.display()
                ),
            ));
        }
        // Nothing answers it, so the previous owner is gone and its socket file is
        // debris. Removed through the checked path, which refuses to unlink anything
        // that is not a socket sitting in the verified registration directory.
        registry::remove_stale_socket(&socket_path)?;
        let listener = UnixListener::bind(&socket_path)?;
        // Belt and braces. On Linux the connect permission is the socket's write
        // bit, and `bind` takes the ambient umask — so this is the control that
        // still holds if the directory's mode is ever loosened from outside.
        #[cfg(unix)]
        let _ = std::fs::set_permissions(
            &socket_path,
            std::os::unix::fs::PermissionsExt::from_mode(0o600),
        );
        registry::register(&workspace_root, &socket_path, std::process::id())?;

        Ok(Listener {
            listener,
            socket_path,
            workspace_root,
            stopping: Arc::new(AtomicBool::new(false)),
            _lock: lock,
        })
    }

    pub fn socket_path(&self) -> &Path {
        &self.socket_path
    }

    pub fn workspace_root(&self) -> &Path {
        &self.workspace_root
    }

    /// A handle that ends the accept loop.
    pub fn stopper(&self) -> Stopper {
        Stopper {
            stopping: Arc::clone(&self.stopping),
            socket_path: self.socket_path.clone(),
        }
    }

    /// Accept clients until stopped, serving each on its own thread.
    ///
    /// `dispatch` and `host` are shared, which is what makes two clients on one
    /// workspace root share one session and one index.
    ///
    /// A client that disconnects mid-request ends its own thread and nothing else: the
    /// send fails, the loop over its receiver ends, and the session it was using stays
    /// exactly as it was for every other client.
    pub fn accept_loop(&self, dispatch: Arc<Dispatch>, host: Arc<SessionHost>) -> io::Result<()> {
        self.accept_loop_with(
            Arc::new(move |connection, host| {
                server::serve(connection, &dispatch, host);
            }),
            host,
        )
    }

    /// Accept clients and pass each complete framed connection to `serve`.
    ///
    /// The umbrella binary uses this hook to route an LSP `initialize` frame to
    /// the editor frontend and an `oxabl/handshake` frame to the query dispatch,
    /// while both routes retain this listener's one-client-per-thread isolation.
    pub fn accept_loop_with(
        &self,
        serve: Arc<ConnectionService>,
        host: Arc<SessionHost>,
    ) -> io::Result<()> {
        let mut clients = Vec::new();
        for stream in self.listener.incoming() {
            if self.stopping.load(Ordering::SeqCst) {
                break;
            }
            let stream = match stream {
                Ok(stream) => stream,
                // One failed accept is not a reason to stop serving the clients
                // already connected.
                Err(_) => continue,
            };
            // Every liveness probe is a real accepted connection that disconnects at
            // once (see `registry::socket_is_answering`), and a client poll loop makes
            // one every 20ms. Without this the finished handles accumulate for the
            // daemon's whole life — thousands of them for an editor that polled while
            // starting. The join below still waits for whatever is still serving.
            clients.retain(|client: &thread::JoinHandle<()>| !client.is_finished());
            let serve = Arc::clone(&serve);
            let host = Arc::clone(&host);
            clients.push(thread::spawn(move || {
                let (connection, threads) = connection_over(stream);
                serve(&connection, &host);
                // Dropping the sender lets the writer thread finish.
                drop(connection);
                threads.shutdown();
            }));
        }
        // Let every client finish in flight rather than cutting responses off.
        for client in clients {
            let _ = client.join();
        }
        Ok(())
    }
}

impl Drop for Listener {
    /// Remove the socket and the registration, so a crashed-looking registration is
    /// not left behind by an orderly exit.
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.socket_path);
        let _ = registry::unregister(&self.workspace_root);
    }
}

/// Ends an accept loop from another thread.
pub struct Stopper {
    stopping: Arc<AtomicBool>,
    socket_path: PathBuf,
}

impl Stopper {
    /// Stop accepting.
    ///
    /// The flag alone is not enough: `incoming()` blocks, so nothing would observe it
    /// until the next client arrived. A throwaway connection is made to wake the
    /// accept, which then sees the flag and breaks.
    pub fn stop(&self) {
        self.stopping.store(true, Ordering::SeqCst);
        let _ = UnixStream::connect(&self.socket_path);
    }
}

/// Handles for one client's framing threads.
pub struct ClientThreads {
    reader: thread::JoinHandle<()>,
    writer: thread::JoinHandle<()>,
    control: Option<UnixStream>,
}

impl ClientThreads {
    /// Wait for both threads, so a response is not cut off by an early return.
    pub fn join(self) {
        let _ = self.reader.join();
        let _ = self.writer.join();
    }

    /// Close both socket directions, then wait for the framing threads.
    ///
    /// Dropping the channel pair cannot wake a reader blocked on the socket. The
    /// control clone makes that read return, which lets both peers observe the
    /// disconnect and prevents a teardown deadlock.
    pub fn shutdown(mut self) {
        if let Some(stream) = self.control.take() {
            let _ = stream.shutdown(Shutdown::Both);
        }
        self.join();
    }
}

/// Build a [`Connection`] over `stream`, with a reader and a writer thread.
///
/// This is the part the transport crate keeps to itself for TCP. `Message::read` and
/// `Message::write` are public, and `Connection`'s channel fields are public, so the
/// framing is reused rather than reimplemented — only the socket type differs.
pub fn connection_over(stream: UnixStream) -> (Connection, ClientThreads) {
    let (write_half, control) = match (stream.try_clone(), stream.try_clone()) {
        (Ok(write_half), Ok(control)) => (write_half, control),
        // A stream that cannot be cloned cannot be served. Hand back a connection
        // whose channels are already closed so the caller's loop ends immediately,
        // rather than panicking a thread that is holding no lock.
        _ => return closed_connection(),
    };

    let (inbound_tx, inbound_rx): (Sender<Message>, Receiver<Message>) = bounded(0);
    let reader = thread::spawn(move || {
        let mut reader = BufReader::new(stream);
        // A malformed frame ends this client's session rather than the daemon: the
        // peer is speaking something the framing cannot read, and there is no id to
        // report an error against.
        while let Ok(Some(message)) = Message::read(&mut reader) {
            let is_exit = matches!(&message, Message::Notification(n) if n.method == "exit");
            if inbound_tx.send(message).is_err() {
                break;
            }
            if is_exit {
                break;
            }
        }
    });

    let (outbound_tx, outbound_rx): (Sender<Message>, Receiver<Message>) = bounded(0);
    let writer = thread::spawn(move || {
        let mut stream = write_half;
        for message in outbound_rx {
            // A write failure means the client is gone. Stop writing; the reader
            // will end too, and no other client is affected.
            if message.write(&mut stream).is_err() {
                break;
            }
        }
    });

    (
        Connection {
            sender: outbound_tx,
            receiver: inbound_rx,
        },
        ClientThreads {
            reader,
            writer,
            control: Some(control),
        },
    )
}

/// A connection whose channels are already closed, for the unserveable stream above.
fn closed_connection() -> (Connection, ClientThreads) {
    let (sender, _dead_rx) = bounded(0);
    let (_dead_tx, receiver) = bounded(0);
    (
        Connection { sender, receiver },
        ClientThreads {
            reader: thread::spawn(|| {}),
            writer: thread::spawn(|| {}),
            control: None,
        },
    )
}
