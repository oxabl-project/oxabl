//! The message loop, over any connection the LSP framing can carry.
//!
//! Transport-agnostic on purpose. This unit serves stdio, which is what the editor
//! reaches today; sockets and a multi-client accept loop arrive next and drive the
//! same loop per accepted client. Keeping the loop free of the listener is what lets
//! the second transport be a second caller rather than a second loop.
//!
//! # One request's failure is one request's failure
//!
//! Every request goes through [`Dispatch::call`], which contains a panic and reports
//! it as that request's error. Nothing here unwinds past a single response, so a file
//! that panics the analysis costs the query that reached it and leaves the daemon and
//! every other client serving.

use lsp_server::{Connection, Message, Response};
use serde_json::Value;

use crate::dispatch::Dispatch;
use crate::session::Sessions;

/// Serve `connection` until the peer disconnects or asks to shut down.
///
/// Returns whether the shutdown was clean — the peer sent `shutdown` before it
/// stopped talking — so a caller can distinguish an orderly close from a client that
/// died mid-session.
pub fn serve(connection: &Connection, dispatch: &Dispatch, sessions: &mut Sessions) -> bool {
    let mut shutdown_requested = false;
    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if request.method == "shutdown" {
                    shutdown_requested = true;
                    let _ = connection
                        .sender
                        .send(Response::new_ok(request.id, Value::Null).into());
                    continue;
                }
                let params = request.params;
                let response = match dispatch.call(sessions, &request.method, params) {
                    Ok(result) => Response::new_ok(request.id, result),
                    // A reported failure, not a dropped request: a client waiting on
                    // a response that never arrives is the one outcome worse than an
                    // error, because nothing times it out.
                    Err(error) => Response::new_err(request.id, error.code, error.message),
                };
                let _ = connection.sender.send(response.into());
            }
            Message::Notification(notification) => {
                if notification.method == "exit" {
                    return shutdown_requested;
                }
                // A notification has no id, so there is nowhere to report a failure
                // to. Unknown ones are ignored, which is what the protocol requires.
                let _ = dispatch.call(sessions, &notification.method, notification.params);
            }
            // The daemon issues no requests of its own yet, so a response is
            // something no peer should be sending.
            Message::Response(_) => {}
        }
    }
    shutdown_requested
}

/// Serve stdio: the transport the editor reaches the session core through today.
///
/// Joins the framing threads before returning, so a caller that exits immediately
/// afterwards does not truncate the last response.
pub fn serve_stdio(dispatch: &Dispatch, sessions: &mut Sessions) -> anyhow::Result<bool> {
    let (connection, io_threads) = Connection::stdio();
    let clean = serve(&connection, dispatch, sessions);
    io_threads.join()?;
    Ok(clean)
}
