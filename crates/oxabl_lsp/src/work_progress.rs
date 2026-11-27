//! Work-done progress for a request that waits for the workspace pass or for
//! the subtype index (`$/progress`). See `docs/design/type-hierarchy.md`.
//!
//! The report starts after [`DELAY`], so a request that waits a moment shows
//! nothing. It uses the `workDoneToken` that the request carries. When the
//! request carries none, and the client said that it supports
//! `window/workDoneProgress/create`, the report asks the client for a token and
//! starts when the client has agreed. The request thread never blocks for the
//! answer: it looks again at each poll.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use lsp_server::{Message, Notification, Request, RequestId, Response};
use lsp_types::{
    NumberOrString, ProgressParams, ProgressParamsValue, ProgressToken, WorkDoneProgress,
    WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
    WorkDoneProgressReport,
};
use oxabl_daemon::hierarchy::Progress;

/// How long a request waits before the client hears about it.
const DELAY: Duration = Duration::from_millis(250);

/// The title of every report.
const TITLE: &str = "oxabl";

/// The `window/workDoneProgress/create` requests that the client has not
/// answered, by id.
#[derive(Default)]
pub(crate) struct Creations {
    next: AtomicU64,
    pending: Mutex<HashMap<RequestId, Sender<bool>>>,
}

impl Creations {
    fn ask(&self) -> (ProgressToken, RequestId, Receiver<bool>) {
        let n = self.next.fetch_add(1, Ordering::Relaxed);
        let (sender, receiver) = mpsc::channel();
        let id = RequestId::from(format!("oxabl-progress-{n}"));
        self.lock().insert(id.clone(), sender);
        (
            NumberOrString::String(format!("oxabl/progress/{n}")),
            id,
            receiver,
        )
    }

    fn forget(&self, id: &RequestId) {
        self.lock().remove(id);
    }

    /// Route the client's answer to the report that asked.
    pub(crate) fn answered(&self, response: &Response) {
        if let Some(sender) = self.lock().remove(&response.id) {
            let _ = sender.send(response.error.is_none());
        }
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<RequestId, Sender<bool>>> {
        self.pending
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

enum State {
    /// Nothing shown yet.
    Waiting,
    /// The client was asked for a token and has not answered.
    Creating {
        token: ProgressToken,
        id: RequestId,
        answer: Receiver<bool>,
    },
    /// The report is open on `token`, and `last` is the message it shows.
    Open { token: ProgressToken, last: String },
    /// The client takes no report, or has ended it.
    Off,
}

/// The progress of one request.
pub(crate) struct Report {
    sender: crossbeam_channel::Sender<Message>,
    creations: Arc<Creations>,
    /// The token the request carried.
    token: Option<ProgressToken>,
    /// Whether the client takes `window/workDoneProgress/create`.
    can_create: bool,
    started: Instant,
    state: Mutex<State>,
}

impl Report {
    pub(crate) fn new(
        sender: crossbeam_channel::Sender<Message>,
        creations: Arc<Creations>,
        token: Option<ProgressToken>,
        can_create: bool,
    ) -> Report {
        Report {
            sender,
            creations,
            token,
            can_create,
            started: Instant::now(),
            state: Mutex::new(State::Waiting),
        }
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, State> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn notify(&self, token: &ProgressToken, value: WorkDoneProgress) {
        let _ = self.sender.send(Message::Notification(Notification::new(
            "$/progress".to_string(),
            ProgressParams {
                token: token.clone(),
                value: ProgressParamsValue::WorkDone(value),
            },
        )));
    }

    fn begin(&self, token: ProgressToken, message: String, percentage: Option<u32>) -> State {
        self.notify(
            &token,
            WorkDoneProgress::Begin(WorkDoneProgressBegin {
                title: TITLE.to_string(),
                cancellable: Some(false),
                message: Some(message.clone()),
                percentage,
            }),
        );
        State::Open {
            token,
            last: message,
        }
    }

    /// Show `message`, after the delay, if the client takes a report.
    fn show(&self, message: String, percentage: Option<u32>) {
        let mut state = self.lock();
        let current = std::mem::replace(&mut *state, State::Off);
        *state = match current {
            State::Waiting if self.started.elapsed() < DELAY => State::Waiting,
            State::Waiting => match (&self.token, self.can_create) {
                (Some(token), _) => self.begin(token.clone(), message, percentage),
                (None, true) => {
                    let (token, id, answer) = self.creations.ask();
                    let _ = self.sender.send(Message::Request(Request::new(
                        id.clone(),
                        "window/workDoneProgress/create".to_string(),
                        WorkDoneProgressCreateParams {
                            token: token.clone(),
                        },
                    )));
                    State::Creating { token, id, answer }
                }
                (None, false) => State::Off,
            },
            State::Creating { token, id, answer } => match answer.try_recv() {
                Ok(true) => self.begin(token, message, percentage),
                Ok(false) | Err(TryRecvError::Disconnected) => {
                    self.creations.forget(&id);
                    State::Off
                }
                Err(TryRecvError::Empty) => State::Creating { token, id, answer },
            },
            State::Open { token, last } if last != message => {
                self.notify(
                    &token,
                    WorkDoneProgress::Report(WorkDoneProgressReport {
                        cancellable: Some(false),
                        message: Some(message.clone()),
                        percentage,
                    }),
                );
                State::Open {
                    token,
                    last: message,
                }
            }
            other => other,
        };
    }

    /// End the report, if one is open. Call it before the answer goes out.
    pub(crate) fn end(&self) {
        let mut state = self.lock();
        match std::mem::replace(&mut *state, State::Off) {
            State::Open { token, .. } => {
                self.notify(
                    &token,
                    WorkDoneProgress::End(WorkDoneProgressEnd { message: None }),
                );
            }
            State::Creating { id, .. } => self.creations.forget(&id),
            State::Waiting | State::Off => {}
        }
    }
}

fn percentage(done: usize, total: usize) -> Option<u32> {
    (total > 0).then(|| (done.min(total) * 100 / total) as u32)
}

impl Progress for Report {
    fn waiting_for_pass(&self, indexed: u32, total: u32) {
        self.show(
            format!("Analysing the workspace: {indexed} of {total} files"),
            percentage(indexed as usize, total as usize),
        );
    }

    fn reading_classes(&self, read: usize, total: usize) {
        let message = if total == 0 {
            "Reading class files".to_string()
        } else {
            format!("Reading class files: {read} of {total}")
        };
        self.show(message, percentage(read, total));
    }
}

/// Ends the report when the request ends, also by a panic.
pub(crate) struct Ending(pub(crate) Arc<Report>);

impl Drop for Ending {
    fn drop(&mut self) {
        self.0.end();
    }
}
