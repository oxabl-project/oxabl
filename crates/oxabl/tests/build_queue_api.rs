//! The ready queue of the build scheduler through the public `oxabl::build`
//! API: an idle worker takes the ready job of highest priority, and a job that
//! a completion made ready is in the queue before any worker takes its next job.

use std::sync::Arc;
use std::sync::mpsc::channel;
use std::thread;
use std::time::Duration;

use oxabl::build::schedule::{Queue, Take};

/// Starts a worker thread that takes one job, and returns what it took.
fn taker(queue: &Arc<Queue>) -> std::sync::mpsc::Receiver<Take> {
    let (tx, rx) = channel();
    let queue = Arc::clone(queue);
    thread::spawn(move || {
        let _ = tx.send(queue.take());
    });
    rx
}

#[test]
fn no_job_goes_out_while_a_reported_completion_is_unhandled() {
    let queue = Arc::new(Queue::new());
    queue.push_all([(0, 1, "a.p".to_string())]);
    queue.completion_sent();

    let taken = taker(&queue);

    assert!(
        taken.recv_timeout(Duration::from_millis(300)).is_err(),
        "a worker took a job before the coordinator handled the completion"
    );
    queue.completion_handled();
    assert_eq!(
        taken.recv_timeout(Duration::from_secs(10)).unwrap(),
        Take::Job(0)
    );
}

#[test]
fn a_job_that_the_completion_made_ready_goes_before_a_job_of_lower_priority() {
    let queue = Arc::new(Queue::new());
    queue.push_all([(0, 1, "a.p".to_string())]);
    queue.completion_sent();
    let taken = taker(&queue);
    thread::sleep(Duration::from_millis(50));

    queue.push_all([(1, 3, "pkg/User.cls".to_string())]);
    queue.completion_handled();

    assert_eq!(
        taken.recv_timeout(Duration::from_secs(10)).unwrap(),
        Take::Job(1)
    );
}

#[test]
fn jobs_that_become_ready_together_go_out_in_priority_order_whatever_their_push_order() {
    let queue = Arc::new(Queue::new());
    let taken = taker(&queue);
    thread::sleep(Duration::from_millis(50));

    queue.push_all(
        [(0, 1, "a.p"), (1, 3, "pkg/Base.cls"), (2, 2, "b.p")]
            .into_iter()
            .map(|(job, priority, path)| {
                thread::sleep(Duration::from_millis(100));
                (job, priority, path.to_string())
            }),
    );

    assert_eq!(
        taken.recv_timeout(Duration::from_secs(10)).unwrap(),
        Take::Job(1)
    );
}
