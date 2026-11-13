//! The workspace lock against a child process that another thread forked
//! while the lock was held: until the child execs, it holds a copy of every
//! descriptor of the process, the lock file's included.

use std::os::unix::process::CommandExt;
use std::process::Command;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

use oxabl_build::lock::{LockOutcome, try_acquire};

/// How long the child waits between its fork and its exec.
const BEFORE_EXEC: Duration = Duration::from_secs(3);

#[test]
fn a_released_lock_is_free_while_a_child_forked_under_it_has_not_yet_execed() {
    let state = tempfile::tempdir().unwrap();
    let LockOutcome::Held(lock) = try_acquire(state.path()) else {
        panic!("a new state directory has a free lock");
    };
    let (spawned, wait_for_spawn) = mpsc::channel();
    let spawner = thread::spawn(move || {
        let mut command = Command::new("true");
        // SAFETY: the closure runs in the child between fork and exec and
        // only sleeps.
        unsafe {
            command.pre_exec(|| {
                std::thread::sleep(BEFORE_EXEC);
                Ok(())
            });
        }
        // `spawn` returns only after the child's exec.
        let child = command.spawn();
        let _ = spawned.send(());
        child.unwrap().wait().unwrap()
    });
    // The fork happens at once; the exec only after the child's sleep.
    thread::sleep(Duration::from_millis(500));
    assert!(
        wait_for_spawn.try_recv().is_err(),
        "the child execed before the lock was released"
    );

    drop(lock);
    let again = try_acquire(state.path());

    assert!(
        matches!(again, LockOutcome::Held(_)),
        "the lock stays taken while a child that copied its descriptor has not execed"
    );
    drop(again);
    assert!(spawner.join().unwrap().success());
}
