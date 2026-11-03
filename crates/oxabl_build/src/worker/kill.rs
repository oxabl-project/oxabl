//! Stopping worker processes and everything in their process group.

use std::process::Child;
use std::time::{Duration, Instant};

use rustix::process::{Pid, Signal as RustixSignal, kill_process_group};

pub enum Signal {
    Term,
    Kill,
}

/// Sends `signal` to the process group `pgid`; an empty group is not an error.
pub fn signal_group(pgid: u32, signal: Signal) {
    let Some(pid) = i32::try_from(pgid).ok().and_then(Pid::from_raw) else {
        return;
    };
    let signal = match signal {
        Signal::Term => RustixSignal::TERM,
        Signal::Kill => RustixSignal::KILL,
    };
    let _ = kill_process_group(pid, signal);
}

/// Waits for `child` to exit; true when it did within `timeout`.
pub fn wait_exit(child: &mut Child, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    loop {
        match child.try_wait() {
            Ok(Some(_)) => return true,
            Ok(None) => {}
            Err(_) => return true,
        }
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

/// `SIGTERM` to the group, `SIGKILL` after `grace`, then reaps the child.
pub fn terminate(child: &mut Child, grace: Duration) {
    if matches!(child.try_wait(), Ok(Some(_))) {
        return;
    }
    signal_group(child.id(), Signal::Term);
    if !wait_exit(child, grace) {
        signal_group(child.id(), Signal::Kill);
        let _ = child.kill();
        let _ = child.wait();
    }
}
