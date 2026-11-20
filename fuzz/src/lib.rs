//! What the fuzz targets share.

use std::sync::mpsc::{Receiver, Sender, channel};
use std::sync::{Mutex, OnceLock};

type Job = (Vec<u8>, fn(&[u8]));

struct Worker {
    jobs: Sender<Job>,
    done: Receiver<()>,
}

static WORKER: OnceLock<Mutex<Worker>> = OnceLock::new();

/// Run `check` on `data` with a stack of at least `oxabl_common::stack::SIZE`,
/// as the CLI and the daemon have: a smaller stack can overflow on the deepest
/// tree the parser accepts, and that shows as a false crash.
///
/// `fuzz/run.sh` raises `ulimit -s` so that libFuzzer's own (main) thread is
/// large enough, and `check` runs directly. Otherwise, for example for a target
/// started by hand, one worker thread on the analysis stack serves every input.
/// The hop to that thread costs about 0.5 ms per input, and a thread per input
/// made the targets about 20 times slower.
pub fn on_analysis_stack(data: &[u8], check: fn(&[u8])) {
    static LARGE_MAIN_STACK: OnceLock<bool> = OnceLock::new();
    if *LARGE_MAIN_STACK.get_or_init(main_stack_is_large) {
        return check(data);
    }
    let worker = WORKER.get_or_init(|| {
        let (jobs, inbox) = channel::<Job>();
        let (finished, done) = channel();
        oxabl_common::stack::builder()
            .name("analysis".to_string())
            .spawn(move || {
                for (data, check) in inbox {
                    check(&data);
                    if finished.send(()).is_err() {
                        return;
                    }
                }
            })
            .expect("failed to spawn the analysis thread");
        Mutex::new(Worker { jobs, done })
    });
    let worker = worker.lock().expect("an earlier input panicked");
    worker
        .jobs
        .send((data.to_vec(), check))
        .expect("the analysis thread ended");
    worker.done.recv().expect("the analysis thread ended");
}

/// Whether `RLIMIT_STACK`, the most the main thread's stack can grow to, is at
/// least the analysis stack.
fn main_stack_is_large() -> bool {
    #[repr(C)]
    struct Rlimit {
        current: u64,
        maximum: u64,
    }
    const RLIMIT_STACK: i32 = 3;
    const RLIM_INFINITY: u64 = u64::MAX;
    unsafe extern "C" {
        fn getrlimit(resource: i32, limit: *mut Rlimit) -> i32;
    }
    let mut limit = Rlimit {
        current: 0,
        maximum: 0,
    };
    // SAFETY: `limit` is a valid `struct rlimit` (two `rlim_t`, 64-bit on
    // Linux and macOS, where RLIMIT_STACK is 3).
    let read = unsafe { getrlimit(RLIMIT_STACK, &mut limit) } == 0;
    read && (limit.current == RLIM_INFINITY || limit.current >= oxabl_common::stack::SIZE as u64)
}
