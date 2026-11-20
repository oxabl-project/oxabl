// Coalescing of build requests for one root, the way rust-analyzer's flycheck
// and watch-mode compilers do it:
//
// - a trailing debounce with a maximum wait, so a constant stream of writes
//   cannot starve the build;
// - single flight: one running build and at most one pending request. A
//   request while a build runs sends that build one `SIGINT`; the pending
//   build starts when it has exited and the debounce has settled.
//
// Like `server.ts`, this module is free of any `vscode` import; the timers
// are the global ones, so tests drive it with fake timers.

export const MAX_WAIT_MS = 5_000;
const BACKOFF_START_MS = 1_000;
const BACKOFF_MAX_MS = 30_000;

export interface SchedulerHooks {
  /** Start a build. `exited` must be called when it ends. */
  start(): void;
  /** Send the running build one `SIGINT`. */
  cancel(): void;
  /** The debounce in milliseconds, read at each request. */
  debounceMs(): number;
}

export class Scheduler {
  private running = false;
  private cancelSent = false;
  private dirty = false;
  private timer: NodeJS.Timeout | undefined;
  private burstStart = 0;
  private preemptions = 0;

  constructor(private readonly hooks: SchedulerHooks) {}

  /** A file changed: a build is wanted. `slackMs` lengthens the debounce of this request. */
  request(slackMs = 0): void {
    this.dirty = true;
    this.cancelRunning();
    const now = Date.now();
    if (!this.timer) {
      this.burstStart = now;
    }
    const wait = Math.max(0, Math.min(this.hooks.debounceMs() + slackMs, this.burstStart + MAX_WAIT_MS - now));
    this.arm(wait);
  }

  /** A build is wanted now, without the debounce (the Build command). */
  requestNow(): void {
    this.dirty = true;
    this.cancelRunning();
    this.arm(0);
  }

  /** The running build ended, whatever the exit code. */
  exited(): void {
    this.running = false;
    this.cancelSent = false;
    this.preemptions = 0;
    this.startWhenReady();
  }

  /** The running build was stopped by another process: ask for one more, after a growing pause. */
  preempted(): void {
    this.running = false;
    this.cancelSent = false;
    this.dirty = true;
    const wait = Math.min(BACKOFF_MAX_MS, BACKOFF_START_MS * 2 ** this.preemptions);
    this.preemptions += 1;
    this.arm(wait);
  }

  /** Drop the pending request and stop the running build. */
  cancelAll(): void {
    this.dirty = false;
    clearTimeout(this.timer);
    this.timer = undefined;
    this.cancelRunning();
  }

  isRunning(): boolean {
    return this.running;
  }

  private cancelRunning(): void {
    if (this.running && !this.cancelSent) {
      this.cancelSent = true;
      this.hooks.cancel();
    }
  }

  private arm(ms: number): void {
    clearTimeout(this.timer);
    this.timer = setTimeout(() => {
      this.timer = undefined;
      this.startWhenReady();
    }, ms);
  }

  private startWhenReady(): void {
    if (this.dirty && !this.timer && !this.running) {
      this.dirty = false;
      this.running = true;
      this.hooks.start();
    }
  }
}
