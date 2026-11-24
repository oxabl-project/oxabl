import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { MAX_WAIT_MS, Scheduler } from "./scheduler";

function setup(debounce = 500) {
  const calls = { start: 0, cancel: 0 };
  const scheduler = new Scheduler({
    start: () => void calls.start++,
    cancel: () => void calls.cancel++,
    debounceMs: () => debounce,
  });
  return { calls, scheduler };
}

beforeEach(() => vi.useFakeTimers());
afterEach(() => vi.useRealTimers());

describe("Scheduler", () => {
  it("starts one build after the debounce, however many requests came", () => {
    const { calls, scheduler } = setup();
    for (let i = 0; i < 10; i++) {
      scheduler.request();
      vi.advanceTimersByTime(100);
    }
    expect(calls.start).toBe(0);
    vi.advanceTimersByTime(500);
    expect(calls).toEqual({ start: 1, cancel: 0 });
  });

  it("lengthens the debounce by the slack of a request", () => {
    const { calls, scheduler } = setup();
    scheduler.request(600);
    vi.advanceTimersByTime(1_099);
    expect(calls.start).toBe(0);
    vi.advanceTimersByTime(1);
    expect(calls.start).toBe(1);
  });

  it("starts a build at the maximum wait when requests never stop", () => {
    const { calls, scheduler } = setup();
    for (let t = 0; t < MAX_WAIT_MS + 1_000; t += 100) {
      scheduler.request();
      vi.advanceTimersByTime(100);
    }
    expect(calls.start).toBe(1);
  });

  it("sends the running build one SIGINT for a burst and starts one build after it", () => {
    const { calls, scheduler } = setup();
    scheduler.requestNow();
    vi.advanceTimersByTime(0);
    expect(calls.start).toBe(1);

    for (let i = 0; i < 200; i++) {
      scheduler.request();
      vi.advanceTimersByTime(10);
    }
    expect(calls.cancel).toBe(1);
    scheduler.exited();
    expect(calls.start).toBe(1);
    vi.advanceTimersByTime(500);
    expect(calls.start).toBe(2);
    scheduler.exited();
    vi.advanceTimersByTime(10_000);
    expect(calls).toEqual({ start: 2, cancel: 1 });
  });

  it("waits for the debounce after the cancelled build exited", () => {
    const { calls, scheduler } = setup();
    scheduler.requestNow();
    vi.advanceTimersByTime(0);
    scheduler.request();
    scheduler.exited();
    vi.advanceTimersByTime(499);
    expect(calls.start).toBe(1);
    vi.advanceTimersByTime(1);
    expect(calls.start).toBe(2);
  });

  it("does not start a second build while one runs", () => {
    const { calls, scheduler } = setup();
    scheduler.requestNow();
    vi.advanceTimersByTime(0);
    scheduler.request();
    vi.advanceTimersByTime(5_000);
    expect(calls.start).toBe(1);
  });

  it("asks for one more build after a preemption, with a growing pause", () => {
    const { calls, scheduler } = setup();
    scheduler.requestNow();
    vi.advanceTimersByTime(0);
    scheduler.preempted();
    vi.advanceTimersByTime(999);
    expect(calls.start).toBe(1);
    vi.advanceTimersByTime(1);
    expect(calls.start).toBe(2);
    scheduler.preempted();
    vi.advanceTimersByTime(1_999);
    expect(calls.start).toBe(2);
    vi.advanceTimersByTime(1);
    expect(calls.start).toBe(3);
  });

  it("cancelAll drops the pending request and stops the running build", () => {
    const { calls, scheduler } = setup();
    scheduler.requestNow();
    vi.advanceTimersByTime(0);
    scheduler.request();
    scheduler.cancelAll();
    scheduler.exited();
    vi.advanceTimersByTime(10_000);
    expect(calls).toEqual({ start: 1, cancel: 1 });
  });
});
