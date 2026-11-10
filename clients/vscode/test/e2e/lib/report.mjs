// One line per check; the run fails when any check failed.
const results = [];

export function check(name, ok, detail = "") {
  results.push({ name, ok });
  console.log(`${ok ? "ok  " : "FAIL"} ${name}${!ok && detail ? ` -- ${detail}` : ""}`);
  return ok;
}

export function failures() {
  return results.filter((r) => !r.ok);
}

export function total() {
  return results.length;
}

export async function waitFor(what, fn, { timeout = 30_000, interval = 250 } = {}) {
  const end = Date.now() + timeout;
  for (;;) {
    const value = await fn();
    if (value) return value;
    if (Date.now() > end) {
      throw new Error(`timed out after ${timeout / 1000}s waiting for ${what}`);
    }
    await new Promise((r) => setTimeout(r, interval));
  }
}

export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
