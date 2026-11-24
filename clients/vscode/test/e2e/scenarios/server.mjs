import fs from "node:fs";
import path from "node:path";
import { check, sleep } from "../lib/report.mjs";
import { runCommand, waitForLanguage } from "../lib/editor.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { EXT_DIR, WORK_DIR } from "../lib/paths.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// Scenarios about what the extension costs the machine. The soak scenario sets a
// runtime directory of its own, so no daemon meets one of a running editor.
//
//   OXABL_E2E_CRASH_SECONDS   how long the crash-loop scenario watches (default 20)
//   OXABL_E2E_TYPING_SECONDS  how long the soak scenario types at the speed of a fast typist
//                             (default 30); the window must stay responsive
//   OXABL_E2E_SOAK_SECONDS    how long the soak scenario then types as fast as it can (default 30)
//   OXABL_E2E_SOAK_DELAY      milliseconds between the keys of that stress typing (default 5)
//   OXABL_E2E_HOLD_KEY        a key (Delete, Backspace) to hold from the top of the file instead of
//                             the stress typing; OXABL_E2E_HOLD_RATE presses a second (default 40,
//                             the repeat rate of many desktops)
//   OXABL_E2E_OXABL           the oxabl binary the soak scenario runs (required for it)
//   OXABL_E2E_NO_SERVER       1 turns the server off (oxabl.enable false): the same typing then
//                             measures the editor and the grammar alone
//   OXABL_E2E_BIG_FILE        the name of the large file (default big.p); big.txt gives a plain-text
//                             control that no grammar tokenizes, big.c the same text in VS Code's
//                             own C grammar
//   OXABL_E2E_LANGUAGE        the language the status bar shows for that file (default ABL for .p,
//                             C for .c, else Plain Text)
//   OXABL_E2E_SETTINGS        JSON object of more user settings, for example
//                             {"editor.experimental.asyncTokenization":false}
//   OXABL_E2E_PROFILE         a file that receives a CPU profile of the window during the typing
//   OXABL_E2E_RUNTIME         the runtime directory of the soak scenario (required for it); keep
//                             the path short: VS Code's own socket must fit in 107 characters
//   OXABL_E2E_REPORT          a file that receives the measurements as JSON lines

const report = (row) => {
  console.log(`     ${JSON.stringify(row)}`);
  if (process.env.OXABL_E2E_REPORT) fs.appendFileSync(process.env.OXABL_E2E_REPORT, `${JSON.stringify(row)}\n`);
};

const CLK_TCK = 100;

const readFile = (file) => {
  try {
    return fs.readFileSync(file, "utf8").replaceAll("\0", " ");
  } catch {
    return "";
  }
};

function readProc(pid) {
  try {
    const stat = fs.readFileSync(`/proc/${pid}/stat`, "utf8");
    const fields = stat.slice(stat.lastIndexOf(")") + 2).split(" ");
    const status = fs.readFileSync(`/proc/${pid}/status`, "utf8");
    return {
      pid,
      cpu: (Number(fields[11]) + Number(fields[12])) / CLK_TCK,
      threads: Number(/Threads:\s+(\d+)/.exec(status)?.[1]),
      rssMb: Math.round(Number(/VmRSS:\s+(\d+)/.exec(status)?.[1]) / 1024),
    };
  } catch {
    return null;
  }
}

/**
 * Processes of the scenario. oxabl's are found by the runtime directory in their
 * environment, VS Code's by the profile directory in the command line. The extension
 * host is the parent of the oxabl server.
 */
function census(profileRoot, runtimeDir) {
  const out = { vscode: [], extensionHost: [], oxablLsp: [], oxablDaemon: [] };
  const parents = new Set();
  const vscode = [];
  for (const entry of fs.readdirSync("/proc").filter((d) => /^\d+$/.test(d))) {
    const pid = Number(entry);
    if (pid === process.pid) continue;
    const cmd = readFile(`/proc/${pid}/cmdline`).trim();
    const oxablCmd = /oxabl\s+(daemon\s|lsp$)/.test(cmd);
    if (oxablCmd && runtimeDir && readFile(`/proc/${pid}/environ`).includes(`XDG_RUNTIME_DIR=${runtimeDir}`)) {
      const info = readProc(pid);
      if (!info) continue;
      if (cmd.includes(" daemon ")) out.oxablDaemon.push(info);
      else {
        out.oxablLsp.push(info);
        parents.add(Number(readFile(`/proc/${pid}/stat`).split(" ")[3]));
      }
    } else if (cmd.includes(profileRoot) && !oxablCmd) {
      vscode.push(pid);
    }
  }
  for (const pid of vscode.filter((p) => !parents.has(p))) {
    const info = readProc(pid);
    if (info) out.vscode.push(info);
  }
  for (const pid of parents) {
    const info = readProc(pid);
    if (info) out.extensionHost.push(info);
  }
  return out;
}

const sum = (list, key) => list.reduce((n, p) => n + p[key], 0);

/** Stops the daemons the scenario started, by the pids their registrations name. */
function stopDaemons(runtimeDir) {
  const dir = path.join(runtimeDir, "oxabl", "daemon");
  if (!fs.existsSync(dir)) return;
  for (const file of fs.readdirSync(dir).filter((f) => f.endsWith(".json"))) {
    try {
      process.kill(JSON.parse(fs.readFileSync(path.join(dir, file), "utf8")).pid, "SIGTERM");
    } catch {}
  }
}

/** The server exits the moment it starts; the scenario counts how often the client starts it. */
export async function serverCrashLoop(ctx) {
  const name = "server-crash-loop";
  const seconds = Number(process.env.OXABL_E2E_CRASH_SECONDS ?? 20);
  const profile = makeProfile(name, { files: ["sample.p"], settings: { "oxabl.updates": "off" } });
  const log = path.join(profile.root, "starts.log");
  const script = path.join(profile.root, "fake-oxabl.sh");
  fs.writeFileSync(script, `#!/bin/sh\ndate +%s.%N >> "${log}"\nexit 1\n`, { mode: 0o755 });
  const settings = path.join(profile.userData, "User", "settings.json");
  fs.writeFileSync(settings, JSON.stringify({ ...JSON.parse(fs.readFileSync(settings, "utf8")), "oxabl.server.path": script }));
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, { open: ["sample.p"], env: { OXABL_UPDATE_API_URL: server.url } });
  try {
    await waitForLanguage(vs.page, "ABL");
    await sleep(seconds * 1000);
    const starts = fs.existsSync(log) ? fs.readFileSync(log, "utf8").trim().split("\n").map(Number) : [];
    report({
      scenario: name,
      watchedSeconds: seconds,
      starts: starts.length,
      gapsSeconds: starts.slice(1).map((t, i) => Math.round((t - starts[i]) * 100) / 100),
    });
    check(`${name}: a server that exits at once is started at most 5 times in ${seconds}s`, starts.length >= 1 && starts.length <= 5, `started ${starts.length} times`);
  } finally {
    await vs.close();
    await server.close();
  }
}

// A fuzzed ABL file: the resource files joined, then damaged in a few places.
export function fuzzedFile(bytes) {
  const dir = path.resolve(EXT_DIR, "../../resources");
  const pool = fs
    .readdirSync(dir)
    .filter((f) => /\.(abl|i)$/.test(f))
    .sort()
    .map((f) => path.join(dir, f));
  const parts = [];
  for (let i = 0, size = 0; size < bytes; i++) {
    const text = fs.readFileSync(pool[i % pool.length], "latin1");
    parts.push(text);
    size += text.length;
  }
  let text = parts.join("\n");
  let seed = 7;
  const next = () => (seed = (Math.imul(seed, 1103515245) + 12345) >>> 0) / 4294967296;
  const noise = ["/*", '"', "{", "~", "&IF", "(", "\n", "END."];
  for (let i = 0; i < 400; i++) {
    const at = Math.floor(next() * text.length);
    text = text.slice(0, at) + noise[Math.floor(next() * noise.length)] + text.slice(at);
  }
  return text.slice(0, bytes);
}

// The longest time the window's main thread may be blocked while a fast typist types.
const STALL_LIMIT_MS = 1000;
// The share of a sample (about 10 s) in which the main thread may be blocked in slices over 100 ms.
const BLOCKED_SHARE_LIMIT = 0.1;
// The time the window has to answer after the stress typing; a click times out after the same time.
const ANSWER_LIMIT_MS = 30_000;
// The status bar language of the large file, by extension.
const LANGUAGES = { ".p": "ABL", ".c": "C", ".rs": "Rust" };
// A fast typist or a held key: about 39 characters a second. The window starts to block at about 45.
const TYPIST_DELAY_MS = 20;
// Fewer characters a second than this mean the window did not take the keys at the typist's speed
// (Playwright waits for the window), so it was under less load than the check intends.
const TYPIST_MIN_CHARS_PER_SECOND = 28;

/**
 * Counts in the window how late a 50 ms timer fires: the time the main thread was blocked.
 * `takeStalls` reads and resets the longest delay and the sum of the delays over 100 ms.
 */
async function watchStalls(page) {
  await page.evaluate(() => {
    globalThis.__oxablStalls = { maxMs: 0, totalMs: 0 };
    let last = performance.now();
    setInterval(() => {
      const now = performance.now();
      const late = Math.round(now - last - 50);
      last = now;
      const s = globalThis.__oxablStalls;
      s.maxMs = Math.max(s.maxMs, late);
      if (late > 100) s.totalMs += late;
    }, 50);
  });
}

/** The time the window takes to run a script, or null when it does not answer within `limit`. */
async function answerMs(page, limit) {
  const start = Date.now();
  const answered = await Promise.race([
    page.evaluate(() => true).catch(() => false),
    sleep(limit).then(() => false),
  ]);
  return answered ? Date.now() - start : null;
}

// null when nothing watches (a reload starts a new window).
const takeStalls = (page) =>
  page.evaluate(() => {
    const s = globalThis.__oxablStalls ?? null;
    if (s) globalThis.__oxablStalls = { maxMs: 0, totalMs: 0 };
    return s;
  });

// Text that opens and closes comments, strings and include references, so the state at the
// end of a line changes and the rest of the file must be tokenized again.
const TYPED = ["DEFINE VARIABLE x AS INTEGER NO-UNDO.\n", "/* ", "*/", '"', "{&a}", "IF x THEN DO:\n", "END.\n", "~", "FOR EACH t:\n", "\n"];

/**
 * Types TYPED in a loop for `seconds`, `delay` ms between keys. `onTick(insertions)` runs about
 * every 10 s. Returns the insertions, the characters and the seconds the typing took.
 */
async function typeFor(page, seconds, delay, onTick) {
  const start = Date.now();
  const end = start + seconds * 1000;
  let typed = 0;
  let chars = 0;
  let last = start;
  while (Date.now() < end) {
    const text = `${TYPED[typed % TYPED.length]}x`;
    await page.keyboard.type(text, { delay });
    chars += text.length;
    typed++;
    if (typed % 40 === 0) await page.keyboard.press("Control+Home");
    if (Date.now() - last > 10_000) {
      await onTick(typed);
      last = Date.now();
    }
  }
  return { insertions: typed, chars, seconds: (Date.now() - start) / 1000 };
}

/**
 * Holds `key` as the keyboard repeats it, `rate` presses a second, from the top of the file. Held
 * Delete takes the text below the cursor away, so each comment, string or include reference that
 * it opens or closes changes the state of the rest of the file.
 */
async function holdKey(page, key, seconds, rate, onTick) {
  const start = Date.now();
  const end = start + seconds * 1000;
  let presses = 0;
  let last = start;
  await page.keyboard.press("Control+Home");
  while (Date.now() < end) {
    await page.keyboard.press(key, { delay: 0 });
    presses++;
    const due = start + (presses * 1000) / rate - Date.now();
    if (due > 0) await sleep(due);
    if (Date.now() - last > 10_000) {
      await onTick(presses);
      last = Date.now();
    }
  }
  return { insertions: presses, chars: presses, seconds: (Date.now() - start) / 1000 };
}

/** A number from the environment; a value that is not a finite number of at least `min` stops the run. */
function numberEnv(name, fallback, min) {
  const raw = process.env[name];
  if (raw === undefined) return fallback;
  const n = Number(raw);
  if (raw.trim() === "" || !Number.isFinite(n) || n < min) throw new Error(`${name}=${raw} must be a number of at least ${min}`);
  return n;
}

/**
 * A real oxabl in a real VS Code: a large fuzzed file is edited for a while, then the
 * window is reloaded several times. Reports the memory, threads and CPU of the extension
 * host and the server, the number of oxabl processes after each reload, and how long the
 * window's main thread was blocked.
 *
 * First a fast typist types (about 39 characters a second): the window must never be blocked
 * longer than STALL_LIMIT_MS, and for no more than BLOCKED_SHARE_LIMIT of the time.
 * Then the scenario types as fast as it can (about 78 characters a second). That rate
 * blocks the window for seconds in any language with a grammar, through VS Code's
 * asynchronous tokenization (docs/design/vscode-extension.md, "Input faster than VS Code
 * tokenizes"), so the scenario only reports the stalls and checks that the window still answers.
 */
export async function serverSoak(ctx) {
  const name = "server-soak";
  const oxabl = process.env.OXABL_E2E_OXABL;
  if (!oxabl) throw new Error("OXABL_E2E_OXABL must name the oxabl binary");
  const typingSeconds = numberEnv("OXABL_E2E_TYPING_SECONDS", 30, 0);
  const seconds = numberEnv("OXABL_E2E_SOAK_SECONDS", 30, 0);
  const soakDelay = numberEnv("OXABL_E2E_SOAK_DELAY", 5, 0);
  if (!process.env.OXABL_E2E_RUNTIME) throw new Error("OXABL_E2E_RUNTIME must name a short directory");
  const runtimeDir = path.resolve(process.env.OXABL_E2E_RUNTIME);
  fs.rmSync(runtimeDir, { recursive: true, force: true });
  fs.mkdirSync(runtimeDir, { recursive: true, mode: 0o700 });
  const settings = {
    "oxabl.updates": "off",
    "oxabl.server.path": path.resolve(oxabl),
    "oxabl.enable": !process.env.OXABL_E2E_NO_SERVER,
    ...JSON.parse(process.env.OXABL_E2E_SETTINGS ?? "{}"),
  };
  const profile = makeProfile(name, { files: ["sample.p"], settings });
  const bigFile = process.env.OXABL_E2E_BIG_FILE ?? "big.p";
  const language = process.env.OXABL_E2E_LANGUAGE ?? LANGUAGES[path.extname(bigFile)] ?? "Plain Text";
  const serverOn = !process.env.OXABL_E2E_NO_SERVER;
  fs.writeFileSync(path.join(profile.workspace, bigFile), fuzzedFile(400_000));
  fs.writeFileSync(path.join(profile.workspace, "oxabl.toml"), '[workspace]\nname = "soak"\n');
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, {
    open: [bigFile],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  let lastSampleAt = Date.now();
  const sample = async (at) => {
    const c = census(profile.root, runtimeDir);
    const answer = await answerMs(vs.page, ANSWER_LIMIT_MS);
    const stalls = answer === null ? null : await takeStalls(vs.page);
    const elapsedMs = Date.now() - lastSampleAt;
    lastSampleAt = Date.now();
    report({
      scenario: name,
      at,
      windowAnswerMs: answer,
      windowStallMaxMs: stalls?.maxMs ?? null,
      windowStallTotalMs: stalls?.totalMs ?? null,
      windowBlockedShare: stalls ? Math.round((stalls.totalMs / elapsedMs) * 100) / 100 : null,
      extensionHostRssMb: sum(c.extensionHost, "rssMb"),
      extensionHostThreads: sum(c.extensionHost, "threads"),
      extensionHostCpuSeconds: Math.round(sum(c.extensionHost, "cpu")),
      vscodeProcesses: c.vscode.length,
      vscodeRssMb: sum(c.vscode, "rssMb"),
      oxablLsp: c.oxablLsp.length,
      oxablLspRssMb: sum(c.oxablLsp, "rssMb"),
      oxablDaemons: c.oxablDaemon.length,
      oxablDaemonRssMb: sum(c.oxablDaemon, "rssMb"),
      oxablDaemonThreads: sum(c.oxablDaemon, "threads"),
      oxablDaemonCpuSeconds: Math.round(sum(c.oxablDaemon, "cpu") * 10) / 10,
    });
    return { census: c, answer, stalls, elapsedMs };
  };
  const one = (c) => !serverOn || (c.oxablLsp.length === 1 && c.oxablDaemon.length === 1);
  const counts = (c) => `lsp ${c.oxablLsp.length}, daemons ${c.oxablDaemon.length}`;
  let cdp;
  try {
    await waitForLanguage(vs.page, language);
    await sleep(8000);
    await watchStalls(vs.page);
    await sample("opened");
    if (process.env.OXABL_E2E_PROFILE) {
      cdp = await vs.page.context().newCDPSession(vs.page);
      await cdp.send("Profiler.enable");
      await cdp.send("Profiler.start");
    }
    await vs.page.locator(".monaco-editor .view-lines").first().click();

    if (typingSeconds > 0) {
      let worstMax = 0;
      let worstShare = 0;
      let sampled = 0;
      const typist = async (typed) => {
        if (typed === sampled) return;
        sampled = typed;
        const s = await sample(`typist, after ${typed} insertions`);
        // A window that does not answer, or one that nothing measures, counts as blocked.
        worstMax = Math.max(worstMax, s.stalls?.maxMs ?? Infinity);
        worstShare = Math.max(worstShare, s.stalls ? s.stalls.totalMs / s.elapsedMs : Infinity);
      };
      const typing = await typeFor(vs.page, typingSeconds, TYPIST_DELAY_MS, typist);
      await typist(typing.insertions);
      const rate = typing.chars / typing.seconds;
      check(
        `${name}: the window is never blocked longer than ${STALL_LIMIT_MS} ms while a fast typist types`,
        worstMax <= STALL_LIMIT_MS,
        `blocked for ${worstMax} ms`,
      );
      check(
        `${name}: the window is blocked less than ${BLOCKED_SHARE_LIMIT * 100}% of the time while a fast typist types`,
        worstShare <= BLOCKED_SHARE_LIMIT,
        `blocked for ${Math.round(worstShare * 100)}% of a sample`,
      );
      check(
        `${name}: the window takes the keys of a fast typist at ${TYPIST_MIN_CHARS_PER_SECOND} characters a second or more`,
        rate >= TYPIST_MIN_CHARS_PER_SECOND,
        `${rate.toFixed(1)} characters a second`,
      );
    } else {
      console.log("!!   server-soak: OXABL_E2E_TYPING_SECONDS=0: the fast typist check does not run");
    }

    const hold = process.env.OXABL_E2E_HOLD_KEY;
    const onStress = (n) => sample(`stress, after ${n} insertions`);
    const stress = hold
      ? await holdKey(vs.page, hold, seconds, numberEnv("OXABL_E2E_HOLD_RATE", 40, 1), onStress)
      : await typeFor(vs.page, seconds, soakDelay, onStress);
    if (seconds > 0) report({ scenario: name, at: "stress rate", charsPerSecond: Math.round((stress.chars / stress.seconds) * 10) / 10 });
    const typedS = await sample("after typing");
    check(
      `${name}: the window answers within ${ANSWER_LIMIT_MS / 1000}s after the stress typing`,
      typedS.answer !== null,
      "no answer",
    );
    check(`${name}: one server and one daemon after typing`, one(typedS.census), counts(typedS.census));
    for (let i = 1; i <= 5; i++) {
      await vs.page.locator(".monaco-editor .view-lines").first().click();
      await runCommand(vs.page, "Developer: Reload Window", 60_000);
      await waitForLanguage(vs.page, language, 60_000);
      await sleep(6000);
      const { census: c } = await sample(`after reload ${i}`);
      check(`${name}: after reload ${i} there is one server and one daemon`, one(c), counts(c));
    }
  } finally {
    if (cdp) {
      const { profile: cpu } = await cdp.send("Profiler.stop").catch(() => ({}));
      if (cpu) fs.writeFileSync(process.env.OXABL_E2E_PROFILE, JSON.stringify(cpu));
    }
    await vs.close();
    await sleep(1500);
    const c = census(profile.root, runtimeDir);
    report({ scenario: name, at: "window closed", oxablLsp: c.oxablLsp.length, oxablDaemons: c.oxablDaemon.length });
    check(`${name}: no oxabl server is left after the window closed`, c.oxablLsp.length === 0, counts(c));
    stopDaemons(runtimeDir);
    await server.close();
  }
}
