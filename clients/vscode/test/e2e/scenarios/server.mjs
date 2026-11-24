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
//   OXABL_E2E_SOAK_SECONDS    how long the soak scenario types (default 30)
//   OXABL_E2E_OXABL           the oxabl binary the soak scenario runs (required for it)
//   OXABL_E2E_NO_SERVER       1 turns the server off (oxabl.enable false): the same typing then
//                             measures the editor and the grammar alone
//   OXABL_E2E_BIG_FILE        the name of the large file (default big.p); big.txt gives a plain-text
//                             control that the grammar does not tokenize
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
function fuzzedFile(bytes) {
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

/**
 * A real oxabl in a real VS Code: a large fuzzed file is edited for a while, then the
 * window is reloaded several times. Reports the memory, threads and CPU of the extension
 * host and the server, and the number of oxabl processes after each reload.
 */
export async function serverSoak(ctx) {
  const name = "server-soak";
  const oxabl = process.env.OXABL_E2E_OXABL;
  if (!oxabl) throw new Error("OXABL_E2E_OXABL must name the oxabl binary");
  const seconds = Number(process.env.OXABL_E2E_SOAK_SECONDS ?? 30);
  if (!process.env.OXABL_E2E_RUNTIME) throw new Error("OXABL_E2E_RUNTIME must name a short directory");
  const runtimeDir = path.resolve(process.env.OXABL_E2E_RUNTIME);
  fs.rmSync(runtimeDir, { recursive: true, force: true });
  fs.mkdirSync(runtimeDir, { recursive: true, mode: 0o700 });
  const profile = makeProfile(name, { files: ["sample.p"], settings: { "oxabl.updates": "off", "oxabl.server.path": path.resolve(oxabl), "oxabl.enable": !process.env.OXABL_E2E_NO_SERVER } });
  const bigFile = process.env.OXABL_E2E_BIG_FILE ?? "big.p";
  const language = bigFile.endsWith(".p") ? "ABL" : "Plain Text";
  const serverOn = !process.env.OXABL_E2E_NO_SERVER;
  fs.writeFileSync(path.join(profile.workspace, bigFile), fuzzedFile(400_000));
  fs.writeFileSync(path.join(profile.workspace, "oxabl.toml"), '[workspace]\nname = "soak"\n');
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, {
    open: [bigFile],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const sample = (at) => {
    const c = census(profile.root, runtimeDir);
    report({
      scenario: name,
      at,
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
    return c;
  };
  const one = (c) => !serverOn || (c.oxablLsp.length === 1 && c.oxablDaemon.length === 1);
  const counts = (c) => `lsp ${c.oxablLsp.length}, daemons ${c.oxablDaemon.length}`;
  try {
    await waitForLanguage(vs.page, language);
    await sleep(8000);
    sample("opened");
    await vs.page.locator(".monaco-editor .view-lines").first().click();
    const tokens = ["DEFINE VARIABLE x AS INTEGER NO-UNDO.\n", "/* ", "*/", '"', "{&a}", "IF x THEN DO:\n", "END.\n", "~", "FOR EACH t:\n", "\n"];
    const end = Date.now() + seconds * 1000;
    let typed = 0;
    let lastSample = Date.now();
    while (Date.now() < end) {
      await vs.page.keyboard.type(`${tokens[typed % tokens.length]}x`, { delay: 5 });
      typed++;
      if (typed % 40 === 0) await vs.page.keyboard.press("Control+Home");
      if (Date.now() - lastSample > 10_000) {
        sample(`after ${typed} insertions`);
        lastSample = Date.now();
      }
    }
    const typedC = sample("after typing");
    check(`${name}: one server and one daemon after typing`, one(typedC), counts(typedC));
    for (let i = 1; i <= 5; i++) {
      await vs.page.locator(".monaco-editor .view-lines").first().click();
      await runCommand(vs.page, "Developer: Reload Window", 60_000);
      await waitForLanguage(vs.page, language, 60_000);
      await sleep(6000);
      const c = sample(`after reload ${i}`);
      check(`${name}: after reload ${i} there is one server and one daemon`, one(c), counts(c));
    }
  } finally {
    await vs.close();
    await sleep(1500);
    const c = census(profile.root, runtimeDir);
    report({ scenario: name, at: "window closed", oxablLsp: c.oxablLsp.length, oxablDaemons: c.oxablDaemon.length });
    check(`${name}: no oxabl server is left after the window closed`, c.oxablLsp.length === 0, counts(c));
    stopDaemons(runtimeDir);
    await server.close();
  }
}
