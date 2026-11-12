import fs from "node:fs";
import path from "node:path";

// A stand-in for `oxabl`: a Node script that answers what the extension asks and
// writes down what it was asked. The extension is the program under test; this
// only fakes the binary.
//
// Environment:
//   FAKE_OXABL_LOG     a file that gets one JSON line per invocation, signal and exit
//   FAKE_OXABL_CONFIG  a JSON file read at each invocation:
//     builds[n]   what the n-th `build --events` run does (the last one repeats):
//                 { script: [{ wait: ms } | { event: {...} } | { stderr: "text" }], exit: 0, summary: {...} }
//                 A run that gets SIGINT writes a summary with exit 130 and exits 130.
//     noYield     `build --help` does not mention --yield (true, or only for this version)
//     printConfig { exit, stderr, settings } replaces the answer of `build --print-config`
//
// Without a config, a build reports one unit up to date and exits 0.

const SOURCE = (version) => `#!${process.execPath}
const fs = require("fs");
const VERSION = ${JSON.stringify(version)};
const args = process.argv.slice(2);
const logFile = process.env.FAKE_OXABL_LOG;
const log = (o) => logFile && fs.appendFileSync(logFile, JSON.stringify({ t: Date.now(), pid: process.pid, script: process.argv[1], ...o }) + "\\n");
let config = {};
try { config = JSON.parse(fs.readFileSync(process.env.FAKE_OXABL_CONFIG, "utf8")); } catch {}
const out = (o) => process.stdout.write(JSON.stringify(o) + "\\n");

if (args[0] === "--version") {
  log({ event: "invoke", args });
  console.log("oxabl " + VERSION);
} else if (args[0] === "build" && args.includes("--help")) {
  log({ event: "invoke", args });
  console.log("Usage: oxabl build [OPTIONS]\\n" + (config.noYield === true || config.noYield === VERSION ? "" : "      --yield   step aside for a newer build\\n"));
} else if (args[0] === "build" && args.includes("--print-config")) {
  log({ event: "invoke", args });
  const pc = config.printConfig || {};
  if (pc.stderr) console.error(pc.stderr);
  const settings = pc.settings || {
    "build.output": { value: "out", source: "oxabl.toml" },
    "build.state": { value: ".oxabl/build", source: "default" },
    "build.workers.command": { value: ["fake-worker", "{program}"], source: "oxabl.toml" },
  };
  console.log(JSON.stringify({ settings }));
  process.exit(pc.exit || 0);
} else if (args[0] === "build") {
  const starts = logFile && fs.existsSync(logFile)
    ? fs.readFileSync(logFile, "utf8").split("\\n").filter((l) => l.includes('"event":"build-start"')).length
    : 0;
  const builds = config.builds || [{ script: [{ event: { event: "build-started", rebuild: 0, units: 1, workers: 1, format: 1 } }] }];
  const run = builds[Math.min(starts, builds.length - 1)];
  log({ event: "build-start", args, cwd: process.cwd(), n: starts });
  const finish = (code) => {
    log({ event: "build-exit", code });
    process.exit(code);
  };
  process.on("SIGINT", () => {
    log({ event: "signal", signal: "SIGINT" });
    out({ event: "summary", compiled: 0, failed: 0, fetched: 0, skipped: 0, exit: 130 });
    finish(130);
  });
  (async () => {
    for (const step of run.script || []) {
      if (step.wait) await new Promise((r) => setTimeout(r, step.wait));
      if (step.event) out(step.event);
      if (step.stderr) process.stderr.write(step.stderr);
    }
    const exit = run.exit || 0;
    out({ event: "summary", compiled: 0, failed: 0, fetched: 0, skipped: 0, ...run.summary, exit });
    finish(exit);
  })();
} else if (args[0] === "lsp") {
  log({ event: "invoke", args });
  let buf = Buffer.alloc(0);
  const send = (id, result) => {
    const body = JSON.stringify({ jsonrpc: "2.0", id, result });
    process.stdout.write("Content-Length: " + Buffer.byteLength(body) + "\\r\\n\\r\\n" + body);
  };
  process.stdin.on("data", (chunk) => {
    buf = Buffer.concat([buf, chunk]);
    for (;;) {
      const sep = buf.indexOf("\\r\\n\\r\\n");
      if (sep < 0) return;
      const len = Number(/Content-Length: (\\d+)/i.exec(buf.slice(0, sep).toString())[1]);
      if (buf.length < sep + 4 + len) return;
      const msg = JSON.parse(buf.slice(sep + 4, sep + 4 + len).toString());
      buf = buf.slice(sep + 4 + len);
      if (msg.method === "initialize") send(msg.id, { capabilities: {} });
      else if (msg.method === "shutdown") send(msg.id, null);
      else if (msg.method === "exit") process.exit(0);
    }
  });
  process.stdin.on("end", () => process.exit(0));
} else {
  log({ event: "invoke", args });
  console.error("fake oxabl: unknown command");
  process.exit(2);
}
`;

/** Source of the fake, for a release asset. */
export const fakeOxablSource = SOURCE;

/** Writes the fake as an executable `oxabl` in `dir` and returns its path. */
export function writeFakeOxabl(dir, version = "9.9.9") {
  fs.mkdirSync(dir, { recursive: true });
  const file = path.join(dir, "oxabl");
  fs.writeFileSync(file, SOURCE(version), { mode: 0o755 });
  return file;
}

/** The lines of the fake's log. */
export function readLog(file) {
  return fs.existsSync(file)
    ? fs.readFileSync(file, "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l))
    : [];
}

export const buildStarts = (log) => log.filter((l) => l.event === "build-start");
export const buildExits = (log) => log.filter((l) => l.event === "build-exit");
export const signals = (log) => log.filter((l) => l.event === "signal");
