import fs from "node:fs";
import path from "node:path";
import { clickButton, runCommand, screenshot, statusTexts, toasts, waitForText, watchStatus } from "../lib/editor.mjs";
import { buildExits, buildStarts, fakeOxablSource, readLog, signals, writeFakeOxabl } from "../lib/fake-oxabl.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { addSettings, installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// Scenarios of the build loop. `oxabl` is a fake (lib/fake-oxabl.mjs) that logs
// every invocation and signal; the extension in the real VS Code is under test.

const started = { event: "build-started", format: 1, workers: 2 };
const fetched = (p, cache = "local") => ({ event: "unit-fetched", path: p, cache });
const unitStarted = (p) => ({ event: "unit-started", path: p, worker: 1 });
const unitFinished = (p, status = "ok") => ({ event: "unit-finished", path: p, status, ms: 5 });

/** A build that outlasts the scenario's burst unless it gets SIGINT. */
const LONG_BUILD = {
  script: [{ event: { ...started, rebuild: 5, units: 5 } }, { wait: 60_000 }],
};
const QUICK_BUILD = { script: [{ event: { ...started, rebuild: 0, units: 5 } }] };

/**
 * One scenario: a profile with the fake `oxabl` as `oxabl.server.path`, a fake
 * GitHub API, and VS Code started on it. `body` gets { page, profile, logFile, server }.
 */
async function session(ctx, name, opts, body) {
  const { config = {}, settings = {}, files = ["sample.p", "oxabl.toml"], fake = true, launchOptions = {}, cli } = opts;
  const profile = makeProfile(name, { files });
  const logFile = path.join(profile.root, "fake-oxabl.log");
  const configFile = path.join(profile.root, "fake-oxabl.json");
  fs.writeFileSync(configFile, JSON.stringify(config));
  const exe = fake ? writeFakeOxabl(path.join(profile.root, "fake")) : undefined;
  addSettings(profile, {
    "oxabl.updates": "off",
    "oxabl.build.debounce": 300,
    "editor.formatOnSave": false,
    ...(exe ? { "oxabl.server.path": exe } : {}),
    ...settings,
  });
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0", cli });
  let vs;
  try {
    vs = await launch(ctx.exe, profile, {
      open: ["sample.p"],
      env: { OXABL_UPDATE_API_URL: server.url, FAKE_OXABL_LOG: logFile, FAKE_OXABL_CONFIG: configFile },
      ...launchOptions,
    });
    await body({ page: vs.page, profile, logFile, server, log: () => readLog(logFile) });
  } finally {
    if (vs) await vs.close();
    await server.close();
  }
  return { profile, log: readLog(logFile) };
}

const waitForBuilds = (what, ctxLog, n, timeout = 60_000) =>
  waitFor(what, () => buildStarts(ctxLog()).length >= n, { timeout, interval: 100 });
const waitForExits = (what, ctxLog, n, timeout = 60_000) =>
  waitFor(what, () => buildExits(ctxLog()).length >= n, { timeout, interval: 100 });

/** Types a character and saves, as a person editing the open file does. */
async function editAndSave(page) {
  await page.keyboard.type("x");
  await page.keyboard.press("Control+S");
}

async function focusEditor(page) {
  await page.locator(".monaco-editor .view-lines").first().click();
}

export async function buildOpen(ctx) {
  const name = "build-open";
  const units = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"].map((n) => `src/${n}.p`);
  const config = {
    builds: [
      {
        script: [
          { wait: 800 },
          { event: { ...started, rebuild: 10, units: 12 } },
          ...units.slice(0, 4).flatMap((u) => [{ wait: 120 }, { event: fetched(u, "local") }]),
          ...units.slice(4, 6).flatMap((u) => [{ wait: 120 }, { event: fetched(u, "remote") }]),
          ...units.slice(6).flatMap((u) => [{ wait: 150 }, { event: unitStarted(u) }, { wait: 150 }, { event: unitFinished(u) }]),
          { wait: 800 },
        ],
        summary: { fetched: 6, compiled: 4 },
      },
      QUICK_BUILD,
    ],
  };
  await session(ctx, name, { config }, async ({ page, profile, log }) => {
    const watcher = watchStatus(page);
    await waitForText(page, "oxabl build finished", 60_000);
    await screenshot(page, name);
    const text = (await toasts(page)).find((t) => t.includes("oxabl build finished")) ?? "";
    check(
      `${name}: the notification names the units, the cache share and the time`,
      /^oxabl build finished: 10 units, 60% from cache \(4 local, 2 remote\), 4 compiled in \d+s$/.test(text),
      `got "${text}"`,
    );
    // The check mark stays for 3 s, then the item hides.
    await waitFor("the status bar item to hide", async () => (await statusTexts(page)).length === 0, { timeout: 15_000, interval: 100 });
    await watcher.stop();
    const seen = watcher.seen;
    check(`${name}: the status bar says preparing before build-started`, seen.includes("oxabl: preparing"), seen.join(" | "));
    check(
      `${name}: the status bar shows done/total and the cache hit rate`,
      seen.some((t) => /^oxabl \d+\/10 · \d+% cached$/.test(t)),
      seen.join(" | "),
    );
    check(`${name}: the status bar ends with the check mark`, seen.includes("oxabl"), seen.join(" | "));

    const starts = buildStarts(log());
    check(`${name}: exactly one build at open`, starts.length === 1, `${starts.length}`);
    const args = starts[0]?.args ?? [];
    check(
      `${name}: oxabl build runs with --events --yield --wait-lock 3600 in the root`,
      args.join(" ") === `build --root ${profile.workspace} --events --yield --wait-lock 3600` && starts[0].cwd === profile.workspace,
      `${args.join(" ")} in ${starts[0]?.cwd}`,
    );
    const config = log().filter((l) => l.event === "invoke" && l.args.includes("--print-config"));
    check(
      `${name}: the build setup was read with --print-config --json first`,
      config.length >= 1 && config[0].args.join(" ") === `build --root ${profile.workspace} --print-config --json`,
      JSON.stringify(config[0]?.args),
    );

    // A save builds again and shows the check mark only: the notification is for the first build.
    await runCommand(page, "Notifications: Clear All Notifications");
    await focusEditor(page);
    await editAndSave(page);
    await waitForBuilds(`${name}: the build after the save`, log, 2);
    await waitForExits(`${name}: the build after the save to end`, log, 2);
    await sleep(1_500);
    const after = (await toasts(page)).filter((t) => t.includes("oxabl build finished"));
    check(`${name}: a build after a save shows no notification`, after.length === 0, after.join(" | "));
  });
}

/** Ten saves while a build runs. */
export async function buildBurstSave(ctx) {
  const name = "build-burst-save";
  await session(ctx, name, { config: { builds: [LONG_BUILD, QUICK_BUILD] } }, async ({ page, log }) => {
    await waitForBuilds(`${name}: the first build`, log, 1);
    await focusEditor(page);
    for (let i = 0; i < 10; i++) {
      await editAndSave(page);
      await sleep(60);
    }
    await waitForExits(`${name}: the build after the burst to end`, log, 2);
    await sleep(2_000);
    burstChecks(name, log());
  });
}

/** The watcher event of a save arrives while the build of that save runs: it must not cancel it. */
export async function buildSaveKeepsItsBuild(ctx) {
  const name = "build-save-keeps-its-build";
  const slow = { script: [{ event: { ...started, rebuild: 1, units: 1 } }, { wait: 3_000 }] };
  await session(ctx, name, { config: { builds: [QUICK_BUILD, slow] } }, async ({ page, log }) => {
    await waitForExits(`${name}: the build at open`, log, 1);
    await sleep(1_000);
    await focusEditor(page);
    await editAndSave(page);
    await waitForExits(`${name}: the build of the save to end`, log, 2, 30_000);
    await sleep(2_000);
    const exits = buildExits(log());
    check(`${name}: the build of the save ran to its end`, exits[1]?.code === 0, JSON.stringify(exits));
    check(`${name}: no signal was sent and no second build started`, signals(log()).length === 0 && buildStarts(log()).length === 2, `${signals(log()).length} signals, ${buildStarts(log()).length} builds`);
  });
}

/** Two hundred files written on disk while a build runs, and writes in the output directory. */
export async function buildBurstDisk(ctx) {
  const name = "build-burst-disk";
  await session(ctx, name, { config: { builds: [LONG_BUILD, QUICK_BUILD] } }, async ({ profile, log }) => {
    await waitForBuilds(`${name}: the first build`, log, 1);
    fs.mkdirSync(path.join(profile.workspace, "src"), { recursive: true });
    for (let i = 0; i < 200; i++) {
      fs.writeFileSync(path.join(profile.workspace, "src", `agent${i}.p`), `/* ${i} */\n`);
      await sleep(5);
    }
    await waitForExits(`${name}: the build after the burst to end`, log, 2);
    await sleep(2_000);
    burstChecks(name, log());

    fs.mkdirSync(path.join(profile.workspace, "out"), { recursive: true });
    fs.writeFileSync(path.join(profile.workspace, "out", "compiled.p"), "/* output */\n");
    fs.mkdirSync(path.join(profile.workspace, ".oxabl", "build"), { recursive: true });
    fs.writeFileSync(path.join(profile.workspace, ".oxabl", "build", "state.p"), "/* state */\n");
    await sleep(3_000);
    check(`${name}: files in the output and state directories start no build`, buildStarts(log()).length === 2, `${buildStarts(log()).length} builds`);
  });
}

function burstChecks(name, log) {
  const starts = buildStarts(log);
  const sigs = signals(log);
  const exits = buildExits(log);
  check(`${name}: two builds ran, the one that was running and one after the burst`, starts.length === 2, `${starts.length}`);
  check(`${name}: the running build got exactly one SIGINT`, sigs.length === 1 && sigs[0].pid === starts[0]?.pid, JSON.stringify(sigs));
  check(`${name}: the running build exited 130`, exits.find((e) => e.pid === starts[0]?.pid)?.code === 130, JSON.stringify(exits));
  check(`${name}: the build after the burst completed`, exits.find((e) => e.pid === starts[1]?.pid)?.code === 0, JSON.stringify(exits));
}

export async function buildDiagnostic(ctx) {
  const name = "build-diagnostic";
  const config = {
    builds: [
      {
        script: [
          { event: { ...started, rebuild: 2, units: 2 } },
          { event: unitStarted("sample.p") },
          { wait: 200 },
          { event: { event: "message", path: "sample.p", file: "sample.p", row: 2, column: 5, number: 247, text: "Unknown field or variable name: zzTotal", type: "error" } },
          { event: unitFinished("sample.p", "fail") },
          { event: fetched("other.p") },
        ],
        summary: { failed: 1, fetched: 1 },
        exit: 1,
      },
    ],
  };
  await session(ctx, name, { config }, async ({ page }) => {
    await waitForText(page, "oxabl build failed: 1 unit failed", 60_000);
    check(`${name}: a failed first build shows an error notification with Show Problems`, true);
    await waitFor("the failure in the status bar", async () => (await statusTexts(page)).includes("oxabl: 1 failed"), { timeout: 10_000, interval: 100 });
    check(`${name}: the status bar says "oxabl: 1 failed"`, true);
    await clickButton(page, "Show Problems");
    await waitForText(page, "Unknown field or variable name: zzTotal", 15_000);
    check(`${name}: the compiler message is in the Problems panel`, true);
    await screenshot(page, name);
    await sleep(4_000);
    check(`${name}: the failure stays in the status bar`, (await statusTexts(page)).includes("oxabl: 1 failed"), (await statusTexts(page)).join(" | "));
  });
}

export async function buildUntrusted(ctx) {
  const name = "build-untrusted";
  const settings = {
    "security.workspace.trust.enabled": true,
    "security.workspace.trust.startupPrompt": "never",
    "security.workspace.trust.banner": "never",
    "security.workspace.trust.emptyWindow": false,
  };
  await session(
    ctx,
    name,
    { config: { builds: [QUICK_BUILD] }, settings, launchOptions: { untrusted: true } },
    async ({ page, log }) => {
      // The language server is a sign that the extension runs: it starts in a restricted window.
      await waitFor("the extension to start the language server", () => log().some((l) => l.args?.[0] === "lsp"), { timeout: 60_000, interval: 200 });
      await sleep(6_000);
      const touched = log().filter((l) => l.args?.[0] === "build");
      check(`${name}: an untrusted workspace runs no oxabl build, not even --print-config`, touched.length === 0, JSON.stringify(touched.map((l) => l.args)));

      await runCommand(page, "Workspaces: Manage Workspace Trust");
      await page.getByRole("button", { name: /^Trust/ }).first().click({ timeout: 15_000 });
      await waitForBuilds(`${name}: the build after trust is granted`, log, 1, 30_000);
      check(`${name}: granting trust starts the build`, true);
    },
  );
}

export async function buildExternalPreemption(ctx) {
  const name = "build-external-preemption";
  const config = {
    builds: [
      { script: [{ event: { ...started, rebuild: 3, units: 3 } }, { wait: 1_500 }], exit: 130 },
      { script: [{ event: { ...started, rebuild: 0, units: 3 } }] },
    ],
  };
  await session(ctx, name, { config }, async ({ page, log }) => {
    const watcher = watchStatus(page);
    await waitForBuilds(`${name}: the second build`, log, 2, 60_000);
    await waitForExits(`${name}: the second build to end`, log, 2, 30_000);
    await watcher.stop();
    check(
      `${name}: the status bar says the build runs elsewhere`,
      watcher.seen.some((t) => t.includes("build running elsewhere")),
      watcher.seen.join(" | "),
    );
    check(`${name}: the extension sent no signal`, signals(log()).length === 0);
    check(`${name}: exactly one more build was started`, buildStarts(log()).length === 2, `${buildStarts(log()).length}`);
  });
}

/** The CLI release the install scenarios serve: the fake itself, which answers --version and builds. */
const CLI_VERSION = "5.2.0";
const cliRelease = (digest) => ({ version: CLI_VERSION, binary: Buffer.from(fakeOxablSource(CLI_VERSION)), digest });
const INSTALL_PROMPT = "The oxabl CLI is not installed. It builds and checks this project.";

export async function buildInstall(ctx) {
  const name = "build-install";
  await session(
    ctx,
    name,
    { fake: false, cli: cliRelease("good"), config: { builds: [QUICK_BUILD] } },
    async ({ page, profile, log }) => {
      await waitForText(page, INSTALL_PROMPT, 60_000);
      check(`${name}: the install prompt shows`, true);
      await screenshot(page, name);
      await clickButton(page, "Install");
      await waitForBuilds(`${name}: the build with the installed CLI`, log, 1, 60_000);
      const binDir = path.join(profile.root, "bin");
      const target = path.join(binDir, "oxabl");
      check(`${name}: the CLI is installed in OXABL_INSTALL_DIR, executable`, fs.existsSync(target) && (fs.statSync(target).mode & 0o111) !== 0);
      const leftovers = fs.readdirSync(binDir).filter((f) => f.startsWith(".oxabl-install-"));
      check(`${name}: no staged file is left`, leftovers.length === 0, leftovers.join(" "));
      const recordFile = path.join(profile.root, "data", "oxabl", "install.json");
      const record = fs.existsSync(recordFile) ? JSON.parse(fs.readFileSync(recordFile, "utf8")) : null;
      check(
        `${name}: the install record names the vscode installer, the path and the version`,
        record?.format === 1 && record.installer === "vscode" && record.version === CLI_VERSION && record.path === fs.realpathSync(target),
        JSON.stringify(record),
      );
      const staged = log().find((l) => l.args?.[0] === "--version" && path.basename(l.script).startsWith(".oxabl-install-"));
      check(`${name}: the staged file ran --version before it replaced the target`, Boolean(staged));
      const built = buildStarts(log())[0];
      check(`${name}: the build ran with the installed binary`, built?.script === fs.realpathSync(target), built?.script);
    },
  );
}

export async function buildInstallBadChecksum(ctx) {
  const name = "build-install-bad-checksum";
  await session(ctx, name, { fake: false, cli: cliRelease("bad"), config: { builds: [QUICK_BUILD] } }, async ({ page, profile, log }) => {
    await waitForText(page, INSTALL_PROMPT, 60_000);
    await clickButton(page, "Install");
    await waitForText(page, "could not install the CLI: checksum mismatch", 30_000);
    check(`${name}: the error says the checksum did not match`, true);
    await screenshot(page, name);
    const binDir = path.join(profile.root, "bin");
    const files = fs.existsSync(binDir) ? fs.readdirSync(binDir) : [];
    check(`${name}: nothing is installed, not even a staged file`, files.length === 0, files.join(" "));
    check(`${name}: no install record is written`, !fs.existsSync(path.join(profile.root, "data", "oxabl", "install.json")));
    await sleep(2_000);
    check(`${name}: no build ran`, buildStarts(log()).length === 0);
  });
}
