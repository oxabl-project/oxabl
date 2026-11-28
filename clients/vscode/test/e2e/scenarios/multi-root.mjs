import fs from "node:fs";
import path from "node:path";
import { bodyText, runCommand, screenshot } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile, outputLog, waitForLog } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a window with several workspace folders, each with its own
// `oxabl.toml` and `.df`. Each folder gets its own language server, started when one of its files
// opens: F12 on a table finds the `.df` of that folder only. A file outside every folder gets no
// server. A folder removed from the window stops its server, also while that server starts, and a
// window that goes down to one folder has one server. A restart starts the folders of the schema view.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const df = (table) => `ADD TABLE "${table}"\n  AREA "Data"\n\nADD FIELD "Id" OF "${table}" AS integer\n  ORDER 10\n`;

// Each file has an unused variable of its own: its finding in the Problems panel shows that the
// server of the file's folder has analysed it.
const unused = (tag) => `DEFINE VARIABLE spare-${tag} AS INTEGER NO-UNDO.\n`;
const finding = (tag) => `unused variable \`spare-${tag}\``;

const folderFiles = (name, table, dir = name) => ({
  [`${dir}/oxabl.toml`]: `[workspace]\nname = "${name}"\n[workspace.schema]\nfiles = ["${name}.df"]\n`,
  [`${dir}/${name}.df`]: df(table),
  [`${dir}/main.p`]: `FIND FIRST ${table} NO-LOCK.\n${unused(name)}`,
});

// A test build of the daemon waits 12 s before it registers when the root path holds this marker.
const SLOW_DIR = "slow-OXABL-TEST-HOLD:daemon.ready@12000";
// A test build of the daemon panics before it registers when the root path holds this marker: the server never starts.
const BOOM_DIR = "boom-OXABL-TEST-PANIC:daemon.ready";
// A schema of many tables: the answer of `oxabl/schema` takes a while, so a request that a newer one replaces shows.
const BIG_TABLES = 4000;
const bigDf = Array.from({ length: BIG_TABLES }, (_, i) => `ADD TABLE "Tbl${String(i).padStart(4, "0")}"\n  AREA "Data"\n`).join("\n");

const FILES = {
  ...folderFiles("alpha", "Alpha"),
  ...folderFiles("beta", "Beta"),
  ...folderFiles("gamma", "Gamma"),
  ...folderFiles("slow", "Slow", SLOW_DIR),
  ...folderFiles("boom", "Boom", BOOM_DIR),
  ...folderFiles("big", "Tbl0000"),
  "big/big.df": bigDf,
  "outside.p": `MESSAGE 'outside'.\n${unused("outside")}`,
};

const workspaceFile = (...folders) => JSON.stringify({ folders: folders.map((f) => ({ path: f })) }, null, 2);

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 300 })
    .then(() => true)
    .catch(() => false);

const rows = (page, selector) =>
  page.evaluate((sel) => [...document.querySelectorAll(sel)].map((r) => r.textContent.replace(/ /g, " ").trim()), selector);

const activeTab = (page) => rows(page, ".editor-group-container.active .tab.active");

const cursor = async (page) => {
  const text = await page.evaluate(() => document.querySelector('[id="status.editor.selection"]')?.textContent ?? "");
  const m = /Ln (\d+), Col (\d+)/.exec(text);
  return m ? { line: Number(m[1]), col: Number(m[2]) } : null;
};

/** Opens a file with Quick Open, by its absolute path. A folder that just joined the window may need a moment to be searched. */
async function openFile(page, file) {
  const base = path.basename(file);
  const folder = base === "main.p" ? path.basename(path.dirname(file)) : "";
  await page.keyboard.press("Control+P");
  const input = page.locator(".quick-input-widget input").first();
  await input.waitFor({ state: "visible", timeout: 15_000 });
  // Enter opens the focused row: wait until the typed path is in the box and the file is that row.
  for (let i = 0; i < 6; i++) {
    await input.fill(file);
    const focused = await until(
      `${base} focused in Quick Open`,
      async () =>
        (await input.inputValue()) === file &&
        (await rows(page, ".quick-input-list .monaco-list-row.focused")).some((t) => t.includes(base) && t.includes(folder)),
      5_000,
    );
    if (focused) break;
  }
  await page.keyboard.press("Enter");
  // Two `main.p` tabs differ by the folder name VS Code appends.
  await waitFor(`${base} active`, async () => (await activeTab(page)).some((t) => t.startsWith(base) && t.includes(folder)), {
    timeout: 15_000,
  }).catch(async (err) => {
    await screenshot(page, `multi-root-open-${base}`);
    throw new Error(`${err.message}; tabs: ${(await rows(page, ".tab")).join(" | ")}; quick open: ${(await rows(page, ".quick-input-list .monaco-list-row")).join(" | ")}`);
  });
}

async function moveTo(page, line, col) {
  await page.keyboard.press("Control+G");
  await page.locator(".quick-input-widget input").first().waitFor({ state: "visible", timeout: 15_000 });
  await page.keyboard.type(`${line}:${col}`);
  await page.keyboard.press("Enter");
  await waitFor(`the cursor at ${line}:${col}`, async () => {
    const at = await cursor(page);
    return at?.line === line && at?.col === col;
  });
}

/** True when the Problems panel shows `text`. */
const problemShows = (page, text, timeout = 60_000) =>
  until(`the Problems panel to show ${text}`, async () => (await bodyText(page)).includes(text), timeout);

/**
 * F12 on the table of line 1 of the active `main.p`; true when `<name>.df` opens with the cursor on its
 * table. The server has answered for the file (its finding shows) before F12 is pressed, once.
 */
async function definitionOpens(page, name, label) {
  const ready = await problemShows(page, finding(name));
  check(`multi-root: ${label}: the server of ${name} reports the finding of main.p`, ready);
  await page.locator(".monaco-editor .view-lines").first().click();
  await moveTo(page, 1, "FIND FIRST ".length + 2);
  await page.keyboard.press("F12");
  const opened = await until(`${name}.df open`, async () => (await activeTab(page)).some((t) => t.startsWith(`${name}.df`)), 15_000);
  const at = opened ? await cursor(page) : null;
  check(`multi-root: ${label}: F12 on the table opens ${name}.df`, opened, `active tab: ${(await activeTab(page)).join(" | ")}`);
  check(`multi-root: ${label}: the cursor is on the table`, at?.line === 1, `cursor: ${JSON.stringify(at)}`);
  return opened;
}

const schemaRows = (page) =>
  page.evaluate(() => {
    const pane = [...document.querySelectorAll(".pane")].find((p) => p.querySelector(".pane-header")?.textContent.includes("ABL Schema"));
    return pane ? [...pane.querySelectorAll(".monaco-list-row")].map((r) => r.textContent.replace(/\u00a0/g, " ").trim()) : [];
  });

const startsWith = (list, name) => list.some((r) => r.startsWith(name));

/** Opens the folder row `name` of the ABL Schema view. */
async function expandFolder(page, name) {
  const row = page.locator(".pane", { has: page.locator(".pane-header", { hasText: "ABL Schema" }) }).locator(".monaco-list-row", { hasText: name }).first();
  await row.click();
  if ((await row.getAttribute("aria-expanded")) !== "true") await page.keyboard.press("ArrowRight");
}

/** The `oxabl lsp` processes of this scenario's binary that run in a directory under `root`, with that directory. */
function lspProcesses(oxabl, root) {
  const found = [];
  for (const pid of fs.readdirSync("/proc").filter((d) => /^\d+$/.test(d))) {
    try {
      const args = fs.readFileSync(`/proc/${pid}/cmdline`, "utf8").split("\0");
      if (args[0] !== oxabl || !args.includes("lsp")) continue;
      const cwd = fs.readlinkSync(`/proc/${pid}/cwd`);
      if (cwd === root || cwd.startsWith(root + path.sep)) found.push({ pid: Number(pid), cwd });
    } catch {}
  }
  return found;
}

/** Milliseconds until the output channel holds `text` (polled every 20 ms), or null after `timeout`. */
async function logAfter(profile, text, timeout = 30_000) {
  const t = Date.now();
  while (Date.now() - t < timeout) {
    if (outputLog(profile).includes(text)) return Date.now() - t;
    await sleep(20);
  }
  return null;
}

/** Closes the folder row `name`, so the rows below it are in view. */
async function collapseFolder(page, name) {
  const row = page.locator(".pane", { has: page.locator(".pane-header", { hasText: "ABL Schema" }) }).locator(".monaco-list-row", { hasText: name }).first();
  if ((await row.getAttribute("aria-expanded")) === "true") {
    await row.click();
    if ((await row.getAttribute("aria-expanded")) === "true") await page.keyboard.press("ArrowLeft");
  }
}

/** The roots of the daemons registered under `runtimeDir`. */
function daemonRegistrations(runtimeDir) {
  const dir = path.join(runtimeDir, "oxabl", "daemon");
  if (!fs.existsSync(dir)) return [];
  return fs
    .readdirSync(dir)
    .filter((f) => f.endsWith(".json"))
    .map((f) => fs.readFileSync(path.join(dir, f), "utf8"))
    .filter((text) => alive(JSON.parse(text).pid));
}

const registered = (runtimeDir, dir) => daemonRegistrations(runtimeDir).some((r) => r.includes(dir));

const alive = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

export async function multiRoot(ctx) {
  const name = "multi-root";
  const oxabl = process.env.OXABL_E2E_OXABL;
  if (!oxabl) throw new Error("OXABL_E2E_OXABL must name the oxabl binary");
  if (!process.env.OXABL_E2E_RUNTIME) throw new Error("OXABL_E2E_RUNTIME must name a short directory");
  const runtimeDir = path.resolve(process.env.OXABL_E2E_RUNTIME);
  const server_path = path.resolve(oxabl);
  // The held start of 6b needs a binary of a build with the `test-panics` feature (`cargo build -p oxabl --locked --features oxabl_common/test-panics`);
  // without it the marker in the folder name does nothing and the check would test a short start.
  const holdable = fs.readFileSync(server_path).includes("OXABL-TEST-HOLD");
  check(`${name}: the oxabl binary honours the test hold marker`, holdable, "build it with `cargo build -p oxabl --locked --features oxabl_common/test-panics`");
  fs.mkdirSync(runtimeDir, { recursive: true, mode: 0o700 });
  const profile = makeProfile(name, {
    settings: {
      "oxabl.updates": "off",
      "oxabl.server.path": server_path,
      "oxabl.build.enable": false,
    },
  });
  const root = profile.workspace;
  for (const [file, text] of Object.entries(FILES)) {
    fs.mkdirSync(path.dirname(path.join(root, file)), { recursive: true });
    fs.writeFileSync(path.join(root, file), text);
  }
  const [alpha, beta, gamma, slow] = ["alpha", "beta", "gamma", SLOW_DIR].map((d) => path.join(root, d));
  const outside = path.join(root, "outside.p");
  const wsFile = path.join(root, "two.code-workspace");
  fs.writeFileSync(wsFile, workspaceFile("alpha", "beta", "gamma"));
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  // A short idle time: the daemon of a folder that left the window exits soon after its client.
  const env = { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir, OXABL_DAEMON_IDLE_TIMEOUT: "8s" };
  const logged = (text, timeout) =>
    waitForLog(profile, text, [], timeout)
      .then(() => true)
      .catch(() => false);
  try {
    let vs = await launch(ctx.exe, profile, { target: wsFile, open: ["alpha/main.p"], env });
    try {
      await runCommand(vs.page, "Problems: Focus on Problems View");
      // 1. Only `alpha/main.p` is open: its server answers, and `beta` has none.
      await logged(`starting the language server of folder "alpha" (${alpha})`, 60_000);
      await definitionOpens(vs.page, "alpha", "alpha");
      await sleep(1500);
      check(`${name}: no daemon serves the beta folder before one of its files opens`, !registered(runtimeDir, beta), `registrations: ${daemonRegistrations(runtimeDir).join(" ")}`);

      // 1b. The ABL Schema view has a node for each folder. Opening one starts that folder's server.
      await runCommand(vs.page, "Explorer: Focus on ABL Schema View");
      let seen = [];
      const nodes = await until(
        "the folder nodes",
        async () => ((seen = await schemaRows(vs.page)), ["alpha", "beta", "gamma"].every((n) => startsWith(seen, n))),
        30_000,
      );
      check(`${name}: the schema view has a node for each folder`, nodes, `rows: ${seen.join(" | ")}`);
      await expandFolder(vs.page, "alpha");
      const alphaTable = await until("Alpha in the view", async () => ((seen = await schemaRows(vs.page)), startsWith(seen, "Alpha")), 30_000);
      check(`${name}: the alpha node lists Alpha and not Beta`, alphaTable && !startsWith(seen, "Beta"), `rows: ${seen.join(" | ")}`);
      check(`${name}: opening the alpha node starts no server for beta`, !registered(runtimeDir, beta), `registrations: ${daemonRegistrations(runtimeDir).join(" ")}`);
      await expandFolder(vs.page, "beta");
      const betaTable = await until("Beta in the view", async () => ((seen = await schemaRows(vs.page)), startsWith(seen, "Beta")), 30_000);
      check(`${name}: the beta node lists Beta`, betaTable, `rows: ${seen.join(" | ")}`);
      check(`${name}: opening the beta node starts the daemon of beta`, registered(runtimeDir, beta), `registrations: ${daemonRegistrations(runtimeDir).join(" ")}`);
      await screenshot(vs.page, `${name}-schema`);

      // 2. Open `beta/main.p`: the server of `beta` finds `Beta`.
      await openFile(vs.page, path.join(beta, "main.p"));
      await definitionOpens(vs.page, "beta", "beta");

      // 3. The hover on the table shows it.
      await openFile(vs.page, path.join(beta, "main.p"));
      await moveTo(vs.page, 1, "FIND FIRST ".length + 2);
      await vs.page.keyboard.press("Control+K");
      await vs.page.keyboard.press("Control+I");
      const hovered = await until("the hover of Beta", async () => (await rows(vs.page, ".monaco-hover")).some((t) => t.includes("TABLE Beta")), 15_000);
      check(`${name}: the hover on Beta shows the table`, hovered, `hover: ${(await rows(vs.page, ".monaco-hover")).join(" | ")}`);
      await screenshot(vs.page, `${name}-hover`);

      // 3b. The third folder starts with its first file.
      await openFile(vs.page, path.join(gamma, "main.p"));
      await definitionOpens(vs.page, "gamma", "gamma");

      // 4. Each folder's server started, and logged it.
      for (const [folder, dir] of [["alpha", alpha], ["beta", beta], ["gamma", gamma]]) {
        check(`${name}: the output channel logs the start of the ${folder} server`, await logged(`starting the language server of folder "${folder}" (${dir})`, 10_000));
      }

      // 5. A file outside every folder gets no server. The Problems panel shows findings of folder files
      // at the same moment (positive control), and none of `outside.p`.
      await openFile(vs.page, outside);
      check(
        `${name}: a file outside every folder is logged`,
        await logged(`${outside} is outside every workspace folder; no language server serves it`, 15_000),
      );
      await sleep(8_000);
      const body = await bodyText(vs.page);
      check(`${name}: the Problems panel is rendered with the findings of the folder files`, body.includes(finding("beta")) && body.includes(finding("gamma")));
      check(`${name}: no server reports the finding of a file outside every folder`, !body.includes(finding("outside")), "the Problems panel shows the finding of outside.p");

      // 6. A folder removed from the workspace file stops its server; the window stays multi-root.
      fs.writeFileSync(wsFile, workspaceFile("alpha", "gamma"));
      const reload = await logAfter(profile, `stopped the language server of folder "beta"`);
      check(`${name}: removing the beta folder logs that its server stopped`, reload !== null);
      const gone = await until("the daemon of beta to exit", async () => !registered(runtimeDir, beta), 40_000);
      check(`${name}: the daemon of beta is gone after its client stopped`, gone, `registrations: ${daemonRegistrations(runtimeDir).join(" ")}`);
      await openFile(vs.page, path.join(alpha, "main.p"));
      await definitionOpens(vs.page, "alpha", "alpha after the removal");
      await openFile(vs.page, path.join(gamma, "main.p"));
      await definitionOpens(vs.page, "gamma", "gamma after the removal");

      // 6b. A folder removed while its server starts leaves no `oxabl lsp` process for its root, and no
      // provider of its client. `shutdown` of the client library throws unless the client is `Running`, and
      // a client is `Starting` until `initialize` answers, which the held daemon delays by 12 s: the
      // extension must wait for the start, then stop the client. The removal must reach the extension
      // inside that hold, or the check tests a stop of a running client and passes for nothing (see below).
      fs.writeFileSync(wsFile, workspaceFile("alpha", "gamma", SLOW_DIR));
      await sleep(2_000);
      const startLine = `starting the language server of folder "${SLOW_DIR}" (${slow})`;
      let removedAt = 0;
      const remover = (async () => {
        const t = Date.now();
        while (!outputLog(profile).includes(startLine) && Date.now() - t < 30_000) await sleep(5);
        removedAt = Date.now();
        fs.writeFileSync(wsFile, workspaceFile("alpha", "gamma"));
      })();
      await openFile(vs.page, path.join(slow, "main.p")).catch(() => {});
      await remover;
      check(`${name}: the slow folder started its server`, outputLog(profile).includes(startLine));
      // A stop of a running client ends at once. A stop that came during the hold waits for the start, which
      // answers about 12 s after the spawn: the stop line comes late. A removal that arrived after the hold
      // gives an early stop line, and the check fails, so a vacuous run is flagged. A slow machine cannot fail it.
      const stopLine = `stopped the language server of folder "${SLOW_DIR}"`;
      const stoppedLogged = await logged(stopLine, 40_000);
      const stampOf = (text) => {
        const line = text.split("\n").find((l) => l.includes(stopLine));
        const m = line && /^(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}:\d{2}\.\d{3})/.exec(line);
        return m ? new Date(`${m[1]}T${m[2]}`).getTime() : null;
      };
      const stoppedAt = stampOf(outputLog(profile));
      const waited = stoppedAt === null ? null : stoppedAt - removedAt;
      check(
        `${name}: the stop of the slow folder came after the removal waited for its start`,
        stoppedLogged && waited !== null && waited >= 4_000,
        `the stop line came ${waited} ms after the removal was written (wanted 4000 or more; the beta removal took ${reload} ms)`,
      );
      const stopped = await until("no oxabl lsp process for the slow folder", async () => !lspProcesses(server_path, root).some((p) => p.cwd === slow), 30_000);
      check(`${name}: no oxabl lsp process serves a folder removed while it started`, stopped, `processes: ${JSON.stringify(lspProcesses(server_path, root))}`);
      // A client that was stopped before it registered its features keeps its providers and its document sync:
      // an edit of a file that its selector matches makes it write a failed notification to the channel.
      // The file of the removed folder is still open in its tab.
      await vs.page.locator(".tabs-container .tab", { hasText: "OXABL-TEST-HOLD" }).first().click();
      await vs.page.locator(".monaco-editor .view-lines").first().click();
      await vs.page.keyboard.type("x");
      await sleep(3_000);
      await vs.page.keyboard.press("Control+Z");
      const leaked = outputLog(profile)
        .split("\n")
        .filter((l) => l.includes("Updating trace failed") || l.includes("Sending document notification"));
      check(`${name}: an edit in the removed folder reaches no stopped client`, leaked.length === 0, `channel: ${leaked.join(" | ")}`);
      // The held daemon registers after 12 s: let it, so the run can stop it at the end.
      await sleep(Math.max(0, 17_000 - (Date.now() - removedAt)));

      // 6c. Down to one folder: one server for the window, not a folder server next to it.
      fs.writeFileSync(wsFile, workspaceFile("alpha"));
      check(`${name}: going down to one folder logs that the gamma server stopped`, await logged(`stopped the language server of folder "gamma"`, 30_000));
      await openFile(vs.page, path.join(alpha, "main.p"));
      const one = await until("one oxabl lsp process", async () => lspProcesses(server_path, root).length === 1, 30_000);
      await sleep(3_000);
      check(`${name}: one oxabl lsp process serves the one folder`, one && lspProcesses(server_path, root).length === 1, `processes: ${JSON.stringify(lspProcesses(server_path, root))}`);
    } finally {
      await vs.close();
    }

    // 7. The folders in the other order: each table still finds its own `.df`.
    fs.writeFileSync(wsFile, workspaceFile("beta", "alpha"));
    vs = await launch(ctx.exe, profile, { target: wsFile, open: ["beta/main.p"], env });
    try {
      await runCommand(vs.page, "Problems: Focus on Problems View");
      await definitionOpens(vs.page, "beta", "beta first");
      await openFile(vs.page, path.join(alpha, "main.p"));
      await definitionOpens(vs.page, "alpha", "beta first, alpha");
    } finally {
      await vs.close();
    }

    // 8. A restart starts the folder that the schema view opened, though no file of it is open.
    fs.writeFileSync(wsFile, workspaceFile("alpha", "beta"));
    vs = await launch(ctx.exe, profile, { target: wsFile, open: ["alpha/main.p"], env });
    try {
      await until("the ABL Schema view in the Explorer", async () => (await rows(vs.page, ".pane-header")).some((t) => t.includes("ABL Schema")), 60_000);
      await runCommand(vs.page, "Explorer: Focus on ABL Schema View");
      await until("the beta node", async () => startsWith(await schemaRows(vs.page), "beta"), 60_000);
      await expandFolder(vs.page, "beta");
      let seen = [];
      await until("Beta in the view", async () => ((seen = await schemaRows(vs.page)), startsWith(seen, "Beta")), 60_000);
      const before = lspProcesses(server_path, root).find((p) => p.cwd === beta);
      check(`${name}: the beta node started the server of beta with no beta file open`, before !== undefined, `processes: ${JSON.stringify(lspProcesses(server_path, root))}`);
      const starts = () => outputLog(profile).split(`starting the language server of folder "beta"`).length - 1;
      check(`${name}: the beta server has started once`, starts() === 1, `starts: ${starts()}`);
      await runCommand(vs.page, "oxabl: Restart Server");
      const again = await until("a new server for beta", async () => lspProcesses(server_path, root).some((p) => p.cwd === beta && p.pid !== before?.pid), 60_000);
      check(`${name}: a restart starts the server of the beta folder again`, again && starts() === 2, `starts: ${starts()}, processes: ${JSON.stringify(lspProcesses(server_path, root))}`);
      check(`${name}: the daemon of beta is registered after the restart`, registered(runtimeDir, beta), `registrations: ${daemonRegistrations(runtimeDir).join(" ")}`);
      const listed = await until("Beta in the view", async () => ((seen = await schemaRows(vs.page)), startsWith(seen, "Beta")), 30_000);
      check(`${name}: the beta node lists Beta after the restart`, listed, `rows: ${seen.join(" | ")}`);
    } finally {
      await vs.close();
    }

    // 9. A folder whose server cannot start: its schema node is opened once, and the start is not tried again by
    // itself. A folder with a large schema, never started by a file, lists its tables when its node opens.
    fs.writeFileSync(wsFile, workspaceFile("alpha", "big", BOOM_DIR));
    vs = await launch(ctx.exe, profile, { target: wsFile, open: ["alpha/main.p"], env });
    try {
      await until("the ABL Schema view in the Explorer", async () => (await rows(vs.page, ".pane-header")).some((t) => t.includes("ABL Schema")), 60_000);
      await runCommand(vs.page, "Explorer: Focus on ABL Schema View");
      let seen = [];
      await until("the folder nodes", async () => ((seen = await schemaRows(vs.page)), ["alpha", "big", "boom"].every((n) => startsWith(seen, n))), 60_000);
      // Checks that the node of a folder that no file started lists its tables (the folder starts for the node).
      // It does not guard the order of the schema requests: it passes on the code that had that defect.
      const bigStart = Date.now();
      await expandFolder(vs.page, "big");
      const bigListed = await until("the tables of big", async () => ((seen = await schemaRows(vs.page)), startsWith(seen, "Tbl0000")), 60_000);
      check(`${name}: an unstarted folder with a large schema lists its tables when its node opens`, bigListed, `after ${Date.now() - bigStart} ms, rows: ${seen.slice(0, 8).join(" | ")}`);
      console.log(`   big node listed its tables ${Date.now() - bigStart} ms after the click`);
      await collapseFolder(vs.page, "big");
      await expandFolder(vs.page, "boom");
      // Count each error notification once: tag the nodes that were counted. One failed start of the client
      // library gives two messages (no connection, initialization failed); a retry loop gives many more.
      let errorToasts = 0;
      const toastTexts = [];
      const end = Date.now() + 15_000;
      while (Date.now() < end) {
        const fresh = await vs.page.evaluate(() => {
          const texts = [];
          for (const n of document.querySelectorAll(".notifications-toasts .notification-list-item")) {
            if (n.dataset.counted || !n.textContent.includes("oxabl")) continue;
            n.dataset.counted = "1";
            texts.push(n.textContent.replace(/\s+/g, " ").trim());
          }
          return texts;
        });
        errorToasts += fresh.length;
        toastTexts.push(...fresh);
        await sleep(100);
      }
      const bootStarts = outputLog(profile).split(`starting the language server of folder "${BOOM_DIR}"`).length - 1;
      check(`${name}: a folder whose server fails to start is started once for its opened node`, bootStarts === 1, `starts: ${bootStarts}`);
      check(`${name}: a folder whose server fails to start shows the two messages of one failed start and no more`, errorToasts <= 2, `messages: ${errorToasts}: ${toastTexts.join(" | ")}`);
      await screenshot(vs.page, `${name}-failed-folder`);
    } finally {
      await vs.close();
    }
  } finally {
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
