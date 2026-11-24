import fs from "node:fs";
import path from "node:path";
import { runCommand, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: Find All References lists the uses of a local
// variable and the callers of a program in a file that is not open, and the call hierarchy shows the
// callers and the calls of a procedure and of the program. OXABL_E2E_RUNTIME names a short directory.

const MAIN = [
  "/* References in a made-up order program. */",
  "DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.",
  "",
  "RUN util/recalc.p.",
  "iTotal = iTotal + 1.",
  "RUN tally.",
  "",
  "PROCEDURE tally:",
  "    iTotal = iTotal * 2.",
  "END PROCEDURE.",
  "",
].join("\n");

const FILES = {
  "oxabl.toml": '[workspace]\nname = "references"\n[workspace.sources]\ninclude_paths = ["."]\n',
  "main.p": MAIN,
  "batch.p": "/* runs the recalculation at night */\nRUN util/recalc.p.\nRUN main.p.\n",
  "util/recalc.p": "/* recalculates */\nMESSAGE \"recalc\".\n",
};

const rows = (page, selector) =>
  page.evaluate((sel) => [...document.querySelectorAll(sel)].map((r) => r.textContent.replace(/\u00a0/g, " ").trim()), selector);

const sidebarRows = (page) => rows(page, ".part.sidebar .monaco-list-row");
const sidebarText = (page) => page.evaluate(() => document.querySelector(".part.sidebar")?.textContent ?? "");

const cursor = async (page) => {
  const text = await page.evaluate(() => document.querySelector('[id="status.editor.selection"]')?.textContent ?? "");
  const m = /Ln (\d+), Col (\d+)/.exec(text);
  return m ? { line: Number(m[1]), col: Number(m[2]) } : null;
};

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

function lineCol(text, needle, into = 1) {
  const at = text.indexOf(needle);
  const before = text.slice(0, at);
  const line = before.split("\n").length;
  return { line, col: at - before.lastIndexOf("\n") + into };
}

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 300 })
    .then(() => true)
    .catch(() => false);

export async function references(ctx) {
  const name = "references";
  const oxabl = process.env.OXABL_E2E_OXABL;
  if (!oxabl) throw new Error("OXABL_E2E_OXABL must name the oxabl binary");
  if (!process.env.OXABL_E2E_RUNTIME) throw new Error("OXABL_E2E_RUNTIME must name a short directory");
  const runtimeDir = path.resolve(process.env.OXABL_E2E_RUNTIME);
  fs.mkdirSync(runtimeDir, { recursive: true, mode: 0o700 });
  const profile = makeProfile(name, {
    settings: {
      "oxabl.updates": "off",
      "oxabl.server.path": path.resolve(oxabl),
      "oxabl.build.enable": false,
    },
  });
  for (const [file, text] of Object.entries(FILES)) {
    fs.mkdirSync(path.dirname(path.join(profile.workspace, file)), { recursive: true });
    fs.writeFileSync(path.join(profile.workspace, file), text);
  }
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, {
    open: ["main.p"],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const page = vs.page;
  try {
    const lang = await waitForLanguage(page, "ABL");
    check(`${name}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);
    // The document link shows that the language server answers.
    await until("the language server", async () => (await rows(page, ".monaco-editor .detected-link")).length > 0, 60_000);

    // 1. The uses of a local variable, in the open file.
    const use = lineCol(MAIN, "iTotal + 1", 2);
    await moveTo(page, use.line, use.col);
    await runCommand(page, "References: Find All References");
    const local = await until("5 results", async () => /5 results in 1 file/.test(await sidebarText(page)));
    check(`${name}: a local variable has its 5 references`, local, `sidebar: ${(await sidebarText(page)).slice(0, 300)}`);
    await screenshot(page, `${name}-local`);

    // 2. The callers of a program, in a file that is not open.
    const run = lineCol(MAIN, "util/recalc.p", 2);
    await moveTo(page, run.line, run.col);
    await runCommand(page, "References: Find All References");
    const callers = await until("2 results in 2 files", async () => /2 results in 2 files/.test(await sidebarText(page)));
    check(
      `${name}: a program's callers include a file that is not open`,
      callers && (await sidebarRows(page)).some((t) => t.includes("batch.p")),
      `sidebar: ${(await sidebarText(page)).slice(0, 300)}`,
    );
    await screenshot(page, `${name}-program`);

    // 3. The callers of a procedure.
    const tally = lineCol(MAIN, "tally.", 2);
    await moveTo(page, tally.line, tally.col);
    await runCommand(page, "Calls: Show Call Hierarchy");
    const tallied = await until("main.p calls tally", async () => {
      const shown = await sidebarRows(page);
      return shown.some((t) => t.startsWith("tally")) && shown.some((t) => t.startsWith("main.p"));
    });
    check(`${name}: the call hierarchy shows the caller of a procedure`, tallied, `rows: ${(await sidebarRows(page)).join(" | ")}`);
    await screenshot(page, `${name}-incoming`);

    // 4. The calls of the program itself, from a keyword outside every routine. VS Code asks
    // for no item where the cursor is on no word.
    const keyword = lineCol(MAIN, "DEFINE VARIABLE", 2);
    await moveTo(page, keyword.line, keyword.col);
    await runCommand(page, "Calls: Show Call Hierarchy");
    const program = await until("batch.p calls main.p", async () => {
      const shown = await sidebarRows(page);
      return shown.some((t) => t.startsWith("main.p")) && shown.some((t) => t.startsWith("batch.p"));
    });
    check(`${name}: the call hierarchy shows the caller of the program`, program, `rows: ${(await sidebarRows(page)).join(" | ")}`);
    await runCommand(page, "Calls: Show Outgoing Calls");
    const calls = await until("main.p calls recalc.p and tally", async () => {
      const shown = await sidebarRows(page);
      return shown.some((t) => t.startsWith("recalc.p")) && shown.some((t) => t.startsWith("tally"));
    });
    check(`${name}: the call hierarchy shows the calls of the program`, calls, `rows: ${(await sidebarRows(page)).join(" | ")}`);
    await screenshot(page, `${name}-outgoing`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
