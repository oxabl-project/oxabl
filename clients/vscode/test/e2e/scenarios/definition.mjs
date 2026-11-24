import fs from "node:fs";
import path from "node:path";
import { runCommand, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: F12 on a `RUN` target opens the program, F12
// on a local variable puts the cursor on its declaration, and the `RUN` target is a document link.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const MAIN = [
  "/* Navigation in a made-up order program. */",
  "DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.",
  "",
  "RUN util/recalc.p.",
  "iTotal = iTotal + 1.",
  "",
].join("\n");

const FILES = {
  "oxabl.toml": '[workspace]\nname = "definition"\n[workspace.sources]\ninclude_paths = ["."]\n',
  "main.p": MAIN,
  "util/recalc.p": "/* recalculates */\nMESSAGE \"recalc\".\n",
};

const rows = (page, selector) =>
  page.evaluate((sel) => [...document.querySelectorAll(sel)].map((r) => r.textContent.replace(/\u00a0/g, " ").trim()), selector);

const activeTab = (page) => rows(page, ".editor-group-container.active .tab.active");

/** The 1-based cursor position from the status bar ("Ln 5, Col 3"). */
const cursor = async (page) => {
  const text = await page.evaluate(() => document.querySelector('[id="status.editor.selection"]')?.textContent ?? "");
  const m = /Ln (\d+), Col (\d+)/.exec(text);
  return m ? { line: Number(m[1]), col: Number(m[2]) } : null;
};

/** Put the cursor on 1-based `line`, `col` of the active editor. */
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

/** The 1-based line and column of `needle` in `text`, plus `into` characters. */
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

export async function definition(ctx) {
  const name = "definition";
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

    // 1. The `RUN` target is a link to the program.
    const linked = await until(
      "the RUN target link",
      async () => (await rows(page, ".monaco-editor .detected-link")).some((t) => t.includes("util/recalc.p")),
      60_000,
    );
    check(`${name}: the RUN target is a document link`, linked, `links: ${(await rows(page, ".monaco-editor .detected-link")).join(" | ")}`);

    // 2. F12 on the `RUN` target opens the program.
    const run = lineCol(MAIN, "util/recalc.p", 2);
    await moveTo(page, run.line, run.col);
    await page.keyboard.press("F12");
    const opened = await until("recalc.p open", async () => (await activeTab(page)).some((t) => t.includes("recalc.p")), 30_000);
    check(`${name}: F12 on a RUN target opens the program`, opened, `active tab: ${(await activeTab(page)).join(" | ")}`);
    await screenshot(page, `${name}-run`);

    // 3. F12 on a local variable puts the cursor on its declaration.
    await runCommand(page, "Go Back");
    await waitFor("main.p active", async () => (await activeTab(page)).some((t) => t.startsWith("main.p")), { timeout: 15_000 });
    const use = lineCol(MAIN, "iTotal + 1", 2);
    await moveTo(page, use.line, use.col);
    await page.keyboard.press("F12");
    const declared = lineCol(MAIN, "iTotal", 0);
    let at = null;
    const jumped = await until("the cursor on the declaration", async () => {
      at = await cursor(page);
      return at?.line === declared.line && at?.col === declared.col;
    });
    check(`${name}: F12 on a local variable goes to its declaration`, jumped, `cursor: ${JSON.stringify(at)}, wanted ${JSON.stringify(declared)}`);
    check(`${name}: main.p stays the active file`, (await activeTab(page)).some((t) => t.startsWith("main.p")));
    await screenshot(page, `${name}-variable`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
