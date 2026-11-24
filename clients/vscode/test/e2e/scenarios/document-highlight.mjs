import fs from "node:fs";
import path from "node:path";
import { screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: with the cursor on a variable, VS Code marks
// its other uses. A write gets the strong colour and a read the plain one. A word the server cannot
// resolve gets no read or write mark. VS Code itself marks such a word as text; that is its own
// fallback, not the server's answer.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const MAIN = [
  "/* Highlights in a made-up order program. */",
  "DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.",
  "",
  "iTotal = 1.",
  "iTotal = iTotal + 1.",
  "MESSAGE iTotal.",
  "iTotal = noSuchName.",
  "MESSAGE noSuchName.",
  "",
].join("\n");

const FILES = {
  "oxabl.toml": '[workspace]\nname = "document-highlight"\n',
  "main.p": MAIN,
};

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
  return { line: before.split("\n").length, col: at - before.lastIndexOf("\n") + into };
}

/** How many occurrence marks the editor shows: write (strong), read, and textual. */
const marks = (page) =>
  page.evaluate(() => {
    const count = (sel) => document.querySelectorAll(`.monaco-editor .view-overlays ${sel}`).length;
    return { write: count(".wordHighlightStrong"), read: count(".wordHighlight"), text: count(".wordHighlightText") };
  });

export async function documentHighlight(ctx) {
  const name = "document-highlight";
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

    // 1. A variable: the declaration and the assignments write, the other uses read.
    // The first request can come before the server has the file: move the cursor again until it answers.
    const use = lineCol(MAIN, "iTotal + 1", 2);
    let seen = null;
    let marked = false;
    for (let attempt = 0; attempt < 30 && !marked; attempt++) {
      await moveTo(page, use.line, use.col + (attempt % 2));
      await sleep(1500);
      seen = await marks(page);
      marked = seen.write === 4 && seen.read === 2;
    }
    check(`${name}: a variable shows 4 writes and 2 reads`, marked, `marks: ${JSON.stringify(seen)}`);
    await screenshot(page, `${name}-variable`);

    // 2. A name that does not resolve has no read or write mark.
    const unknown = lineCol(MAIN, "noSuchName.", 2);
    await moveTo(page, unknown.line, unknown.col);
    await sleep(2500);
    seen = await marks(page);
    check(`${name}: an unresolved name shows no read or write marks`, seen.write === 0 && seen.read === 0, `marks: ${JSON.stringify(seen)}`);
    await screenshot(page, `${name}-unresolved`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
