import fs from "node:fs";
import path from "node:path";
import { readLines, runCommand, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: the folds come from the server. The file has
// no indentation, so indentation folding finds no block, and only the server gives ranges the kind
// `comment`. The `&ANALYZE` and `#region` markers still fold. Expand Selection grows to the
// statement and the procedure.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const MAIN = [
  "/* A made-up program with no indentation.",
  "comment-inside */",
  "PROCEDURE ShowTotal:",
  'MESSAGE "proc-inside".',
  "END PROCEDURE.",
  "// #region totals",
  'MESSAGE "region-inside".',
  "// #endregion",
  "&ANALYZE-SUSPEND _UIB-CODE-BLOCK",
  'MESSAGE "analyze-inside".',
  "&ANALYZE-RESUME",
  "IF TRUE THEN DO:",
  'MESSAGE "then-inside".',
  "END.",
  "ELSE DO:",
  'MESSAGE "else-inside".',
  "END.",
  "PROCEDURE Owner:",
  "    DEFINE INPUT PARAMETER pCustNumber AS CHARACTER NO-UNDO.",
  '    if pCustNumber ne "" then',
  "        do:",
  '            MESSAGE "owner-inside".',
  "        end.",
  "END PROCEDURE.",
  "",
].join("\n");

const FILES = {
  "oxabl.toml": '[workspace]\nname = "folding"\n',
  "main.p": MAIN,
};

const visible = async (page) => (await readLines(page)).map((l) => l.text);
const shows = (lines, text) => lines.some((l) => l.includes(text));

/** The number of selected characters from the status bar ("Ln 4, Col 9 (13 selected)"), else 0. */
const selected = async (page) => {
  const text = await page.evaluate(() => document.querySelector('[id="status.editor.selection"]')?.textContent ?? "");
  const m = /\((\d+) selected\)/.exec(text);
  return m ? Number(m[1]) : 0;
};

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 500 })
    .then(() => true)
    .catch(() => false);

export async function folding(ctx) {
  const name = "folding";
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
    await page.locator(".monaco-editor .view-lines").first().click();

    // 1. Only the server gives a range of kind `comment`. Retry until the server has started.
    const comments = await until(
      "Fold All Block Comments to hide the comment",
      async () => {
        await runCommand(page, "Unfold All");
        await runCommand(page, "Fold All Block Comments");
        await sleep(300);
        return !shows(await visible(page), "comment-inside");
      },
      60_000,
    );
    let lines = await visible(page);
    check(`${name}: Fold All Block Comments folds the block comment`, comments, `lines: ${lines.join(" | ")}`);
    check(`${name}: the procedure stays open`, shows(lines, "proc-inside"), `lines: ${lines.join(" | ")}`);
    await screenshot(page, `${name}-comments`);

    // 2. Fold All folds the flat procedure by its syntax and keeps its END line.
    // By its key chord: the palette puts the recent "Unfold All" first for "Fold All".
    await runCommand(page, "Unfold All");
    await page.keyboard.press("Control+K");
    await page.keyboard.press("Control+0");
    await sleep(500);
    lines = await visible(page);
    check(`${name}: Fold All folds the procedure without indentation`, !shows(lines, "proc-inside"), `lines: ${lines.join(" | ")}`);
    check(`${name}: the folded procedure shows its END line`, shows(lines, "END PROCEDURE."), `lines: ${lines.join(" | ")}`);
    check(
      `${name}: Fold All folds both IF branches and keeps the ELSE line`,
      !shows(lines, "then-inside") && !shows(lines, "else-inside") && shows(lines, "ELSE DO:"),
      `lines: ${lines.join(" | ")}`,
    );
    await screenshot(page, `${name}-all`);

    // 3. Fold All Regions folds the #region and &ANALYZE markers, not the procedure.
    await runCommand(page, "Unfold All");
    await runCommand(page, "Fold All Regions");
    await sleep(500);
    lines = await visible(page);
    check(`${name}: Fold All Regions folds the #region`, !shows(lines, "region-inside"), `lines: ${lines.join(" | ")}`);
    check(`${name}: Fold All Regions folds the &ANALYZE block`, !shows(lines, "analyze-inside"), `lines: ${lines.join(" | ")}`);
    check(`${name}: Fold All Regions leaves the procedure open`, shows(lines, "proc-inside"), `lines: ${lines.join(" | ")}`);
    await runCommand(page, "Unfold All");

    // 4. Expand Selection grows from the string to the statement and the procedure.
    const line = MAIN.split("\n").findIndex((l) => l.includes("proc-inside")) + 1;
    await page.keyboard.press("Control+G");
    await page.locator(".quick-input-widget input").first().waitFor({ state: "visible", timeout: 15_000 });
    await page.keyboard.type(`${line}:12`);
    await page.keyboard.press("Enter");
    await sleep(300);
    const statement = 'MESSAGE "proc-inside".'.length;
    const procedure = MAIN.slice(MAIN.indexOf("PROCEDURE"), MAIN.indexOf("END PROCEDURE.") + "END PROCEDURE.".length).length;
    const sizes = [];
    for (let i = 0; i < 8; i++) {
      await page.keyboard.press("Shift+Alt+ArrowRight");
      await sleep(300);
      sizes.push(await selected(page));
    }
    check(`${name}: Expand Selection reaches the statement`, sizes.includes(statement), `sizes ${sizes.join(", ")}, want ${statement}`);
    check(`${name}: Expand Selection reaches the procedure`, sizes.includes(procedure), `sizes ${sizes.join(", ")}, want ${procedure}`);
    check(
      `${name}: each Expand Selection step contains the one before it`,
      sizes.every((s, i) => i === 0 || s >= sizes[i - 1]),
      `sizes ${sizes.join(", ")}`,
    );
    await screenshot(page, `${name}-selection`);

    // 5. Expand Selection reaches the IF statement that owns a DO block on its own line.
    const ownerLine = MAIN.split("\n").findIndex((l) => l.includes("owner-inside")) + 1;
    await page.keyboard.press("Control+G");
    await page.locator(".quick-input-widget input").first().waitFor({ state: "visible", timeout: 15_000 });
    await page.keyboard.type(`${ownerLine}:24`);
    await page.keyboard.press("Enter");
    await sleep(300);
    const owner = MAIN.slice(MAIN.indexOf("if pCustNumber"), MAIN.indexOf("end.\nEND PROCEDURE.") + "end.".length).length;
    const block = MAIN.slice(MAIN.indexOf("do:\n"), MAIN.indexOf("end.\nEND PROCEDURE.") + "end.".length).length;
    const ownerSizes = [];
    for (let i = 0; i < 8; i++) {
      await page.keyboard.press("Shift+Alt+ArrowRight");
      await sleep(300);
      ownerSizes.push(await selected(page));
    }
    check(`${name}: Expand Selection reaches the DO block`, ownerSizes.includes(block), `sizes ${ownerSizes.join(", ")}, want ${block}`);
    check(`${name}: Expand Selection reaches the IF that owns the DO block`, ownerSizes.includes(owner), `sizes ${ownerSizes.join(", ")}, want ${owner}`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
