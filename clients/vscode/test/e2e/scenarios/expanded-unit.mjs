import fs from "node:fs";
import path from "node:path";
import { bodyText, runCommand, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: "oxabl: Show Expanded Compilation Unit" opens a
// read-only view of the source oxabl parsed, with each include folded, F12 back to the source line,
// the findings of the same analysis, and a refresh after an unsaved edit of the root.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const FILES = {
  "oxabl.toml": '[workspace]\nname = "expanded-unit"\n[workspace.sources]\ninclude_paths = ["."]\n',
  "shared.i": "DEFINE VARIABLE used-v AS INTEGER NO-UNDO.\nDEFINE VARIABLE spare-v AS INTEGER NO-UNDO.\n",
  "mid.i": "/* mid */\n{shared.i}\n",
  "nested.p": "{mid.i}\nMESSAGE used-v.\n",
  "missing.p": "{nope.i}\nMESSAGE 1.\n",
};
const UNUSED = "unused variable `spare-v`";

const rows = (page, selector) =>
  page.evaluate((sel) => [...document.querySelectorAll(sel)].map((r) => r.textContent.replace(/\u00a0/g, " ").trim()), selector);

/** The rendered lines of the editor in the active group, top to bottom. */
const visibleLines = (page) =>
  page.evaluate(() => {
    const lines = [...document.querySelectorAll(".editor-group-container.active .monaco-editor .view-lines .view-line")];
    lines.sort((a, b) => parseFloat(a.style.top) - parseFloat(b.style.top));
    return lines.map((l) => l.textContent.replace(/\u00a0/g, " "));
  });

const activeTab = (page) => rows(page, ".editor-group-container.active .tab.active");

async function openFile(page, name) {
  await page.keyboard.press("Control+P");
  await page.locator(".quick-input-widget input").first().waitFor({ state: "visible" });
  await page.keyboard.type(name);
  await sleep(600);
  await page.keyboard.press("Enter");
  await waitFor(`${name} open`, async () => (await activeTab(page)).some((t) => t.includes(name)), { timeout: 15_000 });
}

async function goToLine(page, line) {
  await page.keyboard.press("Control+G");
  await page.locator(".quick-input-widget input").first().waitFor({ state: "visible", timeout: 15_000 });
  await page.keyboard.type(String(line));
  await page.keyboard.press("Enter");
}

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 300 })
    .then(() => true)
    .catch(() => false);

export async function expandedUnit(ctx) {
  const name = "expanded-unit";
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
  for (const [file, text] of Object.entries(FILES)) fs.writeFileSync(path.join(profile.workspace, file), text);
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, {
    open: ["nested.p"],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const page = vs.page;
  try {
    const lang = await waitForLanguage(page, "ABL");
    check(`${name}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);

    // 1. The view opens beside the root, with every include folded.
    await runCommand(page, "oxabl: Show Expanded Compilation Unit");
    let lines = [];
    const opened = await until("the expanded view", async () => (lines = await visibleLines(page)).some((l) => l.includes("the expanded compilation unit of")), 60_000);
    check(`${name}: the view shows the expanded compilation unit`, opened, `lines: ${lines.join(" | ")}`);
    check(`${name}: the view's tab says it is the expansion`, (await activeTab(page)).some((t) => t.includes("nested (expanded).p")), `tab: ${(await activeTab(page)).join(" | ")}`);
    const folded = await until("the includes to fold", async () => {
      lines = await visibleLines(page);
      return lines.some((l) => l.includes("▼ mid.i")) && !lines.some((l) => l.includes("spare-v AS INTEGER"));
    });
    check(`${name}: each include starts folded`, folded, `lines: ${lines.join(" | ")}`);
    check(`${name}: the view is ABL`, (await waitForLanguage(page, "ABL", 10_000)).ok);
    await screenshot(page, `${name}-folded`);

    // 2. Unfolded, the include text shows between its markers.
    await runCommand(page, "Unfold All");
    const unfolded = await until("the include text", async () => (lines = await visibleLines(page)).some((l) => l.includes("spare-v AS INTEGER")));
    check(`${name}: unfolding shows the include text`, unfolded, `lines: ${lines.join(" | ")}`);
    check(`${name}: the nested include has its own markers`, lines.some((l) => l.includes("▼ shared.i")) && lines.some((l) => l.includes("▲ shared.i")), `lines: ${lines.join(" | ")}`);

    // 3. The view is read-only.
    const before = (await visibleLines(page)).join("\n");
    await page.keyboard.type("zzz");
    await sleep(500);
    check(`${name}: typing does not change the view`, !(await visibleLines(page)).join("\n").includes("zzz") && (await visibleLines(page)).join("\n") === before);

    // 4. The finding of the same analysis shows, in the Problems panel.
    await page.keyboard.press("Control+Shift+M");
    check(`${name}: the unused variable in the include shows`, await until(`"${UNUSED}"`, async () => (await bodyText(page)).includes(UNUSED), 30_000));

    // 5. F12 on an expanded line opens the line that wrote it.
    await runCommand(page, "View: Focus Active Editor Group");
    const index = (await visibleLines(page)).findIndex((l) => l.includes("spare-v AS INTEGER"));
    await goToLine(page, index + 1);
    await page.keyboard.press("F12");
    const jumped = await until("shared.i open", async () => (await activeTab(page)).some((t) => t.includes("shared.i")), 15_000);
    check(`${name}: F12 opens the include that wrote the line`, jumped, `active tab: ${(await activeTab(page)).join(" | ")}`);
    await screenshot(page, `${name}-source`);
    // Open on its own, shared.i has fragment findings of its own; close it so the
    // Problems panel shows only the view's.
    await page.keyboard.press("Control+W");

    // 6. An unsaved edit of the root refreshes the view.
    await runCommand(page, "View: Focus First Editor Group");
    await waitFor("nested.p focused", async () => (await activeTab(page)).some((t) => t.startsWith("nested.p")), { timeout: 10_000 });
    await page.keyboard.press("Control+End");
    await page.keyboard.type("MESSAGE spare-v.\n");
    check(`${name}: the finding goes away after the root reads the variable`, await until(`"${UNUSED}" to go`, async () => !(await bodyText(page)).includes(UNUSED), 30_000));
    await screenshot(page, `${name}-refresh`);
    check(`${name}: nested.p on disk is unchanged`, fs.readFileSync(path.join(profile.workspace, "nested.p"), "utf8") === FILES["nested.p"]);

    // 7. A missing include makes a reduced expansion that says so.
    await openFile(page, "missing.p");
    await runCommand(page, "oxabl: Show Expanded Compilation Unit");
    check(`${name}: a missing include says the expansion is reduced`, await until("the reduced notice", async () => (await bodyText(page)).includes("is reduced"), 60_000));
    check(
      `${name}: the view names the missing include`,
      await until("the missing include in the view", async () => (await visibleLines(page)).some((l) => l.includes("include `nope.i` was not found")), 30_000),
    );
    await screenshot(page, `${name}-reduced`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
