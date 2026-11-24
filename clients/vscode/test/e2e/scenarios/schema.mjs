import fs from "node:fs";
import path from "node:path";
import { runCommand, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: the ABL Schema view lists the
// tables of the configured `.df`, the filter finds a field by its description, a
// click on the field opens the `.df` on its `ADD FIELD` line, "Copy table.field"
// copies the qualified name, the details page of a table opens, and a change of the
// `.df` on disk shows in the view, also for a `.df` outside the workspace folder.
// A schema directory without a `.df` is a warning above the tree.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const DF = [
  'ADD TABLE "Customer"',
  '  AREA "Data"',
  '  LABEL "Client"',
  "",
  'ADD FIELD "CustNum" OF "Customer" AS integer',
  '  FORMAT ">>>>9"',
  '  LABEL "Cust Num"',
  "  ORDER 10",
  "",
  'ADD FIELD "Phone" OF "Customer" AS character',
  '  FORMAT "x(20)"',
  '  DESCRIPTION "Home, work and mobile"',
  "  ORDER 20",
  "",
  'ADD INDEX "CustNum" ON "Customer"',
  "  UNIQUE",
  "  PRIMARY",
  '  INDEX-FIELD "CustNum" ASCENDING',
  "",
  'ADD TABLE "Bin"',
  '  AREA "Data"',
  "",
  'ADD FIELD "Shelf" OF "Bin" AS character',
  "  ORDER 10",
  "",
].join("\n");

const EXTRA = 'ADD TABLE "Crate"\n  AREA "Data"\n';

const FILES = {
  "oxabl.toml": '[workspace]\nname = "schema"\n[workspace.schema]\nfiles = ["db/shop.df", "nodf", "../outside/extra.df", "../later"]\n',
  "db/shop.df": DF,
  "main.p": "/* reads the shop */\nFIND FIRST Customer NO-LOCK NO-ERROR.\n",
};

/** The rows of the ABL Schema view, top to bottom. */
const schemaRows = (page) =>
  page.evaluate(() => {
    const pane = [...document.querySelectorAll(".pane")].find((p) => p.querySelector(".pane-header")?.textContent.includes("ABL Schema"));
    if (!pane) return [];
    return [...pane.querySelectorAll(".monaco-list-row")].map((r) => r.textContent.replace(/\u00a0/g, " ").trim());
  });

const schemaMessage = (page) =>
  page.evaluate(() => {
    const pane = [...document.querySelectorAll(".pane")].find((p) => p.querySelector(".pane-header")?.textContent.includes("ABL Schema"));
    return pane?.querySelector(".message")?.textContent.trim() ?? "";
  });

const tabs = (page, selector) =>
  page.evaluate((sel) => [...document.querySelectorAll(sel)].map((r) => r.textContent.replace(/\u00a0/g, " ").trim()), selector);

/** The 1-based cursor position from the status bar ("Ln 5, Col 3"). */
const cursor = async (page) => {
  const text = await page.evaluate(() => document.querySelector('[id="status.editor.selection"]')?.textContent ?? "");
  const m = /Ln (\d+), Col (\d+)/.exec(text);
  return m ? { line: Number(m[1]), col: Number(m[2]) } : null;
};

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 300 })
    .then(() => true)
    .catch(() => false);

const startsWith = (list, name) => list.some((r) => r.startsWith(name));

async function filter(page, text) {
  await runCommand(page, "oxabl: Filter Schema");
  // The Command Palette closes first; type once the filter's own box shows.
  await page.locator('.quick-input-widget input[placeholder^="Part of a table"]').first().waitFor({ state: "visible", timeout: 15_000 });
  await page.keyboard.type(text);
  await page.keyboard.press("Enter");
}

export async function schema(ctx) {
  const name = "schema";
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
      "explorer.openEditors.visible": 0,
    },
  });
  for (const [file, text] of Object.entries(FILES)) {
    fs.mkdirSync(path.dirname(path.join(profile.workspace, file)), { recursive: true });
    fs.writeFileSync(path.join(profile.workspace, file), text);
  }
  // A schema directory with no .df, and a .df outside the workspace folder.
  fs.mkdirSync(path.join(profile.workspace, "nodf"));
  const outside = path.join(profile.root, "outside", "extra.df");
  fs.mkdirSync(path.dirname(outside), { recursive: true });
  fs.writeFileSync(outside, EXTRA);
  // A schema directory outside the workspace that holds no .df yet.
  const later = path.join(profile.root, "later");
  fs.mkdirSync(later);
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

    // 1. The view lists the database and its tables. It shows once the extension has started.
    const registered = await until(
      "the ABL Schema view in the Explorer",
      async () => (await tabs(page, ".pane-header")).some((t) => t.includes("ABL Schema")),
      60_000,
    );
    check(`${name}: the Explorer has the ABL Schema view`, registered, `panes: ${(await tabs(page, ".pane-header")).join(" | ")}`);
    await runCommand(page, "Explorer: Focus on ABL Schema View");
    const folders = page.locator(".pane-header", { hasText: "workspace" }).first();
    if ((await folders.getAttribute("aria-expanded")) === "true") await folders.click();
    let seen = [];
    const listed = await until(
      "the ABL Schema view to list Customer and Bin",
      async () => ((seen = await schemaRows(page)), startsWith(seen, "Customer") && startsWith(seen, "Bin")),
      60_000,
    );
    check(`${name}: the view lists the database and its tables`, listed && startsWith(seen, "shop"), `rows: ${seen.join(" | ")}`);
    check(`${name}: the view lists the .df outside the workspace`, startsWith(seen, "extra"), `rows: ${seen.join(" | ")}`);
    const warned = await schemaMessage(page);
    check(`${name}: the view shows the warning of the empty schema directory`, warned.includes("matched no .df files"), `message: ${warned}`);
    await screenshot(page, `${name}-tree`);

    // 2. The filter finds a field by its description and keeps only that field.
    await filter(page, "mobile");
    const filtered = await until(
      "the filter to keep Phone only",
      async () => ((seen = await schemaRows(page)), startsWith(seen, "Phone") && !startsWith(seen, "CustNum") && !startsWith(seen, "Bin")),
    );
    check(`${name}: the filter finds Phone by its description`, filtered, `rows: ${seen.join(" | ")}`);
    const message = await schemaMessage(page);
    check(`${name}: the view says what the filter is`, message.includes("mobile"), `message: ${message}`);
    await screenshot(page, `${name}-filtered`);

    // 3. A click on the field opens the .df with the cursor on its ADD FIELD line.
    await page
      .locator(".pane .monaco-list-row", { hasText: "Phone" })
      .first()
      .click();
    const opened = await until("shop.df open", async () => (await tabs(page, ".editor-group-container.active .tab.active")).some((t) => t.includes("shop.df")));
    check(`${name}: a click on the field opens shop.df`, opened, `active tab: ${(await tabs(page, ".tab.active")).join(" | ")}`);
    const lines = DF.split("\n");
    const wantLine = lines.findIndex((l) => l.startsWith('ADD FIELD "Phone"')) + 1;
    let at = null;
    const placed = await until("the cursor on the ADD FIELD line", async () => ((at = await cursor(page)), at?.line === wantLine));
    check(`${name}: the cursor is on the ADD FIELD line`, placed && at?.col === lines[wantLine - 1].indexOf('"') + 1, `cursor ${JSON.stringify(at)}, want line ${wantLine}`);
    await screenshot(page, `${name}-opened`);

    // 3b. Copy table.field from the context menu of the field. A click on a menu
    // that has just opened can miss, so the menu gets a moment first.
    await page.locator(".pane .monaco-list-row", { hasText: "Phone" }).first().click({ button: "right" });
    const item = page.locator(".context-view .monaco-menu .action-label", { hasText: "Copy table.field" }).first();
    await item.waitFor({ state: "visible", timeout: 15_000 });
    await sleep(1000);
    await item.click();
    let copied = "";
    const qualified = await until("the clipboard to hold Customer.Phone", async () => (copied = await vs.app.evaluate(({ clipboard }) => clipboard.readText())) === "Customer.Phone");
    check(`${name}: Copy table.field copies Customer.Phone`, qualified, `clipboard: ${JSON.stringify(copied)}`);
    await page.keyboard.press("Escape");

    // 4. A change of the .df on disk shows in the view.
    await runCommand(page, "oxabl: Clear Schema Filter");
    await filter(page, "pallet");
    const none = await until("no match", async () => (await schemaMessage(page)).includes("Nothing matches"));
    check(`${name}: a filter with no match says so`, none, `message: ${await schemaMessage(page)}`);
    fs.writeFileSync(path.join(profile.workspace, "db/shop.df"), `${DF}\nADD TABLE "Pallet"\n  AREA "Data"\n`);
    const grown = await until("Pallet in the view", async () => ((seen = await schemaRows(page)), startsWith(seen, "Pallet")), 30_000);
    check(`${name}: a table added to the .df on disk shows in the view`, grown, `rows: ${seen.join(" | ")}`);
    await screenshot(page, `${name}-refreshed`);

    // 4b. A change of the .df outside the workspace folder shows too.
    await runCommand(page, "oxabl: Clear Schema Filter");
    fs.writeFileSync(outside, `${EXTRA}\nADD TABLE "Barrel"\n  AREA "Data"\n`);
    const expandExtra = page.locator(".pane .monaco-list-row", { hasText: "extra" }).first();
    await until("extra in the view", async () => startsWith(await schemaRows(page), "extra"));
    if ((await expandExtra.getAttribute("aria-expanded")) === "false") await expandExtra.click();
    const barrel = await until("Barrel in the view", async () => ((seen = await schemaRows(page)), startsWith(seen, "Barrel")), 30_000);
    check(`${name}: a table added to a .df outside the workspace shows in the view`, barrel, `rows: ${seen.join(" | ")}`);

    // 4c. A .df created in an outside schema directory that had none shows too.
    // Upper case: the loader takes any case of the extension, and so must the watcher.
    fs.writeFileSync(path.join(later, "LATE.DF"), 'ADD TABLE "Tardy"\n  AREA "Data"\n');
    const tardy = await until("the LATE database in the view", async () => ((seen = await schemaRows(page)), startsWith(seen, "LATE")), 30_000);
    check(`${name}: a .DF created in an empty schema directory outside the workspace shows`, tardy, `rows: ${seen.join(" | ")}`);

    // 5. The details page of the table.
    await until("Customer in the view", async () => startsWith(await schemaRows(page), "Customer"));
    const row = page.locator(".pane .monaco-list-row", { hasText: "Customer" }).first();
    await row.hover();
    await row.locator('.action-label[aria-label="Show Table Details"]').click();
    const details = await until("the details tab", async () => (await tabs(page, ".tab")).some((t) => t.includes("Customer (shop)")));
    check(`${name}: Show Table Details opens a page for the table`, details, `tabs: ${(await tabs(page, ".tab")).join(" | ")}`);
    await screenshot(page, `${name}-details`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}

// The workspace folder is `src`, and its `oxabl.toml` is one directory above:
// the view shows the schema it names, and an edit of it shows in the view.
export async function schemaConfigAbove(ctx) {
  const name = "schema-config-above";
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
      "explorer.openEditors.visible": 0,
    },
  });
  const project = profile.workspace;
  const config = path.join(project, "oxabl.toml");
  const files = {
    "oxabl.toml": '[workspace]\nname = "above"\n[workspace.schema]\nfiles = ["db/alpha.df"]\n',
    "db/alpha.df": 'ADD TABLE "Alpha"\n  AREA "Data"\n',
    "db/beta.df": 'ADD TABLE "Beta"\n  AREA "Data"\n',
    "src/main.p": "MESSAGE 1.\n",
  };
  for (const [file, text] of Object.entries(files)) {
    fs.mkdirSync(path.dirname(path.join(project, file)), { recursive: true });
    fs.writeFileSync(path.join(project, file), text);
  }
  installVsix(ctx.exe, profile, ctx.vsix);
  profile.workspace = path.join(project, "src");
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, {
    open: ["main.p"],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const page = vs.page;
  try {
    const lang = await waitForLanguage(page, "ABL");
    check(`${name}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);
    await until("the ABL Schema view in the Explorer", async () => (await tabs(page, ".pane-header")).some((t) => t.includes("ABL Schema")), 60_000);
    await runCommand(page, "Explorer: Focus on ABL Schema View");
    let seen = [];
    const listed = await until("Alpha in the view", async () => ((seen = await schemaRows(page)), startsWith(seen, "Alpha")), 60_000);
    check(`${name}: the view shows the schema of the oxabl.toml above the folder`, listed, `rows: ${seen.join(" | ")}`);
    fs.writeFileSync(config, '[workspace]\nname = "above"\n[workspace.schema]\nfiles = ["db/alpha.df", "db/beta.df"]\n');
    const grown = await until("the beta database in the view", async () => ((seen = await schemaRows(page)), startsWith(seen, "beta")), 30_000);
    check(`${name}: an edit of the oxabl.toml above the folder shows in the view`, grown, `rows: ${seen.join(" | ")}`);
    await screenshot(page, name);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
