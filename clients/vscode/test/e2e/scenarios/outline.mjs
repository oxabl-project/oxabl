import fs from "node:fs";
import path from "node:path";
import { runCommand, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: the Outline view, the breadcrumbs and
// "Go to Symbol in File" show the symbols of the open file.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const rows = (page, selector) =>
  page.evaluate((sel) => [...document.querySelectorAll(sel)].map((r) => r.textContent.replace(/\u00a0/g, " ").trim()), selector);

/** The Outline view rows as { name, level }, top to bottom. */
const outlineRows = (page) =>
  page.evaluate(() =>
    [...document.querySelectorAll(".outline-tree .monaco-list-row")].map((r) => ({
      text: r.textContent.replace(/\u00a0/g, " ").trim(),
      level: Number(r.getAttribute("aria-level")),
    })),
  );

const find = (list, name) => list.find((r) => r.text.startsWith(name));

async function waitOutline(page, names, timeout = 60_000) {
  let seen = [];
  try {
    await waitFor(
      `the Outline view to list ${names.join(", ")}`,
      async () => {
        seen = await outlineRows(page);
        return names.every((n) => find(seen, n));
      },
      { timeout, interval: 500 },
    );
  } catch {}
  return seen;
}

const crumbs = (page) => rows(page, ".breadcrumbs-control .monaco-breadcrumb-item");

async function openFile(page, name) {
  await page.keyboard.press("Control+P");
  await page.locator(".quick-input-widget input").first().waitFor({ state: "visible" });
  await page.keyboard.type(name);
  await sleep(600);
  await page.keyboard.press("Enter");
  await waitFor(`${name} open`, async () => (await rows(page, ".tab.active")).some((t) => t.includes(name)), { timeout: 15_000 });
}

async function focusEditor(page) {
  await page.locator(".monaco-editor .view-lines").first().click();
}

/** The 1-based cursor position from the status bar ("Ln 5, Col 3"). */
const cursor = async (page) => {
  const text = await page.evaluate(() => document.querySelector('[id="status.editor.selection"]')?.textContent ?? "");
  const m = /Ln (\d+), Col (\d+)/.exec(text);
  return m ? { line: Number(m[1]), col: Number(m[2]) } : null;
};

export async function outline(ctx) {
  const name = "outline";
  const oxabl = process.env.OXABL_E2E_OXABL;
  if (!oxabl) throw new Error("OXABL_E2E_OXABL must name the oxabl binary");
  if (!process.env.OXABL_E2E_RUNTIME) throw new Error("OXABL_E2E_RUNTIME must name a short directory");
  const runtimeDir = path.resolve(process.env.OXABL_E2E_RUNTIME);
  fs.mkdirSync(runtimeDir, { recursive: true, mode: 0o700 });
  const profile = makeProfile(name, {
    files: ["outline.p", "OutlineDemo.cls"],
    settings: {
      "oxabl.updates": "off",
      "oxabl.server.path": path.resolve(oxabl),
      "breadcrumbs.enabled": true,
      "outline.showVariables": true,
      "explorer.openEditors.visible": 0,
    },
  });
  fs.writeFileSync(path.join(profile.workspace, "oxabl.toml"), '[workspace]\nname = "outline"\n');
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, {
    open: ["outline.p"],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const page = vs.page;
  try {
    const lang = await waitForLanguage(page, "ABL");
    check(`${name}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);
    await focusEditor(page);
    await runCommand(page, "Explorer: Focus on Outline View");
    // The folders pane would leave the Outline view too short to render every row.
    const folders = page.locator(".pane-header", { hasText: "workspace" }).first();
    if ((await folders.getAttribute("aria-expanded")) === "true") await folders.click();

    // 1. A procedure file.
    const p = await waitOutline(page, ["ShowOrders", "AddTax", "ttOrder", "cShopName"]);
    console.log(`     outline.p rows: ${JSON.stringify(p.map((r) => `${r.level}:${r.text}`))}`);
    for (const n of ["cShopName", "ttOrder", "btnOk", "CHOOSE OF btnOk", "ShowOrders", "AddTax"]) {
      check(`${name}: outline.p lists ${n}`, !!find(p, n), `rows: ${p.map((r) => r.text).join(" | ")}`);
    }
    const table = find(p, "ttOrder");
    const fieldsUnder = ["OrderNum", "OrderTotal"].every((f) => {
      const row = find(p, f);
      return row && table && row.level === table.level + 1 && p.indexOf(row) > p.indexOf(table);
    });
    check(`${name}: the fields are children of the temp-table`, fieldsUnder, `rows: ${p.map((r) => `${r.level}:${r.text}`).join(" | ")}`);
    const fn = find(p, "AddTax");
    const param = find(p, "pdNet");
    check(
      `${name}: the parameter is a child of the function`,
      !!(fn && param && param.level === fn.level + 1 && p.indexOf(param) === p.indexOf(fn) + 1),
      `rows: ${p.map((r) => `${r.level}:${r.text}`).join(" | ")}`,
    );
    const proc = find(p, "ShowOrders");
    const local = find(p, "iCount");
    check(
      `${name}: the variable in a procedure is a child of it`,
      !!(proc && local && local.level === proc.level + 1),
      `rows: ${p.map((r) => `${r.level}:${r.text}`).join(" | ")}`,
    );

    // 3. An edit shows without reopening the file.
    await focusEditor(page);
    await page.keyboard.press("Control+End");
    await page.keyboard.type("\nPROCEDURE FreshlyTyped:\nEND PROCEDURE.\n");
    const edited = await waitOutline(page, ["FreshlyTyped"], 30_000);
    check(`${name}: an edit adds the new procedure to the Outline view`, !!find(edited, "FreshlyTyped"), `rows: ${edited.map((r) => r.text).join(" | ")}`);

    // 5. Go to Symbol in File.
    await focusEditor(page);
    await page.keyboard.press("Control+Home");
    await page.keyboard.press("Control+Shift+O");
    await page.locator(".quick-input-widget input").first().waitFor({ state: "visible", timeout: 15_000 });
    let listed = [];
    await waitFor(
      "Go to Symbol in File to list ShowOrders",
      async () => {
        listed = await rows(page, ".quick-input-list .monaco-list-row");
        return listed.some((t) => t.includes("ShowOrders"));
      },
      { timeout: 30_000, interval: 300 },
    ).catch(() => {});
    for (const n of ["cShopName", "ttOrder", "ShowOrders", "AddTax"]) {
      check(`${name}: Go to Symbol lists ${n}`, listed.some((t) => t.includes(n)), `rows: ${listed.join(" | ")}`);
    }
    await page.keyboard.type("AddTax");
    await waitFor(
      "AddTax focused in Go to Symbol",
      async () => (await rows(page, ".quick-input-list .monaco-list-row.focused")).some((t) => t.includes("AddTax")),
      { timeout: 15_000, interval: 200 },
    ).catch(() => {});
    await page.keyboard.press("Enter");
    const src = fs.readFileSync(path.join(profile.workspace, "outline.p"), "utf8").split("\n");
    const want = src.findIndex((l) => l.startsWith("FUNCTION AddTax")) + 1;
    const wantCol = src[want - 1].indexOf("AddTax") + 1;
    let at = null;
    await waitFor("the cursor on AddTax", async () => ((at = await cursor(page)), at && at.line === want), { timeout: 10_000, interval: 200 }).catch(() => {});
    check(`${name}: choosing AddTax puts the cursor on its name`, !!at && at.line === want && at.col === wantCol, `cursor ${JSON.stringify(at)}, want line ${want} col ${wantCol}`);
    await screenshot(page, `${name}-p`);

    // 2. A class file.
    await openFile(page, "OutlineDemo.cls");
    const c = await waitOutline(page, ["acme.OutlineDemo", "Render"]);
    console.log(`     OutlineDemo.cls rows: ${JSON.stringify(c.map((r) => `${r.level}:${r.text}`))}`);
    const cls = find(c, "acme.OutlineDemo");
    check(`${name}: the class node is listed`, !!cls, `rows: ${c.map((r) => r.text).join(" | ")}`);
    for (const m of ["Title", "OutlineDemo", "Render"]) {
      const row = c.find((r) => r.text.startsWith(m) && r !== cls);
      check(`${name}: ${m} is a member of the class`, !!(row && cls && row.level === cls.level + 1), `rows: ${c.map((r) => `${r.level}:${r.text}`).join(" | ")}`);
    }
    const render = find(c, "Render");
    const arg = find(c, "pcName");
    check(`${name}: the method parameter is a child of the method`, !!(render && arg && arg.level === render.level + 1), `rows: ${c.map((r) => `${r.level}:${r.text}`).join(" | ")}`);

    // 4. Breadcrumbs at the cursor inside the method.
    const cls_src = fs.readFileSync(path.join(profile.workspace, "OutlineDemo.cls"), "utf8").split("\n");
    const target = cls_src.findIndex((l) => l.includes("cLine = Title")) + 1;
    await focusEditor(page);
    await page.keyboard.press("Control+G");
    await page.locator(".quick-input-widget input").first().waitFor({ state: "visible", timeout: 15_000 });
    await page.keyboard.type(String(target));
    await page.keyboard.press("Enter");
    let trail = [];
    await waitFor(
      "the breadcrumbs to show the class and the method",
      async () => {
        trail = await crumbs(page);
        return trail.some((t) => t.includes("acme.OutlineDemo")) && trail.some((t) => t.includes("Render"));
      },
      { timeout: 30_000, interval: 400 },
    ).catch(() => {});
    check(`${name}: the breadcrumbs show the class`, trail.some((t) => t.includes("acme.OutlineDemo")), `crumbs: ${trail.join(" > ")}`);
    check(`${name}: the breadcrumbs show the method`, trail.some((t) => t.includes("Render")), `crumbs: ${trail.join(" > ")}`);
    await screenshot(page, `${name}-cls`);
  } finally {
    await vs.close();
    await sleep(1500);
    const dir = path.join(runtimeDir, "oxabl", "daemon");
    if (fs.existsSync(dir)) {
      for (const f of fs.readdirSync(dir).filter((x) => x.endsWith(".json"))) {
        try {
          process.kill(JSON.parse(fs.readFileSync(path.join(dir, f), "utf8")).pid, "SIGTERM");
        } catch {}
      }
    }
    await server.close();
  }
}
