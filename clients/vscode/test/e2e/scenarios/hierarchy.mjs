import fs from "node:fs";
import path from "node:path";
import { bodyText, runCommand, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: Go to Super Method, Go to Implementations and the
// type hierarchy over made-up classes, in files that are not open. OXABL_E2E_RUNTIME names a short
// directory.

const SHAPE = [
  "CLASS app.Shape:",
  "    METHOD PUBLIC CHARACTER Describe():",
  '        RETURN "shape".',
  "    END METHOD.",
  "END CLASS.",
  "",
].join("\n");

const SQUARE = [
  "CLASS app.Square INHERITS app.Shape:",
  "    METHOD PUBLIC OVERRIDE CHARACTER Describe():",
  '        RETURN "square".',
  "    END METHOD.",
  "END CLASS.",
  "",
].join("\n");

const CIRCLE = SQUARE.replaceAll("Square", "Circle").replace('"square"', '"circle"');

const FILES = {
  "oxabl.toml": '[workspace]\nname = "hierarchy"\n[workspace.sources]\ninclude_paths = ["."]\n',
  "app/Shape.cls": SHAPE,
  "app/Square.cls": SQUARE,
  "app/Circle.cls": CIRCLE,
};

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

const activeTab = (page) =>
  page.evaluate(() => document.querySelector(".tabs-container .tab.active")?.getAttribute("aria-label") ?? "");

const peekText = (page) =>
  page.evaluate(() => [...document.querySelectorAll(".peekview-widget")].map((w) => w.textContent.replace(/ /g, " ")).join(" | "));

export async function hierarchy(ctx) {
  const name = "hierarchy";
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
    open: ["app/Square.cls"],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const page = vs.page;
  try {
    const lang = await waitForLanguage(page, "ABL");
    check(`${name}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);
    await until("the language server", async () => (await bodyText(page)).includes("Square"), 60_000);

    // 1. Go to Super Method on the override: the class it overrides, in a file that is not open.
    const describe = lineCol(SQUARE, "Describe", 2);
    await moveTo(page, describe.line, describe.col);
    // The server needs a moment to index the workspace before it answers.
    const opened = await until("Shape.cls", async () => {
      await runCommand(page, "oxabl: Go to Super Method");
      await sleep(1500);
      return (await activeTab(page)).includes("Shape.cls");
    }, 90_000);
    check(`${name}: Go to Super Method opens the overridden method`, opened, `active tab: ${await activeTab(page)}`);
    const at = await cursor(page);
    const expected = lineCol(SHAPE, "Describe", 1);
    check(
      `${name}: the cursor is on the name of the method`,
      !!at && at.line === expected.line && at.col >= expected.col && at.col <= expected.col + "Describe".length,
      `cursor: ${JSON.stringify(at)}, expected line ${expected.line}`,
    );
    await screenshot(page, `${name}-super`);

    // 2. Go to Implementations on the method in the base class: both overrides, one in a file not open.
    const base = lineCol(SHAPE, "Describe", 2);
    await moveTo(page, base.line, base.col);
    await runCommand(page, "Go to Implementations");
    const implemented = await until("both implementations", async () => {
      const text = await peekText(page);
      return text.includes("Square.cls") && text.includes("Circle.cls");
    });
    check(`${name}: Go to Implementations lists both overrides`, implemented, `peek: ${(await peekText(page)).slice(0, 300)}`);
    await screenshot(page, `${name}-implementations`);
    await page.keyboard.press("Escape");

    // 3. The type hierarchy of the base class.
    const header = lineCol(SHAPE, "app.Shape", 2);
    await moveTo(page, header.line, header.col);
    await runCommand(page, "Peek Type Hierarchy");
    const shown = await until("the type hierarchy", async () => {
      const text = await peekText(page);
      return text.includes("app.Shape");
    });
    check(`${name}: the type hierarchy shows the class`, shown, `peek: ${(await peekText(page)).slice(0, 300)}`);
    const subtypes = await until("the subtypes", async () => {
      const text = await peekText(page);
      return text.includes("app.Square") && text.includes("app.Circle");
    });
    check(`${name}: the type hierarchy lists the subtypes in files that are not open`, subtypes, `peek: ${(await peekText(page)).slice(0, 300)}`);
    await screenshot(page, `${name}-types`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
