import fs from "node:fs";
import path from "node:path";
import { readLines, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: the suggest widget shows the server's items
// and the snippets in one list. The server serves the snippets, and the extension's own snippet
// provider gives nothing while the server completes, so `trycatch` shows once.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const FILES = {
  "oxabl.toml":
    '[workspace]\nname = "complete"\n\n[workspace.sources]\ninclude_paths = [".", "lib"]\n\n' +
    '[workspace.schema]\nfiles = ["db/shop.df"]\n\n[workspace.style]\nkeyword_case = "Lowercase"\n',
  "db/shop.df":
    'ADD TABLE "Customer"\n  AREA "Data"\n\nADD FIELD "CustNum" OF "Customer" AS integer\n  FORMAT ">>>>9"\n\n' +
    'ADD FIELD "Name" OF "Customer" AS character\n  FORMAT "x(30)"\n\n',
  "main.p": "define variable iCount as integer no-undo.\n",
};

const rowTexts = (page) =>
  page.evaluate(() =>
    [...document.querySelectorAll(".suggest-widget .monaco-list-row")].map((r) => r.textContent.replace(/ /g, " ").trim()),
  );

const visible = async (page) => (await readLines(page)).map((l) => l.text.replace(/ /g, " "));

/** Type `text` at the end of the file on a new line, ask for suggestions, and wait for `label`. */
async function suggest(page, text, label) {
  await page.locator(".monaco-editor .view-lines").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.press("Enter");
  await page.keyboard.type(text);
  let rows = [];
  // The first request can come before the server has the file: ask again until it answers.
  for (let attempt = 0; attempt < 10; attempt++) {
    await page.keyboard.press("Control+Space");
    const found = await waitFor(
      `${label} in the suggest widget`,
      async () => {
        rows = await rowTexts(page);
        return rows.some((t) => t.startsWith(label));
      },
      { timeout: 6_000, interval: 300 },
    ).then(
      () => true,
      () => false,
    );
    if (found) break;
    await page.keyboard.press("Escape");
  }
  return rows;
}

export async function completion(ctx) {
  const name = "completion";
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

    // 1. A statement keyword from the server.
    let rows = await suggest(page, "fi", "find");
    check(`${name}: typing fi shows a find row`, rows.some((t) => t.startsWith("find")), `rows: ${rows.join(" | ")}`);
    await screenshot(page, `${name}-keyword`);
    await page.keyboard.press("Escape");
    await page.keyboard.press("Control+A");
    await page.keyboard.press("Delete");
    await page.keyboard.type(FILES["main.p"]);

    // 2. One trycatch row: the server's snippet, not a second one from the extension.
    rows = await suggest(page, "trycat", "trycatch");
    const trycatch = rows.filter((t) => t.startsWith("trycatch"));
    check(`${name}: typing trycat shows exactly one trycatch row`, trycatch.length === 1, `rows: ${rows.join(" | ")}`);
    await screenshot(page, `${name}-snippet`);
    await page.keyboard.press("Enter");
    await sleep(500);
    const lines = await visible(page);
    check(
      `${name}: choosing it inserts the block in the project's case`,
      lines.some((l) => l.includes("do transaction on error undo, throw:")),
      `lines: ${lines.join(" | ")}`,
    );
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
