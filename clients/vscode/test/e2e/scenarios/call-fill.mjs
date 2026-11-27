import fs from "node:fs";
import path from "node:path";
import { readLines, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: choosing a procedure from completion writes
// the call with one placeholder for each parameter, and Tab moves from one to the next. Choosing a
// program runs the command `oxabl.fillCall`, which asks the server for the arguments after the
// insert. OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107
// characters.

const MAIN =
  "PROCEDURE DoWork:\n" +
  "    DEFINE INPUT PARAMETER piCount AS INTEGER NO-UNDO.\n" +
  "    DEFINE OUTPUT PARAMETER pcName AS CHARACTER NO-UNDO.\n" +
  "END PROCEDURE.\n";

const FILES = {
  "oxabl.toml": '[workspace]\nname = "callfill"\n\n[workspace.sources]\ninclude_paths = ["."]\n',
  "main.p": MAIN,
  "prog.p":
    "DEFINE INPUT PARAMETER pcIn AS CHARACTER NO-UNDO.\n" +
    "DEFINE OUTPUT PARAMETER piOut AS INTEGER NO-UNDO.\n" +
    "piOut = LENGTH(pcIn).\n",
};

const rowTexts = (page) =>
  page.evaluate(() =>
    [...document.querySelectorAll(".suggest-widget .monaco-list-row")].map((r) => r.textContent.replace(/ /g, " ").trim()),
  );

const visible = async (page) => (await readLines(page)).map((l) => l.text.replace(/ /g, " "));

/** Type `text` on a new line at the end of the file, ask for suggestions until `label` shows, and choose it. */
async function choose(page, text, label) {
  await page.locator(".monaco-editor .view-lines").first().click();
  await page.keyboard.press("Control+End");
  await page.keyboard.press("Enter");
  await page.keyboard.type(text);
  let found = false;
  // The first request can come before the server has the file: ask again until it answers.
  for (let attempt = 0; attempt < 10 && !found; attempt++) {
    await page.keyboard.press("Control+Space");
    found = await waitFor(
      `${label} in the suggest widget`,
      async () => (await rowTexts(page)).some((t) => t.startsWith(label)),
      { timeout: 6_000, interval: 300 },
    ).then(
      () => true,
      () => false,
    );
    if (!found) await page.keyboard.press("Escape");
  }
  if (found) {
    // The row of `label` is the only one, or the first: the word picks it.
    await page.keyboard.press("Enter");
  }
  return found;
}

/** Wait until a line of the editor is `wanted`, and give the lines. */
async function lineShows(page, wanted) {
  let lines = [];
  const ok = await waitFor(
    `the line ${wanted}`,
    async () => {
      lines = await visible(page);
      return lines.some((l) => l.trimEnd() === wanted);
    },
    { timeout: 10_000, interval: 300 },
  ).then(
    () => true,
    () => false,
  );
  return { ok, lines };
}

export async function callFill(ctx) {
  const name = "call-fill";
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

    // 1. An internal procedure: the placeholders, with the mode of the OUTPUT parameter.
    const chosen = await choose(page, "RUN DoW", "DoWork");
    check(`${name}: typing RUN DoW shows DoWork`, chosen, `rows: ${(await rowTexts(page)).join(" | ")}`);
    let shown = await lineShows(page, "RUN DoWork (piCount, OUTPUT pcName).");
    check(`${name}: choosing DoWork writes its arguments`, shown.ok, `lines: ${shown.lines.join(" | ")}`);
    await screenshot(page, `${name}-procedure`);
    // The first placeholder is selected: typing replaces it, and Tab goes to the next one.
    await page.keyboard.type("5");
    await page.keyboard.press("Tab");
    await page.keyboard.type("cOut");
    shown = await lineShows(page, "RUN DoWork (5, OUTPUT cOut).");
    check(`${name}: Tab moves from one argument to the next`, shown.ok, `lines: ${shown.lines.join(" | ")}`);

    // 2. A program: the command asks the server after the insert.
    await page.keyboard.press("Escape");
    await page.keyboard.press("Escape");
    const program = await choose(page, "RUN pro", "prog.p");
    check(`${name}: typing RUN pro shows prog.p`, program, `rows: ${(await rowTexts(page)).join(" | ")}`);
    shown = await lineShows(page, "RUN prog.p (pcIn, OUTPUT piOut).");
    check(`${name}: choosing prog.p writes its arguments after the insert`, shown.ok, `lines: ${shown.lines.join(" | ")}`);
    await screenshot(page, `${name}-program`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
