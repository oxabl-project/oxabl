import fs from "node:fs";
import path from "node:path";
import { runCommand, screenshot, statusTexts, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { addSettings, installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: the `&IF` branches the preprocessor does not
// compile are dimmed, a branch decided on a guess is not, the setting turns the dimming off, and in an
// include the dimming follows the selected include context.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const FILES = {
  "oxabl.toml":
    '[workspace]\nname = "inactive-regions"\n[workspace.sources]\ninclude_paths = ["."]\n' +
    '[workspace.preprocessor]\nopsys = "UNIX"\n',
  "dim.p":
    '&IF OPSYS = "WIN32" &THEN\nMESSAGE "windows only".\n&ELSE\nMESSAGE "unix build".\n&ENDIF\n' +
    '&IF SEARCH("x.p") <> ? &THEN\nMESSAGE "guessed branch".\n&ENDIF\n',
  "shared.i":
    '&IF "{&flavour}" = "red" &THEN\nMESSAGE "red flavour".\n&ELSE\nMESSAGE "other flavour".\n&ENDIF\n' +
    '&IF OPSYS = "VMS" &THEN\nMESSAGE "vms only".\n&ENDIF\n',
  "cond.p": 'MESSAGE "before".\n&IF 1 = 1 AND\n    2 = 3 AND\n    3 = 3 &THEN\nMESSAGE "multi".\n&ENDIF\n',
  "red.p": "&SCOPED-DEFINE flavour red\n{shared.i}\n",
};

/** The text of each rendered editor line that holds text with an opacity below 1. */
const dimmedLines = (page) =>
  page.evaluate(() => {
    const lines = [...document.querySelectorAll(".monaco-editor .view-lines .view-line")];
    return lines
      .filter((line) => {
        const walker = document.createTreeWalker(line, NodeFilter.SHOW_TEXT);
        for (let n = walker.nextNode(); n; n = walker.nextNode()) {
          if (n.textContent.trim() && parseFloat(getComputedStyle(n.parentElement).opacity) < 1) return true;
        }
        return false;
      })
      .map((line) => line.textContent.replace(/\u00a0/g, " ").trim());
  });

/** Waits until the dimmed lines are exactly `want`; returns whether they were. */
async function dimmedAre(page, want, timeout = 60_000) {
  let seen = [];
  const ok = await waitFor(
    `the dimmed lines to be ${JSON.stringify(want)}`,
    async () => {
      seen = await dimmedLines(page);
      return JSON.stringify([...seen].sort()) === JSON.stringify([...want].sort());
    },
    { timeout, interval: 300 },
  )
    .then(() => true)
    .catch(() => false);
  return { ok, seen: JSON.stringify(seen) };
}

const rows = (page, selector) =>
  page.evaluate((sel) => [...document.querySelectorAll(sel)].map((r) => r.textContent.replace(/\u00a0/g, " ").trim()), selector);

async function openFile(page, name) {
  await page.keyboard.press("Control+P");
  await page.locator(".quick-input-widget input").first().waitFor({ state: "visible" });
  await page.keyboard.type(name);
  await sleep(600);
  await page.keyboard.press("Enter");
  await waitFor(`${name} open`, async () => (await rows(page, ".tab.active")).some((t) => t.includes(name)), { timeout: 15_000 });
}

async function choose(page, label) {
  await runCommand(page, "oxabl: Select Include Context");
  await waitFor(`the picker to list ${label}`, async () => (await rows(page, ".quick-input-list .monaco-list-row")).some((t) => t.includes(label)), {
    timeout: 60_000,
    interval: 300,
  }).catch(() => {});
  await page.keyboard.type(label);
  await waitFor(
    `${label} focused in the picker`,
    async () => (await rows(page, ".quick-input-list .monaco-list-row.focused")).some((t) => t.includes(label)),
    { timeout: 15_000, interval: 200 },
  ).catch(() => {});
  await page.keyboard.press("Enter");
}

export async function inactiveRegions(ctx) {
  const name = "inactive-regions";
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
    open: ["dim.p"],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const page = vs.page;
  try {
    const lang = await waitForLanguage(page, "ABL");
    check(`${name}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);

    // 1. The branch not taken on UNIX is dimmed; the taken one and the guessed one are not.
    let dimmed = await dimmedAre(page, ['MESSAGE "windows only".']);
    check(`${name}: only the branch that is not compiled is dimmed`, dimmed.ok, dimmed.seen);
    await screenshot(page, `${name}-dimmed`);

    // 2. The setting turns the dimming off and on again, with no restart.
    addSettings(profile, { "oxabl.inactiveRegions.enable": false });
    dimmed = await dimmedAre(page, [], 30_000);
    check(`${name}: oxabl.inactiveRegions.enable false removes the dimming`, dimmed.ok, dimmed.seen);
    addSettings(profile, { "oxabl.inactiveRegions.enable": true });
    dimmed = await dimmedAre(page, ['MESSAGE "windows only".'], 30_000);
    check(`${name}: oxabl.inactiveRegions.enable true dims again`, dimmed.ok, dimmed.seen);

    // 3. Alone, the include does not know `flavour`: only the `OPSYS` branch is dimmed. In red.p, the `&ELSE` is too.
    const VMS = 'MESSAGE "vms only".';
    await openFile(page, "shared.i");
    await waitFor("fragment mode", async () => (await statusTexts(page)).some((t) => t.includes("oxabl: fragment")), {
      timeout: 60_000,
      interval: 200,
    }).catch(() => {});
    dimmed = await dimmedAre(page, [VMS]);
    check(`${name}: an include alone dims only what its includer cannot change`, dimmed.ok, dimmed.seen);
    await choose(page, "red.p");
    dimmed = await dimmedAre(page, ['MESSAGE "other flavour".', VMS]);
    check(`${name}: in red.p the include's other branch is dimmed`, dimmed.ok, dimmed.seen);
    await screenshot(page, `${name}-include`);
    await choose(page, "Fragment mode");
    dimmed = await dimmedAre(page, [VMS]);
    check(`${name}: back in fragment mode only the OPSYS branch is dimmed`, dimmed.ok, dimmed.seen);

    // 4. Typing on the middle line of a multi-line condition changes the decision, and the dimming follows.
    await openFile(page, "cond.p");
    dimmed = await dimmedAre(page, ['MESSAGE "multi".']);
    check(`${name}: a false multi-line condition dims its branch`, dimmed.ok, dimmed.seen);
    await page.keyboard.press("Control+G");
    await page.locator(".quick-input-widget input").first().waitFor({ state: "visible" });
    await page.keyboard.type("3");
    await page.keyboard.press("Enter");
    await page.keyboard.press("Home");
    await page.keyboard.type("NOT ");
    dimmed = await dimmedAre(page, []);
    check(`${name}: typing NOT on the middle line of the condition removes the dimming`, dimmed.ok, dimmed.seen);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
