import fs from "node:fs";
import path from "node:path";
import { bodyText, runCommand, screenshot, statusTexts, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: an open include starts in fragment mode, the
// status bar says so, and "oxabl: Select Include Context" analyses it in a root that includes it.
// OXABL_E2E_RUNTIME names a short directory: VS Code's own socket must fit in 107 characters.

const FILES = {
  "oxabl.toml": '[workspace]\nname = "include-context"\n[workspace.sources]\ninclude_paths = ["."]\n',
  "shared.i": "DEFINE VARIABLE used-v AS INTEGER NO-UNDO.\nDEFINE VARIABLE spare-v AS INTEGER NO-UNDO.\n",
  "reads.p": "{shared.i}\nMESSAGE used-v spare-v.\n",
  "ignores.p": "{shared.i}\nMESSAGE used-v.\n",
};
const UNUSED = "unused variable `spare-v`";

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

/** Choose `label` in the include context picker. */
async function choose(page, label) {
  await runCommand(page, "oxabl: Select Include Context");
  let listed = [];
  await waitFor(
    `the picker to list ${label}`,
    async () => {
      listed = await rows(page, ".quick-input-list .monaco-list-row");
      return listed.some((t) => t.includes(label));
    },
    { timeout: 60_000, interval: 300 },
  ).catch(() => {});
  await page.keyboard.type(label);
  await waitFor(
    `${label} focused in the picker`,
    async () => (await rows(page, ".quick-input-list .monaco-list-row.focused")).some((t) => t.includes(label)),
    { timeout: 15_000, interval: 200 },
  ).catch(() => {});
  await page.keyboard.press("Enter");
  return listed;
}

const statusIs = async (page, text, timeout = 30_000) =>
  waitFor(`the status bar to show "${text}"`, async () => (await statusTexts(page)).some((t) => t.includes(text)), { timeout, interval: 200 })
    .then(() => true)
    .catch(() => false);

const problemsShow = async (page, shown, timeout = 60_000) =>
  waitFor(`the Problems panel ${shown ? "to show" : "not to show"} ${UNUSED}`, async () => (await bodyText(page)).includes(UNUSED) === shown, {
    timeout,
    interval: 300,
  })
    .then(() => true)
    .catch(() => false);

export async function includeContext(ctx) {
  const name = "include-context";
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
    open: ["shared.i"],
    env: { OXABL_UPDATE_API_URL: server.url, XDG_RUNTIME_DIR: runtimeDir },
  });
  const page = vs.page;
  try {
    const lang = await waitForLanguage(page, "ABL");
    check(`${name}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);
    check(`${name}: an open include starts in fragment mode`, await statusIs(page, "oxabl: fragment", 60_000), (await statusTexts(page)).join(" | "));
    await page.keyboard.press("Control+Shift+M");

    // 1. A root that does not read `spare-v`.
    const listed = await choose(page, "ignores.p");
    for (const want of ["Fragment mode", "ignores.p", "reads.p"]) {
      check(`${name}: the picker lists ${want}`, listed.some((t) => t.includes(want)), `rows: ${listed.join(" | ")}`);
    }
    check(`${name}: the status bar names the selected root`, await statusIs(page, "oxabl: in ignores.p"), (await statusTexts(page)).join(" | "));
    check(`${name}: the root's finding in the include shows`, await problemsShow(page, true), "");
    await screenshot(page, `${name}-ignores`);

    // 2. A root that reads it.
    await choose(page, "reads.p");
    check(`${name}: the status bar names the new root`, await statusIs(page, "oxabl: in reads.p"), (await statusTexts(page)).join(" | "));
    check(`${name}: the finding goes away in a root that reads the variable`, await problemsShow(page, false), "");

    // 3. A root file has no include context.
    await openFile(page, "reads.p");
    const hidden = await waitFor("the include context item to hide", async () => !(await statusTexts(page)).some((t) => t.includes("oxabl: in") || t.includes("oxabl: fragment")), {
      timeout: 10_000,
      interval: 200,
    })
      .then(() => true)
      .catch(() => false);
    check(`${name}: a root file shows no include context`, hidden, (await statusTexts(page)).join(" | "));

    // 4. Back in the include, the selection stays; fragment mode can be chosen again.
    await openFile(page, "shared.i");
    check(`${name}: the selection stays with the include`, await statusIs(page, "oxabl: in reads.p"), (await statusTexts(page)).join(" | "));
    await choose(page, "Fragment mode");
    check(`${name}: fragment mode can be chosen again`, await statusIs(page, "oxabl: fragment"), (await statusTexts(page)).join(" | "));
    await screenshot(page, `${name}-fragment`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
