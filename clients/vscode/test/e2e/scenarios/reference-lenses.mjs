import fs from "node:fs";
import path from "node:path";
import { screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { addSettings, installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: the reference-count lenses are off until
// `oxabl.referenceLenses.enable` is on, count the uses of a procedure and the files that run a
// program, open the references when clicked, and go away when the setting is turned off again, with
// no restart. OXABL_E2E_RUNTIME names a short directory, as for `outline`.

const MAIN = [
  "/* A made-up order program. */",
  "RUN util/recalc.p.",
  "RUN tally.",
  "RUN tally.",
  "",
  "PROCEDURE tally:",
  "    MESSAGE \"tally\".",
  "END PROCEDURE.",
  "",
].join("\n");

const FILES = {
  "oxabl.toml": '[workspace]\nname = "lenses"\n[workspace.sources]\ninclude_paths = ["."]\n',
  "main.p": MAIN,
  "batch.p": "RUN main.p.\n",
  "util/recalc.p": "MESSAGE \"recalc\".\n",
};

const lenses = (page) =>
  page.evaluate(() =>
    [...document.querySelectorAll(".monaco-editor .codelens-decoration")].map((e) =>
      e.textContent.replace(/ /g, " ").trim(),
    ),
  );

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 300 })
    .then(() => true)
    .catch(() => false);

export async function referenceLenses(ctx) {
  const name = "reference-lenses";
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
    await sleep(5000);

    // 1. Off by default.
    check(`${name}: no lens before the setting is on`, (await lenses(page)).length === 0, `${await lenses(page)}`);

    // 2. On: the program and the procedure get their counts.
    addSettings(profile, { "oxabl.referenceLenses.enable": true });
    const shown = await until(
      "the counts",
      async () => {
        const found = await lenses(page);
        return found.includes("1 reference · used by 1 file") && found.includes("2 references");
      },
      60_000,
    );
    check(
      `${name}: a program shows its references and files, a procedure its references`,
      shown,
      `lenses: ${await lenses(page)}`,
    );
    await screenshot(page, `${name}-on`);

    // 3. A click lists the references.
    await page.locator(".monaco-editor .codelens-decoration a", { hasText: "2 references" }).first().click();
    const peek = await until("the references view", async () =>
      page.evaluate(() => {
        const zone = document.querySelector(".reference-zone-widget");
        return !!zone && /main\.p/.test(zone.textContent ?? "");
      }),
    );
    check(`${name}: a click opens the references of the procedure`, peek);
    await screenshot(page, `${name}-peek`);
    await page.keyboard.press("Escape");

    // 4. Off again, with no restart.
    addSettings(profile, { "oxabl.referenceLenses.enable": false });
    const gone = await until("the lenses gone", async () => (await lenses(page)).length === 0);
    check(`${name}: turning the setting off removes the lenses`, gone, `lenses: ${await lenses(page)}`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
