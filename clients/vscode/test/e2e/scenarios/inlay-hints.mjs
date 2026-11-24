import fs from "node:fs";
import path from "node:path";
import { readLines, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { addSettings, installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: a project that enables the strict database
// rules shows the lock of a read that names none and the index of a search as inlay hints, and
// the setting `oxabl.inlayHints.locks` turns the lock hints off on their own, with no restart.
// When the project turns the strict rule off, the index hint stays and the lock hint stays off.
// OXABL_E2E_RUNTIME names a short directory, as for `outline`.

const SCHEMA = [
  'ADD TABLE "customer"',
  'ADD FIELD "custnum" OF "customer" AS integer',
  'ADD FIELD "name" OF "customer" AS character',
  'ADD INDEX "custnum" ON "customer"',
  "  UNIQUE",
  "  PRIMARY",
  '  INDEX-FIELD "custnum" ASCENDING',
  "",
].join("\n");

const MAIN = [
  "/* A made-up read of one customer. */",
  "FIND FIRST customer WHERE customer.custnum = 1.",
  "DISPLAY customer.name.",
  "",
].join("\n");

const FILES = {
  "oxabl.toml":
    '[workspace]\nname = "hints"\n[workspace.schema]\nfiles = ["schema.df"]\n' +
    '[workspace.lint]\nenable = ["strict.locks"]\n',
  "schema.df": SCHEMA,
  "main.p": MAIN,
};

/** The text of the rendered line that holds `needle`, inlay hints included. */
async function lineWith(page, needle) {
  const lines = await readLines(page);
  return lines.map((l) => l.text).find((t) => t.includes(needle)) ?? "";
}

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 300 })
    .then(() => true)
    .catch(() => false);

export async function inlayHints(ctx) {
  const name = "inlay-hints";
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

    // 1. The read names no lock: a SHARE-LOCK hint after the table, and the index of the search.
    const shown = await until(
      "the lock and index hints",
      async () => {
        const line = await lineWith(page, "FIND FIRST customer");
        return line.includes("SHARE-LOCK") && line.includes("index custnum");
      },
      60_000,
    );
    check(
      `${name}: a read with no lock phrase shows SHARE-LOCK and its index`,
      shown,
      `line: ${await lineWith(page, "FIND FIRST customer")}`,
    );
    await screenshot(page, `${name}-shown`);

    // 2. The lock hints turn off on their own; the index hint stays.
    addSettings(profile, { "oxabl.inlayHints.locks": false });
    const off = await until("the lock hint gone", async () => {
      const line = await lineWith(page, "FIND FIRST customer");
      return !line.includes("SHARE-LOCK") && line.includes("index custnum");
    });
    check(
      `${name}: oxabl.inlayHints.locks turns the lock hint off and keeps the index hint`,
      off,
      `line: ${await lineWith(page, "FIND FIRST customer")}`,
    );
    await screenshot(page, `${name}-locks-off`);

    // 3. With the lock hints on again and no strict rule, only the index hint shows.
    addSettings(profile, { "oxabl.inlayHints.locks": true });
    const back = await until("the lock hint back", async () =>
      (await lineWith(page, "FIND FIRST customer")).includes("SHARE-LOCK"),
    );
    check(`${name}: oxabl.inlayHints.locks turns the lock hint on again`, back);
    fs.writeFileSync(
      path.join(profile.workspace, "oxabl.toml"),
      '[workspace]\nname = "hints"\n[workspace.schema]\nfiles = ["schema.df"]\n',
    );
    const plain = await until("only the index hint", async () => {
      const line = await lineWith(page, "FIND FIRST customer");
      return !line.includes("SHARE-LOCK") && line.includes("index custnum");
    });
    check(
      `${name}: with no strict rule the index hint shows and the lock hint does not`,
      plain,
      `line: ${await lineWith(page, "FIND FIRST customer")}`,
    );
    await screenshot(page, `${name}-no-strict-rule`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
