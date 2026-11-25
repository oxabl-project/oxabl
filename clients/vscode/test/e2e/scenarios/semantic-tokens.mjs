import fs from "node:fs";
import path from "node:path";
import { readLines, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { stopDaemons } from "../lib/daemon.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { addSettings, installVsix, launch, makeProfile } from "../lib/vscode.mjs";

// A real `oxabl` (OXABL_E2E_OXABL) in a real VS Code: the server's semantic tokens colour a name
// by what it is, with the colours the setting `editor.semanticTokenColorCustomizations` gives each
// token type. A name that does not resolve keeps the colour of the grammar. The setting
// `oxabl.semanticTokens.enable` turns the tokens off and on again, with no restart.
// OXABL_E2E_RUNTIME names a short directory, as for `outline`.

const SCHEMA = [
  'ADD TABLE "Customer"',
  'ADD FIELD "CustNum" OF "Customer" AS integer',
  'ADD FIELD "Name" OF "Customer" AS character',
  "",
].join("\n");

const MAIN = [
  "/* Totals for a made-up shop. */",
  "DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.",
  "FOR EACH Customer NO-LOCK:",
  "    iTotal = iTotal + Customer.CustNum.",
  "END.",
  "iTotal = noSuchName.",
  "",
].join("\n");

const FILES = {
  "oxabl.toml": '[workspace]\nname = "tokens"\n[workspace.schema]\nfiles = ["schema.df"]\n',
  "schema.df": SCHEMA,
  "main.p": MAIN,
};

// One colour for each type that the test looks at. None is a colour of the grammar.
const RED = "#ff0000";
const MAGENTA = "#ff00ff";
const YELLOW = "#ffff00";
const RULES = { variable: RED, databaseField: MAGENTA, table: YELLOW };

/** The colours of `token` (its `nth` occurrence) in the rendered line that holds `line`. */
async function colorsOf(page, line, token, nth = 0) {
  const row = (await readLines(page)).find((l) => l.text.includes(line));
  if (!row) return [];
  let pos = -1;
  for (let i = 0; i <= nth; i++) {
    pos = row.text.indexOf(token, pos + 1);
    if (pos < 0) return [];
  }
  return [...new Set(row.colors.slice(pos, pos + token.length))];
}

const is = async (page, line, token, color, nth = 0) => {
  const got = await colorsOf(page, line, token, nth);
  return got.length === 1 && got[0] === color;
};

const until = (what, fn, timeout = 30_000) =>
  waitFor(what, fn, { timeout, interval: 300 })
    .then(() => true)
    .catch(() => false);

export async function semanticTokens(ctx) {
  const name = "semantic-tokens";
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
      "editor.semanticHighlighting.enabled": true,
      "editor.semanticTokenColorCustomizations": { enabled: true, rules: RULES },
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

    // 1. Each name that resolves has the colour of its type.
    const coloured = await until(
      "the semantic colours",
      async () => is(page, "DEFINE VARIABLE iTotal", "iTotal", RED),
      60_000,
    );
    check(`${name}: a variable has the colour of the type variable`, coloured, `got ${await colorsOf(page, "DEFINE VARIABLE iTotal", "iTotal")}`);
    check(
      `${name}: its use has the same colour`,
      await is(page, "iTotal = iTotal +", "iTotal", RED, 1),
      `got ${await colorsOf(page, "iTotal = iTotal +", "iTotal", 1)}`,
    );
    check(
      `${name}: a database field has the colour of the type databaseField`,
      await is(page, "iTotal = iTotal +", "CustNum", MAGENTA),
      `got ${await colorsOf(page, "iTotal = iTotal +", "CustNum")}`,
    );
    check(
      `${name}: a table in FOR EACH has the colour of the type table`,
      await is(page, "FOR EACH Customer", "Customer", YELLOW),
      `got ${await colorsOf(page, "FOR EACH Customer", "Customer")}`,
    );
    // The grammar colours the names that oxabl does not resolve, the keywords and the comment.
    const unresolved = await colorsOf(page, "iTotal = noSuchName", "noSuchName");
    check(
      `${name}: a name that does not resolve keeps the colour of the grammar`,
      unresolved.length === 1 && ![RED, MAGENTA, YELLOW].includes(unresolved[0]),
      `got ${unresolved}`,
    );
    const keyword = await colorsOf(page, "FOR EACH Customer", "EACH");
    check(
      `${name}: a keyword keeps the colour of the grammar`,
      keyword.length === 1 && ![RED, MAGENTA, YELLOW].includes(keyword[0]),
      `got ${keyword}`,
    );
    await screenshot(page, `${name}-on`);

    // 2. The setting turns the tokens off, with no restart. The grammar colours the names again.
    addSettings(profile, { "oxabl.semanticTokens.enable": false });
    const off = await until("the semantic colours gone", async () => {
      const got = await colorsOf(page, "DEFINE VARIABLE iTotal", "iTotal");
      return got.length > 0 && !got.includes(RED);
    });
    check(`${name}: oxabl.semanticTokens.enable=false removes the colours`, off, `got ${await colorsOf(page, "DEFINE VARIABLE iTotal", "iTotal")}`);
    await screenshot(page, `${name}-off`);

    // 3. And on again.
    addSettings(profile, { "oxabl.semanticTokens.enable": true });
    const back = await until("the semantic colours back", async () => is(page, "DEFINE VARIABLE iTotal", "iTotal", RED));
    check(`${name}: oxabl.semanticTokens.enable=true brings the colours back`, back, `got ${await colorsOf(page, "DEFINE VARIABLE iTotal", "iTotal")}`);
  } finally {
    await vs.close();
    await sleep(1500);
    await stopDaemons(name, runtimeDir);
    await server.close();
  }
}
