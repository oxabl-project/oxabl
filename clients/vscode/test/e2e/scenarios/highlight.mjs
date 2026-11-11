import { COLORS as C } from "../lib/colors.mjs";
import { screenshot, settledLines, waitForLanguage } from "../lib/editor.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep } from "../lib/report.mjs";
import { NO_SERVER_LOG, installVsix, launch, makeProfile, waitForLog } from "../lib/vscode.mjs";

/**
 * Asserts the colour of `token` (its `nth` occurrence in the first line that
 * contains `line`) on the lines read from the editor.
 */
function expectColor(lines, file, { line, token, nth = 0, color, what }) {
  const name = `${file}: ${what} (${token}) is ${color}`;
  const row = lines.find((l) => l.text.includes(line));
  if (!row) return check(name, false, `no rendered line contains "${line}"`);
  let pos = -1;
  for (let i = 0; i <= nth; i++) {
    pos = row.text.indexOf(token, pos + 1);
    if (pos < 0) return check(name, false, `"${token}" not found in "${row.text}"`);
  }
  const got = row.colors.slice(pos, pos + token.length);
  const ok = got.every((c) => c === color);
  return check(name, ok, `got ${[...new Set(got)].join(", ")}`);
}

const P_CHECKS = [
  { line: "Totals for a made-up shop", token: "Totals", color: C.comment, what: "block comment" },
  { line: "A second comment style", token: "second", color: C.comment, what: "line comment" },
  { line: "DEFINE VARIABLE cName", token: "DEFINE", color: C.keyword, what: "keyword" },
  { line: "DEFINE VARIABLE cName", token: "VARIABLE", color: C.keyword, what: "keyword" },
  { line: "DEFINE VARIABLE cName", token: "CHARACTER", color: C.keyword, what: "data type" },
  { line: "DEFINE VARIABLE cName", token: "NO-UNDO", color: C.keyword, what: "modifier" },
  { line: "DEFINE VARIABLE cName", token: "cName", color: C.variable, what: "defined name" },
  { line: "ASSIGN cName", token: "ASSIGN", color: C.keyword, what: "keyword" },
  { line: "ASSIGN cName", token: "cName", color: C.variable, what: "plain identifier" },
  { line: "ASSIGN cName", token: '"widget"', color: C.string, what: "string" },
  { line: "INITIAL 10", token: "10", color: C.number, what: "number" },
  { line: "SUBSTRING(", token: "SUBSTRING", color: C.func, what: "built-in function call" },
  { line: "SUBSTRING(", token: "1", color: C.number, what: "number in a call" },
  { line: "hBuf:QUERY-OPEN", token: "QUERY-OPEN", color: C.func, what: "method after :" },
  { line: "= hBuf:NAME", token: "NAME", color: C.variable, what: "member after : (not a keyword)" },
  { line: "IF iCount", token: "IF", color: C.control, what: "control keyword" },
  { line: "IF iCount", token: "THEN", color: C.control, what: "control keyword" },
  { line: "IF iCount", token: "DO", color: C.control, what: "control keyword" },
  { line: "IF iCount", token: "iCount", color: C.variable, what: "plain identifier" },
  { line: "DISPLAY cName", token: "DISPLAY", color: C.keyword, what: "keyword" },
  { line: "END.", token: "END", color: C.control, what: "control keyword" },
];

const CLS_CHECKS = [
  { line: "CLASS acme.Greeter", token: "CLASS", color: C.keyword, what: "class keyword" },
  { line: "CLASS acme.Greeter", token: "Greeter", color: C.type, what: "class name" },
  { line: "USING Progress", token: "Progress", color: C.type, what: "namespace" },
  { line: "PROPERTY Greeting", token: "PUBLIC", color: C.keyword, what: "modifier" },
  { line: "PROPERTY Greeting", token: "Greeting", color: C.variable, what: "property name" },
  { line: "METHOD PUBLIC VOID", token: "METHOD", color: C.keyword, what: "method keyword" },
  { line: "METHOD PUBLIC VOID", token: "VOID", color: C.keyword, what: "data type" },
  { line: "METHOD PUBLIC VOID", token: "Say", color: C.func, what: "method name" },
  { line: "METHOD PUBLIC VOID", token: "INPUT", color: C.keyword, what: "modifier" },
  { line: "METHOD PUBLIC VOID", token: "pcName", color: C.variable, what: "parameter" },
  { line: "METHOD PUBLIC VOID", token: "CHARACTER", color: C.keyword, what: "data type" },
  { line: "cMsg = ", token: '"Hello, "', color: C.string, what: "string" },
  { line: "MESSAGE cMsg", token: "MESSAGE", color: C.keyword, what: "keyword" },
  { line: "MESSAGE cMsg", token: "VIEW-AS", color: C.keyword, what: "keyword" },
  { line: "END METHOD", token: "END", color: C.control, what: "control keyword" },
  { line: "END CLASS", token: "END", color: C.control, what: "control keyword" },
];

const MD_CHECKS = [
  { line: "DEFINE VARIABLE cName", token: "DEFINE", color: C.keyword, what: "keyword in the abl block" },
  { line: "DEFINE VARIABLE cName", token: "CHARACTER", color: C.keyword, what: "data type in the abl block" },
  { line: "IF cName", token: "IF", color: C.control, what: "control keyword in the abl block" },
  { line: "IF cName", token: '""', color: C.string, what: "string in the abl block" },
  { line: "DISPLAY", token: "DISPLAY", color: C.keyword, what: "keyword in the abl block" },
  { line: "Prose that mentions", token: "DEFINE", color: C.plain, what: "prose before the block" },
  { line: "Prose that mentions", token: "IF", color: C.plain, what: "prose before the block" },
  { line: "More prose after", token: "END", color: C.plain, what: "prose after the block" },
];

// Highlighting needs no server, so a missing binary shows no message: only a line in the
// oxabl output channel.
async function missingServerQuiet(profile, file, vs) {
  const name = `${file}: missing binary`;
  let logged = true;
  await waitForLog(profile, NO_SERVER_LOG).catch(() => (logged = false));
  check(`${name} is logged to the oxabl output channel`, logged);
  // Count notification toasts: their text also sits in a screen-reader live region.
  const toasts = vs.page.locator(".notifications-toasts .notification-list-item-message", { hasText: "oxabl" });
  let seen = 0;
  const end = Date.now() + 8_000;
  while (Date.now() < end) {
    seen = Math.max(seen, await toasts.count());
    await sleep(250);
  }
  check(`${name} shows no message`, seen === 0, `${seen} oxabl message(s)`);
}

// The updater is off here and points at a fake API that counts requests: the
// scenario must not depend on, or reach, the network.
// `activate` is an ABL file opened first so the extension starts (it does not for a Markdown file alone).
async function run(ctx, { name, file, language, checks, missingServer = false, activate = null }) {
  const files = activate ? [activate, file] : [file];
  const profile = makeProfile(name, { settings: { "oxabl.updates": "off" }, files });
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const env = { OXABL_UPDATE_API_URL: server.url };
  try {
    const vs = await launch(ctx.exe, profile, { open: files, env });
    try {
      // The log line comes after the updater's start-up decision, so it shows the extension ran.
      if (activate) {
        await waitForLog(profile, NO_SERVER_LOG);
        await vs.page.locator(".tabs-container .tab", { hasText: file }).first().click();
      }
      const lang = await waitForLanguage(vs.page, language);
      check(`${file}: status bar language is ${language}`, lang.ok, `status bar says ${lang.seen}`);
      const lines = await settledLines(vs.page);
      await screenshot(vs.page, name);
      for (const c of checks) expectColor(lines, file, c);
      if (missingServer) await missingServerQuiet(profile, file, vs);
    } finally {
      await vs.close();
    }
    const paths = server.requests.map((r) => r.path);
    check(`${name}: oxabl.updates is off, so no request reaches the update API`, paths.length === 0, paths.join(" "));
  } finally {
    await server.close();
  }
}

export const highlightP = (ctx) =>
  run(ctx, { name: "highlight-p", file: "sample.p", language: "ABL", checks: P_CHECKS, missingServer: true });
export const highlightCls = (ctx) =>
  run(ctx, { name: "highlight-cls", file: "Greeter.cls", language: "ABL", checks: CLS_CHECKS });
export const markdown = (ctx) =>
  run(ctx, { name: "markdown", file: "notes.md", language: "Markdown", checks: MD_CHECKS, activate: "update.p" });
