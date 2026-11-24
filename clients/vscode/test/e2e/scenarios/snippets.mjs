import fs from "node:fs";
import path from "node:path";
import { readLines, screenshot, waitForLanguage } from "../lib/editor.mjs";
import { startFakeGitHub } from "../lib/fake-github.mjs";
import { check, sleep, waitFor } from "../lib/report.mjs";
import { NO_SERVER_LOG, installVsix, launch, makeProfile, waitForLog } from "../lib/vscode.mjs";

// The snippets need no server: no `oxabl` binary is on the path here. Two projects, one
// with `keyword_case = "Lowercase"` in `[workspace.style]` and one with none, show the keyword
// case of the inserted text. Choosing a snippet from the suggest widget inserts its body.
// The extension keeps what it read from `oxabl.toml`, so the last step changes the file of the
// first project on disk and wants the new case at once.

const PROJECTS = {
  upper: { toml: '[workspace]\nname = "upper"\n', expect: "DO TRANSACTION ON ERROR UNDO, THROW:" },
  lower: {
    toml: '[workspace]\nname = "lower"\n\n[workspace.style]\nkeyword_case = "Lowercase"\n',
    expect: "do transaction on error undo, throw:",
  },
};

const visible = async (page) => (await readLines(page)).map((l) => l.text.replace(/ /g, " "));

export async function snippets(ctx) {
  const name = "snippets";
  const profile = makeProfile(name, { settings: { "oxabl.updates": "off" } });
  for (const [dir, project] of Object.entries(PROJECTS)) {
    fs.mkdirSync(path.join(profile.workspace, dir), { recursive: true });
    fs.writeFileSync(path.join(profile.workspace, dir, "oxabl.toml"), project.toml);
    fs.writeFileSync(path.join(profile.workspace, dir, "new.p"), "");
  }
  installVsix(ctx.exe, profile, ctx.vsix);
  const server = await startFakeGitHub({ installed: ctx.installed, newest: "99.0.0" });
  const vs = await launch(ctx.exe, profile, {
    open: ["upper/new.p", "lower/new.p"],
    env: { OXABL_UPDATE_API_URL: server.url },
  });
  const page = vs.page;
  try {
    await waitForLog(profile, NO_SERVER_LOG);
    for (const [dir, project] of Object.entries(PROJECTS)) {
      await page.locator(".tabs-container .tab", { hasText: "new.p" }).nth(dir === "upper" ? 0 : 1).click();
      const lang = await waitForLanguage(page, "ABL");
      check(`${name}: ${dir}: the language is ABL`, lang.ok, `status bar shows ${lang.seen}`);
      await page.locator(".monaco-editor .view-lines").first().click();
      await page.keyboard.type("trycat");
      await page.keyboard.press("Control+Space");
      const offered = await waitFor(
        "the trycatch snippet in the suggest widget",
        async () => (await page.locator(".suggest-widget .monaco-list-row").allTextContents()).some((t) => t.includes("trycatch")),
        { timeout: 20_000, interval: 300 },
      ).then(() => true, () => false);
      check(`${name}: ${dir}: the suggest widget offers trycatch with no server`, offered);
      await screenshot(page, `${name}-${dir}-widget`);
      await page.keyboard.press("Enter");
      await sleep(500);
      const lines = await visible(page);
      check(
        `${name}: ${dir}: the body starts with "${project.expect}"`,
        lines[0]?.includes(project.expect) === true,
        `lines: ${lines.join(" | ")}`,
      );
      check(
        `${name}: ${dir}: the CATCH block is in the body`,
        lines.some((l) => l.toLowerCase().includes("catch err as progress.lang.error:")),
        `lines: ${lines.join(" | ")}`,
      );
      await screenshot(page, `${name}-${dir}`);
    }

    // The first project had no setting and was read already. A change of its oxabl.toml must reach the next snippet.
    await page.locator(".tabs-container .tab", { hasText: "new.p" }).nth(0).click();
    await page.locator(".monaco-editor .view-lines").first().click();
    await page.keyboard.press("Control+A");
    await page.keyboard.press("Delete");
    fs.writeFileSync(path.join(profile.workspace, "upper", "oxabl.toml"), PROJECTS.lower.toml);
    await sleep(2000);
    await page.keyboard.type("trycat");
    await page.keyboard.press("Control+Space");
    await waitFor(
      "the trycatch snippet in the suggest widget",
      async () => (await page.locator(".suggest-widget .monaco-list-row").allTextContents()).some((t) => t.includes("trycatch")),
      { timeout: 20_000, interval: 300 },
    ).catch(() => undefined);
    await page.keyboard.press("Enter");
    await sleep(500);
    const changed = await visible(page);
    check(
      `${name}: a change of oxabl.toml on disk gives the new case`,
      changed[0]?.includes(PROJECTS.lower.expect) === true,
      `lines: ${changed.join(" | ")}`,
    );
  } finally {
    await vs.close();
    await server.close();
  }
}
