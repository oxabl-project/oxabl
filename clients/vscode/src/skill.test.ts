import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { installSkill, skillInstallArgs } from "./skill";

const manifest = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "package.json"), "utf8"));

let tmp: string;
beforeEach(() => {
  tmp = fs.mkdtempSync(path.join(os.tmpdir(), "oxabl-skill-"));
});
afterEach(() => {
  fs.rmSync(tmp, { recursive: true, force: true });
});

/** A shell script that logs its arguments and then runs `body`. */
function fakeBinary(body: string): { binary: string; args: () => string } {
  const log = path.join(tmp, "args.log");
  const binary = path.join(tmp, "oxabl");
  fs.writeFileSync(binary, `#!/bin/sh\nprintf '%s\\n' "$*" > '${log}'\n${body}\n`, { mode: 0o755 });
  return { binary, args: () => fs.readFileSync(log, "utf8").trim() };
}

describe("installSkill", () => {
  it("runs `oxabl skill install` and returns one line for the store and one per link", async () => {
    const fake = fakeBinary(
      `echo '/h/.local/share/oxabl/skills/oxabl: installed'\necho '/h/.agents/skills/oxabl: linked'\necho '/h/.claude/skills/oxabl: linked'`,
    );

    const result = await installSkill(fake.binary);

    expect(fake.args()).toBe("skill install");
    expect(result).toEqual({
      ok: true,
      lines: [
        "/h/.local/share/oxabl/skills/oxabl: installed",
        "/h/.agents/skills/oxabl: linked",
        "/h/.claude/skills/oxabl: linked",
      ],
    });
  });

  it("passes no folder, force or other flag", () => {
    expect(skillInstallArgs()).toEqual(["skill", "install"]);
  });

  it("returns the error line of a refused install", async () => {
    const fake = fakeBinary(`echo 'error: not installed by oxabl: /h/.claude/skills/oxabl (a file); nothing was written' >&2\nexit 2`);

    const result = await installSkill(fake.binary);

    expect(result).toEqual({
      ok: false,
      message: "error: not installed by oxabl: /h/.claude/skills/oxabl (a file); nothing was written",
    });
  });

  it("says a binary without the skill command is too old", async () => {
    const fake = fakeBinary(`echo "error: unrecognized subcommand 'skill'" >&2\nexit 2`);

    const result = await installSkill(fake.binary);

    expect(result.ok).toBe(false);
    expect(!result.ok && result.message).toContain("too old");
  });

  it("does not throw when the binary cannot start", async () => {
    const result = await installSkill(path.join(tmp, "missing"));

    expect(result.ok).toBe(false);
  });
});

describe("the install-skill command manifest", () => {
  it("is contributed with its palette title and activates without an ABL file", () => {
    const commands: { command: string; title: string }[] = manifest.contributes.commands;
    expect(commands.find((c) => c.command === "oxabl.installSkill")?.title).toBe(
      "oxabl: Install agent skill",
    );
    expect(manifest.activationEvents).toContain("onCommand:oxabl.installSkill");
  });
});
