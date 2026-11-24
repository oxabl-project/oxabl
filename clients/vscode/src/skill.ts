// The command `oxabl: Install agent skill in this workspace`: the resolved
// binary writes the skill into a workspace folder.
//
// Like `server.ts`, this module is free of any `vscode` import.

import { runCli } from "./cli";

const INSTALL_TIMEOUT_MS = 30_000;

export type SkillResult =
  /** One line per target, as `oxabl skill install` prints them. */
  | { ok: true; lines: string[] }
  | { ok: false; message: string };

/** The arguments of `oxabl skill install` for a workspace folder. */
export function skillInstallArgs(folder: string): string[] {
  return ["skill", "install", "--dir", folder];
}

/** Installs the skill with `binary` into `folder`; never throws. */
export async function installSkill(binary: string, folder: string): Promise<SkillResult> {
  try {
    const out = await runCli(binary, skillInstallArgs(folder), INSTALL_TIMEOUT_MS);
    return { ok: true, lines: out.stdout.split("\n").filter((line) => line.trim() !== "") };
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    if (/unrecognized subcommand/.test(message)) {
      return {
        ok: false,
        message: `${binary} is too old for \`oxabl skill\`. Run "oxabl: Install or Update the CLI".`,
      };
    }
    return { ok: false, message };
  }
}
