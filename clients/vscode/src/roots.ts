// Which build root a changed file belongs to, and which changes to ignore.
//
// Like `server.ts`, this module is free of any `vscode` import.

import * as path from "node:path";

/** Files whose change can change the result of a build. */
const BUILD_INPUT = /\.(?:p|w|cls|i|v|df)$/i;

export function isBuildInput(file: string): boolean {
  return BUILD_INPUT.test(file) || path.basename(file) === "oxabl.toml" || file.endsWith(`${path.sep}.oxabl${path.sep}cache.toml`);
}

export function isConfigFile(file: string): boolean {
  return path.basename(file) === "oxabl.toml";
}

/** The root with the longest path that holds `file`, if any. */
export function ownerRoot(file: string, roots: Iterable<string>): string | undefined {
  let best: string | undefined;
  for (const root of roots) {
    if (isInside(file, root) && (!best || root.length > best.length)) {
      best = root;
    }
  }
  return best;
}

/** Whether `file` is `dir` or below it. */
export function isInside(file: string, dir: string): boolean {
  const rel = path.relative(dir, file);
  return rel === "" || (!rel.startsWith("..") && !path.isAbsolute(rel));
}

/** A change under `.git` or under a directory the build writes is no input. */
export function isIgnored(file: string, ignoreDirs: string[]): boolean {
  return file.split(path.sep).includes(".git") || ignoreDirs.some((dir) => isInside(file, dir));
}
