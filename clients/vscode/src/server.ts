// Binary discovery for the oxabl language server (KTD4).
//
// This module is deliberately free of any `vscode` import so the discovery
// logic is a pure function testable under vitest without an extension host.
// `extension.ts` reads the VS Code settings/environment and passes them in.

import * as fs from "node:fs";
import * as path from "node:path";

/** Inputs to discovery — everything that would otherwise touch global state. */
export interface DiscoveryEnv {
  /** The `oxabl.server.path` setting, verbatim (may be undefined/blank). */
  configuredPath?: string;
  /** Directories from `PATH`, in search order. */
  pathDirs: string[];
  /** Existence probe (injected so tests need no real filesystem). */
  fileExists?: (p: string) => boolean;
  /** Platform string; defaults to the host's. Controls executable names. */
  platform?: NodeJS.Platform;
}

/** Result of resolving the server binary. */
export type DiscoveryResult =
  | { kind: "found"; command: string; source: "setting" | "path" }
  | { kind: "not-found" };

/** Executable candidate names for the current platform. */
export function serverCandidateNames(platform: NodeJS.Platform = process.platform): string[] {
  return platform === "win32"
    ? ["oxabl.exe", "oxabl.cmd", "oxabl.bat", "oxabl"]
    : ["oxabl"];
}

/** Split a raw `PATH` value into directories for the given platform. */
export function splitPath(rawPath: string | undefined, platform: NodeJS.Platform = process.platform): string[] {
  if (!rawPath) {
    return [];
  }
  const sep = platform === "win32" ? ";" : ":";
  return rawPath.split(sep).filter((d) => d.length > 0);
}

/**
 * Resolve the `oxabl` binary. Precedence (KTD4):
 *   1. `oxabl.server.path` setting, when set to a non-blank value — trusted
 *      verbatim (highest precedence); a bad path surfaces at spawn time (R8).
 *   2. `oxabl` discovered on `PATH`.
 *   3. Nothing found — the caller surfaces an actionable error (R8).
 */
export function resolveServerBinary(env: DiscoveryEnv): DiscoveryResult {
  const platform = env.platform ?? process.platform;
  const fileExists = env.fileExists ?? ((p: string) => fs.existsSync(p));

  const configured = env.configuredPath?.trim();
  if (configured) {
    return { kind: "found", command: configured, source: "setting" };
  }

  const candidates = serverCandidateNames(platform);
  for (const dir of env.pathDirs) {
    for (const name of candidates) {
      if (fileExists(path.join(dir, name))) {
        return { kind: "found", command: path.join(dir, name), source: "path" };
      }
    }
  }

  return { kind: "not-found" };
}
