// Binary discovery for the oxabl language server (KTD4).
//
// This module is deliberately free of any `vscode` import so the discovery
// logic is a pure function testable under vitest without an extension host.
// `extension.ts` reads the VS Code settings/environment and passes them in.

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

/** Inputs to discovery — everything that would otherwise touch global state. */
export interface DiscoveryEnv {
  /** The `oxabl.server.path` setting, verbatim (may be undefined/blank). */
  configuredPath?: string;
  /** The default install path (`defaultInstallPath`), tried after the setting and before `PATH`. */
  installPath?: string;
  /** Directories from `PATH`, in search order. */
  pathDirs: string[];
  /** Existence probe (injected so tests need no real filesystem). */
  fileExists?: (p: string) => boolean;
  /** Platform string; defaults to the host's. Controls executable names. */
  platform?: NodeJS.Platform;
}

/** Result of resolving the server binary. */
export type DiscoveryResult =
  | { kind: "found"; command: string; source: "setting" | "install" | "path" }
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
 *   2. The default install path, `~/.local/bin/oxabl` (see `defaultInstallPath`).
 *   3. `oxabl` discovered on `PATH`.
 *   4. Nothing found — the caller surfaces an actionable error (R8).
 */
export function resolveServerBinary(env: DiscoveryEnv): DiscoveryResult {
  const platform = env.platform ?? process.platform;
  const fileExists = env.fileExists ?? ((p: string) => fs.existsSync(p));

  const configured = env.configuredPath?.trim();
  if (configured) {
    return { kind: "found", command: configured, source: "setting" };
  }

  if (env.installPath && fileExists(env.installPath)) {
    return { kind: "found", command: env.installPath, source: "install" };
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

/** `${OXABL_INSTALL_DIR:-$HOME/.local/bin}`, where the installer and the extension put `oxabl`. */
export function defaultInstallDir(
  env: NodeJS.ProcessEnv = process.env,
  home: string = os.homedir(),
): string {
  return env.OXABL_INSTALL_DIR || path.join(home, ".local", "bin");
}

export function defaultInstallPath(env?: NodeJS.ProcessEnv, home?: string): string {
  return path.join(defaultInstallDir(env, home), "oxabl");
}
