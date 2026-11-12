// Calls to the `oxabl` binary that the extension makes besides the language
// server and the build itself: its version, its capabilities and the
// effective build configuration of a root.
//
// Like `server.ts`, this module is free of any `vscode` import.

import * as cp from "node:child_process";
import * as path from "node:path";

export interface CliOutput {
  stdout: string;
  stderr: string;
}

/** Run `binary args` and resolve with its output; a non-zero exit or a timeout rejects. */
export function runCli(binary: string, args: string[], timeoutMs: number): Promise<CliOutput> {
  return new Promise((resolve, reject) => {
    cp.execFile(binary, args, { timeout: timeoutMs, maxBuffer: 4 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) {
        reject(new Error(stderr.trim().split("\n")[0] || err.message));
      } else {
        resolve({ stdout, stderr });
      }
    });
  });
}

/** `X.Y.Z` from the output `oxabl X.Y.Z` of `oxabl --version`. */
export function parseVersionOutput(out: string): string | undefined {
  return /^oxabl (\d+\.\d+\.\d+)\s*$/m.exec(out)?.[1];
}

/** The version of `binary`, or `undefined` when it does not answer (an old binary has no `--version`). */
export async function binaryVersion(binary: string): Promise<string | undefined> {
  try {
    return parseVersionOutput((await runCli(binary, ["--version"], 15_000)).stdout);
  } catch {
    return undefined;
  }
}

/** Whether `oxabl build` knows `--yield` (the capability the build loop needs). */
export async function supportsYield(binary: string): Promise<boolean> {
  try {
    return (await runCli(binary, ["build", "--help"], 15_000)).stdout.includes("--yield");
  } catch {
    return false;
  }
}

/** What the extension needs to know about a root. */
export interface RootConfig {
  /** Absolute directories whose changes never trigger a build. */
  ignore: string[];
}

function textValue(settings: Record<string, { value?: unknown } | undefined>, key: string): string | undefined {
  const value = settings[key]?.value;
  return typeof value === "string" && value !== "" ? value : undefined;
}

/**
 * The root's build setup from `oxabl build --print-config --json`, or
 * `undefined` when the root does not build: no `build.output` or no worker
 * command.
 */
export function parsePrintConfig(stdout: string, root: string): RootConfig | undefined {
  let settings: Record<string, { value?: unknown } | undefined>;
  try {
    settings = (JSON.parse(stdout) as { settings?: typeof settings }).settings ?? {};
  } catch {
    return undefined;
  }
  const output = textValue(settings, "build.output");
  const command = settings["build.workers.command"]?.value;
  if (!output || !Array.isArray(command) || command.length === 0) {
    return undefined;
  }
  const dirs = [
    output,
    textValue(settings, "build.state"),
    textValue(settings, "build.store"),
    textValue(settings, "build.outputs.xref"),
    textValue(settings, "build.outputs.debug_list"),
    textValue(settings, "build.outputs.listing"),
  ];
  return { ignore: dirs.flatMap((d) => (d ? [path.resolve(root, d)] : [])) };
}

/** `RootConfig` of `root`, or `undefined` when it does not build (or the call fails). */
export async function readRootConfig(
  binary: string,
  root: string,
  extraArgs: string[],
): Promise<RootConfig | undefined> {
  try {
    const out = await runCli(binary, ["build", "--root", root, "--print-config", "--json", ...extraArgs], 30_000);
    return parsePrintConfig(out.stdout, root);
  } catch {
    return undefined;
  }
}
