// Install of the released `oxabl` binary, the way `install.sh` and
// `oxabl self-update` do it: download into the target directory, verify the
// SHA-256, run `--version`, then rename over the target in one step. Any
// failure removes the staged file and leaves the old binary.
//
// Like `server.ts`, this module is free of any `vscode` import.

import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

import { parseVersionOutput, runCli } from "./cli";
import { REPO_PATH, UpdateError, getJson, listTags, request, type UpdateEnv } from "./net";
import { newestVersion, parseChecksum } from "./update";

const TAG_PREFIX = "oxabl-v";
export const BINARY_ASSET = "oxabl-x86_64-unknown-linux-musl";
const MAX_CHECKSUM_BYTES = 4096;
const MAX_BINARY_BYTES = 300 * 1024 * 1024;
const VERSION_TIMEOUT_MS = 15_000;

/** The newest release of the CLI, with both assets present. */
export interface CliRelease {
  version: string;
  binaryUrl: string;
  sha256Url: string;
}

/**
 * The newest `oxabl-vX.Y.Z` release. A missing tag, a draft, a pre-release or
 * a missing asset throws: that is "not established", never "up to date".
 */
export async function findCliRelease(env: UpdateEnv, log: (message: string) => void): Promise<CliRelease> {
  const newest = newestVersion(await listTags(TAG_PREFIX, env), TAG_PREFIX);
  if (!newest) {
    throw new UpdateError("no oxabl releases found");
  }
  const tag = `${TAG_PREFIX}${newest}`;
  const { json } = await getJson(`${env.apiBase}${REPO_PATH}/releases/tags/${tag}`, env);
  const r = json as { draft?: unknown; prerelease?: unknown; assets?: unknown } | null;
  if (!r || typeof r !== "object" || !Array.isArray(r.assets)) {
    throw new UpdateError("unexpected release JSON");
  }
  if (r.draft !== false || r.prerelease !== false) {
    throw new UpdateError(`release ${tag} is a draft or a pre-release`);
  }
  const urlOf = (name: string): string | undefined => {
    const asset = (r.assets as { name?: unknown; browser_download_url?: unknown }[]).find(
      (a) => a?.name === name,
    );
    return typeof asset?.browser_download_url === "string" ? asset.browser_download_url : undefined;
  };
  const binaryUrl = urlOf(BINARY_ASSET);
  const sha256Url = urlOf(`${BINARY_ASSET}.sha256`);
  if (!binaryUrl || !sha256Url) {
    throw new UpdateError(`release ${tag} has no complete assets yet`);
  }
  log(`newest oxabl release is v${newest}`);
  return { version: newest, binaryUrl, sha256Url };
}

/** `${XDG_DATA_HOME:-$HOME/.local/share}/oxabl/install.json`. */
export function installRecordPath(env: NodeJS.ProcessEnv = process.env, home: string = os.homedir()): string {
  return path.join(env.XDG_DATA_HOME || path.join(home, ".local", "share"), "oxabl", "install.json");
}

interface InstallRecord {
  format: 1;
  installer: string;
  path: string;
  version: string;
}

/** The recorded install path, or `undefined` when the record is absent or malformed. */
export async function readInstallRecord(recordPath: string): Promise<InstallRecord | undefined> {
  try {
    const parsed = JSON.parse(await fs.promises.readFile(recordPath, "utf8")) as Partial<InstallRecord> | null;
    if (parsed?.format === 1 && typeof parsed.path === "string" && path.isAbsolute(parsed.path)) {
      return parsed as InstallRecord;
    }
  } catch {
    // Absent or unreadable: nothing is managed.
  }
  return undefined;
}

/** A binary is managed when the record's path is the binary, both canonicalised. */
export async function isManaged(binary: string, recordPath: string): Promise<boolean> {
  const record = await readInstallRecord(recordPath);
  if (!record) {
    return false;
  }
  try {
    const [a, b] = await Promise.all([fs.promises.realpath(record.path), fs.promises.realpath(binary)]);
    return a === b;
  } catch {
    return false;
  }
}

export interface InstallTarget {
  /** The binary to write, e.g. `~/.local/bin/oxabl`. */
  path: string;
  /** The install record to write. */
  recordPath: string;
}

/** Download, verify and install `release`; write the record. Throws an `UpdateError` and installs nothing on a failure. */
export async function installCli(release: CliRelease, env: UpdateEnv, target: InstallTarget): Promise<void> {
  const sumFile = await request(release.sha256Url, env, undefined, MAX_CHECKSUM_BYTES);
  const expected = parseChecksum(sumFile.body.toString("utf8"), BINARY_ASSET);
  const binary = await request(release.binaryUrl, env, undefined, MAX_BINARY_BYTES);
  if (crypto.createHash("sha256").update(binary.body).digest("hex") !== expected) {
    throw new UpdateError("checksum mismatch");
  }

  const dir = path.dirname(target.path);
  await fs.promises.mkdir(dir, { recursive: true });
  const canonical = path.join(await fs.promises.realpath(dir), path.basename(target.path));
  const staged = path.join(dir, `.oxabl-install-${crypto.randomBytes(6).toString("hex")}`);
  try {
    await fs.promises.writeFile(staged, binary.body, { mode: 0o755, flag: "wx" });
    await fs.promises.chmod(staged, 0o755);
    const out = await runCli(staged, ["--version"], VERSION_TIMEOUT_MS).catch((err: Error) => {
      throw new UpdateError(`the downloaded binary does not run: ${err.message}`);
    });
    if (parseVersionOutput(out.stdout) !== release.version) {
      throw new UpdateError(`the downloaded binary says "${out.stdout.trim()}", not oxabl ${release.version}`);
    }
    await fs.promises.rename(staged, target.path);
  } catch (err) {
    await fs.promises.rm(staged, { force: true });
    throw err;
  }

  const record: InstallRecord = { format: 1, installer: "vscode", path: canonical, version: release.version };
  const tmp = `${target.recordPath}.tmp-${crypto.randomBytes(6).toString("hex")}`;
  try {
    await fs.promises.mkdir(path.dirname(target.recordPath), { recursive: true });
    await fs.promises.writeFile(tmp, `${JSON.stringify(record)}\n`);
    await fs.promises.rename(tmp, target.recordPath);
  } catch (err) {
    await fs.promises.rm(tmp, { force: true });
    throw new UpdateError(
      `installed ${target.path}, but could not write the install record: ${err instanceof Error ? err.message : err}`,
    );
  }
}
