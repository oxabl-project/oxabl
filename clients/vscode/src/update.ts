// Discovery, download and verification of extension updates from GitHub
// Releases.
//
// `http` and `https` carry no `node:` prefix: VS Code's proxy support
// intercepts the plain names in every version.
//
// Like `server.ts`, this module is free of any `vscode` import so the whole
// flow runs under vitest against a local HTTP server. `updates.ts` drives it
// from the extension. Every failure throws an `UpdateError` with a short
// reason; nothing is installed on a failure (fail closed).

import * as crypto from "node:crypto";
import * as fs from "node:fs";
import * as http from "http";
import * as https from "https";
import * as path from "node:path";

export const DEFAULT_API_BASE = "https://api.github.com";
const REPO_PATH = "/repos/oxabl-project/oxabl";
const TAG_PREFIX = "oxabl_vscode-v";
const MAX_REDIRECTS = 5;
const TIMEOUT_MS = 30_000;
const MAX_JSON_BYTES = 5 * 1024 * 1024;
const MAX_CHECKSUM_BYTES = 4096;
const MAX_VSIX_BYTES = 200 * 1024 * 1024;
const MAX_PAGES = 10;

export class UpdateError extends Error {}

/** A reason that leaves the update "not established" without a fault: the check was sound. */
export class NotReadyError extends UpdateError {}

/** Where and how to talk to the release API. */
export interface UpdateEnv {
  /** API base URL without a trailing slash. */
  apiBase: string;
  /** Allow `http:` URLs, and then only for a loopback host. Set only for the test hook. */
  allowHttp: boolean;
  /** Installed extension version, for the `User-Agent` header. */
  installedVersion: string;
  timeoutMs?: number;
}

/** Hosts that may use `http:` and may carry the API test hook. */
export function isLoopbackHost(hostname: string): boolean {
  return hostname === "127.0.0.1" || hostname === "localhost" || hostname === "[::1]";
}

/** A newer release whose assets are complete. */
export interface AvailableUpdate {
  version: string;
  vsixName: string;
  vsixUrl: string;
  sha256Url: string;
}

export type Version = [number, number, number];

/** Parse `X.Y.Z` (numbers only); anything else, a pre-release included, is `undefined`. */
export function parseVersion(text: string): Version | undefined {
  const m = /^(\d+)\.(\d+)\.(\d+)$/.exec(text);
  return m ? [Number(m[1]), Number(m[2]), Number(m[3])] : undefined;
}

export function compareVersions(a: Version, b: Version): number {
  for (let i = 0; i < 3; i++) {
    if (a[i] !== b[i]) {
      return a[i] < b[i] ? -1 : 1;
    }
  }
  return 0;
}

/**
 * The leading `X.Y.Z` of an installed version, which may carry a suffix: a
 * build such as `0.3.0-dev` counts as `0.3.0`.
 */
export function installedVersion(text: string): Version | undefined {
  const m = /^(\d+\.\d+\.\d+)/.exec(text);
  return m ? parseVersion(m[1]) : undefined;
}

const EXTENSION_DIR = /^oxabl-project\.oxabl-(\d+\.\d+\.\d+)$/;

/**
 * The highest version of this extension installed in `dir` (VS Code's
 * extensions folder) that is not marked obsolete, if any. The `.obsolete`
 * file is a JSON object keyed by folder name. A missing file marks nothing;
 * one that cannot be read or parsed leaves the answer open (`undefined`).
 */
export async function highestInstalledVersion(dir: string): Promise<string | undefined> {
  let names: string[];
  try {
    names = await fs.promises.readdir(dir);
  } catch {
    return undefined;
  }
  let obsolete: Record<string, unknown> = {};
  let text: string | undefined;
  try {
    text = await fs.promises.readFile(path.join(dir, ".obsolete"), "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code !== "ENOENT") {
      return undefined;
    }
  }
  if (text !== undefined) {
    try {
      const parsed: unknown = JSON.parse(text);
      if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
        return undefined;
      }
      obsolete = parsed as Record<string, unknown>;
    } catch {
      return undefined;
    }
  }
  let best: Version | undefined;
  for (const name of names) {
    const m = EXTENSION_DIR.exec(name);
    const v = m && !Object.hasOwn(obsolete, name) ? parseVersion(m[1]) : undefined;
    if (v && (!best || compareVersions(v, best) > 0)) {
      best = v;
    }
  }
  return best?.join(".");
}

/** What a successful install leaves in `globalState`, which VS Code keeps per profile. */
export interface PendingReload {
  version: string;
  /** The extensions folder of the host that installed it. */
  dir: string;
}

/** The record as stored, or `undefined` when it is absent or malformed. */
export function readPendingReload(record: unknown): PendingReload | undefined {
  const r = record as { version?: unknown; dir?: unknown } | null | undefined;
  if (typeof r?.version === "string" && typeof r.dir === "string" && parseVersion(r.version)) {
    return { version: r.version, dir: r.dir };
  }
  return undefined;
}

/** A record is spent once the running version is not older than it; a malformed one is spent too. */
export function isPendingReloadSpent(record: unknown, running: string): boolean {
  const pending = readPendingReload(record);
  const current = installedVersion(running);
  return !pending || !current || compareVersions(parseVersion(pending.version)!, current) <= 0;
}

/** What this window knows about an install that waits for a reload. */
export interface PendingState {
  /** The stored record, whatever it holds. */
  record: unknown;
  /** The extensions folder of this host. */
  dir: string;
  /** `highestInstalledVersion(dir)`. */
  onDisk: string | undefined;
}

/**
 * The version that waits for a reload, if any: the profile's record names this
 * host's folder, a version above the running one and at least the newest tag,
 * and the folder holds that version or a higher one that is not obsolete.
 */
export function pendingReloadVersion(
  state: PendingState | undefined,
  running: Version,
  newest: Version,
): string | undefined {
  const pending = readPendingReload(state?.record);
  if (!state || !pending || pending.dir !== state.dir) {
    return undefined;
  }
  const version = parseVersion(pending.version)!;
  const onDisk = state.onDisk === undefined ? undefined : parseVersion(state.onDisk);
  if (
    compareVersions(version, running) > 0 &&
    compareVersions(version, newest) >= 0 &&
    onDisk &&
    compareVersions(onDisk, version) >= 0
  ) {
    return pending.version;
  }
  return undefined;
}

/** The highest `oxabl_vscode-vX.Y.Z` release version among Git refs, if any. */
export function newestVersion(refs: unknown): string | undefined {
  if (!Array.isArray(refs)) {
    throw new UpdateError("unexpected tag list from the release API");
  }
  let best: Version | undefined;
  for (const entry of refs) {
    const ref = (entry as { ref?: unknown } | null)?.ref;
    if (typeof ref !== "string" || !ref.startsWith(`refs/tags/${TAG_PREFIX}`)) {
      continue;
    }
    const v = parseVersion(ref.slice(`refs/tags/${TAG_PREFIX}`.length));
    if (v && (!best || compareVersions(v, best) > 0)) {
      best = v;
    }
  }
  return best?.join(".");
}

/** Parse `sha256sum` output: a 64-hex digest, then optionally the file name. */
export function parseChecksum(text: string, vsixName: string): string {
  const m = /^([0-9a-f]{64})(?:\s+\*?(\S.*?))?$/i.exec(text.trim());
  if (!m) {
    throw new UpdateError("malformed checksum file");
  }
  if (m[2] !== undefined && m[2] !== vsixName) {
    throw new UpdateError("checksum file is for a different file");
  }
  return m[1].toLowerCase();
}

interface Response {
  body: Buffer;
  link: string | undefined;
}

function request(
  url: string,
  env: UpdateEnv,
  accept: string | undefined,
  maxBytes: number,
  redirects = MAX_REDIRECTS,
): Promise<Response> {
  return new Promise((resolve, reject) => {
    let parsed: URL;
    try {
      parsed = new URL(url);
    } catch {
      reject(new UpdateError(`invalid URL ${url}`));
      return;
    }
    const secure = parsed.protocol === "https:";
    const loopbackHttp = parsed.protocol === "http:" && env.allowHttp && isLoopbackHost(parsed.hostname);
    if (!secure && !loopbackHttp) {
      reject(new UpdateError(`refusing ${parsed.protocol} URL from ${parsed.hostname}`));
      return;
    }

    const headers: Record<string, string> = {
      "User-Agent": `oxabl-vscode/${env.installedVersion}`,
    };
    if (accept) {
      headers.Accept = accept;
    }

    const timeoutMs = env.timeoutMs ?? TIMEOUT_MS;
    let timer: NodeJS.Timeout | undefined;
    const req = (secure ? https : http).get(parsed, { headers }, (res) => {
      const status = res.statusCode ?? 0;

      if ([301, 302, 303, 307, 308].includes(status) && res.headers.location) {
        res.resume();
        clearTimeout(timer);
        if (redirects <= 0) {
          reject(new UpdateError(`too many redirects from ${parsed.hostname}`));
          return;
        }
        const next = new URL(res.headers.location, parsed).toString();
        request(next, env, accept, maxBytes, redirects - 1).then(resolve, reject);
        return;
      }
      if (status !== 200) {
        res.resume();
        clearTimeout(timer);
        reject(new UpdateError(`HTTP ${status} from ${parsed.hostname}`));
        return;
      }

      const chunks: Buffer[] = [];
      let size = 0;
      res.on("data", (chunk: Buffer) => {
        size += chunk.length;
        if (size > maxBytes) {
          req.destroy(new UpdateError(`response from ${parsed.hostname} is too large`));
          return;
        }
        chunks.push(chunk);
      });
      res.on("end", () => {
        clearTimeout(timer);
        resolve({ body: Buffer.concat(chunks), link: [res.headers.link ?? []].flat().join(", ") || undefined });
      });
      res.on("error", (err) => {
        clearTimeout(timer);
        reject(err);
      });
    });
    timer = setTimeout(() => {
      req.destroy(new UpdateError(`timeout talking to ${parsed.hostname}`));
    }, timeoutMs);
    req.on("error", (err) => {
      clearTimeout(timer);
      reject(
        err instanceof UpdateError
          ? err
          : new UpdateError(`${(err as NodeJS.ErrnoException).code ?? err.message} from ${parsed.hostname}`),
      );
    });
  });
}

const JSON_ACCEPT = "application/vnd.github+json";

async function getJson(url: string, env: UpdateEnv): Promise<{ json: unknown; next?: string }> {
  const res = await request(url, env, JSON_ACCEPT, MAX_JSON_BYTES);
  let json: unknown;
  try {
    json = JSON.parse(res.body.toString("utf8"));
  } catch {
    throw new UpdateError(`invalid JSON from ${new URL(url).hostname}`);
  }
  const next = res.link ? /<([^>]+)>\s*;\s*rel="next"/.exec(res.link)?.[1] : undefined;
  return { json, next };
}

/** The outcome of a check that reached a verdict. Anything else throws an `UpdateError`. */
export type UpdateCheck =
  | { kind: "current" }
  | { kind: "pending-reload"; version: string }
  | { kind: "available"; update: AvailableUpdate };

/**
 * Look for a newer release than `installed`. `current` means the newest tag is
 * not newer. `pending-reload` means this profile installed a version that waits
 * for a reload (see `pendingReloadVersion`). A missing tag or an incomplete
 * newest release throws: that is "not established", not "up to date".
 */
export async function findUpdate(
  installed: string,
  env: UpdateEnv,
  log: (message: string) => void,
  pending?: PendingState,
): Promise<UpdateCheck> {
  const current = installedVersion(installed);
  if (!current) {
    throw new UpdateError(`cannot parse the installed version ${installed}`);
  }

  let refs: unknown[] = [];
  let url: string | undefined = `${env.apiBase}${REPO_PATH}/git/matching-refs/tags/${TAG_PREFIX}`;
  for (let page = 0; url && page < MAX_PAGES; page++) {
    const { json, next } = await getJson(url, env);
    if (!Array.isArray(json)) {
      throw new UpdateError("unexpected tag list from the release API");
    }
    refs = refs.concat(json);
    url = next;
  }
  if (url) {
    throw new UpdateError("tag list truncated");
  }

  const newest = newestVersion(refs);
  if (!newest) {
    throw new NotReadyError("no extension releases found");
  }
  log(`installed v${installed}, newest tag v${newest}`);
  if (compareVersions(parseVersion(newest)!, current) <= 0) {
    return { kind: "current" };
  }
  const waiting = pendingReloadVersion(pending, current, parseVersion(newest)!);
  if (waiting) {
    log(`v${waiting} is installed and waits for a reload`);
    return { kind: "pending-reload", version: waiting };
  }

  const tag = `${TAG_PREFIX}${newest}`;
  const notComplete = new NotReadyError(`release v${newest} is not complete yet`);
  const { json: release } = await getJson(`${env.apiBase}${REPO_PATH}/releases/tags/${tag}`, env);
  const r = release as { draft?: unknown; prerelease?: unknown; assets?: unknown } | null;
  if (!r || typeof r !== "object" || !Array.isArray(r.assets)) {
    throw new UpdateError("unexpected release JSON");
  }
  if (r.draft !== false || r.prerelease !== false) {
    log(`release ${tag} is a draft or a pre-release`);
    throw notComplete;
  }

  const vsixName = `oxabl-vscode-${newest}.vsix`;
  const urlOf = (name: string): string | undefined => {
    const asset = (r.assets as { name?: unknown; browser_download_url?: unknown }[]).find(
      (a) => a?.name === name,
    );
    return typeof asset?.browser_download_url === "string" ? asset.browser_download_url : undefined;
  };
  const vsixUrl = urlOf(vsixName);
  const sha256Url = urlOf(`${vsixName}.sha256`);
  if (!vsixUrl || !sha256Url) {
    log(`release ${tag} has no complete assets yet`);
    throw notComplete;
  }
  return { kind: "available", update: { version: newest, vsixName, vsixUrl, sha256Url } };
}

/**
 * Download the VSIX and its checksum, verify the digest, and write the VSIX
 * under `dir`. Returns the path. Nothing is written when verification fails.
 */
export async function downloadVerified(
  update: AvailableUpdate,
  env: UpdateEnv,
  dir: string,
): Promise<string> {
  const sumFile = await request(update.sha256Url, env, undefined, MAX_CHECKSUM_BYTES);
  const expected = parseChecksum(sumFile.body.toString("utf8"), update.vsixName);
  const vsix = await request(update.vsixUrl, env, undefined, MAX_VSIX_BYTES);
  const actual = crypto.createHash("sha256").update(vsix.body).digest("hex");
  if (actual !== expected) {
    throw new UpdateError("checksum mismatch");
  }
  await fs.promises.mkdir(dir, { recursive: true });
  const file = path.join(dir, update.vsixName);
  await fs.promises.writeFile(file, vsix.body);
  return file;
}
